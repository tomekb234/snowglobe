#ifndef COMPILER_HPP
#define COMPILER_HPP

#include "ast.hpp"
#include "program.hpp"
#include "diagcol.hpp"
#include <optional>
#include <unordered_map>
#include <utility>
#include <memory>
#include <functional>

namespace sg {
    using std::unordered_map;
    using std::pair;
    using std::move;
    using std::make_unique;
    using std::vector;
    using std::reference_wrapper;
    using std::variant;
    using std::function;

    enum global_name_kind {
        VARIABLE,
        CONSTANT,
        FUNCTION,
        STRUCT,
        ENUM
    };

    class compiler {
        struct global_name {
            global_name_kind kind;
            prog::global_index index;
            bool compiled;
        };

        prog::program& program;
        diagnostic_collector& diags;
        unordered_map<string, global_name> global_names;
        vector<prog::global_var> constants;

        friend class conversion_compiler;
        friend class function_compiler;

        public:

        compiler(prog::program& program, diagnostic_collector& diags) : program(program), diags(diags) { }

        bool compile(const ast::program& ast);

        private:

        // Utilities

        template<typename T>
        [[noreturn]] void error(T&& diag, location loc) {
            diag.loc = { loc };
            diags.add(make_unique<T>(move(diag)));
            throw 0;
        }

        template<typename T>
        [[noreturn]] void error(T&& diag, const ast::node& ast) {
            error(move(diag), ast.loc);
        }

        template<typename T>
        void warning(T&& diag, location loc) {
            diag.loc = { loc };
            diags.add(make_unique<T>(move(diag)));
        }

        template<typename T>
        void warning(T&& diag, const ast::node& ast) {
            warning(move(diag), ast.loc);
        }

        global_name& get_global_name(const ast::node& ast, const string& name, bool allow_uncompiled = false);
        global_name& get_global_name(const ast::node& ast, const string& name, global_name_kind expected_kind, bool allow_uncompiled = false);

        // Global variables, structs and enums

        prog::global_var compile_global_var(const ast::var_def& ast);
        prog::struct_type declare_struct_type(const ast::struct_def& ast);
        prog::enum_type declare_enum_type(const ast::enum_def& ast);
        void compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type);
        void compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type);

        // Functions

        prog::global_func declare_global_func(const ast::func_def& ast);
        void compile_global_func(const ast::func_def& ast, prog::global_func& global_func);

        // Constants

        pair<prog::constant, prog::type> compile_constant(const ast::expr& ast);
        pair<prog::constant, prog::type> compile_constant_tuple(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items_ast);
        pair<prog::constant, prog::type> compile_constant_array(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items_ast);
        pair<prog::constant, prog::type> compile_constant_application(const ast::node& ast, const ast::expr& receiver_ast, const vector<ast::ptr<ast::expr_marked>>& args_ast);
        pair<prog::constant, prog::type> compile_constant_name(const ast::node& ast, const string& name);
        pair<prog::constant, prog::type> compile_constant_variant_name(const ast::node& ast, const string& name, const string& variant_name);
        pair<prog::constant, prog::type> compile_constant_literal(const ast::literal_expr& ast);
        pair<prog::constant, prog::primitive_type> compile_int_token(const ast::int_token& ast);
        pair<prog::constant, prog::primitive_type> compile_float_token(const ast::float_token& ast);
        size_t compile_constant_size(const ast::const_int& ast);
        prog::constant convert_constant(const ast::node& ast, prog::constant value, const prog::type& type, const prog::type& new_type);

        // Types

        prog::type compile_type(const ast::type& ast, bool allow_uncompiled = false);
        prog::type_local compile_type_local(const ast::type_local& ast, bool allow_uncompiled = false);
        prog::type compile_user_type(const ast::type& ast, bool allow_uncompiled = false);
        prog::primitive_type compile_primitive_type(const ast::primitive_type& ast);
        vector<prog::ptr<prog::type>> compile_tuple_type(const vector<ast::ptr<ast::type>>& ast, bool allow_uncompiled = false);
        prog::array_type compile_array_type(const ast::array_type& ast, bool allow_uncompiled = false);
        prog::ptr_type compile_ptr_type(const ast::ptr_type& ast);
        prog::type_pointed compile_type_pointed(const ast::type_pointed& ast);
        prog::inner_ptr_type compile_inner_ptr_type(const ast::inner_ptr_type& ast);
        prog::func_type compile_func_type(const ast::func_type& ast, bool allow_uncompiled = false);
        prog::func_with_ptr_type compile_func_with_ptr_type(const ast::func_with_ptr_type& ast, bool allow_uncompiled = false);

        bool type_copyable(const prog::type& type);
        bool type_trivially_copyable(const prog::type& type);

        // Conversions

        prog::type common_supertype(const ast::node& ast, const prog::type& type1, const prog::type& type2);
    };

    class conversion_compiler {
        compiler& clr;
        function<prog::reg_index()> new_reg;
        function<void(prog::instr&&)> add_instr;

        public:

        conversion_compiler(compiler& clr, function<prog::reg_index()> new_reg, function<void(prog::instr&&)> add_instr) : clr(clr), new_reg(new_reg), add_instr(add_instr) { }

        prog::reg_index convert(const ast::node& ast, const prog::type& type1, const prog::type& type2, prog::reg_index value);
        prog::reg_index convert(const ast::node& ast, const prog::type_local& type1, const prog::type_local& type2, prog::reg_index value);
        prog::reg_index convert(const ast::node& ast, const prog::type_local& type1, const prog::type& type2, prog::reg_index value);
        prog::reg_index convert_call(const ast::node& ast, const prog::type_local& type1, const prog::type_local& type2, prog::reg_index value);
        optional<prog::reg_index> try_convert(const prog::type& type1, const prog::type& type2, bool confined, prog::reg_index value);

        private:

        optional<prog::reg_index> try_convert_ptr_kind(prog::ptr_type::kind_t kind1, prog::ptr_type::kind_t kind2, bool confined, prog::reg_index value);
        optional<prog::reg_index> try_convert_ptr_target(const prog::type_pointed& type1, const prog::type_pointed& type2, prog::reg_index value);
    };

    class function_compiler {
        struct variable {
            prog::ptr<prog::type_local> tp;
            // TODO
        };

        struct frame {
            vector<prog::instr> instrs;
            vector<string> vars;
            bool always_returns;
        };

        compiler& clr;
        prog::global_func& func;
        conversion_compiler conv_clr;
        vector<frame> frames;
        prog::reg_index reg_counter;
        vector<variable> vars;
        unordered_map<string, vector<prog::var_index>> var_names;

        prog::reg_index new_reg();
        void add_instr(prog::instr&& instr);

        public:

        function_compiler(compiler& clr, prog::global_func& func) : clr(clr), func(func), conv_clr(clr, [this] () { return new_reg(); }, [this] (prog::instr&& instr) { add_instr(move(instr)); }) { }

        void compile(const ast::func_def& ast);

        private:

        struct lvalue {
            enum {
                LOCAL_VAR,
                GLOBAL_VAR
            };

            variant<
                prog::var_index, // LOCAL_VAR
                prog::global_index // GLOBAL_VAR
            > value;
        };

        void push_frame();
        void pop_frame();
        prog::var_index add_var(string name, prog::ptr<prog::type_local> type);
        optional<prog::var_index> get_var(string name);

        void add_cleanup_instrs(bool all_frames = false);
        void add_return_instr(const optional<ast::ptr<ast::expr>>& ast);
        void compile_stmt_block(const ast::stmt_block& ast, bool cleanup = true);
        lvalue compile_left_expr(const ast::expr& ast, optional<reference_wrapper<const prog::type_local>> implicit_type);
        pair<prog::reg_index, prog::type_local> compile_expr(const ast::expr& ast);
    };
}

#endif
