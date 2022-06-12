#ifndef COMPILER_HPP
#define COMPILER_HPP

#include "ast.hpp"
#include "program.hpp"
#include "diagcol.hpp"
#include <optional>
#include <unordered_map>
#include <utility>
#include <tuple>
#include <memory>
#include <functional>

namespace sg {
    using std::unordered_map;
    using std::pair;
    using std::tuple;
    using std::move;
    using std::make_unique;
    using std::vector;
    using std::variant;
    using std::function;

    template<typename T>
    using ref = std::reference_wrapper<T>;

    typedef vector<ast::ptr<ast::expr_marked>> args_ast_vector;
    typedef function<size_t(const ast::node&, string)> arg_with_name_function;

    enum struct global_name_kind {
        VAR,
        CONST,
        FUNC,
        STRUCT,
        ENUM
    };

    class compiler {
        struct global_name {
            global_name_kind kind;
            prog::global_index index;
            bool compiled;
        };

        prog::program& prog;
        diagnostic_collector& diags;
        unordered_map<string, global_name> global_names;
        vector<prog::global_var> consts;

        friend class conversion_compiler;
        friend class function_compiler;

        public:

        compiler(prog::program& prog, diagnostic_collector& diags) : prog(prog), diags(diags) { }

        bool compile(const ast::program& ast);

        private:

        struct compiler_error { };

        // Utilities

        template<typename T>
        [[noreturn]] void error(T&& diag, location loc) {
            diag.loc = { loc };
            diags.add(make_unique<T>(move(diag)));
            throw compiler_error();
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

        global_name& get_global_name(const ast::node& ast, const string& name, bool allow_uncompiled_types = false);
        global_name& get_global_name(const ast::node& ast, const string& name, global_name_kind expected_kind, bool allow_uncompiled_types = false);

        vector<ref<const ast::expr>> order_args(
                const ast::node& ast,
                const args_ast_vector& args_ast,
                optional<arg_with_name_function> arg_with_name,
                optional<size_t> expected_count);

        // Globals

        prog::global_var compile_global_var(const ast::var_def& ast);
        prog::global_func declare_global_func(const ast::func_def& ast);
        prog::struct_type declare_struct_type(const ast::struct_def& ast);
        prog::enum_type declare_enum_type(const ast::enum_def& ast);
        void compile_global_func(const ast::func_def& ast, prog::global_func& func);
        void compile_struct_type(const ast::struct_def& ast, prog::struct_type& st);
        void compile_enum_type(const ast::enum_def& ast, prog::enum_type& en);

        // Constants

        pair<prog::constant, prog::type> compile_const(const ast::expr& ast);
        pair<prog::constant, prog::type> compile_const_tuple(const ast::node& ast, const args_ast_vector& args_ast);
        pair<prog::constant, prog::type> compile_const_array(const ast::node& ast, const args_ast_vector& args_ast);
        pair<prog::constant, prog::type> compile_const_application(const ast::node& ast, const ast::expr& receiver_ast, const args_ast_vector& args_ast);
        pair<prog::constant, prog::type> compile_const_name(const ast::node& ast, const string& name);
        pair<prog::constant, prog::type> compile_const_variant_name(const ast::node& ast, const string& name, const string& variant_name);
        pair<prog::constant, prog::type> compile_const_literal(const ast::literal_expr& ast);
        pair<prog::constant, prog::primitive_type> compile_int_token(const ast::int_token& ast);
        pair<prog::constant, prog::primitive_type> compile_float_token(const ast::float_token& ast);
        size_t compile_const_size(const ast::const_int& ast);
        prog::constant convert_const(const ast::node& ast, prog::constant value, const prog::type& type, const prog::type& new_type);

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

        prog::type common_supertype(const ast::node& ast, const prog::type& type_a, const prog::type& type_b);
    };

    class conversion_compiler {
        compiler& clr;
        function<prog::reg_index()> new_reg;
        function<void(prog::instr&&)> add_instr;

        public:

        conversion_compiler(compiler& clr, decltype(new_reg) new_reg, decltype(add_instr) add_instr) : clr(clr), new_reg(new_reg), add_instr(add_instr) { }

        prog::reg_index convert(const ast::node& ast, prog::reg_index value, const prog::type& type, const prog::type& new_type, bool confined = false);
        prog::reg_index convert(const ast::node& ast, prog::reg_index value, const prog::type_local& type, const prog::type_local& new_type);
        prog::reg_index convert(const ast::node& ast, prog::reg_index value, const prog::type_local& type, const prog::type& new_type);
        optional<prog::reg_index> try_convert(prog::reg_index value, const prog::type& type, const prog::type& new_type, bool confined);

        private:

        optional<prog::reg_index> try_convert_ptr_kind(prog::reg_index value, prog::ptr_type::kind_t kind, prog::ptr_type::kind_t new_kind, bool confined);
        optional<prog::reg_index> try_convert_ptr_target(prog::reg_index value, const prog::type_pointed& type, const prog::type_pointed& new_type);
    };

    class function_compiler {
        struct var_state {
            bool initialized;
            bool uninitialized;
            bool moved_out;
        };

        static constexpr var_state VAR_INITIALIZED = { true, false, false };
        static constexpr var_state VAR_UNINITIALIZED = { false, true, false };
        static constexpr var_state VAR_MOVED_OUT = { false, false, true };

        struct frame {
            vector<prog::instr> instrs;
            vector<string> vars;
        };

        compiler& clr;
        prog::global_func& func;
        conversion_compiler conv_clr;
        vector<frame> frames;
        prog::reg_index reg_counter = 0;
        vector<prog::ptr<prog::type_local>> var_types;
        vector<var_state> var_states;
        unordered_map<string, vector<prog::var_index>> var_names;
        bool returned = false;

        prog::reg_index new_reg();
        void add_instr(prog::instr&& instr);

        public:

        function_compiler(compiler& clr, prog::global_func& func) : clr(clr), func(func), conv_clr(clr,
                    [this] () { return new_reg(); },
                    [this] (prog::instr&& instr) { add_instr(move(instr)); }) { }

        void compile(const ast::func_def& ast);

        private:

        struct lvalue {
            enum {
                VAR,
                GLOBAL_VAR
            };

            variant<
                prog::var_index, // VAR
                prog::global_index // GLOBAL_VAR
            > value;
        };

        void push_frame();
        prog::instr_block pop_frame();

        prog::var_index add_var(string name, prog::ptr<prog::type_local> type);
        optional<prog::var_index> get_var(string name);
        void restore_var_states(const vector<var_state>& states);
        void merge_var_states(const vector<var_state>& states);

        prog::reg_index add_copy_instrs(prog::reg_index value, const prog::type& type);
        void add_delete_instrs(prog::reg_index value, const prog::type_local& type);
        void add_cleanup_instrs(bool all_frames = false);

        void compile_stmt_block(const ast::stmt_block& ast, bool cleanup = true);
        void compile_return(const optional<ast::ptr<ast::expr>>& ast);
        prog::branch_instr compile_if_stmt_branches(const ast::if_stmt& ast, size_t index = 0);

        lvalue compile_left_expr(const ast::expr& ast, optional<ref<const prog::type_local>> implicit_type);

        pair<prog::reg_index, prog::type_local> compile_expr(const ast::expr& ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_tuple(const ast::node& ast, const args_ast_vector& args_ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_array(const ast::node& ast, const args_ast_vector& args_ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_application(const ast::node& ast, const ast::expr& receiver_ast, const args_ast_vector& args_ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_unary_operation(const ast::unary_operation_expr& ast);

        tuple<vector<ref<const ast::expr>>, vector<prog::reg_index>, vector<prog::type>, bool> compile_args(
                const ast::node& ast,
                const args_ast_vector& args_ast,
                optional<arg_with_name_function> arg_with_name,
                optional<size_t> expected_number,
                bool confined);

        vector<prog::reg_index> compile_call_args(
                const ast::node& ast,
                const args_ast_vector& args_ast,
                const prog::func_type& ftype,
                optional<ref<const prog::global_func>> func);
    };
}

#endif
