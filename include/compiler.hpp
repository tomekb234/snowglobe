#ifndef COMPILER_HPP
#define COMPILER_HPP

#include "ast.hpp"
#include "program.hpp"
#include "diagcol.hpp"
#include <utility>
#include <optional>
#include <tuple>
#include <variant>
#include <memory>
#include <vector>
#include <functional>
#include <unordered_map>

namespace sg {
    using std::move;
    using std::pair;
    using std::tuple;
    using std::variant;
    using std::make_unique;
    using std::monostate;
    using std::vector;
    using std::function;
    using std::unordered_map;

    template<typename T>
    using ptr = std::unique_ptr<T>;

    template<typename T>
    using cref = std::reference_wrapper<const T>;

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

        struct compilation_error { };

        prog::program& prog;
        diagnostic_collector& diags;
        vector<prog::global_var> consts;
        unordered_map<string, global_name> global_names;
        unordered_map<prog::global_index, prog::global_index> func_wrappers;

        friend class conversion_compiler;
        friend class function_compiler;

        public:

        compiler(prog::program& prog, diagnostic_collector& diags) : prog(prog), diags(diags) { }

        bool compile(const ast::program& ast);

        private:

        // Utilities

        template<typename T>
        [[noreturn]] void error(T&& diag, location loc) {
            diag.loc = { loc };
            diags.add(make_unique<T>(move(diag)));
            throw compilation_error();
        }

        template<typename T>
        void warning(T&& diag, location loc) {
            diag.loc = { loc };
            diags.add(make_unique<T>(move(diag)));
        }

        const global_name& get_global_name(string name, location loc);
        const global_name& get_global_name(string name, vector<global_name_kind> expected_kinds, location loc);
        const global_name& get_global_type(string name, bool allow_uncompiled, location loc);
        prog::global_index get_global_func_wrapper(prog::global_index func_index);

        vector<cref<ast::expr>> order_args(
                vector<cref<ast::expr_marked>> asts,
                optional<function<size_t(string, location)>> arg_with_name,
                optional<size_t> expected_count,
                location loc);

        // Globals

        prog::global_var compile_global_var(const ast::var_def& ast);
        prog::global_func declare_global_func(const ast::func_def& ast);
        prog::struct_type declare_struct_type(const ast::struct_def& ast);
        prog::enum_type declare_enum_type(const ast::enum_def& ast);

        void compile_global_func(const ast::func_def& ast, prog::global_func& func);
        void compile_struct_type(const ast::struct_def& ast, prog::struct_type& st);
        void compile_enum_type(const ast::enum_def& ast, prog::enum_type& en);

        prog::global_func make_global_func_wrapper(prog::global_index func_index);

        // Constants

        pair<prog::constant, prog::type> compile_const(const ast::expr& ast);
        pair<prog::constant, prog::type> compile_const_tuple(vector<cref<ast::expr_marked>> asts, location loc);
        pair<prog::constant, prog::type> compile_const_array(vector<cref<ast::expr_marked>> asts, location loc);
        pair<prog::constant, prog::type> compile_const_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, location loc);
        pair<prog::constant, prog::type> compile_const_name(string name, location loc);
        pair<prog::constant, prog::type> compile_const_variant_name(string name, string variant_name, location loc);
        pair<prog::constant, prog::type> compile_const_literal(const ast::literal_expr& ast);

        pair<prog::constant, prog::number_type> compile_int_token(const ast::int_token& ast);
        pair<prog::constant, prog::number_type> compile_float_token(const ast::float_token& ast);

        size_t compile_const_size(const ast::const_int& ast);

        prog::constant convert_const(prog::constant value, const prog::type& type, const prog::type& new_type, location loc);

        // Types

        prog::type compile_type(const ast::type& ast, bool allow_uncompiled);
        prog::type_local compile_type_local(const ast::type_local& ast, bool allow_uncompiled);
        prog::type compile_user_type(const ast::type& ast, bool allow_uncompiled);
        prog::number_type compile_number_type(const ast::number_type& ast);
        vector<prog::type> compile_tuple_type(vector<cref<ast::type>> asts, bool allow_uncompiled);
        prog::array_type compile_array_type(const ast::array_type& ast, bool allow_uncompiled);
        prog::ptr_type compile_ptr_type(const ast::ptr_type& ast);
        prog::type_pointed compile_type_pointed(const ast::type_pointed& ast);
        prog::inner_ptr_type compile_inner_ptr_type(const ast::inner_ptr_type& ast);
        prog::func_type compile_func_type(const ast::func_type& ast, bool allow_uncompiled);
        prog::func_with_ptr_type compile_func_with_ptr_type(const ast::func_with_ptr_type& ast, bool allow_uncompiled);

        bool type_copyable(const prog::type& type);
        bool type_trivial(const prog::type& type);

        prog::type common_supertype(const prog::type& type_a, const prog::type& type_b, location loc);
    };

    class conversion_compiler {
        compiler& clr;
        function<prog::reg_index()> new_reg;
        function<void(prog::instr&&)> add_instr;

        public:

        conversion_compiler(compiler& clr, decltype(new_reg) new_reg, decltype(add_instr) add_instr) : clr(clr), new_reg(new_reg), add_instr(add_instr) { }

        prog::reg_index convert(prog::reg_index value, const prog::type& type, const prog::type& new_type, bool confined, location loc);
        prog::reg_index convert(prog::reg_index value, const prog::type& type, const prog::type& new_type, location loc);
        prog::reg_index convert(prog::reg_index value, const prog::type_local& type, const prog::type_local& new_type, location loc);
        prog::reg_index convert(prog::reg_index value, const prog::type_local& type, const prog::type& new_type, location loc);
        optional<prog::reg_index> try_convert(prog::reg_index value, const prog::type& type, const prog::type& new_type, bool confined);

        private:

        optional<prog::reg_index> try_convert_ptr_kind(prog::reg_index value, prog::ptr_type::kind_t kind, prog::ptr_type::kind_t new_kind, bool confined);
        optional<prog::reg_index> try_convert_ptr_target(prog::reg_index value, const prog::type_pointed& type, const prog::type_pointed& new_type);
    };

    class function_compiler {
        typedef unsigned char var_state;
        typedef size_t frame_index;

        struct variable {
            optional<string> name;
            prog::type_local type;
            var_state state;
            size_t outside_loop;
            size_t outside_confinement;
        };

        struct frame {
            vector<prog::instr> instrs;
            vector<prog::var_index> vars;
            vector<string> var_names;
            bool loop;
            vector<function<void()>> cleanup_actions;
        };

        struct lvalue {
            enum {
                IGNORED,
                VAR,
                GLOBAL_VAR,
                TUPLE,
                ARRAY,
                STRUCT,
                ENUM_VARIANT
            };

            variant<
                monostate, // IGNORE
                prog::var_index, // VAR
                prog::global_index, // GLOBAL_VAR
                vector<ptr<lvalue>>, // TUPLE
                vector<ptr<lvalue>>, // ARRAY
                pair<prog::global_index, vector<ptr<lvalue>>>, // STRUCT
                tuple<prog::global_index, prog::variant_index, vector<ptr<lvalue>>> // ENUM_VARIANT
            > value;
        };

        static constexpr var_state VAR_INITIALIZED = 1 << 0;
        static constexpr var_state VAR_UNINITIALIZED = 1 << 1;
        static constexpr var_state VAR_MOVED_OUT = 1 << 2;

        compiler& clr;
        prog::global_func& func;
        conversion_compiler conv_clr;
        prog::reg_index reg_counter = 0;
        vector<variable> vars;
        vector<frame> frames;
        unordered_map<string, vector<prog::var_index>> var_names;
        bool returned = false;

        prog::reg_index new_reg();
        prog::reg_index unit_reg();
        void add_instr(prog::instr&& instr);

        public:

        function_compiler(compiler& clr, prog::global_func& func) : clr(clr), func(func), conv_clr(clr,
                    [this] () { return new_reg(); },
                    [this] (prog::instr&& instr) { add_instr(move(instr)); }) { }

        void compile(const ast::func_def& ast);

        private:

        // Frames

        void push_frame();
        void push_loop_frame();
        void push_confining_frame();
        prog::instr_block pop_frame();
        prog::instr_block pop_loop_frame();
        prog::instr_block pop_confining_frame();

        // Variables

        prog::var_index add_var(prog::type_local&& type);
        prog::var_index add_var(string name, prog::type_local&& type);
        optional<prog::var_index> try_get_var(string name);
        vector<var_state> backup_var_states();
        void restore_var_states(const vector<var_state>& states);
        void merge_var_states(const vector<var_state>& states);

        // Instructions

        void add_instrs(prog::instr_block&& block);
        void add_copy_instrs(prog::reg_index value, const prog::type& type);
        void add_delete_instrs(prog::reg_index value, const prog::type& type);
        void add_delete_instrs(prog::reg_index value, const prog::type_local& type);

        // Utilities

        prog::var_index get_var(string name, location loc);
        void delete_var(prog::var_index index, location loc);
        void add_cleanup_action(function<void()> cleanup_action);
        void cleanup_frame(frame_index index, location loc);
        void add_return(prog::reg_index value, location loc);
        void add_break(location loc);
        void add_continue(location loc);
        prog::branch_instr make_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch);
        void add_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch);
        void add_loop(function<prog::reg_index()> head, function<void()> true_branch, function<void()> false_branch, function<void()> end);

        // Statements

        void compile_stmt_block(const ast::stmt_block& ast, bool cleanup);
        void compile_stmt(const ast::stmt& ast);
        void compile_locally_block_stmt(const ast::locally_block_stmt& ast);
        void compile_swap_stmt(const ast::swap_stmt& ast);
        void compile_swap_block_stmt(const ast::swap_block_stmt& ast);
        void compile_if_stmt_branches(const ast::if_stmt& ast, size_t branch_index);
        void compile_match_stmt(const ast::match_stmt& ast);
        void compile_match_stmt_branches(const ast::match_stmt& ast, prog::reg_index value, const prog::type_local& type, size_t branch_index);
        void compile_while_stmt(const ast::while_stmt& ast);
        void compile_for_stmt(const ast::for_stmt& ast);
        void compile_assignment(const lvalue& lval, prog::reg_index value, const prog::type_local& type, location loc);

        // Expressions

        pair<prog::reg_index, prog::type_local> compile_expr(const ast::expr& ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_var_read(prog::var_index var_index, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_confinement(prog::var_index var_index, location loc);
        pair<prog::reg_index, prog::type_local> compile_global_name(string name, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_variant_name(string name, string variant_name, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_return(optional<cref<ast::expr>> ast, location loc);
        pair<prog::reg_index, prog::type_local> compile_tuple(vector<cref<ast::expr_marked>> asts, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_array(vector<cref<ast::expr_marked>> asts, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_unary_operation(const ast::unary_operation_expr& ast);
        pair<prog::reg_index, prog::type_local> compile_binary_operation(const ast::binary_operation_expr& ast);
        pair<prog::reg_index, prog::type_local> compile_numeric_cast(const ast::numeric_cast_expr& ast);
        pair<prog::reg_index, prog::type_local> compile_conditional(const ast::conditional_expr& ast);

        lvalue compile_left_expr(const ast::expr& ast, optional<cref<prog::type_local>> implicit_type);
        lvalue compile_left_tuple(vector<cref<ast::expr_marked>> asts, optional<cref<prog::type_local>> implicit_type, location loc);
        lvalue compile_left_array(vector<cref<ast::expr_marked>> asts, optional<cref<prog::type_local>> implicit_type, location loc);
        lvalue compile_left_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, optional<cref<prog::type_local>> implicit_type, location loc);

        tuple<vector<cref<ast::expr>>, vector<prog::reg_index>, vector<prog::type>, bool> compile_args(
                vector<cref<ast::expr_marked>> asts,
                optional<function<size_t(string, location)>> arg_with_name,
                optional<size_t> expected_count,
                bool confined,
                location loc);

        vector<prog::reg_index> compile_call_args(
                vector<cref<ast::expr_marked>> asts,
                const prog::func_type& ftype,
                optional<cref<prog::global_func>> func,
                location loc);
    };
}

#endif
