#ifndef COMPILER_HPP
#define COMPILER_HPP

#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include <optional>
#include <unordered_map>
#include <utility>
#include <memory>

namespace sg {
    using std::optional;
    using std::unordered_map;
    using std::pair;
    using std::move;
    using std::make_unique;

    class compiler {
        struct global_name {
            enum kind_t {
                VARIABLE,
                CONSTANT,
                FUNCTION,
                STRUCT,
                ENUM
            } kind;

            size_t index;
            bool compiled;
        };

        diagnostic_collector& diags;
        prog::program program;
        unordered_map<string, global_name> global_names;
        vector<prog::global_var> constants;

        template<typename T>
        [[noreturn]] void error(T&& diag, const ast::node& ast) {
            diag.loc = { ast.loc };
            diags.add(make_unique<T>(move(diag)));
            throw 0;
        }

        void compile_program(const ast::program& ast);

        public:

        compiler(diagnostic_collector& diags) : diags(diags) { }

        optional<prog::program> compile(const ast::program& ast);

        private:

        // Utilities

        global_name& get_global_name(const ast::node& ast, const string& name, bool allow_uncompiled = false);
        global_name& get_global_name(const ast::node& ast, const string& name, global_name::kind_t expected_kind, bool allow_uncompiled = false);

        // Variables

        prog::global_var compile_global_var(const ast::var_def& ast);

        // Functions

        prog::global_func declare_global_func(const ast::func_def& ast);
        void compile_global_func(const ast::func_def& ast, prog::global_func& global_func);

        // Structs

        prog::struct_type declare_struct_type(const ast::struct_def& ast);
        void compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type);

        // Enums

        prog::enum_type declare_enum_type(const ast::enum_def& ast);
        void compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type);
        bool has_uncompiled_type(const prog::type& type);

        // Constants

        pair<prog::constant, prog::type> compile_constant_expr(const ast::expr& ast);
        pair<prog::constant, prog::type> compile_constant_tuple(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items_ast);
        pair<prog::constant, prog::type> compile_constant_array(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items_ast);
        pair<prog::constant, prog::type> compile_constant_application(const ast::node& ast, const ast::expr& receiver_ast, const vector<ast::ptr<ast::expr_marked>>& args_ast);
        pair<prog::constant, prog::type> compile_constant_name(const ast::node& ast, const string& name);
        pair<prog::constant, prog::type> compile_constant_variant_name(const ast::node& ast, const string& name, const string& variant_name);
        pair<prog::constant, prog::type> compile_constant_literal(const ast::literal_expr& ast);
        pair<prog::constant, prog::primitive_type> compile_int_token(const ast::int_token& ast);
        pair<prog::constant, prog::primitive_type> compile_float_token(const ast::float_token& ast);
        pair<prog::constant, prog::type> compile_constant_ptr(const ast::node& ast, const string& name);
        pair<prog::constant, prog::type> compile_constant_sized_array(const ast::sized_array_expr& ast);
        pair<prog::constant, prog::type> compile_constant_length(const ast::node& ast, const ast::expr& target_ast);
        pair<prog::constant, prog::type> compile_constant_extract(const ast::extract_expr& ast);
        pair<prog::constant, prog::type> compile_constant_ptr_extract(const ast::ptr_extract_expr& ast);
        size_t compile_constant_size(const ast::const_integer& ast);
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

        // Subtyping

        bool subtype(const prog::type& type1, const prog::type& type2, bool confined = false);
        bool ptr_target_subtype(const prog::type_pointed& type1, const prog::type_pointed& type2);
        bool func_subtype(const prog::func_type& func1, const prog::func_type& func2);
        bool ptr_kind_trivial(prog::ptr_type::kind_t kind, bool confined = false);
        bool ptr_subkind(prog::ptr_type::kind_t kind1, prog::ptr_type::kind_t kind2, bool confined = false);
        prog::type common_supertype(const ast::node& ast, const prog::type& type1, const prog::type& type2, bool confined = false);
    };
}

#endif
