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
            enum {
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

        prog::global_var compile_global_var(const ast::var_def& ast);

        prog::global_func declare_global_func(const ast::func_def& ast);
        prog::struct_type declare_struct_type(const ast::struct_def& ast);
        prog::enum_type declare_enum_type(const ast::enum_def& ast);

        void compile_global_func(const ast::func_def& ast, prog::global_func& global_func);
        void compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type);
        void compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type);

        pair<prog::constant, prog::type> compile_constant_expr(const ast::expr& ast);
        pair<prog::constant, prog::type> compile_constant_tuple(const vector<ast::ptr<ast::expr_marked>>& asts, const ast::node& ast);
        pair<prog::constant, prog::type> compile_constant_array(const vector<ast::ptr<ast::expr_marked>>& asts, const ast::node& ast);
        pair<prog::constant, prog::type> compile_constant_name(const string& name, const ast::node& ast);
        pair<prog::constant, prog::type> compile_constant_literal(const ast::const_expr& ast);
        pair<prog::constant, prog::primitive_type> compile_int_token(const ast::int_token& ast);
        pair<prog::constant, prog::primitive_type> compile_float_token(const ast::float_token& ast);

        prog::type compile_type(const ast::type& ast);
        prog::primitive_type compile_primitive_type(const ast::primitive_type& ast);
        vector<prog::ptr<prog::type>> compile_tuple_type(const vector<ast::ptr<ast::type>>& ast);
        prog::array_type compile_array_type(const ast::array_type& ast);
        prog::ptr_type compile_ptr_type(const ast::ptr_type& ast);
        prog::type_pointed compile_type_pointed(const ast::type_pointed& ast);
        prog::inner_ptr_type compile_inner_ptr_type(const ast::inner_ptr_type& ast);
        prog::func_type compile_func_type(const ast::func_type& ast);
        prog::type_local compile_type_local(const ast::type_local& ast);
        prog::func_with_ptr_type compile_func_with_ptr_type(const ast::func_with_ptr_type& ast);

        prog::constant convert_constant(const ast::node& ast, const prog::constant& constant, const prog::type& from_tp, const prog::type& to_tp);

        bool subtype(const prog::type& type1, const prog::type& type2, bool confined = false);
        bool ptr_target_subtype(const prog::type_pointed& type1, const prog::type_pointed& type2);
        bool func_subtype(const prog::func_type& func1, const prog::func_type& func2);
        bool ptr_kind_trivial(prog::ptr_type::kind_t kind, bool confined = false);
        bool ptr_subkind(prog::ptr_type::kind_t kind1, prog::ptr_type::kind_t kind2, bool confined = false);

        prog::type common_supertype(const ast::node& ast, const prog::type& type1, const prog::type& type2);

        prog::constant copy_constant(const prog::constant& source);
        prog::type copy_type(const prog::type& source);
        prog::array_type copy_array_type(const prog::array_type& source);
        prog::ptr_type copy_ptr_type(const prog::ptr_type& source);
        prog::inner_ptr_type copy_inner_ptr_type(const prog::inner_ptr_type& source);
        prog::func_type copy_func_type(const prog::func_type& source);
        prog::func_with_ptr_type copy_func_with_ptr_type(const prog::func_with_ptr_type& source);
        prog::type_pointed copy_type_pointed(const prog::type_pointed& source);
        prog::type_local copy_type_local(const prog::type_local& source);

        public:

        compiler(diagnostic_collector& diags) : diags(diags) { }

        optional<prog::program> compile(const ast::program& ast);
    };
}

#endif
