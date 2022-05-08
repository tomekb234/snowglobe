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
        vector<unique_ptr<prog::global_var>> constants;

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
        pair<prog::constant, prog::type> compile_constant_literal(const ast::const_expr& ast);
        pair<prog::constant, prog::primitive_type> compile_int_token(const ast::int_token& ast);
        pair<prog::constant, prog::primitive_type> compile_float_token(const ast::float_token& ast);

        prog::type compile_type(const ast::type& ast);
        prog::primitive_type compile_primitive_type(const ast::primitive_type& ast);

        prog::constant convert_constant(const ast::node& ast, const prog::constant& constant, const prog::type& from_tp, const prog::type& to_tp);
        bool types_equal(const ast::node& ast, const prog::type& type1, const prog::type& type2);

        public:

        compiler(diagnostic_collector& diags) : diags(diags) { }

        optional<prog::program> compile(const ast::program& ast);
    };
}

#endif
