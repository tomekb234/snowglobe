#ifndef COMPILER_HPP
#define COMPILER_HPP

#include "ast.hpp"
#include "program.hpp"
#include "location.hpp"
#include "diagnostics.hpp"
#include <optional>
#include <unordered_map>

namespace sg {
    using std::optional;
    using std::unordered_map;

    class compiler {
        struct global_name {
            enum {
                VARIABLE,
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

        template<typename T>
        [[noreturn]] void error(T&& diag) {
            // TODO locations
            diags.add(move(diag));
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

        prog::constant compile_constant(const ast::expr& ast);
        prog::type compile_type(const ast::type& ast);
        prog::primitive_type compile_primitive_type(const ast::primitive_type& ast);

        prog::type constant_type(const prog::constant& constant);
        prog::constant convert_constant(const prog::constant& constant, const prog::type& from_tp, const prog::type& to_tp);

        public:

        compiler(diagnostic_collector& diags) : diags(diags) { }

        optional<prog::program> compile(const ast::program& ast);
    };
}

#endif
