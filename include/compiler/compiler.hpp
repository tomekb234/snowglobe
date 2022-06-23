#ifndef COMPILER_HPP
#define COMPILER_HPP

#include "ast.hpp"
#include "program.hpp"
#include "diagcol.hpp"

namespace sg {
    using std::make_unique;

    template<typename T>
    using ptr = std::unique_ptr<T>;

    template<typename T>
    using cref = std::reference_wrapper<const T>;

    class compiler_base {
        protected:

        struct compilation_error { };

        prog::program& prog;
        diagnostic_collector& diags;

        compiler_base(const compiler_base& clr) : prog(clr.prog), diags(clr.diags) { }
        compiler_base(prog::program& prog, diagnostic_collector& diags) : prog(prog), diags(diags) { }

        template<typename T>
        [[noreturn]] void error(T&& diag) {
            diags.add(make_unique<T>(move(diag)));
            throw compilation_error();
        }

        template<typename T>
        void warning(T&& diag) {
            diags.add(make_unique<T>(move(diag)));
        }
    };

    enum struct global_name_kind {
        VAR,
        CONST,
        FUNC,
        STRUCT,
        ENUM
    };

    class compiler : compiler_base {
        struct global_name {
            global_name_kind kind;
            prog::global_index index;
            bool compiled;
        };

        vector<prog::global_var> consts;
        unordered_map<string, global_name> global_names;
        unordered_map<prog::global_index, prog::global_index> func_wrappers;

        friend class compiler_utils;
        friend class type_compiler;
        friend class constant_compiler;
        friend class function_compiler;
        friend class function_utils;
        friend class conversion_generator;
        friend class assignment_generator;
        friend class statement_compiler;
        friend class expression_compiler;
        friend class lvalue_compiler;

        public:

        compiler(prog::program& prog, diagnostic_collector& diags) : compiler_base(prog, diags) { }

        void compile_builtins(const ast::program& ast, string builtin_name);
        bool compile(const ast::program& ast);

        private:

        const global_name& get_global_name(string name, location loc);
        const global_name& get_global_name(string name, vector<global_name_kind> expected_kinds, location loc);
        const global_name& get_global_type(string name, bool allow_uncompiled, location loc);
        prog::global_index get_global_func_wrapper(prog::global_index func_index);

        prog::global_var compile_global_var(const ast::var_def& ast);
        prog::global_func declare_global_func(const ast::func_def& ast);
        prog::struct_type declare_struct_type(const ast::struct_def& ast);
        prog::enum_type declare_enum_type(const ast::enum_def& ast);

        void compile_global_func(const ast::func_def& ast, prog::global_func& func);
        void compile_struct_type(const ast::struct_def& ast, prog::struct_type& st);
        void compile_enum_type(const ast::enum_def& ast, prog::enum_type& en);

        prog::global_func make_func_wrapper(prog::global_index func_index);
        prog::global_func make_struct_destructor(prog::global_index struct_index);
        prog::global_func make_enum_destructor(prog::global_index enum_index);
        prog::global_func make_cleanup_func();
    };
}

#endif
