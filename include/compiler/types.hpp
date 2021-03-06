#ifndef TYPES_HPP
#define TYPES_HPP

#include "compiler/compiler.hpp"

namespace sg {
    class type_compiler : compiler_base {
        compiler& clr;
        bool allow_uncompiled;

        public:

        type_compiler(compiler& clr, bool allow_uncompiled) : compiler_base(clr), clr(clr), allow_uncompiled(allow_uncompiled) { }

        prog::type compile(const ast::type& ast);
        prog::type_local compile_local(const ast::type_local& ast);

        private:

        prog::type compile_user_type(string name, location loc);
        prog::number_type compile_number_type(const ast::number_type& ast);
        vector<prog::type> compile_tuple_type(vector<cref<ast::type>> asts);
        prog::array_type compile_array_type(const ast::array_type& ast);
        prog::ptr_type compile_ptr_type(const ast::ptr_type& ast);
        prog::type_pointed compile_type_pointed(const ast::type_pointed& ast);
        prog::inner_ptr_type compile_inner_ptr_type(const ast::inner_ptr_type& ast);
        prog::func_type compile_func_type(const ast::func_type& ast);
        prog::func_with_ptr_type compile_func_with_ptr_type(const ast::func_with_ptr_type& ast);
    };
}

#endif
