#ifndef LVALUES_HPP
#define LVALUES_HPP

#include "compiler/functions.hpp"

namespace sg {
    class lvalue_compiler : compiler_base {
        function_compiler& fclr;
        compiler& clr;
        optional<cref<prog::type_local>> implicit_type;

        public:

        lvalue_compiler(function_compiler& fclr, optional<cref<prog::type_local>> implicit_type)
            : compiler_base(fclr), fclr(fclr), clr(fclr.clr), implicit_type(implicit_type) { }

        lvalue compile(const ast::expr& ast);

        private:

        lvalue compile_tuple(vector<cref<ast::expr_marked>> asts, location loc);
        lvalue compile_array(vector<cref<ast::expr_marked>> asts, location loc);
        lvalue compile_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, location loc);
        lvalue compile_name(string name, location loc);
        lvalue compile_var_decl(const ast::var_decl_expr& ast);
        lvalue compile_dereference(const ast::expr& ast);
        lvalue compile_extraction(const ast::expr& expr_ast, const ast::extraction_expr& extr_ast);
    };
}

#endif
