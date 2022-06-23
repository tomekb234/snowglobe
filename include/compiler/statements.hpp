#ifndef STATEMENTS_HPP
#define STATEMENTS_HPP

#include "compiler/functions.hpp"

namespace sg {
    class statement_compiler : compiler_base {
        function_compiler& fclr;
        compiler& clr;

        public:

        statement_compiler(function_compiler& fclr) : compiler_base(fclr), fclr(fclr), clr(fclr.clr) { }

        void compile_block(const ast::stmt_block& ast, bool cleanup);

        private:

        void compile(const ast::stmt& ast);
        void compile_expr_eval(const ast::expr& ast);
        void compile_assignment(const ast::assignment_stmt& ast);
        void compile_compound_assignment(const ast::compound_assignment_stmt& ast);
        void compile_locally_block(const ast::locally_block_stmt& ast);
        void compile_swap(const ast::swap_stmt& ast);
        void compile_swap_block(const ast::swap_block_stmt& ast);
        void compile_if_branches(const ast::if_stmt& ast, size_t branch_index);
        void compile_match(const ast::match_stmt& ast);
        void compile_match_branches(const ast::match_stmt& ast, prog::reg_index value, const prog::type_local& type, size_t branch_index);
        void compile_while(const ast::while_stmt& ast);
        void compile_for(const ast::for_stmt& ast);
    };
}

#endif
