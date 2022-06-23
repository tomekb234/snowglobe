#ifndef CONSTANTS_HPP
#define CONSTANTS_HPP

#include "compiler/compiler.hpp"

namespace sg {
    using std::pair;

    class constant_compiler : compiler_base {
        compiler& clr;

        public:

        constant_compiler(compiler& clr) : compiler_base(clr), clr(clr) { }

        pair<prog::constant, prog::type> compile(const ast::expr& ast);
        pair<prog::constant, prog::type> compile_literal(const ast::literal_expr& ast);

        size_t compile_size(const ast::const_int& ast);

        private:

        pair<prog::constant, prog::type> compile_tuple(vector<cref<ast::expr_marked>> asts, location loc);
        pair<prog::constant, prog::type> compile_array(vector<cref<ast::expr_marked>> asts, location loc);
        pair<prog::constant, prog::type> compile_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, location loc);
        pair<prog::constant, prog::type> compile_name(string name, location loc);
        pair<prog::constant, prog::type> compile_variant_name(string name, string variant_name, location loc);

        pair<prog::constant, prog::number_type> compile_int_token(const ast::int_token& ast);
        pair<prog::constant, prog::number_type> compile_float_token(const ast::float_token& ast);
    };
}

#endif
