#ifndef COMPILER_UTILS_HPP
#define COMPILER_UTILS_HPP

#include "compiler/compiler.hpp"
#include <functional>

namespace sg {
    using std::function;
    using std::optional;

    class compiler_utils : compiler_base {
        compiler& clr;

        public:

        compiler_utils(compiler& clr) : compiler_base(clr), clr(clr) { }

        prog::type common_supertype(const prog::type& type_a, const prog::type& type_b, location loc);

        prog::constant convert_const(prog::constant value, const prog::type& type, const prog::type& new_type, location loc);

        vector<cref<ast::expr>> order_args(
                vector<cref<ast::expr_marked>> asts,
                optional<function<size_t(string, location)>> arg_with_name,
                optional<size_t> expected_count,
                location loc);
    };
}

#endif
