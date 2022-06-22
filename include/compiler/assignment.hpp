#ifndef ASSIGNMENT_HPP
#define ASSIGNMENT_HPP

#include "compiler/functions.hpp"

namespace sg {
    class assignment_compiler : compiler_base {
        function_compiler& fclr;
        compiler& clr;

        public:

        assignment_compiler(function_compiler& fclr) : compiler_base(fclr), fclr(fclr), clr(fclr.clr) { }

        void add(const lvalue& lval, prog::reg_index value, const prog::type_local& type, location loc);

        pair<prog::reg_index, prog::type_local> add_read_for_swap(const lvalue& lval, location loc);
        void add_write_from_swap(const lvalue& lval, prog::reg_index value, const prog::type_local& type, location loc);
    };
}

#endif
