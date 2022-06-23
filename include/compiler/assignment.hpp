#ifndef ASSIGNMENT_HPP
#define ASSIGNMENT_HPP

#include "compiler/functions.hpp"

namespace sg {
    class assignment_generator : compiler_base {
        function_compiler& fclr;
        compiler& clr;
        prog::reg_index value;
        const prog::type_local& type;
        location loc;

        public:

        assignment_generator(function_compiler& fclr, prog::reg_index value, const prog::type_local& type, location loc)
            : compiler_base(fclr), fclr(fclr), clr(fclr.clr), value(value), type(type), loc(loc) { }

        void add(const lvalue& lval);
        void add_from_swap(const lvalue& lval);

        private:

        void add_to_var(prog::var_index var_index);
        void add_to_global_var(prog::global_index var_index);
        void add_to_tuple(vector<cref<lvalue>> lvals);
        void add_to_array(vector<cref<lvalue>> lvals);
        void add_to_struct(prog::global_index struct_index, vector<cref<lvalue>> lvals);
        void add_to_dereference(prog::reg_index ptr_value, const prog::type& target_type);

        void add_to_var_from_swap(prog::var_index var_index);
        void add_to_global_var_from_swap(prog::global_index var_index);
        void add_to_tuple_from_swap(vector<cref<lvalue>> lvals);
        void add_to_array_from_swap(vector<cref<lvalue>> lvals);
        void add_to_struct_from_swap(prog::global_index struct_index, vector<cref<lvalue>> lvals);
        void add_to_dereference_from_swap(prog::reg_index ptr_value, const prog::type& target_type);
    };
}

#endif
