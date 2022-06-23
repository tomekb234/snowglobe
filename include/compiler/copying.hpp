#ifndef COPYING_HPP
#define COPYING_HPP

#include "compiler/functions.hpp"

namespace sg {
    class copy_generator : compiler_base {
        function_compiler& fclr;
        prog::reg_index value;

        public:

        copy_generator(function_compiler& fclr, prog::reg_index value) : compiler_base(fclr), fclr(fclr), value(value) { }

        void add(const prog::type& type);

        private:

        void add_of_struct(const prog::struct_type& st);
        void add_of_enum_variants(const prog::enum_type& en, prog::variant_index variant_index);
        void add_of_tuple(vector<cref<prog::type>> types);
        void add_of_array(const prog::array_type& array_type);
        void add_of_optional(const prog::type& inner_type);
        void add_of_ptr(prog::ptr_type::kind_t kind);
    };
}

#endif
