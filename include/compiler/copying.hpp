#ifndef COPYING_HPP
#define COPYING_HPP

#include "compiler/functions.hpp"

namespace sg {
    class copy_generator : compiler_base {
        function_compiler& fclr;

        public:

        copy_generator(function_compiler& fclr) : compiler_base(fclr), fclr(fclr) { }

        void add(prog::reg_index value, const prog::type& type);

        private:

        void add_for_struct(prog::reg_index value, const prog::struct_type& st);
        void add_for_enum_variants(prog::reg_index value, const prog::enum_type& en, prog::variant_index variant_index);
        void add_for_tuple(prog::reg_index value, vector<cref<prog::type>> types);
        void add_for_array(prog::reg_index value, const prog::array_type& array_type);
        void add_for_optional(prog::reg_index value, const prog::type& inner_type);
        void add_for_ptr(prog::reg_index value, prog::ptr_type::kind_t kind);
    };
}

#endif
