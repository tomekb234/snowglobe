#ifndef DELETION_HPP
#define DELETION_HPP

#include "compiler/functions.hpp"

namespace sg {
    class deletion_generator : compiler_base {
        function_compiler& fclr;

        public:

        deletion_generator(function_compiler& fclr) : compiler_base(fclr), fclr(fclr) { }

        void add(prog::reg_index value, const prog::type& type);

        void add_struct_destructor(prog::reg_index value, const prog::struct_type& st);
        void add_enum_variants_destructor(prog::reg_index value, const prog::enum_type& en, prog::variant_index variant_index);

        private:

        void add_for_struct(prog::reg_index value, const prog::struct_type& st);
        void add_for_enum(prog::reg_index value, const prog::enum_type& en);
        void add_for_tuple(prog::reg_index value, vector<cref<prog::type>> types);
        void add_for_array(prog::reg_index value, const prog::array_type& array_type);
        void add_for_optional(prog::reg_index value, const prog::type& inner_type);
        void add_for_ptr(prog::reg_index value, prog::ptr_type::kind_t kind, const prog::type_pointed& target_type);
    };
}

#endif
