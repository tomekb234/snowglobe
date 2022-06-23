#ifndef DELETION_HPP
#define DELETION_HPP

#include "compiler/functions.hpp"

namespace sg {
    class deletion_generator : compiler_base {
        function_compiler& fclr;
        prog::reg_index value;

        public:

        deletion_generator(function_compiler& fclr, prog::reg_index value) : compiler_base(fclr), fclr(fclr), value(value) { }

        void add_struct_destructor(const prog::struct_type& st);
        void add_enum_variants_destructor(const prog::enum_type& en, prog::variant_index variant_index);

        void add(const prog::type& type);

        private:

        void add_of_struct(const prog::struct_type& st);
        void add_of_enum(const prog::enum_type& en);
        void add_of_tuple(vector<cref<prog::type>> types);
        void add_of_array(const prog::array_type& array_type);
        void add_of_optional(const prog::type& inner_type);
        void add_of_ptr(prog::ptr_type::kind_t kind, const prog::type_pointed& target_type);
    };
}

#endif
