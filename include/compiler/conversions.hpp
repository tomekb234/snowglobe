#ifndef CONVERSIONS_HPP
#define CONVERSIONS_HPP

#include "compiler/functions.hpp"

namespace sg {
    class conversion_generator : compiler_base {
        function_compiler& fclr;
        compiler& clr;
        prog::reg_index value;
        const prog::type& new_type;
        bool confined;

        public:

        conversion_generator(function_compiler& fclr, prog::reg_index value, const prog::type& new_type, bool confined)
            : compiler_base(fclr), fclr(fclr), clr(fclr.clr), value(value), new_type(new_type), confined(confined) { }

        conversion_generator(function_compiler& fclr, prog::reg_index value, const prog::type& new_type)
            : conversion_generator(fclr, value, new_type, false) { }

        conversion_generator(function_compiler& fclr, prog::reg_index value, const prog::type_local& new_type)
            : conversion_generator(fclr, value, *new_type.tp, new_type.confined) { }

        prog::reg_index convert_from(const prog::type& type, location loc);
        prog::reg_index convert_from(const prog::type_local& type, location loc);

        optional<prog::reg_index> try_convert_from(const prog::type& type);

        private:

        prog::reg_index convert_from_never();
        optional<prog::reg_index> try_convert_from_number(const prog::number_type& ntype);
        optional<prog::reg_index> try_convert_from_tuple(const vector<cref<prog::type>> tuple);
        optional<prog::reg_index> try_convert_from_array(const prog::array_type& array);
        optional<prog::reg_index> try_convert_from_optional(const prog::type& inner);
        optional<prog::reg_index> try_convert_from_ptr(const prog::ptr_type& ptr);
        optional<prog::reg_index> try_convert_from_inner_ptr(const prog::inner_ptr_type& inptr);
        optional<prog::reg_index> try_convert_from_func_with_ptr(const prog::func_with_ptr_type& fptr);
        optional<prog::reg_index> try_convert_from_known_func(prog::global_index index);

        optional<prog::reg_index> try_convert_ptr_kind(prog::reg_index value, prog::ptr_type::kind_t kind, prog::ptr_type::kind_t new_kind);
        optional<prog::reg_index> try_convert_ptr_target(prog::reg_index value, const prog::type_pointed& type, const prog::type_pointed& new_type);
        bool ptr_kind_trivial(prog::ptr_type::kind_t kind);
    };
}

#endif
