#ifndef CONVERSIONS_HPP
#define CONVERSIONS_HPP

#include "compiler/functions.hpp"

namespace sg {
    class conversion_compiler : compiler_base {
        function_compiler& fclr;
        compiler& clr;

        public:

        conversion_compiler(function_compiler& fclr) : compiler_base(fclr), fclr(fclr), clr(fclr.clr) { }

        prog::reg_index convert(prog::reg_index value, const prog::type& type, const prog::type& new_type, bool confined, location loc);
        prog::reg_index convert(prog::reg_index value, const prog::type& type, const prog::type& new_type, location loc);
        prog::reg_index convert(prog::reg_index value, const prog::type_local& type, const prog::type_local& new_type, location loc);
        prog::reg_index convert(prog::reg_index value, const prog::type_local& type, const prog::type& new_type, location loc);
        optional<prog::reg_index> try_convert(prog::reg_index value, const prog::type& type, const prog::type& new_type, bool confined);

        private:

        optional<prog::reg_index> try_convert_ptr_kind(prog::reg_index value, prog::ptr_type::kind_t kind, prog::ptr_type::kind_t new_kind, bool confined);
        optional<prog::reg_index> try_convert_ptr_target(prog::reg_index value, const prog::type_pointed& type, const prog::type_pointed& new_type);
    };
}

#endif
