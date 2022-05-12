#include "program.hpp"
#include "utils.hpp"
#include <utility>
#include <variant>

namespace sg::prog {
    using namespace sg::utils;

    using std::move;
    using std::make_pair;
    using std::monostate;

    constant copy_constant(const constant& source) {
        switch (INDEX(source)) {
            case constant::UNIT:
                return VARIANT(constant, UNIT, monostate{ });

            case constant::BOOL:
                return VARIANT(constant, BOOL, GET(source, BOOL));

            case constant::INT:
                return VARIANT(constant, INT, GET(source, INT));

            case constant::FLOAT:
                return VARIANT(constant, FLOAT, GET(source, FLOAT));

            case constant::STRUCT: {
                auto vec = copy_ptr_vector<constant>(GET(source, STRUCT), copy_constant);
                return VARIANT(constant, STRUCT, move(vec));
            }

            case constant::ENUM: {
                auto& p = GET(source, ENUM);
                auto vec = copy_ptr_vector<constant>(p.second, copy_constant);
                return VARIANT(constant, ENUM, make_pair(p.first, move(vec)));
            }

            case constant::TUPLE: {
                auto vec = copy_ptr_vector<constant>(GET(source, TUPLE), copy_constant);
                return VARIANT(constant, TUPLE, move(vec));
            }

            case constant::ARRAY: {
                auto vec = copy_ptr_vector<constant>(GET(source, ARRAY), copy_constant);
                return VARIANT(constant, ARRAY, move(vec));
            }

            case constant::SIZED_ARRAY: {
                auto& p = GET(source, SIZED_ARRAY);
                return VARIANT(constant, SIZED_ARRAY, make_pair(make_ptr(copy_constant(*p.first)), p.second));
            }

            case constant::SOME:
                return VARIANT(constant, SOME, make_ptr(copy_constant(*GET(source, SOME))));

            case constant::NONE:
                return VARIANT(constant, NONE, monostate());

            case constant::GLOBAL_PTR:
                return VARIANT(constant, GLOBAL_PTR, GET(source, GLOBAL_PTR));

            case constant::GLOBAL_INNER_PTR: {
                auto& p = GET(source, GLOBAL_INNER_PTR);
                auto vec = copy_ptr_vector<inner_location>(p.second);
                return VARIANT(constant, GLOBAL_INNER_PTR, make_pair(p.first, move(vec)));
            }

            case constant::GLOBAL_FUNC_PTR:
                return VARIANT(constant, GLOBAL_FUNC_PTR, GET(source, GLOBAL_FUNC_PTR));
        }

        UNREACHABLE;
    }

    type copy_type(const type& source) {
        switch (INDEX(source)) {
            case type::NEVER:
                return VARIANT(type, NEVER, monostate());

            case type::PRIMITIVE:
                return VARIANT(type, PRIMITIVE, make_ptr(primitive_type { GET(source, PRIMITIVE)->tp }));

            case type::STRUCT:
                return VARIANT(type, STRUCT, GET(source, STRUCT));

            case type::ENUM:
                return VARIANT(type, ENUM, GET(source, ENUM));

            case type::TUPLE: {
                auto vec = copy_ptr_vector<type>(GET(source, TUPLE), copy_type);
                return VARIANT(type, TUPLE, move(vec));
            }

            case type::ARRAY:
                return VARIANT(type, ARRAY, make_ptr(copy_array_type(*GET(source, ARRAY))));

            case type::OPTIONAL:
                return VARIANT(type, OPTIONAL, make_ptr(copy_type(*GET(source, OPTIONAL))));

            case type::PTR:
                return VARIANT(type, PTR, make_ptr(copy_ptr_type(*GET(source, PTR))));

            case type::INNER_PTR:
                return VARIANT(type, INNER_PTR, make_ptr(copy_inner_ptr_type(*GET(source, INNER_PTR))));

            case type::FUNC:
                return VARIANT(type, FUNC, make_ptr(copy_func_type(*GET(source, FUNC))));

            case type::GLOBAL_FUNC:
                return VARIANT(type, GLOBAL_FUNC, make_ptr(copy_func_type(*GET(source, GLOBAL_FUNC))));

            case type::FUNC_WITH_PTR:
                return VARIANT(type, FUNC_WITH_PTR, make_ptr(copy_func_with_ptr_type(*GET(source, FUNC_WITH_PTR))));
            case type::STRUCT_CTOR:
                return VARIANT(type, STRUCT_CTOR, GET(source, STRUCT_CTOR));

            case type::ENUM_CTOR:
                return VARIANT(type, ENUM_CTOR, GET(source, ENUM_CTOR));
        }

        UNREACHABLE;
    }

    array_type copy_array_type(const array_type& source) {
        return { make_ptr(copy_type(*source.tp)), source.size };
    }

    ptr_type copy_ptr_type(const ptr_type& source) {
        return { source.kind, make_ptr(copy_type_pointed(*source.target_tp)) };
    }

    inner_ptr_type copy_inner_ptr_type(const inner_ptr_type& source) {
        return { copy_ptr_type(source), make_ptr(copy_type_pointed(*source.owner_tp)) };
    }

    func_type copy_func_type(const func_type& source) {
        auto vec = copy_ptr_vector<type_local>(source.param_tps, copy_type_local);
        return { move(vec), make_ptr(copy_type(*source.return_tp)) };
    }

    func_with_ptr_type copy_func_with_ptr_type(const func_with_ptr_type& source) {
        return { copy_func_type(source), copy_ptr_type(source) }; // maybe FIXME
    }

    type_pointed copy_type_pointed(const type_pointed& source) {
        return { make_ptr(copy_type(*source.tp)), source.slice };
    }

    type_local copy_type_local(const type_local& source) {
        return { make_ptr(copy_type(*source.tp)), source.confined };
    }
}
