#include "program.hpp"
#include "utils.hpp"
#include <utility>
#include <variant>

namespace sg::prog {
    using namespace sg::utils;

    using std::move;
    using std::make_pair;
    using std::monostate;
    using std::ostream;

    static void print_ptr_kind(ostream& stream, const program& prog, const ptr_type::kind_t& tp);
    static void print_type_pointed(ostream& stream, const program& prog, const type_pointed& tp);
    static void print_func_type(ostream& stream, const program& prog, const func_type& func);

    constant copy_constant(const constant& source) {
        switch (INDEX(source)) {
            case constant::UNIT:
                return VARIANT(constant, UNIT, monostate{ });

            case constant::BOOL:
                return VARIANT(constant, BOOL, GET(source, BOOL));

            case constant::INT:
                return VARIANT(constant, INT, GET(source, INT));

            case constant::FLOAT32:
                return VARIANT(constant, FLOAT32, GET(source, FLOAT32));

            case constant::FLOAT64:
                return VARIANT(constant, FLOAT64, GET(source, FLOAT64));

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

            case constant::GLOBAL_VAR_PTR:
                return VARIANT(constant, GLOBAL_VAR_PTR, GET(source, GLOBAL_VAR_PTR));

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
        return { copy_func_type(source), copy_ptr_type(source) };
    }

    type_pointed copy_type_pointed(const type_pointed& source) {
        return { make_ptr(copy_type(*source.tp)), source.slice };
    }

    type_local copy_type_local(const type_local& source) {
        return { make_ptr(copy_type(*source.tp)), source.confined };
    }

    void print_type(ostream& stream, const program& prog, const type& tp) {
        switch (INDEX(tp)) {
            case type::NEVER:
                stream << "never";
                break;

            case type::PRIMITIVE: {
                auto& ptype = *GET(tp, PRIMITIVE);

                switch (ptype.tp) {
                    case primitive_type::UNIT:
                        stream << "()";
                        break;

                    case primitive_type::BOOL:
                        stream << "bool";
                        break;

                    case primitive_type::I8:
                        stream << "i8";
                        break;

                    case primitive_type::I16:
                        stream << "i16";
                        break;

                    case primitive_type::I32:
                        stream << "i32";
                        break;

                    case primitive_type::I64:
                        stream << "i64";
                        break;

                    case primitive_type::U8:
                        stream << "u8";
                        break;

                    case primitive_type::U16:
                        stream << "u16";
                        break;

                    case primitive_type::U32:
                        stream << "u32";
                        break;

                    case primitive_type::U64:
                        stream << "u64";
                        break;

                    case primitive_type::F32:
                        stream << "f32";
                        break;

                    case primitive_type::F64:
                        stream << "f64";
                        break;
                }
            } break;

            case type::STRUCT:
                stream << prog.struct_types[GET(tp, STRUCT)]->name;
                break;

            case type::ENUM:
                stream << prog.enum_types[GET(tp, ENUM)]->name;
                break;

            case type::TUPLE: {
                auto& tuple = GET(tp, TUPLE);

                stream << '(';

                auto first = true;
                for (auto& coord : tuple) {
                    if (!first)
                        stream << ", ";
                    first = false;
                    print_type(stream, prog, *coord);
                }

                stream << ')';
            } break;

            case type::ARRAY: {
                auto& array = *GET(tp, ARRAY);
                stream << '[';
                print_type(stream, prog, *array.tp);
                stream << "; " << array.size << ']';
            } break;

            case type::OPTIONAL: {
                stream << '?';
                print_type(stream, prog, *GET(tp, OPTIONAL));
            } break;

            case type::PTR: {
                auto& ptr = *GET(tp, PTR);
                print_ptr_kind(stream, prog, ptr.kind);
                print_type_pointed(stream, prog, *ptr.target_tp);
            } break;

            case type::INNER_PTR: {
                auto& inptr = *GET(tp, INNER_PTR);
                print_ptr_kind(stream, prog, inptr.kind);
                print_type_pointed(stream, prog, *inptr.owner_tp);
                stream << '.';
                print_type_pointed(stream, prog, *inptr.target_tp);
            } break;

            case type::FUNC: {
                stream << "func ";
                print_func_type(stream, prog, *GET(tp, FUNC));
            } break;

            case type::GLOBAL_FUNC: {
                stream << "func $";
                print_func_type(stream, prog, *GET(tp, GLOBAL_FUNC));
            } break;

            case type::FUNC_WITH_PTR: {
                auto& func = *GET(tp, FUNC_WITH_PTR);
                stream << "func ";
                print_ptr_kind(stream, prog, func.kind);
                print_type_pointed(stream, prog, *func.target_tp);
                stream << ' ';
                print_func_type(stream, prog, func);
            } break;

            case type::STRUCT_CTOR:
                stream << "<struct " << prog.struct_types[GET(tp, STRUCT_CTOR)]->name << " constructor>";
                break;

            case type::ENUM_CTOR: {
                auto& en = *prog.enum_types[GET(tp, ENUM_CTOR).first];
                auto var = GET(tp, ENUM_CTOR).second;
                stream << "<enum " << en.name;
                stream << " variant " << en.variants[var]->name << " constructor>";
            } break;
        }
    }

    static void print_ptr_kind(ostream& stream, const program& prog, const ptr_type::kind_t& tp) {
        switch (tp) {
            case ptr_type::GLOBAL:
                stream << '$';
                break;

            case ptr_type::BASIC:
                stream << '&';
                break;

            case ptr_type::SHARED:
                stream << '*';
                break;

            case ptr_type::WEAK:
                stream << '~';
                break;

            case ptr_type::UNIQUE:
                stream << '@';
                break;
        }
    }

    static void print_type_pointed(ostream& stream, const program& prog, const type_pointed& tp) {
        if (tp.slice)
            stream << '[';

        print_type(stream, prog, *tp.tp);

        if (tp.slice)
            stream << ']';
    }

    static void print_func_type(ostream& stream, const program& prog, const func_type& func) {
        stream << '(';

        auto first = true;
        for (auto& param : func.param_tps) {
            if (!first)
                stream << ", ";
            first = false;
            if (!param->confined)
                stream << "!";
            print_type(stream, prog, *param->tp);
        }

        stream << ") -> ";
        print_type(stream, prog, *func.return_tp);
    }
}
