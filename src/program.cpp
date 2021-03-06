#include "program.hpp"
#include "utils.hpp"

namespace sg::prog {
    using namespace sg::utils;

    const type NEVER_TYPE = VARIANT(type, NEVER, monostate());
    const type UNIT_TYPE = VARIANT(type, UNIT, monostate());
    const type BOOL_TYPE = VARIANT(type, NUMBER, make_ptr(number_type { number_type::BOOL }));
    const type CHAR_TYPE = VARIANT(type, NUMBER, make_ptr(number_type { number_type::U8 }));
    const type SIZE_TYPE = VARIANT(type, NUMBER, make_ptr(number_type { number_type::U64 }));
    const type UNIT_PTR_TYPE = make_ptr_type(VARIANT(type, UNIT, monostate()), ptr_type::GLOBAL, false);

    const type_local NEVER_TYPE_LOCAL = { make_ptr(copy_type(NEVER_TYPE)), false };
    const type_local UNIT_TYPE_LOCAL = { make_ptr(copy_type(UNIT_TYPE)), false };
    const type_local BOOL_TYPE_LOCAL = { make_ptr(copy_type(BOOL_TYPE)), false };
    const type_local CHAR_TYPE_LOCAL = { make_ptr(copy_type(CHAR_TYPE)), false };
    const type_local SIZE_TYPE_LOCAL = { make_ptr(copy_type(SIZE_TYPE)), false };
    const type_local UNIT_PTR_TYPE_LOCAL = { make_ptr(copy_type(UNIT_PTR_TYPE)), false };

    static array_type copy_array_type(const array_type& tp);
    static inner_ptr_type copy_inner_ptr_type(const inner_ptr_type& tp);
    static func_type copy_func_type(const func_type& tp);
    static func_with_ptr_type copy_func_with_ptr_type(const func_with_ptr_type& tp);

    static bool ptr_types_equal(const ptr_type& type_a, const ptr_type& type_b);
    static bool types_pointed_equal(const type_pointed& type_a, const type_pointed& type_b);

    static void print_ptr_kind(ostream& stream, const ptr_type::kind_t& tp);
    static void print_type_pointed(ostream& stream, const program& prog, const type_pointed& tp);
    static void print_func_type(ostream& stream, const program& prog, const func_type& func);

    type make_ptr_type(type&& tp, ptr_type::kind_t kind, bool slice) {
        auto tp_pointed = type_pointed { into_ptr(tp), slice };
        auto ptr_tp = ptr_type { kind, into_ptr(tp_pointed) };
        return VARIANT(type, PTR, into_ptr(ptr_tp));
    }

    constant copy_const(const constant& value) {
        switch (INDEX(value)) {
            case constant::UNIT:
                return VARIANT(constant, UNIT, monostate{ });

            case constant::NUMBER: {
                auto number = GET(value, NUMBER);
                return VARIANT(constant, NUMBER, number);
            } break;

            case constant::STRUCT: {
                auto& value_ptrs = GET(value, STRUCT);
                return VARIANT(constant, STRUCT, copy_ptr_vector<constant>(value_ptrs, copy_const));
            }

            case constant::ENUM: {
                auto& [variant_index, value_ptrs] = GET(value, ENUM);
                return VARIANT(constant, ENUM, make_pair(variant_index, copy_ptr_vector<constant>(value_ptrs, copy_const)));
            }

            case constant::TUPLE: {
                auto& value_ptrs = GET(value, TUPLE);
                return VARIANT(constant, TUPLE, copy_ptr_vector<constant>(value_ptrs, copy_const));
            }

            case constant::ARRAY: {
                auto& value_ptrs = GET(value, ARRAY);
                return VARIANT(constant, ARRAY, copy_ptr_vector<constant>(value_ptrs, copy_const));
            }

            case constant::OPTIONAL: {
                auto& value_ptr = GET(value, OPTIONAL);
                return VARIANT(constant, OPTIONAL, value_ptr ? optional<ptr<constant>>(make_ptr(copy_const(**value_ptr))) : optional<ptr<constant>>());
            }

            case constant::GLOBAL_VAR_PTR:
                return VARIANT(constant, GLOBAL_VAR_PTR, GET(value, GLOBAL_VAR_PTR));

            case constant::GLOBAL_VAR_SLICE:
                return VARIANT(constant, GLOBAL_VAR_SLICE, GET(value, GLOBAL_VAR_SLICE));

            case constant::GLOBAL_FUNC_PTR:
                return VARIANT(constant, GLOBAL_FUNC_PTR, GET(value, GLOBAL_FUNC_PTR));

            case constant::GLOBAL_FUNC_WRAPPER_PTR:
                return VARIANT(constant, GLOBAL_FUNC_WRAPPER_PTR, GET(value, GLOBAL_FUNC_WRAPPER_PTR));
        }

        UNREACHABLE;
    }

    type copy_type(const type& tp) {
        switch (INDEX(tp)) {
            case type::NEVER:
                return VARIANT(type, NEVER, monostate());

            case type::UNIT:
                return VARIANT(type, UNIT, monostate());

            case type::NUMBER:
                return VARIANT(type, NUMBER, make_ptr(number_type { GET(tp, NUMBER)->tp }));

            case type::STRUCT:
                return VARIANT(type, STRUCT, GET(tp, STRUCT));

            case type::ENUM:
                return VARIANT(type, ENUM, GET(tp, ENUM));

            case type::TUPLE: {
                auto& type_ptrs = GET(tp, TUPLE);
                return VARIANT(type, TUPLE, copy_ptr_vector<type>(type_ptrs, copy_type));
            }

            case type::ARRAY:
                return VARIANT(type, ARRAY, make_ptr(copy_array_type(*GET(tp, ARRAY))));

            case type::OPTIONAL:
                return VARIANT(type, OPTIONAL, make_ptr(copy_type(*GET(tp, OPTIONAL))));

            case type::PTR:
                return VARIANT(type, PTR, make_ptr(copy_ptr_type(*GET(tp, PTR))));

            case type::INNER_PTR:
                return VARIANT(type, INNER_PTR, make_ptr(copy_inner_ptr_type(*GET(tp, INNER_PTR))));

            case type::FUNC:
                return VARIANT(type, FUNC, make_ptr(copy_func_type(*GET(tp, FUNC))));

            case type::GLOBAL_FUNC:
                return VARIANT(type, GLOBAL_FUNC, make_ptr(copy_func_type(*GET(tp, GLOBAL_FUNC))));

            case type::FUNC_WITH_PTR:
                return VARIANT(type, FUNC_WITH_PTR, make_ptr(copy_func_with_ptr_type(*GET(tp, FUNC_WITH_PTR))));

            case type::KNOWN_FUNC:
                return VARIANT(type, KNOWN_FUNC, GET(tp, KNOWN_FUNC));

            case type::STRUCT_CTOR:
                return VARIANT(type, STRUCT_CTOR, GET(tp, STRUCT_CTOR));

            case type::ENUM_CTOR:
                return VARIANT(type, ENUM_CTOR, GET(tp, ENUM_CTOR));
        }

        UNREACHABLE;
    }

    type_local copy_type_local(const type_local& tp) {
        return { make_ptr(copy_type(*tp.tp)), tp.confined };
    }

    ptr_type copy_ptr_type(const ptr_type& tp) {
        return { tp.kind, make_ptr(copy_type_pointed(*tp.target_tp)) };
    }

    type_pointed copy_type_pointed(const type_pointed& tp) {
        return { make_ptr(copy_type(*tp.tp)), tp.slice };
    }

    static array_type copy_array_type(const array_type& tp) {
        return { make_ptr(copy_type(*tp.tp)), tp.size };
    }

    static inner_ptr_type copy_inner_ptr_type(const inner_ptr_type& tp) {
        return { copy_ptr_type(tp), make_ptr(copy_type_pointed(*tp.owner_tp)) };
    }

    static func_type copy_func_type(const func_type& tp) {
        auto vec = copy_ptr_vector<type_local>(tp.param_tps, copy_type_local);
        return { move(vec), make_ptr(copy_type(*tp.return_tp)) };
    }

    static func_with_ptr_type copy_func_with_ptr_type(const func_with_ptr_type& tp) {
        return { copy_func_type(tp), copy_ptr_type(tp) };
    }

    bool types_equal(const type& type_a, const type& type_b) {
        if (INDEX(type_a) != INDEX(type_b))
            return false;

        switch (INDEX(type_a)) {
            case type::NEVER:
                return true;

            case type::NUMBER:
                return GET(type_a, NUMBER)->tp == GET(type_b, NUMBER)->tp;

            case type::STRUCT:
                return GET(type_a, STRUCT) == GET(type_b, STRUCT);

            case type::ENUM:
                return GET(type_a, ENUM) == GET(type_b, ENUM);

            case type::TUPLE: {
                auto tuple_a = as_cref_vector(GET(type_a, TUPLE));
                auto tuple_b = as_cref_vector(GET(type_b, TUPLE));
                auto size = tuple_a.size();

                if (size != tuple_b.size())
                    return false;

                for (size_t index = 0; index < size; index++) {
                    if (!types_equal(tuple_a[index], tuple_b[index]))
                        return false;
                }

                return true;
            }

            case type::ARRAY: {
                auto& array_a = *GET(type_a, ARRAY);
                auto& array_b = *GET(type_b, ARRAY);
                return array_a.size == array_b.size && types_equal(*array_a.tp, *array_b.tp);
            }

            case type::OPTIONAL:
                return types_equal(*GET(type_a, OPTIONAL), *GET(type_b, OPTIONAL));

            case type::PTR:
                return ptr_types_equal(*GET(type_a, PTR), *GET(type_b, PTR));

            case type::INNER_PTR: {
                auto& inptr_a = *GET(type_a, INNER_PTR);
                auto& inptr_b = *GET(type_b, INNER_PTR);
                return ptr_types_equal(inptr_a, inptr_b) && types_pointed_equal(*inptr_a.owner_tp, *inptr_b.owner_tp);
            }

            case type::FUNC:
                return func_types_equal(*GET(type_a, FUNC), *GET(type_b, FUNC));

            case type::GLOBAL_FUNC:
                return func_types_equal(*GET(type_a, GLOBAL_FUNC), *GET(type_b, GLOBAL_FUNC));

            case type::FUNC_WITH_PTR: {
                auto& fptr_a = *GET(type_a, FUNC_WITH_PTR);
                auto& fptr_b = *GET(type_b, FUNC_WITH_PTR);
                return func_types_equal(fptr_a, fptr_b) && ptr_types_equal(fptr_a, fptr_b);
            }

            case type::KNOWN_FUNC:
                return GET(type_a, KNOWN_FUNC) == GET(type_b, KNOWN_FUNC);

            case type::STRUCT_CTOR:
                return GET(type_a, STRUCT_CTOR) == GET(type_b, STRUCT_CTOR);

            case type::ENUM_CTOR:
                return GET(type_a, ENUM_CTOR) == GET(type_b, ENUM_CTOR);
        }

        return true;
    }

    bool types_local_equal(const type_local& type_a, const type_local& type_b) {
        return type_a.confined == type_b.confined && types_equal(*type_a.tp, *type_b.tp);
    }

    bool func_types_equal(const func_type& type_a, const func_type& type_b) {
        if (!types_equal(*type_a.return_tp, *type_b.return_tp))
            return false;

        auto& params_a = type_a.param_tps;
        auto& params_b = type_b.param_tps;

        if (params_a.size() != params_b.size())
            return false;

        for (size_t index = 0; index < params_a.size(); index++) {
            if (!types_local_equal(*params_a[index], *params_b[index]))
                return false;
        }

        return true;
    }

    static bool ptr_types_equal(const ptr_type& type_a, const ptr_type& type_b) {
        return type_a.kind == type_b.kind && types_pointed_equal(*type_a.target_tp, *type_b.target_tp);
    }

    static bool types_pointed_equal(const type_pointed& type_a, const type_pointed& type_b) {
        return type_a.slice == type_b.slice && types_equal(*type_a.tp, *type_b.tp);
    }

    func_type get_func_type(const global_func& func) {
        vector<type_local> param_types;

        for (auto& param : func.params)
            param_types.push_back(copy_type_local(*param->tp));

        return { into_ptr_vector(param_types), make_ptr(copy_type(*func.return_tp)) };
    }

    bool type_copyable(const program& prog, const type& tp) {
        switch (INDEX(tp)) {
            case type::NEVER:
            case type::UNIT:
            case type::NUMBER:
                return true;

            case type::STRUCT:
                return prog.struct_types[GET(tp, STRUCT)]->copyable;

            case type::ENUM:
                return prog.enum_types[GET(tp, ENUM)]->copyable;

            case type::TUPLE: {
                for (const type& tp : as_cref_vector(GET(tp, TUPLE)))
                    if (!type_copyable(prog, tp))
                        return false;
                return true;
            }

            case type::ARRAY:
                return type_copyable(prog, *GET(tp, ARRAY)->tp);

            case type::OPTIONAL:
                return type_copyable(prog, *GET(tp, OPTIONAL));

            case type::PTR:
                return GET(tp, PTR)->kind != ptr_type::UNIQUE;

            case type::INNER_PTR:
                return GET(tp, INNER_PTR)->kind != ptr_type::UNIQUE;

            case type::FUNC:
            case type::GLOBAL_FUNC:
                return true;

            case type::FUNC_WITH_PTR:
                return GET(tp, FUNC_WITH_PTR)->kind != ptr_type::UNIQUE;

            case type::KNOWN_FUNC:
            case type::STRUCT_CTOR:
            case type::ENUM_CTOR:
                return true;

        }

        UNREACHABLE;
    }

    bool type_trivial(const program& prog, const type& tp) {
        switch (INDEX(tp)) {
            case type::NEVER:
            case type::NUMBER:
            case type::UNIT:
                return true;

            case type::STRUCT:
                return prog.struct_types[GET(tp, STRUCT)]->trivial;

            case type::ENUM:
                return prog.enum_types[GET(tp, ENUM)]->trivial;

            case type::TUPLE: {
                for (const type& tp : as_cref_vector(GET(tp, TUPLE)))
                    if (!type_trivial(prog, tp))
                        return false;
                return true;
            }

            case type::ARRAY:
                return type_trivial(prog, *GET(tp, ARRAY)->tp);

            case type::OPTIONAL:
                return type_trivial(prog, *GET(tp, OPTIONAL));

            case type::PTR:
                return GET(tp, PTR)->kind == ptr_type::GLOBAL;

            case type::INNER_PTR:
                return GET(tp, INNER_PTR)->kind == ptr_type::GLOBAL;

            case type::FUNC:
                return false;

            case type::GLOBAL_FUNC:
                return true;

            case type::FUNC_WITH_PTR:
                return false;

            case type::KNOWN_FUNC:
            case type::STRUCT_CTOR:
            case type::ENUM_CTOR:
                return true;
        }

        UNREACHABLE;
    }

    void print_type(ostream& stream, const program& prog, const type& tp) {
        switch (INDEX(tp)) {
            case type::NEVER:
                stream << "never";
                break;

            case type::UNIT:
                stream << "()";
                break;

            case type::NUMBER: {
                auto& ntype = *GET(tp, NUMBER);

                switch (ntype.tp) {
                    case number_type::BOOL:
                        stream << "bool";
                        break;

                    case number_type::I8:
                        stream << "i8";
                        break;

                    case number_type::I16:
                        stream << "i16";
                        break;

                    case number_type::I32:
                        stream << "i32";
                        break;

                    case number_type::I64:
                        stream << "i64";
                        break;

                    case number_type::U8:
                        stream << "u8";
                        break;

                    case number_type::U16:
                        stream << "u16";
                        break;

                    case number_type::U32:
                        stream << "u32";
                        break;

                    case number_type::U64:
                        stream << "u64";
                        break;

                    case number_type::F32:
                        stream << "f32";
                        break;

                    case number_type::F64:
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
                auto tuple = as_cref_vector(GET(tp, TUPLE));

                stream << '(';

                auto first = true;
                for (auto& field : tuple) {
                    if (!first)
                        stream << ", ";
                    first = false;
                    print_type(stream, prog, field);
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
                print_ptr_kind(stream, ptr.kind);
                print_type_pointed(stream, prog, *ptr.target_tp);
            } break;

            case type::INNER_PTR: {
                auto& inptr = *GET(tp, INNER_PTR);
                print_ptr_kind(stream, inptr.kind);
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
                print_ptr_kind(stream, func.kind);
                print_type_pointed(stream, prog, *func.target_tp);
                stream << ' ';
                print_func_type(stream, prog, func);
            } break;

            case type::KNOWN_FUNC: {
                auto& func = *prog.global_funcs[GET(tp, KNOWN_FUNC)];
                if (func.name)
                    stream << "<function " << *func.name << ">";
                else
                    stream << "<internal function>";
                break;
            }

            case type::STRUCT_CTOR: {
                auto& st = *prog.struct_types[GET(tp, STRUCT_CTOR)];
                stream << "<struct " << st.name << " constructor>";
                break;
            }

            case type::ENUM_CTOR: {
                auto& en = *prog.enum_types[GET(tp, ENUM_CTOR).first];
                auto variant = GET(tp, ENUM_CTOR).second;
                stream << "<enum " << en.name;
                stream << " variant " << en.variants[variant]->name << " constructor>";
            } break;
        }
    }

    static void print_ptr_kind(ostream& stream, const ptr_type::kind_t& tp) {
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
        for (const type_local& tp : as_cref_vector(func.param_tps)) {
            if (!first)
                stream << ", ";
            first = false;
            if (!tp.confined)
                stream << "!";
            print_type(stream, prog, *tp.tp);
        }

        stream << ") -> ";
        print_type(stream, prog, *func.return_tp);
    }
}
