#include "compiler/conversions.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::reg_index conversion_generator::convert_from(const prog::type& type, location loc) {
        auto result = try_convert_from(type);

        if (!result)
            error(diags::not_convertible(prog, copy_type(type), copy_type(new_type), loc));

        return *result;
    }

    prog::reg_index conversion_generator::convert_from(const prog::type_local& type, location loc) {
        if (type.confined != confined && !type_trivial(prog, *type.tp))
            error(diags::confinement_mismatch(type.confined, loc));

        return convert_from(*type.tp, loc);
    }

    optional<prog::reg_index> conversion_generator::try_convert_from(const prog::type& type) {
        if (types_equal(type, new_type))
            return { value };

        switch (INDEX(type)) {
            case prog::type::NEVER:
                return convert_from_never();

            case prog::type::NUMBER: {
                auto& ntype = *GET(type, NUMBER);
                return try_convert_from_number(ntype);
            }

            case prog::type::TUPLE: {
                auto tuple = as_cref_vector(GET(type, TUPLE));
                return try_convert_from_tuple(tuple);
            }

            case prog::type::ARRAY: {
                auto& array = *GET(type, ARRAY);
                return try_convert_from_array(array);
            }

            case prog::type::OPTIONAL: {
                auto& inner = *GET(type, OPTIONAL);
                return try_convert_from_optional(inner);
            }

            case prog::type::PTR: {
                auto& ptr = *GET(type, PTR);
                return try_convert_from_ptr(ptr);
            }

            case prog::type::INNER_PTR: {
                auto& inptr = *GET(type, INNER_PTR);
                return try_convert_from_inner_ptr(inptr);
            }

            case prog::type::FUNC_WITH_PTR: {
                auto& fptr = *GET(type, FUNC_WITH_PTR);
                return try_convert_from_func_with_ptr(fptr);
            }

            case prog::type::KNOWN_FUNC: {
                auto index = GET(type, KNOWN_FUNC);
                return try_convert_from_known_func(index);
            }
        }

        return { };
    }

    prog::reg_index conversion_generator::convert_from_never() {
        auto result = fclr.new_reg();
        auto instr = prog::from_never_instr { make_ptr(copy_type(new_type)), result };
        fclr.add_instr(VARIANT(prog::instr, FROM_NEVER, into_ptr(instr)));
        return result;
    }

    optional<prog::reg_index> conversion_generator::try_convert_from_number(const prog::number_type& ntype) {
        if (!INDEX_EQ(new_type, NUMBER))
            return { };

        auto& new_ntype = *GET(new_type, NUMBER);

        #define CONVERT(instr_name) { \
            auto result = fclr.new_reg(); \
            auto instr = prog::numeric_conversion_instr { value, into_ptr(new_ntype), result }; \
            fclr.add_instr(VARIANT(prog::instr, instr_name, into_ptr(instr))); \
            return { result }; \
        }

        using num = prog::number_type;

        switch (ntype.tp) {
            case num::BOOL: {
                switch (new_ntype.tp) {
                    case num::I8: case num::I16: case num::I32: case num::I64:
                    case num::U8: case num::U16: case num::U32: case num::U64:
                        CONVERT(ZERO_EXT);
                    default:
                        break;
                }
            } break;

            case num::I8: {
                switch (new_ntype.tp) {
                    case num::I16: case num::I32: case num::I64:
                        CONVERT(SIGNED_EXT);
                    default:
                        break;
                }
            } break;

            case num::I16: {
                switch (new_ntype.tp) {
                    case num::I32: case num::I64:
                        CONVERT(SIGNED_EXT);
                    default:
                        break;
                }
            } break;

            case num::I32: {
                switch (new_ntype.tp) {
                    case num::I64:
                        CONVERT(SIGNED_EXT);
                    default:
                        break;
                }
            } break;

            case num::U8: {
                switch (new_ntype.tp) {
                    case num::U16: case num::U32: case num::U64:
                    case num::I16: case num::I32: case num::I64:
                        CONVERT(ZERO_EXT);
                    default:
                        break;
                }
            } break;

            case num::U16: {
                switch (new_ntype.tp) {
                    case num::U32: case num::U64:
                    case num::I32: case num::I64:
                        CONVERT(ZERO_EXT);
                    default:
                        break;
                }
            } break;

            case num::U32: {
                switch (new_ntype.tp) {
                    case num::U64: case num::I64:
                        CONVERT(ZERO_EXT);
                    default:
                        break;
                }
            } break;

            case num::F32: {
                switch (new_ntype.tp) {
                    case num::F64:
                        CONVERT(FLOAT_EXT);
                    default:
                        break;
                }
            } break;

            default:
                break;
        }

        #undef CONVERT

        return { };
    }

    optional<prog::reg_index> conversion_generator::try_convert_from_tuple(const vector<cref<prog::type>> tuple) {
        if (!INDEX_EQ(new_type, TUPLE))
            return { };

        auto new_tuple = as_cref_vector(GET(new_type, TUPLE));
        auto size = tuple.size();

        if (size != new_tuple.size())
            return { };

        vector<prog::reg_index> values;

        for (size_t index = 0; index < size; index++) {
            auto extracted = fclr.new_reg();
            auto instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

            auto result = conversion_generator(fclr, extracted, new_tuple[index], confined).try_convert_from(tuple[index]);

            if (result)
                values.push_back(*result);
            else
                return { };
        }

        auto result = fclr.new_reg();
        auto instr = prog::make_tuple_instr { values, result };
        fclr.add_instr(VARIANT(prog::instr, MAKE_TUPLE, into_ptr(instr)));
        return { result };
    }

    optional<prog::reg_index> conversion_generator::try_convert_from_array(const prog::array_type& array) {
        if (!INDEX_EQ(new_type, ARRAY))
            return { };

        auto& new_array = *GET(new_type, ARRAY);

        if (array.size != new_array.size)
            return { };

        fclr.push_frame();
        auto extracted = fclr.new_reg();
        auto inner_result = conversion_generator(fclr, extracted, *new_array.tp, confined).try_convert_from(*array.tp);
        auto block = fclr.pop_frame();

        if (!inner_result)
            return { };

        auto result = fclr.new_reg();
        auto instr = prog::transform_instr { value, extracted, into_ptr(block), *inner_result, result };
        fclr.add_instr(VARIANT(prog::instr, TRANSFORM_ARRAY, into_ptr(instr)));
        return { result };
    }

    optional<prog::reg_index> conversion_generator::try_convert_from_optional(const prog::type& inner) {
        if (!INDEX_EQ(new_type, OPTIONAL))
            return { };

        auto& new_inner = *GET(new_type, OPTIONAL);

        fclr.push_frame();
        auto extracted = fclr.new_reg();
        auto inner_result = conversion_generator(fclr, extracted, new_inner, confined).try_convert_from(inner);
        auto block = fclr.pop_frame();

        if (!inner_result)
            return { };

        auto result = fclr.new_reg();
        auto instr = prog::transform_instr { value, extracted, into_ptr(block), *inner_result, result };
        fclr.add_instr(VARIANT(prog::instr, TRANSFORM_OPTIONAL, into_ptr(instr)));
        return { result };
    }

    optional<prog::reg_index> conversion_generator::try_convert_from_ptr(const prog::ptr_type& ptr) {
        if (!INDEX_EQ(new_type, PTR))
            return { };

        auto& new_ptr = *GET(new_type, PTR);

        auto result = try_convert_ptr_kind(value, ptr.kind, new_ptr.kind);
        if (!result)
            return { };

        result = try_convert_ptr_target(*result, *ptr.target_tp, *new_ptr.target_tp);
        if (!result)
            return { };

        return { *result };
    }

    optional<prog::reg_index> conversion_generator::try_convert_from_inner_ptr(const prog::inner_ptr_type& inptr) {
        auto& inner = *inptr.target_tp;
        auto& outer = *inptr.owner_tp;

        if (INDEX_EQ(new_type, PTR)) {
            auto& new_ptr = *GET(new_type, PTR);
            auto& new_target = *new_ptr.target_tp;

            if (!ptr_kind_trivial(inptr.kind))
                return { };

            auto inner_value = fclr.new_reg();
            auto instr = prog::ptr_conversion_instr { value, inner_value };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_INNER_PTR, into_ptr(instr)));

            auto result = try_convert_ptr_kind(inner_value, inptr.kind, new_ptr.kind);
            if (!result)
                return { };

            result = try_convert_ptr_target(*result, inner, new_target);
            if (!result)
                return { };

            return { *result };
        }

        else if (INDEX_EQ(new_type, INNER_PTR)) {
            auto& new_inptr = *GET(new_type, INNER_PTR);
            auto& new_inner = *new_inptr.target_tp;
            auto& new_outer = *new_inptr.owner_tp;

            auto outer_value = fclr.new_reg();
            auto outer_extract_instr = prog::ptr_conversion_instr { value, outer_value };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_OUTER_PTR, into_ptr(outer_extract_instr)));

            auto inner_value = fclr.new_reg();
            auto inner_extract_instr = prog::ptr_conversion_instr { value, inner_value };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_INNER_PTR, into_ptr(inner_extract_instr)));

            auto outer_result = try_convert_ptr_kind(outer_value, inptr.kind, new_inptr.kind);
            if (!outer_result)
                return { };

            outer_result = try_convert_ptr_target(*outer_result, outer, new_outer);
            if (!outer_result)
                return { };

            auto inner_result = try_convert_ptr_target(inner_value, inner, new_inner);
            if (!inner_result)
                return { };

            auto result = fclr.new_reg();
            auto instr = prog::make_joint_inner_ptr_instr { *inner_result, *outer_result, result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_JOINT_INNER_PTR, into_ptr(instr)));
            return { result };
        }

        return { };
    }

    optional<prog::reg_index> conversion_generator::try_convert_from_func_with_ptr(const prog::func_with_ptr_type& fptr) {
        if (INDEX_EQ(new_type, FUNC_WITH_PTR)) {
            auto& new_fptr = *GET(new_type, FUNC_WITH_PTR);

            if (!func_types_equal(fptr, new_fptr))
                return { };

            auto ptr_value = fclr.new_reg();
            auto extract_ptr_instr = prog::ptr_conversion_instr { value, ptr_value };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(extract_ptr_instr)));

            auto func_value = fclr.new_reg();
            auto extract_func_instr = prog::ptr_conversion_instr { value, func_value };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FUNC_PTR, into_ptr(extract_func_instr)));

            auto ptr_result = try_convert_ptr_kind(ptr_value, fptr.kind, new_fptr.kind);
            if (!ptr_result)
                return { };

            ptr_result = try_convert_ptr_target(*ptr_result, *fptr.target_tp, *new_fptr.target_tp);
            if (!ptr_result)
                return { };

            auto result = fclr.new_reg();
            auto make_instr = prog::make_joint_func_ptr_instr { *ptr_result, func_value, result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_JOINT_FUNC_PTR, into_ptr(make_instr)));

            return { result };
        }

        else if (INDEX_EQ(new_type, FUNC) && ptr_kind_trivial(fptr.kind))
            return { value };

        else if (INDEX_EQ(new_type, PTR)) {
            auto& new_ptr = *GET(new_type, PTR);

            auto extracted = fclr.new_reg();
            auto instr = prog::ptr_conversion_instr { value, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(instr)));

            auto result = try_convert_ptr_kind(extracted, fptr.kind, new_ptr.kind);
            if (!result)
                return { };

            result = try_convert_ptr_target(*result, *fptr.target_tp, *new_ptr.target_tp);
            if (!result)
                return { };

            return { *result };
        }

        return { };
    }

    optional<prog::reg_index> conversion_generator::try_convert_from_known_func(prog::global_index index) {
        auto& func = *prog.global_funcs[index];
        auto ftype = get_func_type(func);

        if (INDEX_EQ(new_type, GLOBAL_FUNC)) {
            if (!func_types_equal(ftype, *GET(new_type, GLOBAL_FUNC)))
                return { };

            auto result = fclr.new_reg();
            auto constant = VARIANT(prog::constant, GLOBAL_FUNC_PTR, index);
            auto type = VARIANT(prog::type, KNOWN_FUNC, index);
            auto make_instr = prog::make_const_instr { into_ptr(constant), into_ptr(type), result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(make_instr)));
            return { result };
        }

        else if (INDEX_EQ(new_type, FUNC)) {
            if (!func_types_equal(ftype, *GET(new_type, FUNC)))
                return { };

            auto wrapper_index = clr.get_global_func_wrapper(index);

            auto value = fclr.new_reg();
            auto constant = VARIANT(prog::constant, GLOBAL_FUNC_PTR, wrapper_index);
            auto type = VARIANT(prog::type, KNOWN_FUNC, index);
            auto make_instr = prog::make_const_instr { into_ptr(constant), into_ptr(type), value };
            fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(make_instr)));

            auto result = fclr.new_reg();
            auto into_instr = prog::make_joint_func_ptr_instr { value, { }, result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_JOINT_FUNC_PTR, into_ptr(into_instr)));

            return { result };
        }

        return { };
    }

    optional<prog::reg_index> conversion_generator::try_convert_ptr_kind(prog::reg_index value, prog::ptr_type::kind_t kind, prog::ptr_type::kind_t new_kind) {
        if (kind == new_kind)
            return { value };

        switch (kind) {
            case prog::ptr_type::GLOBAL: {
                if (new_kind == prog::ptr_type::BASIC)
                    return { value };
            } break;

            case prog::ptr_type::SHARED: {
                switch (new_kind) {
                    case prog::ptr_type::WEAK: {
                        if (!confined) {
                            fclr.add_instr(VARIANT(prog::instr, DECR_REF_COUNT, value));
                            fclr.add_instr(VARIANT(prog::instr, INCR_WEAK_REF_COUNT, value));
                        } return { value };
                    }

                    case prog::ptr_type::BASIC: {
                        if (confined) {
                            auto result = fclr.new_reg();
                            auto instr = prog::ptr_conversion_instr { value, result };
                            fclr.add_instr(VARIANT(prog::instr, FORGET_REF_COUNTER, into_ptr(instr)));
                            return { result };
                        }
                    } break;

                    default:
                        break;
                }
            } break;

            case prog::ptr_type::UNIQUE: {
                switch (new_kind) {
                    case prog::ptr_type::SHARED: {
                        if (!confined) {
                            auto result = fclr.new_reg();
                            auto instr = prog::ptr_conversion_instr { value, result };
                            fclr.add_instr(VARIANT(prog::instr, ALLOC_REF_COUNTER, into_ptr(instr)));
                            return { result };
                        }
                    } break;

                    case prog::ptr_type::BASIC: {
                        if (confined)
                            return { value };
                    } break;

                    default:
                        break;
                }
            } break;

            default:
                break;
        }

        return { };
    }

    optional<prog::reg_index> conversion_generator::try_convert_ptr_target(
            prog::reg_index value,
            const prog::type_pointed& type,
            const prog::type_pointed& new_type) {
        if (type.slice == new_type.slice && types_equal(*type.tp, *new_type.tp))
            return { value };

        if (!type.slice && new_type.slice && INDEX_EQ(*type.tp, ARRAY)) {
            auto& array = *GET(*type.tp, ARRAY);

            if (types_equal(*array.tp, *new_type.tp) || INDEX_EQ(*array.tp, NEVER)) {
                auto result = fclr.new_reg();
                auto instr = prog::ptr_conversion_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, ARRAY_PTR_INTO_SLICE, into_ptr(instr)));
                return { result };
            }
        }

        if (type.slice && new_type.slice && INDEX_EQ(*type.tp, NEVER))
            return { value };

        return { };
    }

    bool conversion_generator::ptr_kind_trivial(prog::ptr_type::kind_t kind) {
        switch (kind) {
            case prog::ptr_type::GLOBAL:
            case prog::ptr_type::BASIC:
                return true;

            case prog::ptr_type::SHARED:
            case prog::ptr_type::UNIQUE: {
                if (confined)
                    return true;
            } break;

            default:
                break;
        }

        return false;
    }
}
