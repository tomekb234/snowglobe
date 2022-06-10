#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    static bool ptr_kind_trivial(prog::ptr_type::kind_t kind, bool confined);

    prog::type compiler::common_supertype(const ast::node& ast, const prog::type& type1, const prog::type& type2) {
        auto new_reg = [] () -> prog::reg_index { return 0; };
        auto add_instr = [] (prog::instr&&) { };
        conversion_compiler conv_clr(*this, new_reg, add_instr);

        if (conv_clr.try_convert(type1, type2, false, 0))
            return prog::copy_type(type2);

        if (conv_clr.try_convert(type2, type1, false, 0))
            return prog::copy_type(type1);

        error(diags::no_common_supertype(program, copy_type(type1), copy_type(type2)), ast);
    }

    prog::reg_index conversion_compiler::convert(const ast::node& ast, const prog::type& type1, const prog::type& type2, prog::reg_index value) {
        return convert(ast, type1, type2, false, value);
    }

    prog::reg_index conversion_compiler::convert(const ast::node& ast, const prog::type_local& type1, const prog::type_local& type2, prog::reg_index value) {
        if (type1.confined != type2.confined && !clr.type_trivially_copyable(*type1.tp))
            clr.error(diags::type_confinement_mismatch(type1.confined, type2.confined, false), ast);

        return convert(ast, *type1.tp, *type2.tp, type2.confined, value);
    }

    prog::reg_index conversion_compiler::convert(const ast::node& ast, const prog::type_local& type1, const prog::type& type2, prog::reg_index value) {
        if (type1.confined && !clr.type_trivially_copyable(*type1.tp))
            clr.error(diags::type_confinement_mismatch(type1.confined, false, false), ast);

        return convert(ast, *type1.tp, type2, false, value);
    }

    prog::reg_index conversion_compiler::convert_call(const ast::node& ast, const prog::type_local& type1, const prog::type_local& type2, prog::reg_index value) {
        if ((!type1.confined || type2.confined) && !clr.type_trivially_copyable(*type1.tp))
            clr.error(diags::type_confinement_mismatch(type1.confined, type2.confined, true), ast);

        return convert(ast, *type1.tp, *type2.tp, type2.confined, value);
    }

    prog::reg_index conversion_compiler::convert(const ast::node& ast, const prog::type& type1, const prog::type& type2, bool confined, prog::reg_index value) {
        auto result = try_convert(type1, type2, confined, value);

        if (!result)
            clr.error(diags::not_convertible(clr.program, copy_type(type1), copy_type(type2)), ast);

        return *result;
    }

    optional<prog::reg_index> conversion_compiler::try_convert(const prog::type& type1, const prog::type& type2, bool confined, prog::reg_index value) {
        if (prog::types_equal(type1, type2))
            return { value };

        switch (INDEX(type1)) {
            case prog::type::NEVER:
                return { value };

            case prog::type::PRIMITIVE: {
                if (!INDEX_EQ(type2, PRIMITIVE))
                    break;

                auto& ptype1 = *GET(type1, PRIMITIVE);
                auto& ptype2 = *GET(type2, PRIMITIVE);

                switch (ptype1.tp) {
                    case prog::primitive_type::BOOL: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::I8:
                            case prog::primitive_type::I16:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64:
                            case prog::primitive_type::U8:
                            case prog::primitive_type::U16:
                            case prog::primitive_type::U32:
                            case prog::primitive_type::U64: {
                                auto result = new_reg();
                                add_instr(VARIANT(prog::instr, ZERO_EXT, make_ptr(prog::primitive_conversion_instr { value, into_ptr(ptype2), result })));
                                return { result };
                            }

                            default:
                                break;
                        }
                    } break;

                    case prog::primitive_type::I8: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::I16:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64: {
                                auto result = new_reg();
                                add_instr(VARIANT(prog::instr, SIGNED_EXT, make_ptr(prog::primitive_conversion_instr { value, into_ptr(ptype2), result })));
                                return { result };
                            }

                            default:
                                break;
                        }
                    } break;

                    case prog::primitive_type::I16: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64: {
                                auto result = new_reg();
                                add_instr(VARIANT(prog::instr, SIGNED_EXT, make_ptr(prog::primitive_conversion_instr { value, into_ptr(ptype2), result })));
                                return { result };
                            }

                            default:
                                break;
                        }
                    } break;

                    case prog::primitive_type::I32: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::I64: {
                                auto result = new_reg();
                                add_instr(VARIANT(prog::instr, SIGNED_EXT, make_ptr(prog::primitive_conversion_instr { value, into_ptr(ptype2), result })));
                                return { result };
                            }

                            default:
                                break;
                        }
                    } break;

                    case prog::primitive_type::U8: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::U16:
                            case prog::primitive_type::U32:
                            case prog::primitive_type::U64:
                            case prog::primitive_type::I16:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64: {
                                auto result = new_reg();
                                add_instr(VARIANT(prog::instr, ZERO_EXT, make_ptr(prog::primitive_conversion_instr { value, into_ptr(ptype2), result })));
                                return { result };
                            }

                            default:
                                break;
                        }
                    } break;

                    case prog::primitive_type::U16: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::U32:
                            case prog::primitive_type::U64:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64: {
                                auto result = new_reg();
                                add_instr(VARIANT(prog::instr, ZERO_EXT, make_ptr(prog::primitive_conversion_instr { value, into_ptr(ptype2), result })));
                                return { result };
                            }

                            default:
                                break;
                        }
                    } break;

                    case prog::primitive_type::U32: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::U64:
                            case prog::primitive_type::I64: {
                                auto result = new_reg();
                                add_instr(VARIANT(prog::instr, ZERO_EXT, make_ptr(prog::primitive_conversion_instr { value, into_ptr(ptype2), result })));
                                return { result };
                            }

                            default:
                                break;
                        }
                    } break;

                    case prog::primitive_type::F32: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::F64: {
                                auto result = new_reg();
                                add_instr(VARIANT(prog::instr, FLOAT_EXT, make_ptr(prog::primitive_conversion_instr { value, into_ptr(ptype2), result })));
                                return { result };
                            }

                            default:
                                break;
                        }
                    } break;

                    default:
                        break;
                }
            } break;

            case prog::type::TUPLE: {
                if (!INDEX_EQ(type2, TUPLE))
                    break;

                auto& tuple1 = GET(type1, TUPLE);
                auto& tuple2 = GET(type2, TUPLE);
                auto size = tuple1.size();

                if (size != tuple2.size())
                    break;

                auto ok = true;
                vector<prog::reg_index> values;

                for (size_t index = 0; index < size; index++) {
                    auto extracted = new_reg();
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, make_ptr(prog::extract_field_instr { value, index, extracted })));

                    auto result = try_convert(*tuple1[index], *tuple2[index], confined, extracted);

                    if (result)
                        values.push_back(*result);
                    else {
                        ok = false;
                        break;
                    }
                }

                if (ok) {
                    auto result = new_reg();
                    add_instr(VARIANT(prog::instr, MAKE_TUPLE, make_ptr(prog::make_tuple_instr { values, result })));
                    return { result };
                }
            } break;

            case prog::type::ARRAY: {
                if (!INDEX_EQ(type2, ARRAY))
                    break;

                auto& array1 = *GET(type1, ARRAY);
                auto& array2 = *GET(type2, ARRAY);

                if (array1.size != array2.size)
                    break;

                auto extracted = new_reg();
                vector<prog::instr> inner_instrs;

                conversion_compiler inner_clr(clr, new_reg, [&] (prog::instr&& instr) { inner_instrs.push_back(move(instr)); });
                auto inner_result = inner_clr.try_convert(*array1.tp, *array2.tp, confined, extracted);
                if (!inner_result)
                    break;

                auto result = new_reg();
                add_instr(VARIANT(prog::instr, TRANSFORM_ARRAY, make_ptr(prog::transform_instr { value, extracted, into_ptr_vector(inner_instrs), *inner_result, result })));
                return { result };
            }

            case prog::type::OPTIONAL: {
                if (INDEX_EQ(type2, PRIMITIVE) && GET(type2, PRIMITIVE)->tp == prog::primitive_type::BOOL) {
                    auto result = new_reg();
                    add_instr(VARIANT(prog::instr, TEST_OPTIONAL, make_ptr(prog::test_optional_instr { value, result })));
                    return { result };
                }

                if (!INDEX_EQ(type2, OPTIONAL))
                    break;

                auto& inner1 = *GET(type1, OPTIONAL);
                auto& inner2 = *GET(type2, OPTIONAL);

                auto extracted = new_reg();
                vector<prog::instr> inner_instrs;

                conversion_compiler inner_clr(clr, new_reg, [&] (prog::instr&& instr) { inner_instrs.push_back(move(instr)); });
                auto inner_result = inner_clr.try_convert(inner1, inner2, confined, extracted);
                if (!inner_result)
                    break;

                auto result = new_reg();
                add_instr(VARIANT(prog::instr, TRANSFORM_OPTIONAL, make_ptr(prog::transform_instr { value, extracted, into_ptr_vector(inner_instrs), *inner_result, result })));
                return { result };
            }

            case prog::type::PTR: {
                if (!INDEX_EQ(type2, PTR))
                    break;

                auto& ptr1 = *GET(type1, PTR);
                auto& ptr2 = *GET(type2, PTR);

                auto result = try_convert_ptr_kind(ptr1.kind, ptr2.kind, confined, value);
                if (!result)
                    break;

                result = try_convert_ptr_target(*ptr1.target_tp, *ptr2.target_tp, *result);
                if (!result)
                    break;

                return { *result };
            }

            case prog::type::INNER_PTR: {
                auto& inptr1 = *GET(type1, INNER_PTR);
                auto& inner1 = *inptr1.target_tp;
                auto& outer1 = *inptr1.owner_tp;

                if (INDEX_EQ(type2, PTR)) {
                    auto& ptr2 = *GET(type2, PTR);
                    auto& target2 = *ptr2.target_tp;

                    if (!ptr_kind_trivial(inptr1.kind, confined))
                        break;

                    auto inner_value = new_reg();
                    add_instr(VARIANT(prog::instr, EXTRACT_INNER_PTR, make_ptr(prog::ptr_conversion_instr { value, inner_value })));

                    auto result = try_convert_ptr_kind(inptr1.kind, ptr2.kind, confined, inner_value);
                    if (!result)
                        break;

                    result = try_convert_ptr_target(inner1, target2, *result);
                    if (!result)
                        break;

                    return { *result };
                }

                else if (INDEX_EQ(type2, INNER_PTR)) {
                    auto& inptr2 = *GET(type2, INNER_PTR);
                    auto& inner2 = *inptr2.target_tp;
                    auto& outer2 = *inptr2.owner_tp;

                    auto outer_value = new_reg();
                    add_instr(VARIANT(prog::instr, EXTRACT_OUTER_PTR, make_ptr(prog::ptr_conversion_instr { value, outer_value })));

                    auto inner_value = new_reg();
                    add_instr(VARIANT(prog::instr, EXTRACT_INNER_PTR, make_ptr(prog::ptr_conversion_instr { value, inner_value })));

                    auto outer_result = try_convert_ptr_kind(inptr1.kind, inptr2.kind, confined, outer_value);
                    if (!outer_result)
                        break;

                    outer_result = try_convert_ptr_target(outer1, outer2, *outer_result);
                    if (!outer_result)
                        break;

                    auto inner_result = try_convert_ptr_target(inner1, inner2, inner_value);
                    if (!inner_result)
                        break;

                    auto result = new_reg();
                    add_instr(VARIANT(prog::instr, MAKE_INNER_PTR, make_ptr(prog::make_inner_ptr_instr { *outer_result, *inner_result, result })));
                    return { result };
                }
            } break;

            case prog::type::FUNC_WITH_PTR: {
                auto& fptr1 = *GET(type1, FUNC_WITH_PTR);

                if (INDEX_EQ(type2, FUNC_WITH_PTR)) {
                    auto& fptr2 = *GET(type2, FUNC_WITH_PTR);

                    auto result = try_convert_ptr_kind(fptr1.kind, fptr2.kind, confined, value);
                    if (!result)
                        break;

                    if (prog::func_types_equal(fptr1, fptr2))
                        return { *result };
                }

                else if (INDEX_EQ(type2, FUNC) && ptr_kind_trivial(fptr1.kind, confined)) {
                    auto& func2 = *GET(type2, FUNC);

                    auto result = new_reg();
                    add_instr(VARIANT(prog::instr, EXTRACT_FUNC, make_ptr(prog::ptr_conversion_instr { value, result })));

                    if (prog::func_types_equal(fptr1, func2))
                        return { result };
                }

                else if (INDEX_EQ(type2, PTR)) {
                    auto& ptr2 = *GET(type2, PTR);

                    auto extracted = new_reg();
                    add_instr(VARIANT(prog::instr, EXTRACT_PTR, make_ptr(prog::ptr_conversion_instr { value, extracted })));

                    auto result = try_convert_ptr_kind(fptr1.kind, ptr2.kind, confined, extracted);
                    if (!result)
                        break;

                    result = try_convert_ptr_target(*fptr1.target_tp, *ptr2.target_tp, *result);
                    if (!result)
                        break;

                    return { *result };
                }
            } break;

            case prog::type::KNOWN_FUNC: {
                auto index = GET(type1, KNOWN_FUNC);
                auto& func = *clr.program.global_funcs[index];

                if (INDEX_EQ(type2, GLOBAL_FUNC)) {
                    auto ftype1 = prog::get_func_type(func);
                    auto& ftype2 = *GET(type2, GLOBAL_FUNC);

                    if (!prog::func_types_equal(ftype1, ftype2))
                        break;

                    auto result = new_reg();
                    add_instr(VARIANT(prog::instr, MAKE_GLOBAL_FUNC_PTR, make_ptr(prog::make_global_ptr_instr { index, result })));
                    return { result };
                }

                else if (INDEX_EQ(type2, FUNC)) {
                    auto ftype1 = prog::get_func_type(func);
                    auto& ftype2 = *GET(type2, GLOBAL_FUNC);

                    if (!prog::func_types_equal(ftype1, ftype2))
                        break;

                    // TODO
                }
            } break;
        }

        return { };
    }

    optional<prog::reg_index> conversion_compiler::try_convert_ptr_kind(prog::ptr_type::kind_t kind1, prog::ptr_type::kind_t kind2, bool confined, prog::reg_index value) {
        if (kind1 == kind2)
            return { value };

        switch (kind1) {
            case prog::ptr_type::GLOBAL: {
                switch (kind2) {
                    case prog::ptr_type::BASIC:
                        return { value };

                    case prog::ptr_type::SHARED:
                    case prog::ptr_type::WEAK: {
                        if (confined) {
                            auto result = new_reg();
                            add_instr(VARIANT(prog::instr, MAKE_FAKE_SHARED_PTR, make_ptr(prog::ptr_conversion_instr { value, result })));
                            return { result };
                        }
                    } break;

                    case prog::ptr_type::UNIQUE: {
                        if (confined)
                            return { value };
                    } break;

                    default:
                        break;
                }
            } break;

            case prog::ptr_type::BASIC: {
                switch (kind2) {
                    case prog::ptr_type::SHARED:
                    case prog::ptr_type::WEAK: {
                        if (confined) {
                            auto result = new_reg();
                            add_instr(VARIANT(prog::instr, MAKE_FAKE_SHARED_PTR, make_ptr(prog::ptr_conversion_instr { value, result })));
                            return { result };
                        }
                    } break;

                    case prog::ptr_type::UNIQUE: {
                        if (confined)
                            return { value };
                    } break;

                    default:
                        break;
                }
            } break;

            case prog::ptr_type::SHARED: {
                switch (kind2) {
                    case prog::ptr_type::WEAK: {
                        if (confined)
                            return { value };
                        else {
                            auto result = new_reg();
                            add_instr(VARIANT(prog::instr, ADD_WEAK_REF, make_ptr(prog::ptr_conversion_instr { value, result })));
                            return { result };
                        }
                    } break;

                    case prog::ptr_type::BASIC:
                    case prog::ptr_type::UNIQUE: {
                        if (confined) {
                            auto result = new_reg();
                            add_instr(VARIANT(prog::instr, FORGET_REF_COUNTER, make_ptr(prog::ptr_conversion_instr { value, result })));
                            return { result };
                        }
                    } break;

                    default:
                        break;
                }
            } break;

            case prog::ptr_type::UNIQUE: {
                switch (kind2) {
                    case prog::ptr_type::SHARED: {
                        if (confined) {
                            auto result = new_reg();
                            add_instr(VARIANT(prog::instr, MAKE_FAKE_SHARED_PTR, make_ptr(prog::ptr_conversion_instr { value, result })));
                            return { result };
                        } else {
                            auto result = new_reg();
                            add_instr(VARIANT(prog::instr, MAKE_SHARED_PTR, make_ptr(prog::ptr_conversion_instr { value, result })));
                            return { result };
                        }
                    } break;

                    case prog::ptr_type::WEAK: {
                        if (confined) {
                            auto result = new_reg();
                            add_instr(VARIANT(prog::instr, MAKE_FAKE_SHARED_PTR, make_ptr(prog::ptr_conversion_instr { value, result })));
                            return { result };
                        } else {
                            auto result = new_reg();
                            add_instr(VARIANT(prog::instr, MAKE_EMPTY_WEAK_PTR, make_ptr(prog::ptr_conversion_instr { value, result })));
                            // TODO this should cause a warning
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

    optional<prog::reg_index> conversion_compiler::try_convert_ptr_target(const prog::type_pointed& type1, const prog::type_pointed& type2, prog::reg_index value) {
        if (type1.slice == type2.slice && prog::types_equal(*type1.tp, *type2.tp))
            return { value };

        if (!type1.slice && type2.slice && INDEX_EQ(*type1.tp, ARRAY)) {
            auto& array1 = *GET(*type1.tp, ARRAY);

            if (prog::types_equal(*array1.tp, *type2.tp) || INDEX_EQ(*array1.tp, NEVER)) {
                auto result = new_reg();
                add_instr(VARIANT(prog::instr, MAKE_SLICE, make_ptr(prog::ptr_conversion_instr { value, result })));
                return { result };
            }
        }

        if (type1.slice && type2.slice && INDEX_EQ(*type1.tp, NEVER))
            return { value };

        return { };
    }

    static bool ptr_kind_trivial(prog::ptr_type::kind_t kind, bool confined) {
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
