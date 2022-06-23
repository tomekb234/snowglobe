#include "compiler/compiler_utils.hpp"
#include "compiler/functions.hpp"
#include "compiler/conversions.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::type compiler_utils::common_supertype(const prog::type& type_a, const prog::type& type_b, location loc) {
        prog::global_func func = { { }, { }, { }, make_ptr(copy_type(prog::UNIT_TYPE)), { }, { } };
        function_compiler fclr(clr, func);

        fclr.init();

        if (conversion_generator(fclr, 0, type_b).try_convert_from(type_a))
            return copy_type(type_b);

        if (conversion_generator(fclr, 0, type_a).try_convert_from(type_b))
            return copy_type(type_a);

        fclr.commit();

        error(diags::no_common_supertype(prog, copy_type(type_a), copy_type(type_b), loc));
    }

    prog::constant compiler_utils::convert_const(prog::constant value, const prog::type& type, const prog::type& new_type, location loc) {
        unordered_map<prog::reg_index, prog::constant> reg_values;
        unordered_map<prog::reg_index, prog::type> reg_types;

        reg_values[0] = move(value);
        reg_types[0] = copy_type(type);

        function<void(const prog::instr&)> do_instr;

        do_instr = [&] (const prog::instr& instr) {
            switch (INDEX(instr)) {
                case prog::instr::MAKE_CONST: {
                    auto& make_instr = *GET(instr, MAKE_CONST);
                    reg_values[make_instr.result] = copy_const(*make_instr.value);
                    reg_types[make_instr.result] = copy_type(*make_instr.tp);
                } break;

                case prog::instr::FROM_NEVER:
                    break;

                case prog::instr::ZERO_EXT:
                case prog::instr::SIGNED_EXT:
                case prog::instr::FLOAT_EXT: {
                    auto& conversion_instr =
                        INDEX_EQ(instr, ZERO_EXT) ? *GET(instr, ZERO_EXT)
                        : INDEX_EQ(instr, SIGNED_EXT) ? *GET(instr, SIGNED_EXT)
                        : *GET(instr, FLOAT_EXT);

                    auto value_reg = conversion_instr.value;
                    auto result_reg = conversion_instr.result;
                    auto& new_ntype = *conversion_instr.new_type;

                    auto number = GET(reg_values[value_reg], NUMBER);
                    auto& ntype = *GET(reg_types[value_reg], NUMBER);

                    #define CONVERSION(from, type_from, to, type_to) { \
                        if (ntype.tp == prog::number_type::from && new_ntype.tp == prog::number_type::to) { \
                            auto decoded = decode_number<type_from>(number); \
                            auto converted = static_cast<type_to>(decoded); \
                            auto encoded = encode_number(converted); \
                            reg_values[result_reg] = VARIANT(prog::constant, NUMBER, encoded); \
                            reg_types[result_reg] = VARIANT(prog::type, NUMBER, make_ptr(prog::number_type { new_ntype.tp })); \
                            break; \
                        } \
                    }

                    CONVERSION(BOOL, bool, I8, int8_t);
                    CONVERSION(BOOL, bool, I16, int16_t);
                    CONVERSION(BOOL, bool, I32, int32_t);
                    CONVERSION(BOOL, bool, I64, int64_t);
                    CONVERSION(BOOL, bool, U8, uint8_t);
                    CONVERSION(BOOL, bool, U16, uint16_t);
                    CONVERSION(BOOL, bool, U32, uint32_t);
                    CONVERSION(BOOL, bool, U64, uint64_t);

                    CONVERSION(I8, int8_t, I16, int16_t);
                    CONVERSION(I8, int8_t, I32, int32_t);
                    CONVERSION(I8, int8_t, I64, int64_t);

                    CONVERSION(I16, int16_t, I32, int32_t);
                    CONVERSION(I16, int16_t, I64, int64_t);

                    CONVERSION(I32, int32_t, I64, int64_t);

                    CONVERSION(U8, uint8_t, U16, uint16_t);
                    CONVERSION(U8, uint8_t, U32, uint32_t);
                    CONVERSION(U8, uint8_t, U64, uint64_t);
                    CONVERSION(U8, uint8_t, I16, int16_t);
                    CONVERSION(U8, uint8_t, I32, int32_t);
                    CONVERSION(U8, uint8_t, I64, int64_t);

                    CONVERSION(U16, uint16_t, U32, uint32_t);
                    CONVERSION(U16, uint16_t, U64, uint64_t);
                    CONVERSION(U16, uint16_t, I32, int32_t);
                    CONVERSION(U16, uint16_t, I64, int64_t);

                    CONVERSION(U32, uint32_t, U64, uint64_t);
                    CONVERSION(U32, uint16_t, I64, int64_t);

                    CONVERSION(F32, float, F64, double);

                    UNREACHABLE;

                    #undef CONVERSION
                } break;

                case prog::instr::EXTRACT_FIELD: {
                    auto& extract_instr = *GET(instr, EXTRACT_FIELD);

                    auto values = as_cref_vector(GET(reg_values[extract_instr.value], TUPLE));
                    auto types = as_cref_vector(GET(reg_types[extract_instr.value], TUPLE));

                    reg_values[extract_instr.result] = copy_const(values[extract_instr.field]);
                    reg_types[extract_instr.result] = copy_type(types[extract_instr.field]);
                } break;

                case prog::instr::MAKE_TUPLE: {
                    auto& make_instr = *GET(instr, MAKE_TUPLE);

                    vector<prog::constant> values;
                    vector<prog::type> types;

                    for (auto reg : make_instr.values) {
                        values.push_back(copy_const(reg_values[reg]));
                        types.push_back(copy_type(reg_types[reg]));
                    }

                    reg_values[make_instr.result] = VARIANT(prog::constant, TUPLE, into_ptr_vector(values));
                    reg_types[make_instr.result] = VARIANT(prog::type, TUPLE, into_ptr_vector(types));
                } break;

                case prog::instr::TRANSFORM_ARRAY: {
                    auto& transform_instr = *GET(instr, TRANSFORM_ARRAY);

                    auto values = as_cref_vector(GET(reg_values[transform_instr.value], ARRAY));
                    vector<prog::constant> new_values;
                    auto new_type = copy_type(prog::NEVER_TYPE);

                    for (const prog::constant& extracted : values) {
                        reg_values[transform_instr.extracted] = copy_const(extracted);
                        for (const prog::instr& instr : as_cref_vector(transform_instr.block->instrs))
                            do_instr(instr);
                        new_values.push_back(copy_const(reg_values[transform_instr.transformed]));
                        new_type = copy_type(reg_types[transform_instr.transformed]);
                    }

                    auto size = new_values.size();

                    reg_values[transform_instr.result] = VARIANT(prog::constant, ARRAY, into_ptr_vector(new_values));
                    reg_types[transform_instr.result] = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(new_type), size }));
                } break;

                case prog::instr::TRANSFORM_OPTIONAL: {
                    auto& transform_instr = *GET(instr, TRANSFORM_OPTIONAL);

                    auto value = as_optional_cref(GET(reg_values[transform_instr.value], OPTIONAL));
                    optional<prog::constant> new_value;
                    auto new_type = copy_type(prog::NEVER_TYPE);

                    if (value) {
                        reg_values[transform_instr.extracted] = copy_const(*value);
                        for (const prog::instr& instr : as_cref_vector(transform_instr.block->instrs))
                            do_instr(instr);
                        new_value = { copy_const(reg_values[transform_instr.transformed]) };
                        new_type = copy_type(reg_types[transform_instr.transformed]);
                    }

                    reg_values[transform_instr.result] = VARIANT(prog::constant, OPTIONAL, into_optional_ptr(new_value));
                    reg_types[transform_instr.result] = VARIANT(prog::type, OPTIONAL, into_ptr(new_type));
                } break;

                case prog::instr::MAKE_JOINT_FUNC_PTR: {
                    auto& make_instr = *GET(instr, MAKE_JOINT_FUNC_PTR);
                    auto index = GET(reg_values[make_instr.func_ptr], GLOBAL_FUNC_PTR);
                    reg_values[make_instr.result] = VARIANT(prog::constant, GLOBAL_FUNC_WRAPPER_PTR, index);
                } break;

                case prog::instr::ARRAY_PTR_INTO_SLICE: {
                    auto& conversion_instr = *GET(instr, ARRAY_PTR_INTO_SLICE);
                    auto index = GET(reg_values[conversion_instr.value], GLOBAL_VAR_PTR);
                    reg_values[conversion_instr.result] = VARIANT(prog::constant, GLOBAL_VAR_SLICE, index);
                } break;

                default:
                    UNREACHABLE;
            }
        };

        prog::global_func func = { { }, { }, { }, make_ptr(copy_type(prog::UNIT_TYPE)), { }, { } };
        function_compiler fclr(clr, func);

        fclr.init();
        auto result = conversion_generator(fclr, 0, new_type).convert_from(type, loc);
        fclr.commit();

        for (const prog::instr& instr : as_cref_vector(func.instrs))
            do_instr(instr);

        return move(reg_values[result]);
    }

    vector<cref<ast::expr>> compiler_utils::order_args(
            vector<cref<ast::expr_marked>> asts,
            optional<function<size_t(string, location)>> arg_with_name,
            optional<size_t> expected_count,
            location loc) {
        auto count = asts.size();

        if (expected_count && count != *expected_count)
            error(diags::invalid_argument_count(count, *expected_count, loc));

        vector<bool> used(count, false);
        vector<const ast::expr*> value_ast_ptrs(count);

        for (const ast::expr_marked& arg_ast : asts) {
            size_t index = 0;
            const ast::expr* value_ast_ptr;

            switch (INDEX(arg_ast)) {
                case ast::expr_marked::EXPR: {
                    while (index < count && used[index])
                        index++;
                    value_ast_ptr = GET(arg_ast, EXPR).get();
                } break;

                case ast::expr_marked::EXPR_WITH_NAME: {
                    auto name = GET(arg_ast, EXPR_WITH_NAME).first;
                    if (arg_with_name)
                        index = (*arg_with_name)(name, arg_ast.loc);
                    else
                        error(diags::invalid_argument_marker(arg_ast.loc));
                    value_ast_ptr = GET(arg_ast, EXPR_WITH_NAME).second.get();
                } break;

                case ast::expr_marked::EXPR_WITH_INDEX: {
                    index = GET(arg_ast, EXPR_WITH_INDEX).first;
                    value_ast_ptr = GET(arg_ast, EXPR_WITH_INDEX).second.get();
                } break;

                default:
                    UNREACHABLE;
            }

            if (index >= count)
                error(diags::invalid_argument_index(index, count, arg_ast.loc));
            if (used[index])
                error(diags::duplicate_argument_index(index, arg_ast.loc));

            value_ast_ptrs[index] = value_ast_ptr;
            used[index] = true;
        }

        for (size_t index = 0; index < count; index++) {
            if (!used[index])
                error(diags::missing_argument(index, loc));
        }

        vector<cref<ast::expr>> value_asts;

        for (auto value_ast_ptr : value_ast_ptrs)
            value_asts.push_back(*value_ast_ptr);

        return value_asts;
    }
}
