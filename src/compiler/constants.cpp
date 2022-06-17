#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    template<typename T>
    static optional<T> try_make_number(unsigned long long abs_value, bool negative);

    template<typename T>
    static unsigned long long encode_number(T number);

    template<typename T>
    static T decode_number(unsigned long long number);

    pair<prog::constant, prog::type> compiler::compile_const(const ast::expr& ast) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
                return compile_const_tuple(as_cref_vector(GET(ast, TUPLE)), ast.loc);

            case ast::expr::ARRAY:
                return compile_const_array(as_cref_vector(GET(ast, ARRAY)), ast.loc);

            case ast::expr::APPLICATION: {
                auto& [receiver_ast_ptr, arg_ast_ptrs] = GET(ast, APPLICATION);
                return compile_const_application(*receiver_ast_ptr, as_cref_vector(arg_ast_ptrs), ast.loc);
            }

            case ast::expr::NAME:
                return compile_const_name(GET(ast, NAME), ast.loc);

            case ast::expr::VARIANT_NAME: {
                auto [name, variant_name] = GET(ast, VARIANT_NAME);
                return compile_const_variant_name(name, variant_name, ast.loc);
            }

            case ast::expr::LITERAL:
                return compile_const_literal(*GET(ast, LITERAL));

            case ast::expr::UNARY_OPERATION:
            case ast::expr::BINARY_OPERATION:
            case ast::expr::NUMERIC_CAST:
                error(diags::not_implemented(), ast.loc); // TODO compile-time arithmetic

            case ast::expr::SOME: {
                auto [inner_value, inner_type] = compile_const(*GET(ast, SOME));
                auto value = VARIANT(prog::constant, OPTIONAL, optional<ptr<prog::constant>>(into_ptr(inner_value)));
                auto type = VARIANT(prog::type, OPTIONAL, into_ptr(inner_type));
                return { move(value), move(type) };
            }

            case ast::expr::NONE: {
                auto value = VARIANT(prog::constant, OPTIONAL, optional<ptr<prog::constant>>());
                auto type = VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, NEVER, monostate())));
                return { move(value), move(type) };
            }

            case ast::expr::REFERENCE: {
                auto& name = GET(ast, REFERENCE);
                auto index = get_global_name(name, { global_name_kind::VAR }, ast.loc).index;
                auto& target_type = *prog.global_vars[index]->tp;
                auto type_pointed = prog::type_pointed { make_ptr(prog::copy_type(target_type)), false };
                auto ptr_type = prog::ptr_type { prog::ptr_type::GLOBAL, into_ptr(type_pointed) };
                auto value = VARIANT(prog::constant, GLOBAL_VAR_PTR, index);
                auto type = VARIANT(prog::type, PTR, into_ptr(ptr_type));
                return { move(value), move(type) };
            }

            case ast::expr::SIZED_ARRAY: {
                auto& array_ast = *GET(ast, SIZED_ARRAY);
                auto [inner_value, inner_type] = compile_const(*array_ast.value);
                auto size = compile_const_size(*array_ast.size);
                auto array_type = prog::array_type { into_ptr(inner_type), size };
                auto value = VARIANT(prog::constant, SIZED_ARRAY, make_pair(into_ptr(inner_value), size));
                auto type = VARIANT(prog::type, ARRAY, into_ptr(array_type));
                return { move(value), move(type) };
            }

            case ast::expr::LENGTH: {
                auto& target_ast = *GET(ast, LENGTH);
                auto target_type = compile_const(target_ast).second;
                if (!INDEX_EQ(target_type, ARRAY))
                    error(diags::expected_array_type(prog, copy_type(target_type)), target_ast.loc);
                auto size = GET(target_type, ARRAY)->size;
                auto ntype = prog::number_type { prog::number_type::U64 };
                auto value = VARIANT(prog::constant, NUMBER, make_pair(encode_number(size), copy_make_ptr(ntype)));
                auto type = VARIANT(prog::type, NUMBER, copy_make_ptr(ntype));
                return { move(value), move(type) };
            }

            default:
                error(diags::expression_not_constant(), ast.loc);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_const_tuple(vector<cref<ast::expr_marked>> asts, location loc) {
        auto value_asts = order_args(asts, { }, { }, loc);
        auto count = value_asts.size();

        if (count == 0) {
            auto value = VARIANT(prog::constant, UNIT, prog::monostate());
            auto type = VARIANT(prog::type, UNIT, prog::monostate());
            return { move(value), move(type) };
        }

        vector<prog::constant> values;
        vector<prog::type> types;

        for (auto& value_ast : value_asts) {
            auto [value, type] = compile_const(value_ast);
            values.push_back(move(value));
            types.push_back(move(type));
        }

        if (count == 1)
            return { move(values[0]), move(types[0]) };

        auto value = VARIANT(prog::constant, TUPLE, into_ptr_vector(values));
        auto type = VARIANT(prog::type, TUPLE, into_ptr_vector(types));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_const_array(vector<cref<ast::expr_marked>> asts, location loc) {
        auto value_asts = order_args(asts, { }, { }, loc);
        auto count = value_asts.size();

        vector<prog::constant> values;
        vector<prog::type> types;
        auto common_type = VARIANT(prog::type, NEVER, monostate());

        for (auto& value_ast : value_asts) {
            auto [value, type] = compile_const(value_ast);
            common_type = common_supertype(common_type, type, loc);
            values.push_back(move(value));
            types.push_back(move(type));
        }

        for (size_t index = 0; index < count; index++)
            values[index] = convert_const(move(values[index]), types[index], common_type, value_asts[index].get().loc);

        auto value = VARIANT(prog::constant, ARRAY, into_ptr_vector(values));
        auto type = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(common_type), count }));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_const_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, location loc) {
        auto receiver_type = compile_const(receiver_ast).second;

        switch (INDEX(receiver_type)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(receiver_type, STRUCT_CTOR);
                auto& st = *prog.struct_types[struct_index];
                auto count = st.fields.size();

                auto arg_with_name = [&] (string name, location loc) -> size_t {
                    auto iter = st.field_names.find(name);
                    if (iter == st.field_names.end())
                        error(diags::unknown_struct_field(st, name), loc);
                    return iter->second;
                };

                auto value_asts = order_args(arg_asts, { arg_with_name }, { count }, loc);

                vector<prog::constant> values;

                for (size_t index = 0; index < count; index++) {
                    auto [value, type] = compile_const(value_asts[index]);
                    auto& field_type = *st.fields[index]->tp;
                    value = convert_const(move(value), type, field_type, value_asts[index].get().loc);
                    values.push_back(move(value));
                }

                auto value = VARIANT(prog::constant, STRUCT, make_pair(struct_index, into_ptr_vector(values)));
                auto type = VARIANT(prog::type, STRUCT, struct_index);
                return { move(value), move(type) };
            }

            case prog::type::ENUM_CTOR: {
                auto [enum_index, variant_index] = GET(receiver_type, ENUM_CTOR);
                auto& en = *prog.enum_types[enum_index];
                auto& variant = *en.variants[variant_index];
                auto count = variant.tps.size();

                auto value_asts = order_args(arg_asts, { }, { count }, loc);

                vector<prog::constant> values;

                for (size_t index = 0; index < count; index++) {
                    auto [value, type] = compile_const(value_asts[index]);
                    auto& field_type = *variant.tps[index];
                    value = convert_const(move(value), type, field_type, value_asts[index].get().loc);
                    values.push_back(move(value));
                }

                auto value = VARIANT(prog::constant, ENUM, make_tuple(enum_index, variant_index, into_ptr_vector(values)));
                auto type = VARIANT(prog::type, ENUM, enum_index);
                return { move(value), move(type) };
            }

            case prog::type::FUNC:
            case prog::type::FUNC_WITH_PTR:
            case prog::type::GLOBAL_FUNC:
            case prog::type::KNOWN_FUNC:
                error(diags::expression_not_constant(), loc);

            default:
                error(diags::invalid_expression(), loc);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_const_name(string name, location loc) {
        auto& gname = get_global_name(name, loc);

        switch (gname.kind) {
            case global_name_kind::VAR:
                error(diags::expression_not_constant(), loc);

            case global_name_kind::CONST: {
                auto& var = consts[gname.index];
                return { prog::copy_const(*var.value), prog::copy_type(*var.tp) };
            }

            case global_name_kind::FUNC: {
                auto value = VARIANT(prog::constant, UNIT, monostate());
                auto type = VARIANT(prog::type, KNOWN_FUNC, gname.index);
                return { move(value), move(type) };
            }

            case global_name_kind::STRUCT: {
                auto value = VARIANT(prog::constant, UNIT, monostate());
                auto type = VARIANT(prog::type, STRUCT_CTOR, gname.index);
                return { move(value), move(type) };
            }

            case global_name_kind::ENUM:
                error(diags::invalid_expression(), loc);
        }

        UNREACHABLE;
    }

    pair<prog::constant, prog::type> compiler::compile_const_variant_name(string name, string variant_name, location loc) {
        auto enum_index = get_global_name(name, { global_name_kind::ENUM }, loc).index;
        auto& en = *prog.enum_types[enum_index];

        auto iter = en.variant_names.find(variant_name);
        if (iter == en.variant_names.end())
            error(diags::unknown_enum_variant(en, name), loc);
        auto variant_index = iter->second;

        if (en.variants[variant_index]->tps.empty()) {
            auto value = VARIANT(prog::constant, ENUM, make_tuple(enum_index, variant_index, vector<ptr<prog::constant>>()));
            auto type = VARIANT(prog::type, ENUM, enum_index);
            return { move(value), move(type) };
        }

        auto value = VARIANT(prog::constant, UNIT, monostate());
        auto type = VARIANT(prog::type, ENUM_CTOR, make_pair(enum_index, variant_index));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_const_literal(const ast::literal_expr& ast) {
        switch (INDEX(ast)) {
            case ast::literal_expr::BOOL: {
                auto ntype = prog::number_type { prog::number_type::BOOL };
                auto value = VARIANT(prog::constant, NUMBER, make_pair(encode_number(GET(ast, BOOL)), copy_make_ptr(ntype)));
                auto type = VARIANT(prog::type, NUMBER, copy_make_ptr(ntype));
                return {move(value), move(type)};
            }

            case ast::literal_expr::CHAR: {
                auto ntype = prog::number_type { prog::number_type::U8 };
                auto value = VARIANT(prog::constant, NUMBER, make_pair(encode_number(static_cast<uint8_t>(GET(ast, CHAR))), copy_make_ptr(ntype)));
                auto type = VARIANT(prog::type, NUMBER, copy_make_ptr(ntype));
                return {move(value), move(type)};
            }

            case ast::literal_expr::STRING: {
                auto str = GET(ast, STRING);
                auto size = str.length();

                auto char_ntype = prog::number_type { prog::number_type::U8 };
                auto char_type = VARIANT(prog::type, NUMBER, copy_make_ptr(char_ntype));
                auto array_type = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(char_type), size }));

                vector<prog::constant> char_values;
                for (char ch : str) {
                    auto number = encode_number(static_cast<uint8_t>(ch));
                    char_values.push_back(VARIANT(prog::constant, NUMBER, make_pair(number, copy_make_ptr(char_ntype))));
                }

                auto array_value = VARIANT(prog::constant, ARRAY, into_ptr_vector(char_values));

                auto index = prog.global_vars.size();
                prog.global_vars.push_back(make_ptr(prog::global_var { optional<string>(), make_ptr(copy_type(array_type)), into_ptr(array_value) }));

                auto value = VARIANT(prog::constant, GLOBAL_VAR_PTR, index);
                auto type_pointed = prog::type_pointed { into_ptr(array_type), false };
                auto type = VARIANT(prog::type, PTR, make_ptr(prog::ptr_type { prog::ptr_type::GLOBAL, into_ptr(type_pointed) }));
                return { move(value), move(type) };
            }

            case ast::literal_expr::INT: {
                auto[value, ntype] = compile_int_token(*GET(ast, INT));
                auto type = VARIANT(prog::type, NUMBER, into_ptr(ntype));
                return { move(value), move(type) };
            }

            case ast::literal_expr::FLOAT: {
                auto[value, ntype] = compile_float_token(*GET(ast, FLOAT));
                auto type = VARIANT(prog::type, NUMBER, into_ptr(ntype));
                return { move(value), move(type) };
            }
        }

        UNREACHABLE;
    }

    pair<prog::constant, prog::number_type> compiler::compile_int_token(const ast::int_token& ast) {
        #define RETURN_IF_OK(type, type_marker) { \
            auto value = try_make_number<type>(ast.value, ast.negative); \
            auto ntype = prog::number_type { prog::number_type::type_marker }; \
            if (value) return { VARIANT(prog::constant, NUMBER, make_pair(encode_number(*value), copy_make_ptr(ntype))), ntype }; \
        }

        #define RETURN_OR_ERROR(type, type_marker) { \
            RETURN_IF_OK(type, type_marker); \
            error(diags::int_overflow(ast.value, ast.negative, is_signed<type>(), 8 * sizeof(type)), ast.loc); \
        }

        switch (ast.marker) {
            case ast::int_token::NONE:
            case ast::int_token::I: {
                RETURN_IF_OK(int8_t, I8);
                RETURN_IF_OK(int16_t, I16);
                RETURN_IF_OK(int32_t, I32);
                RETURN_OR_ERROR(int64_t, I64);
            }

            case ast::int_token::I8:
                RETURN_OR_ERROR(int8_t, I8);

            case ast::int_token::I16:
                RETURN_OR_ERROR(int16_t, I16);

            case ast::int_token::I32:
                RETURN_OR_ERROR(int32_t, I32);

            case ast::int_token::I64:
                RETURN_OR_ERROR(int64_t, I64);

            case ast::int_token::U: {
                RETURN_IF_OK(uint8_t, U8);
                RETURN_IF_OK(uint16_t, U16);
                RETURN_IF_OK(uint32_t, U32);
                RETURN_OR_ERROR(uint64_t, U64);
            }

            case ast::int_token::U8:
                RETURN_OR_ERROR(uint8_t, U8);

            case ast::int_token::U16:
                RETURN_OR_ERROR(uint16_t, U16);

            case ast::int_token::U32:
                RETURN_OR_ERROR(uint32_t, U32);

            case ast::int_token::U64:
                RETURN_OR_ERROR(uint64_t, U64);
        }

        UNREACHABLE;

        #undef RETURN_OR_ERROR
        #undef RETURN_IF_OK
    }

    pair<prog::constant, prog::number_type> compiler::compile_float_token(const ast::float_token& ast) {
        auto value = ast.negative ? -ast.value : ast.value;

        switch (ast.marker) {
            case ast::float_token::NONE:
            case ast::float_token::F:
            case ast::float_token::F64: {
                auto ntype = prog::number_type { prog::number_type::F64 };
                return { VARIANT(prog::constant, NUMBER, make_pair(encode_number(value), copy_make_ptr(ntype))), ntype };
            }

            case ast::float_token::F32: {
                auto reduced = static_cast<float>(value);

                if (value != reduced)
                    error(diags::single_float_overflow(value), ast.loc);

                auto ntype = prog::number_type { prog::number_type::F32 };
                return { VARIANT(prog::constant, NUMBER, make_pair(encode_number(reduced), copy_make_ptr(ntype))), ntype };
            }
        }

        UNREACHABLE;
    }

    size_t compiler::compile_const_size(const ast::const_int& ast) {
        switch (INDEX(ast)) {
            case ast::const_int::INT:
                return GET(ast, INT);

            case ast::const_int::NAME: {
                auto index = get_global_name(GET(ast, NAME), { global_name_kind::CONST }, ast.loc).index;
                auto& value = *consts[index].value;

                if (!INDEX_EQ(value, NUMBER))
                    error(diags::invalid_size_constant_type(), ast.loc);

                auto& [number, ntype_ptr] = GET(value, NUMBER);

                switch (ntype_ptr->tp) {
                    case prog::number_type::U8:
                        return decode_number<uint8_t>(number);

                    case prog::number_type::U16:
                        return decode_number<uint16_t>(number);

                    case prog::number_type::U32:
                        return decode_number<uint32_t>(number);

                    case prog::number_type::U64:
                        return decode_number<uint64_t>(number);

                    default:
                        error(diags::invalid_size_constant_type(), ast.loc);
                }
            }
        }

        UNREACHABLE;
    }

    prog::constant compiler::convert_const(prog::constant value, const prog::type& type, const prog::type& new_type, location loc) {
        vector<prog::constant> reg_values;
        prog::reg_index reg_counter = 0;

        reg_values.push_back(move(value));

        auto new_reg = [&] () -> prog::reg_index {
            reg_values.emplace_back();
            return ++reg_counter;
        };

        function<void(const prog::instr&)> do_instr;

        do_instr = [&] (const prog::instr& instr) {
            switch (INDEX(instr)) {
                case prog::instr::ZERO_EXT:
                case prog::instr::SIGNED_EXT:
                case prog::instr::FLOAT_EXT: {
                    auto& conversion_instr =
                        INDEX_EQ(instr, ZERO_EXT) ? *GET(instr, ZERO_EXT)
                        : INDEX_EQ(instr, SIGNED_EXT) ? *GET(instr, SIGNED_EXT)
                        : *GET(instr, FLOAT_EXT);

                    auto& [number, ntype_ptr] = GET(reg_values[conversion_instr.value], NUMBER);

                    #define NUMERIC_CONVERSION(from, type_from, to, type_to) { \
                        if (ntype_ptr->tp == prog::number_type::from && conversion_instr.new_type->tp == prog::number_type::to) { \
                            auto decoded = decode_number<type_from>(number); \
                            auto converted = static_cast<type_to>(decoded); \
                            auto encoded = encode_number(converted); \
                            reg_values[conversion_instr.result] = VARIANT(prog::constant, NUMBER, make_pair(encoded, copy_make_ptr(*conversion_instr.new_type))); \
                            break; \
                        } \
                    }

                    NUMERIC_CONVERSION(BOOL, bool, I8, int8_t);
                    NUMERIC_CONVERSION(BOOL, bool, I16, int16_t);
                    NUMERIC_CONVERSION(BOOL, bool, I32, int32_t);
                    NUMERIC_CONVERSION(BOOL, bool, I64, int64_t);
                    NUMERIC_CONVERSION(BOOL, bool, U8, uint8_t);
                    NUMERIC_CONVERSION(BOOL, bool, U16, uint16_t);
                    NUMERIC_CONVERSION(BOOL, bool, U32, uint32_t);
                    NUMERIC_CONVERSION(BOOL, bool, U64, uint64_t);

                    NUMERIC_CONVERSION(I8, int8_t, I16, int16_t);
                    NUMERIC_CONVERSION(I8, int8_t, I32, int32_t);
                    NUMERIC_CONVERSION(I8, int8_t, I64, int64_t);

                    NUMERIC_CONVERSION(I16, int16_t, I32, int32_t);
                    NUMERIC_CONVERSION(I16, int16_t, I64, int64_t);

                    NUMERIC_CONVERSION(I32, int32_t, I64, int64_t);

                    NUMERIC_CONVERSION(U8, uint8_t, U16, uint16_t);
                    NUMERIC_CONVERSION(U8, uint8_t, U32, uint32_t);
                    NUMERIC_CONVERSION(U8, uint8_t, U64, uint64_t);
                    NUMERIC_CONVERSION(U8, uint8_t, I16, int16_t);
                    NUMERIC_CONVERSION(U8, uint8_t, I32, int32_t);
                    NUMERIC_CONVERSION(U8, uint8_t, I64, int64_t);

                    NUMERIC_CONVERSION(U16, uint16_t, U32, uint32_t);
                    NUMERIC_CONVERSION(U16, uint16_t, U64, uint64_t);
                    NUMERIC_CONVERSION(U16, uint16_t, I32, int32_t);
                    NUMERIC_CONVERSION(U16, uint16_t, I64, int64_t);

                    NUMERIC_CONVERSION(U32, uint32_t, U64, uint64_t);
                    NUMERIC_CONVERSION(U32, uint16_t, I64, int64_t);

                    NUMERIC_CONVERSION(F32, float, F64, double);

                    UNREACHABLE;

                    #undef NUMERIC_CONV
                } break;

                case prog::instr::EXTRACT_FIELD: {
                    auto& extract_instr = *GET(instr, EXTRACT_FIELD);
                    auto values = as_cref_vector(GET(reg_values[extract_instr.value], TUPLE));
                    reg_values[extract_instr.result] = copy_const(values[extract_instr.field]);
                } break;

                case prog::instr::MAKE_TUPLE: {
                    auto& make_instr = *GET(instr, MAKE_TUPLE);
                    vector<prog::constant> values;
                    for (auto value : make_instr.values)
                        values.push_back(copy_const(reg_values[value]));
                    reg_values[make_instr.result] = VARIANT(prog::constant, TUPLE, into_ptr_vector(values));
                } break;

                case prog::instr::TRANSFORM_ARRAY: {
                    auto& transform_instr = *GET(instr, TRANSFORM_ARRAY);
                    auto values = as_cref_vector(GET(reg_values[transform_instr.value], ARRAY));

                    vector<prog::constant> new_values;
                    for (const prog::constant& extracted : values) {
                        reg_values[transform_instr.extracted] = copy_const(extracted);
                        for (auto& instr_ptr : transform_instr.block->instrs)
                            do_instr(*instr_ptr);
                        new_values.push_back(copy_const(reg_values[transform_instr.transformed]));
                    }

                    reg_values[transform_instr.result] = VARIANT(prog::constant, ARRAY, into_ptr_vector(new_values));
                } break;

                case prog::instr::TRANSFORM_OPTIONAL: {
                    auto& transform_instr = *GET(instr, TRANSFORM_OPTIONAL);
                    auto value = as_optional_cref(GET(reg_values[transform_instr.value], OPTIONAL));

                    optional<prog::constant> new_value;
                    if (value) {
                        reg_values[transform_instr.extracted] = copy_const(*value);
                        for (auto& instr_ptr : transform_instr.block->instrs)
                            do_instr(*instr_ptr);
                        new_value = { copy_const(reg_values[transform_instr.transformed]) };
                    }

                    reg_values[transform_instr.result] = VARIANT(prog::constant, OPTIONAL, into_optional_ptr(new_value));
                } break;

                case prog::instr::GET_GLOBAL_FUNC_PTR: {
                    auto& get_ptr_instr = *GET(instr, GET_GLOBAL_FUNC_PTR);
                    reg_values[get_ptr_instr.result] = VARIANT(prog::constant, GLOBAL_FUNC_PTR, get_ptr_instr.index);
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

        conversion_compiler conv_clr(*this, new_reg, do_instr);
        auto result = conv_clr.convert(0, type, new_type, loc);

        return move(reg_values[result]);
    }

    template<typename T>
    static optional<T> try_make_number(unsigned long long abs_value, bool negative) {
        if (abs_value == 0)
            return 0;
        if (!negative)
            return abs_value <= numeric_limits<T>::max() ? static_cast<T>(abs_value) : optional<T>();
        if (is_unsigned<T>())
            return { };
        return abs_value - 1 <= numeric_limits<T>::max() ? -static_cast<T>(abs_value - 1) - 1 : optional<T>();
    }

    template<typename T>
    static unsigned long long encode_number(T number) {
        unsigned long long result;
        reinterpret_cast<T&>(result) = number;
        return result;
    }

    template<typename T>
    static T decode_number(unsigned long long number) {
        return reinterpret_cast<T&>(number);
    }
}
