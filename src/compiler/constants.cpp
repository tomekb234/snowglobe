#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"
#include <limits>

namespace sg {
    using namespace sg::utils;

    using std::monostate;
    using std::make_pair;
    using std::numeric_limits;
    using std::is_signed;
    using std::is_unsigned;

    template<typename T>
    static optional<T> try_make_number(unsigned long long abs_value, bool negative);

    template<typename T>
    static unsigned long long encode_number(T number);

    template<typename T>
    static T decode_number(unsigned long long number);

    pair<prog::constant, prog::type> compiler::compile_const(const ast::expr& ast) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
                return compile_const_tuple(ast, GET(ast, TUPLE));

            case ast::expr::ARRAY:
                return compile_const_array(ast, GET(ast, ARRAY));

            case ast::expr::APPLICATION: {
                auto& [receiver_ast, args_ast] = GET(ast, APPLICATION);
                return compile_const_application(ast, *receiver_ast, args_ast);
            }

            case ast::expr::NAME:
                return compile_const_name(ast, GET(ast, NAME));

            case ast::expr::VARIANT_NAME: {
                auto& [name, variant_name] = GET(ast, VARIANT_NAME);
                return compile_const_variant_name(ast, name, variant_name);
            }

            case ast::expr::LITERAL:
                return compile_const_literal(*GET(ast, LITERAL));

            case ast::expr::UNARY_OPERATION:
            case ast::expr::BINARY_OPERATION:
            case ast::expr::NUMERIC_CAST:
                error(diags::not_implemented(), ast); // TODO compile-time arithmetic

            case ast::expr::SOME: {
                auto[inner_value, inner_type] = compile_const(*GET(ast, SOME));
                auto value = VARIANT(prog::constant, OPTIONAL, optional<prog::ptr<prog::constant>>(into_ptr(inner_value)));
                auto type = VARIANT(prog::type, OPTIONAL, into_ptr(inner_type));
                return { move(value), move(type) };
            }

            case ast::expr::NONE: {
                auto value = VARIANT(prog::constant, OPTIONAL, optional<prog::ptr<prog::constant>>());
                auto type = VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, NEVER, monostate())));
                return { move(value), move(type) };
            }

            case ast::expr::REFERENCE: {
                auto& name = GET(ast, REFERENCE);
                auto& global_name = get_global_name(ast, name, global_name_kind::VAR);
                auto& target_type = *prog.global_vars[global_name.index]->tp;
                auto type_pointed = prog::type_pointed { make_ptr(prog::copy_type(target_type)), false };
                auto ptr_type = prog::ptr_type { prog::ptr_type::GLOBAL, into_ptr(type_pointed) };
                auto value = VARIANT(prog::constant, GLOBAL_VAR_PTR, global_name.index);
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
                    error(diags::expected_array_type(prog, copy_type(target_type)), target_ast);
                auto size = GET(target_type, ARRAY)->size;
                auto value = VARIANT(prog::constant, PRIMITIVE, encode_number(size));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type { prog::primitive_type::U64 }));
                return { move(value), move(type) };
            }

            default:
                error(diags::expression_not_constant(), ast);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_const_tuple(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& args_ast) {
        auto values_ast = order_args(ast, args_ast, { }, { });
        auto size = values_ast.size();

        if (size == 0) {
            auto value = VARIANT(prog::constant, UNIT, prog::monostate());
            auto type = VARIANT(prog::type, UNIT, prog::monostate());
            return { move(value), move(type) };
        }

        vector<prog::constant> values;
        vector<prog::type> types;

        for (auto& value_ast : values_ast) {
            auto [value, type] = compile_const(value_ast);
            values.push_back(move(value));
            types.push_back(move(type));
        }

        if (size == 1)
            return { move(values[0]), move(types[0]) };

        auto value = VARIANT(prog::constant, TUPLE, into_ptr_vector(values));
        auto type = VARIANT(prog::type, TUPLE, into_ptr_vector(types));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_const_array(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& args_ast) {
        auto values_ast = order_args(ast, args_ast, { }, { });
        auto size = values_ast.size();

        vector<prog::constant> values;
        vector<prog::type> types;
        auto common_type = VARIANT(prog::type, NEVER, monostate());

        for (auto& value_ast : values_ast) {
            auto [value, type] = compile_const(value_ast);
            common_type = common_supertype(ast, common_type, type);
            values.push_back(move(value));
            types.push_back(move(type));
        }

        for (size_t index = 0; index < size; index++)
            values[index] = convert_const(values_ast[index], move(values[index]), types[index], common_type);

        auto value = VARIANT(prog::constant, ARRAY, into_ptr_vector(values));
        auto type = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(common_type), size }));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_const_application(const ast::node& ast, const ast::expr& receiver_ast, const vector<ast::ptr<ast::expr_marked>>& args_ast) {
        auto receiver_type = compile_const(receiver_ast).second;

        switch (INDEX(receiver_type)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(receiver_type, STRUCT_CTOR);
                auto& struct_type = *prog.struct_types[struct_index];
                auto size = struct_type.fields.size();

                auto arg_with_name = [&] (const ast::node& ast, string name) -> size_t {
                    auto it = struct_type.field_names.find(name);
                    if (it == struct_type.field_names.end())
                        error(diags::invalid_struct_field(struct_type, name), ast);
                    return it->second;
                };

                auto values_ast = order_args(ast, args_ast, { arg_with_name }, { size });

                vector<prog::constant> values;

                for (size_t index = 0; index < size; index++) {
                    auto [value, type] = compile_const(values_ast[index]);
                    auto& field_type = *struct_type.fields[index]->tp;
                    value = convert_const(values_ast[index], move(value), type, field_type);
                    values.push_back(move(value));
                }

                auto value = VARIANT(prog::constant, STRUCT, into_ptr_vector(values));
                auto type = VARIANT(prog::type, STRUCT, struct_index);
                return { move(value), move(type) };
            }

            case prog::type::ENUM_CTOR: {
                auto [enum_index, variant_index] = GET(receiver_type, ENUM_CTOR);
                auto& enum_type = *prog.enum_types[enum_index];
                auto& enum_variant = *enum_type.variants[variant_index];
                auto size = enum_variant.tps.size();

                auto values_ast = order_args(ast, args_ast, { }, { size });

                vector<prog::constant> values;

                for (size_t index = 0; index < size; index++) {
                    auto [value, type] = compile_const(values_ast[index]);
                    auto& field_type = *enum_variant.tps[index];
                    value = convert_const(values_ast[index], move(value), type, field_type);
                    values.push_back(move(value));
                }

                auto value = VARIANT(prog::constant, ENUM, make_pair(variant_index, into_ptr_vector(values)));
                auto type = VARIANT(prog::type, ENUM, enum_index);
                return { move(value), move(type) };
            }

            case prog::type::FUNC:
            case prog::type::FUNC_WITH_PTR:
            case prog::type::GLOBAL_FUNC:
            case prog::type::KNOWN_FUNC:
                error(diags::expression_not_constant(), ast);

            default:
                error(diags::invalid_expression(), ast);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_const_name(const ast::node& ast, const string& name) {
        auto& global_name = get_global_name(ast, name);

        switch (global_name.kind) {
            case global_name_kind::VAR:
                error(diags::expression_not_constant(), ast);

            case global_name_kind::CONST: {
                auto& global_var = consts[global_name.index];
                return { prog::copy_const(*global_var.value), prog::copy_type(*global_var.tp) };
            }

            case global_name_kind::FUNC: {
                auto value = VARIANT(prog::constant, UNIT, monostate());
                auto type = VARIANT(prog::type, KNOWN_FUNC, global_name.index);
                return { move(value), move(type) };
            }

            case global_name_kind::STRUCT: {
                auto value = VARIANT(prog::constant, UNIT, monostate());
                auto type = VARIANT(prog::type, STRUCT_CTOR, global_name.index);
                return { move(value), move(type) };
            }

            case global_name_kind::ENUM:
                error(diags::invalid_expression(), ast);
        }

        UNREACHABLE;
    }

    pair<prog::constant, prog::type> compiler::compile_const_variant_name(const ast::node& ast, const string& name, const string& variant_name) {
        auto& global_name = get_global_name(ast, name, global_name_kind::ENUM);
        auto& enum_type = *prog.enum_types[global_name.index];

        auto it = enum_type.variant_names.find(variant_name);
        if (it == enum_type.variant_names.end())
            error(diags::invalid_enum_variant(enum_type, name), ast);
        auto variant_index = it->second;

        if (enum_type.variants[variant_index]->tps.empty()) {
            auto value = VARIANT(prog::constant, ENUM, make_pair(variant_index, vector<prog::ptr<prog::constant>>{ }));
            auto type = VARIANT(prog::type, ENUM, global_name.index);
            return { move(value), move(type) };
        }

        auto value = VARIANT(prog::constant, UNIT, monostate());
        auto type = VARIANT(prog::type, ENUM_CTOR, make_pair(global_name.index, variant_index));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_const_literal(const ast::literal_expr& ast) {
        switch (INDEX(ast)) {
            case ast::literal_expr::BOOL: {
                auto value = VARIANT(prog::constant, PRIMITIVE, encode_number(GET(ast, BOOL)));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::BOOL}));
                return {move(value), move(type)};
            }

            case ast::literal_expr::CHAR: {
                auto value = VARIANT(prog::constant, PRIMITIVE, encode_number(static_cast<uint8_t>(GET(ast, CHAR))));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::U8}));
                return {move(value), move(type)};
            }

            case ast::literal_expr::STRING: {
                auto str = GET(ast, STRING);
                auto size = str.length();

                auto char_type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type { prog::primitive_type::U8 }));
                auto array_type = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(char_type), size }));

                vector<prog::ptr<prog::constant>> char_values;
                for (char ch : str)
                    char_values.push_back(make_ptr(VARIANT(prog::constant, PRIMITIVE, encode_number(static_cast<uint8_t>(ch)))));
                auto array_value = VARIANT(prog::constant, ARRAY, move(char_values));

                auto index = prog.global_vars.size();
                prog.global_vars.push_back(make_ptr(prog::global_var { optional<string>(), make_ptr(copy_type(array_type)), into_ptr(array_value) }));

                auto value = VARIANT(prog::constant, GLOBAL_VAR_PTR, index);
                auto type_pointed = prog::type_pointed { into_ptr(array_type), false };
                auto type = VARIANT(prog::type, PTR, make_ptr(prog::ptr_type { prog::ptr_type::GLOBAL, into_ptr(type_pointed) }));
                return { move(value), move(type) };
            }

            case ast::literal_expr::INT: {
                auto[value, primitive_type] = compile_int_token(*GET(ast, INT));
                auto type = VARIANT(prog::type, PRIMITIVE, into_ptr(primitive_type));
                return { move(value), move(type) };
            }

            case ast::literal_expr::FLOAT: {
                auto[value, primitive_type] = compile_float_token(*GET(ast, FLOAT));
                auto type = VARIANT(prog::type, PRIMITIVE, into_ptr(primitive_type));
                return { move(value), move(type) };
            }
        }

        UNREACHABLE;
    }

    pair<prog::constant, prog::primitive_type> compiler::compile_int_token(const ast::int_token& ast) {
        #define RETURN_IF_OK(type, type_marker) { \
            auto opt_val = try_make_number<type>(ast.value, ast.negative); \
            if (opt_val) return { VARIANT(prog::constant, PRIMITIVE, encode_number(*opt_val)), prog::primitive_type { prog::primitive_type::type_marker } }; \
        }

        #define RETURN_OR_ERROR(type, type_marker) { \
            RETURN_IF_OK(type, type_marker) \
            error(diags::int_overflow(ast.value, ast.negative, is_signed<type>(), 8 * sizeof(type)), ast); \
        }

        switch (ast.marker) {
            case ast::int_token::NONE:
            case ast::int_token::I: {
                RETURN_IF_OK(int8_t, I8)
                RETURN_IF_OK(int16_t, I16)
                RETURN_IF_OK(int32_t, I32)
                RETURN_OR_ERROR(int64_t, I64)
            }

            case ast::int_token::I8:
                RETURN_OR_ERROR(int8_t, I8)

            case ast::int_token::I16:
                RETURN_OR_ERROR(int16_t, I16)

            case ast::int_token::I32:
                RETURN_OR_ERROR(int32_t, I32)

            case ast::int_token::I64:
                RETURN_OR_ERROR(int64_t, I64)

            case ast::int_token::U: {
                RETURN_IF_OK(uint8_t, U8)
                RETURN_IF_OK(uint16_t, U16)
                RETURN_IF_OK(uint32_t, U32)
                RETURN_OR_ERROR(uint64_t, U64)
            }

            case ast::int_token::U8:
                RETURN_OR_ERROR(uint8_t, U8)

            case ast::int_token::U16:
                RETURN_OR_ERROR(uint16_t, U16)

            case ast::int_token::U32:
                RETURN_OR_ERROR(uint32_t, U32)

            case ast::int_token::U64:
                RETURN_OR_ERROR(uint64_t, U64)
        }

        UNREACHABLE;

        #undef RETURN_OR_ERROR
        #undef RETURN_IF_OK
    }

    pair<prog::constant, prog::primitive_type> compiler::compile_float_token(const ast::float_token& ast) {
        auto value = ast.negative ? -ast.value : ast.value;

        switch (ast.marker) {
            case ast::float_token::NONE:
            case ast::float_token::F:
            case ast::float_token::F64:
                return { VARIANT(prog::constant, PRIMITIVE, encode_number(value)), prog::primitive_type {prog::primitive_type::F64} };

            case ast::float_token::F32: {
                auto single = static_cast<float>(value);

                if (value != single)
                    error(diags::single_float_overflow(value), ast);

                return { VARIANT(prog::constant, PRIMITIVE, encode_number(single)), prog::primitive_type {prog::primitive_type::F32} };
            }
        }

        UNREACHABLE;
    }

    size_t compiler::compile_const_size(const ast::const_int& ast) {
        switch (INDEX(ast)) {
            case ast::const_int::INT:
                return GET(ast, INT);

            case ast::const_int::NAME: {
                auto& global_name = get_global_name(ast, GET(ast, NAME), global_name_kind::CONST);
                auto& global_var = consts[global_name.index];

                if (!INDEX_EQ(*global_var.tp, PRIMITIVE))
                    error(diags::invalid_size_constant_type(), ast);

                auto value = GET(*global_var.value, PRIMITIVE);

                switch (GET(*global_var.tp, PRIMITIVE)->tp) {
                    case prog::primitive_type::U8:
                        return decode_number<uint8_t>(value);

                    case prog::primitive_type::U16:
                        return decode_number<uint16_t>(value);

                    case prog::primitive_type::U32:
                        return decode_number<uint32_t>(value);

                    case prog::primitive_type::U64:
                        return decode_number<uint64_t>(value);

                    default:
                        error(diags::invalid_size_constant_type(), ast);
                }
            }
        }

        UNREACHABLE;
    }

    prog::constant compiler::convert_const(const ast::node& ast, prog::constant value, const prog::type& type, const prog::type& new_type) {
        vector<prog::constant> values;
        vector<prog::type> types;
        prog::reg_index reg_counter = 0;

        values.push_back(move(value));
        types.push_back(prog::copy_type(type));

        auto new_reg = [&] () -> prog::reg_index {
            values.emplace_back();
            types.emplace_back();
            return ++reg_counter;
        };

        function<void(const prog::instr&)> do_instr;

        do_instr = [&] (const prog::instr& instr) {
            switch (INDEX(instr)) {
                case prog::instr::ZERO_EXT:
                case prog::instr::SIGNED_EXT:
                case prog::instr::FLOAT_EXT: {
                    auto& conv =
                        INDEX_EQ(instr, ZERO_EXT) ? *GET(instr, ZERO_EXT)
                        : INDEX_EQ(instr, SIGNED_EXT) ? *GET(instr, SIGNED_EXT)
                        : *GET(instr, FLOAT_EXT);

                    auto pvalue = GET(values[conv.value], PRIMITIVE);
                    auto& ptype = *GET(types[conv.value], PRIMITIVE);

                    #define PRIMITIVE_CONV(from, type_from, to, type_to) { \
                        if (ptype.tp == prog::primitive_type::from && conv.tp->tp == prog::primitive_type::to) { \
                            auto decoded = decode_number<type_from>(pvalue); \
                            auto converted = static_cast<type_to>(decoded); \
                            auto encoded = encode_number(converted); \
                            values[conv.result] = VARIANT(prog::constant, PRIMITIVE, encoded); \
                            types[conv.result] = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type(*conv.tp))); \
                            break; \
                        } \
                    }

                    PRIMITIVE_CONV(BOOL, bool, I8, int8_t);
                    PRIMITIVE_CONV(BOOL, bool, I16, int16_t);
                    PRIMITIVE_CONV(BOOL, bool, I32, int32_t);
                    PRIMITIVE_CONV(BOOL, bool, I64, int64_t);
                    PRIMITIVE_CONV(BOOL, bool, U8, uint8_t);
                    PRIMITIVE_CONV(BOOL, bool, U16, uint16_t);
                    PRIMITIVE_CONV(BOOL, bool, U32, uint32_t);
                    PRIMITIVE_CONV(BOOL, bool, U64, uint64_t);

                    PRIMITIVE_CONV(I8, int8_t, I16, int16_t);
                    PRIMITIVE_CONV(I8, int8_t, I32, int32_t);
                    PRIMITIVE_CONV(I8, int8_t, I64, int64_t);

                    PRIMITIVE_CONV(I16, int16_t, I32, int32_t);
                    PRIMITIVE_CONV(I16, int16_t, I64, int64_t);

                    PRIMITIVE_CONV(I32, int32_t, I64, int64_t);

                    PRIMITIVE_CONV(U8, uint8_t, U16, uint16_t);
                    PRIMITIVE_CONV(U8, uint8_t, U32, uint32_t);
                    PRIMITIVE_CONV(U8, uint8_t, U64, uint64_t);
                    PRIMITIVE_CONV(U8, uint8_t, I16, int16_t);
                    PRIMITIVE_CONV(U8, uint8_t, I32, int32_t);
                    PRIMITIVE_CONV(U8, uint8_t, I64, int64_t);

                    PRIMITIVE_CONV(U16, uint16_t, U32, uint32_t);
                    PRIMITIVE_CONV(U16, uint16_t, U64, uint64_t);
                    PRIMITIVE_CONV(U16, uint16_t, I32, int32_t);
                    PRIMITIVE_CONV(U16, uint16_t, I64, int64_t);

                    PRIMITIVE_CONV(U32, uint32_t, U64, uint64_t);
                    PRIMITIVE_CONV(U32, uint16_t, I64, int64_t);

                    PRIMITIVE_CONV(F32, float, F64, double);

                    UNREACHABLE;

                    #undef PRIMITIVE_CONV
                } break;

                case prog::instr::EXTRACT_COORD: {
                    auto& extr = *GET(instr, EXTRACT_COORD);
                    auto& tuple_value = GET(values[extr.value], TUPLE);
                    auto& tuple_type = GET(types[extr.value], TUPLE);

                    values[extr.result] = copy_const(*tuple_value[extr.coord]);
                    types[extr.result] = copy_type(*tuple_type[extr.coord]);
                } break;

                case prog::instr::MAKE_TUPLE: {
                    auto& make = *GET(instr, MAKE_TUPLE);

                    vector<prog::constant> tuple_values;
                    vector<prog::type> tuple_types;

                    for (auto value : make.values) {
                        tuple_values.push_back(copy_const(values[value]));
                        tuple_types.push_back(copy_type(types[value]));
                    }

                    values[make.result] = VARIANT(prog::constant, TUPLE, into_ptr_vector(tuple_values));
                    types[make.result] = VARIANT(prog::type, TUPLE, into_ptr_vector(tuple_types));
                } break;

                case prog::instr::TRANSFORM_ARRAY: {
                    auto& transform = *GET(instr, TRANSFORM_ARRAY);
                    auto& array_value = GET(values[transform.value], ARRAY);
                    auto& array_type = *GET(types[transform.value], ARRAY);

                    vector<prog::constant> new_values;
                    prog::type new_type = VARIANT(prog::type, NEVER, monostate());

                    for (auto& extracted : array_value) {
                        values[transform.extracted] = copy_const(*extracted);
                        types[transform.extracted] = copy_type(*array_type.tp);

                        for (auto& instr : transform.instrs->instrs)
                            do_instr(*instr);

                        new_values.push_back(copy_const(values[transform.inner_result]));
                        new_type = copy_type(types[transform.inner_result]);
                    }

                    values[transform.result] = VARIANT(prog::constant, ARRAY, into_ptr_vector(new_values));
                    types[transform.result] = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(new_type), array_type.size }));
                } break;

                case prog::instr::TRANSFORM_OPTIONAL: {
                    auto& transform = *GET(instr, TRANSFORM_OPTIONAL);
                    auto& optional_value = GET(values[transform.value], OPTIONAL);
                    auto& optional_type = *GET(types[transform.value], OPTIONAL);

                    optional<prog::constant> new_value;
                    prog::type new_type = VARIANT(prog::type, NEVER, monostate());

                    if (optional_value) {
                        values[transform.extracted] = copy_const(**optional_value);
                        types[transform.extracted] = copy_type(optional_type);

                        for (auto& instr : transform.instrs->instrs)
                            do_instr(*instr);

                        new_value = { copy_const(values[transform.inner_result]) };
                        new_type = copy_type(types[transform.inner_result]);
                    }

                    values[transform.result] = VARIANT(prog::constant, OPTIONAL, into_optional_ptr(new_value));
                    types[transform.result] = VARIANT(prog::type, OPTIONAL, into_ptr(new_type));
                } break;

                case prog::instr::GET_GLOBAL_FUNC_PTR: {
                    auto& get_ptr = *GET(instr, GET_GLOBAL_FUNC_PTR);
                    values[get_ptr.result] = VARIANT(prog::constant, GLOBAL_FUNC_PTR, get_ptr.index);
                } break;

                case prog::instr::ARRAY_PTR_INTO_SLICE: {
                    auto& conv = *GET(instr, ARRAY_PTR_INTO_SLICE);
                    auto& var = GET(values[conv.value], GLOBAL_VAR_PTR);
                    values[conv.result] = VARIANT(prog::constant, GLOBAL_VAR_SLICE, var);
                } break;

                default:
                    UNREACHABLE;
            }
        };

        conversion_compiler conv_clr(*this, new_reg, do_instr);
        auto result = conv_clr.convert(ast, 0, type, new_type);

        return move(values[result]);
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
