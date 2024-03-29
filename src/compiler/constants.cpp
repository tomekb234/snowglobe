#include "compiler/constants.hpp"
#include "compiler/expressions.hpp"
#include "compiler/compiler_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    pair<prog::constant, prog::type> constant_compiler::compile(const ast::expr& ast) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE: {
                auto expr_asts = as_cref_vector(GET(ast, TUPLE));
                return compile_tuple(expr_asts, ast.loc);
            }

            case ast::expr::ARRAY: {
                auto expr_asts = as_cref_vector(GET(ast, ARRAY));
                return compile_array(expr_asts, ast.loc);
            }

            case ast::expr::APPLICATION: {
                auto& [receiver_ast_ptr, arg_ast_ptrs] = GET(ast, APPLICATION);
                return compile_application(*receiver_ast_ptr, as_cref_vector(arg_ast_ptrs), ast.loc);
            }

            case ast::expr::NAME: {
                auto name = GET(ast, NAME);
                return compile_name(name, ast.loc);
            }

            case ast::expr::VARIANT_NAME: {
                auto [name, variant_name] = GET(ast, VARIANT_NAME);
                return compile_variant_name(name, variant_name, ast.loc);
            }

            case ast::expr::LITERAL: {
                auto& expr_ast = *GET(ast, LITERAL);
                return compile_literal(expr_ast);
            }

            case ast::expr::SOME: {
                auto& expr_ast = *GET(ast, SOME);
                return compile_some(expr_ast);
            }

            case ast::expr::NONE:
                return compile_none();

            case ast::expr::GLOBAL_VAR_REF: {
                auto name = GET(ast, GLOBAL_VAR_REF);
                return compile_global_var_ref(name, ast.loc);
            }

            case ast::expr::LENGTH: {
                auto& expr_ast = *GET(ast, LENGTH);
                return compile_length(expr_ast);
            }

            default:
                error(diags::expression_not_constant(ast.loc));
        }
    }

    pair<prog::constant, prog::type> constant_compiler::compile_literal(const ast::literal_expr& ast) {
        switch (INDEX(ast)) {
            case ast::literal_expr::BOOL: {
                auto value = VARIANT(prog::constant, NUMBER, encode_number(GET(ast, BOOL)));
                return {move(value), copy_type(prog::BOOL_TYPE)};
            }

            case ast::literal_expr::CHAR: {
                auto value = VARIANT(prog::constant, NUMBER, encode_number(static_cast<uint8_t>(GET(ast, CHAR))));
                return {move(value), copy_type(prog::CHAR_TYPE)};
            }

            case ast::literal_expr::STRING: {
                auto str = GET(ast, STRING);
                auto size = str.length();

                vector<prog::constant> char_values;
                for (char ch : str) {
                    auto number = encode_number(static_cast<uint8_t>(ch));
                    char_values.push_back(VARIANT(prog::constant, NUMBER, number));
                }

                auto array_type = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { make_ptr(copy_type(prog::CHAR_TYPE)), size }));
                auto array_value = VARIANT(prog::constant, ARRAY, into_ptr_vector(char_values));

                auto index = prog.global_vars.size();
                auto var = prog::global_var { optional<string>(), make_ptr(copy_type(array_type)), into_ptr(array_value) };
                prog.global_vars.push_back(into_ptr(var));

                auto value = VARIANT(prog::constant, GLOBAL_VAR_PTR, index);
                auto type = prog::make_ptr_type(move(array_type), prog::ptr_type::GLOBAL, false);
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

    size_t constant_compiler::compile_size(const ast::const_int& ast) {
        switch (INDEX(ast)) {
            case ast::const_int::INT:
                return GET(ast, INT);

            case ast::const_int::NAME: {
                auto index = clr.get_global_name(GET(ast, NAME), { global_name_kind::CONST }, ast.loc).index;
                auto& value = *clr.consts[index].value;
                auto& type = *clr.consts[index].tp;

                if (!INDEX_EQ(type, NUMBER))
                    error(diags::invalid_size_constant_type(prog, copy_type(type), ast.loc));

                auto ntype = GET(type, NUMBER)->tp;
                auto number = GET(value, NUMBER);

                switch (ntype) {
                    case prog::number_type::U8:
                        return decode_number<uint8_t>(number);

                    case prog::number_type::U16:
                        return decode_number<uint16_t>(number);

                    case prog::number_type::U32:
                        return decode_number<uint32_t>(number);

                    case prog::number_type::U64:
                        return decode_number<uint64_t>(number);

                    default:
                        error(diags::invalid_size_constant_type(prog, copy_type(type), ast.loc));
                }
            }
        }

        UNREACHABLE;
    }
    pair<prog::constant, prog::type> constant_compiler::compile_tuple(vector<cref<ast::expr_marked>> asts, location loc) {
        auto value_asts = compiler_utils(clr).order_args(asts, { }, { }, loc);
        auto count = value_asts.size();

        if (count == 0) {
            auto value = VARIANT(prog::constant, UNIT, prog::monostate());
            return { move(value), copy_type(prog::UNIT_TYPE) };
        }

        vector<prog::constant> values;
        vector<prog::type> types;

        for (auto& value_ast : value_asts) {
            auto [value, type] = constant_compiler(clr).compile(value_ast);
            values.push_back(move(value));
            types.push_back(move(type));
        }

        if (count == 1)
            return { move(values[0]), move(types[0]) };

        auto value = VARIANT(prog::constant, TUPLE, into_ptr_vector(values));
        auto type = VARIANT(prog::type, TUPLE, into_ptr_vector(types));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> constant_compiler::compile_array(vector<cref<ast::expr_marked>> asts, location loc) {
        auto value_asts = compiler_utils(clr).order_args(asts, { }, { }, loc);
        auto count = value_asts.size();

        vector<prog::constant> values;
        vector<prog::type> types;
        auto common_type = copy_type(prog::NEVER_TYPE);

        for (auto& value_ast : value_asts) {
            auto [value, type] = constant_compiler(clr).compile(value_ast);
            common_type = compiler_utils(clr).common_supertype(common_type, type, loc);
            values.push_back(move(value));
            types.push_back(move(type));
        }

        for (size_t index = 0; index < count; index++)
            values[index] = compiler_utils(clr).convert_const(move(values[index]), types[index], common_type, value_asts[index].get().loc);

        auto value = VARIANT(prog::constant, ARRAY, into_ptr_vector(values));
        auto type = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(common_type), count }));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> constant_compiler::compile_application(
            const ast::expr& receiver_ast,
            vector<cref<ast::expr_marked>> arg_asts,
            location loc) {
        auto receiver_type = constant_compiler(clr).compile(receiver_ast).second;

        switch (INDEX(receiver_type)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(receiver_type, STRUCT_CTOR);
                auto& st = *prog.struct_types[struct_index];
                auto count = st.fields.size();

                auto arg_with_name = [&] (string name, location loc) -> size_t {
                    auto iter = st.field_names.find(name);
                    if (iter == st.field_names.end())
                        error(diags::unknown_struct_field(st, name, loc));
                    return iter->second;
                };

                auto value_asts = compiler_utils(clr).order_args(arg_asts, { arg_with_name }, { count }, loc);

                vector<prog::constant> values;

                for (size_t index = 0; index < count; index++) {
                    auto [value, type] = constant_compiler(clr).compile(value_asts[index]);
                    auto& field_type = *st.fields[index]->tp;
                    value = compiler_utils(clr).convert_const(move(value), type, field_type, value_asts[index].get().loc);
                    values.push_back(move(value));
                }

                auto value = VARIANT(prog::constant, STRUCT, into_ptr_vector(values));
                auto type = VARIANT(prog::type, STRUCT, struct_index);
                return { move(value), move(type) };
            }

            case prog::type::ENUM_CTOR: {
                auto [enum_index, variant_index] = GET(receiver_type, ENUM_CTOR);
                auto& en = *prog.enum_types[enum_index];
                auto& variant = *en.variants[variant_index];
                auto count = variant.tps.size();

                auto value_asts = compiler_utils(clr).order_args(arg_asts, { }, { count }, loc);

                vector<prog::constant> values;

                for (size_t index = 0; index < count; index++) {
                    auto [value, type] = constant_compiler(clr).compile(value_asts[index]);
                    auto& field_type = *variant.tps[index];
                    value = compiler_utils(clr).convert_const(move(value), type, field_type, value_asts[index].get().loc);
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
                error(diags::expression_not_constant(loc));

            default:
                error(diags::invalid_expression(loc));
        }
    }

    pair<prog::constant, prog::type> constant_compiler::compile_name(string name, location loc) {
        auto& gname = clr.get_global_name(name, loc);

        switch (gname.kind) {
            case global_name_kind::VAR:
                error(diags::expression_not_constant(loc));

            case global_name_kind::CONST: {
                auto& var = clr.consts[gname.index];
                return { copy_const(*var.value), copy_type(*var.tp) };
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
                error(diags::invalid_expression(loc));
        }

        UNREACHABLE;
    }

    pair<prog::constant, prog::type> constant_compiler::compile_variant_name(string name, string variant_name, location loc) {
        auto enum_index = clr.get_global_name(name, { global_name_kind::ENUM }, loc).index;
        auto& en = *prog.enum_types[enum_index];

        auto iter = en.variant_names.find(variant_name);
        if (iter == en.variant_names.end())
            error(diags::unknown_enum_variant(en, name, loc));
        auto variant_index = iter->second;

        if (en.variants[variant_index]->tps.empty()) {
            auto value = VARIANT(prog::constant, ENUM, make_pair(variant_index, vector<ptr<prog::constant>>()));
            auto type = VARIANT(prog::type, ENUM, enum_index);
            return { move(value), move(type) };
        }

        auto value = VARIANT(prog::constant, UNIT, monostate());
        auto type = VARIANT(prog::type, ENUM_CTOR, make_pair(enum_index, variant_index));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> constant_compiler::compile_some(const ast::expr& ast) {
        auto [inner_value, inner_type] = constant_compiler(clr).compile(ast);
        auto value = VARIANT(prog::constant, OPTIONAL, optional<ptr<prog::constant>>(into_ptr(inner_value)));
        auto type = VARIANT(prog::type, OPTIONAL, into_ptr(inner_type));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> constant_compiler::compile_none() {
        auto value = VARIANT(prog::constant, OPTIONAL, optional<ptr<prog::constant>>());
        auto type = VARIANT(prog::type, OPTIONAL, make_ptr(copy_type(prog::NEVER_TYPE)));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> constant_compiler::compile_global_var_ref(string name, location loc) {
        auto index = clr.get_global_name(name, { global_name_kind::VAR }, loc).index;
        auto value = VARIANT(prog::constant, GLOBAL_VAR_PTR, index);
        auto& var_type = *prog.global_vars[index]->tp;
        auto type = prog::make_ptr_type(copy_type(var_type), prog::ptr_type::GLOBAL, false);
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> constant_compiler::compile_length(const ast::expr& ast) {
        auto target_type = constant_compiler(clr).compile(ast).second;
        if (!INDEX_EQ(target_type, ARRAY))
            error(diags::invalid_type(prog, move(target_type), diags::type_kind::ARRAY, ast.loc));
        auto size = GET(target_type, ARRAY)->size;
        auto value = VARIANT(prog::constant, NUMBER, encode_number(size));
        return { move(value), copy_type(prog::SIZE_TYPE) };
    }

    pair<prog::constant, prog::number_type> constant_compiler::compile_int_token(const ast::int_token& ast) {
        #define RETURN_IF_OK(type, type_marker) { \
            auto value = try_make_number<type>(ast.value, ast.negative); \
            auto ntype = prog::number_type { prog::number_type::type_marker }; \
            if (value) return { VARIANT(prog::constant, NUMBER, encode_number(*value)), ntype }; \
        }

        #define RETURN_OR_ERROR(type, type_marker) { \
            RETURN_IF_OK(type, type_marker); \
            error(diags::int_overflow(ast.value, ast.negative, is_signed<type>(), 8 * sizeof(type), ast.loc)); \
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

    pair<prog::constant, prog::number_type> constant_compiler::compile_float_token(const ast::float_token& ast) {
        auto value = ast.negative ? -ast.value : ast.value;

        switch (ast.marker) {
            case ast::float_token::NONE:
            case ast::float_token::F:
            case ast::float_token::F64: {
                auto ntype = prog::number_type { prog::number_type::F64 };
                return { VARIANT(prog::constant, NUMBER, encode_number(value)), ntype };
            }

            case ast::float_token::F32: {
                auto reduced = static_cast<float>(value);
                auto ntype = prog::number_type { prog::number_type::F32 };
                return { VARIANT(prog::constant, NUMBER, encode_number(reduced)), ntype };
            }
        }

        UNREACHABLE;
    }
}
