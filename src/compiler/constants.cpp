#include "compiler.hpp"
#include "compiler_diagnostics.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "utils.hpp"
#include <string>
#include <variant>
#include <limits>

namespace sg {
    using namespace sg::utils;

    using std::monostate;
    using std::numeric_limits;
    using std::is_signed;
    using std::is_unsigned;

    template<typename T>
    static optional<T> try_make_number(unsigned long long abs_value, bool negative);

    template<typename T>
    static unsigned long long encode_number(T number);

    template<typename T>
    static T decode_number(unsigned long long number);

    pair<prog::constant, prog::type> compiler::compile_constant(const ast::expr& ast) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
                return compile_constant_tuple(ast, GET(ast, TUPLE));

            case ast::expr::ARRAY:
                return compile_constant_array(ast, GET(ast, ARRAY));

            case ast::expr::APPLICATION: {
                auto& [receiver_ast, args_ast] = GET(ast, APPLICATION);
                return compile_constant_application(ast, *receiver_ast, args_ast);
            }

            case ast::expr::NAME:
                return compile_constant_name(ast, GET(ast, NAME));

            case ast::expr::VARIANT_NAME: {
                auto& [name, variant_name] = GET(ast, VARIANT_NAME);
                return compile_constant_variant_name(ast, name, variant_name);
            }

            case ast::expr::LITERAL:
                return compile_constant_literal(*GET(ast, LITERAL));

            case ast::expr::UNARY_OPERATION:
            case ast::expr::BINARY_OPERATION:
            case ast::expr::NUMERIC_CAST:
                error(diags::not_implemented(), ast); // TODO compile-time arithmetic

            case ast::expr::SOME: {
                auto[inner_value, inner_type] = compile_constant(*GET(ast, SOME));
                auto value = VARIANT(prog::constant, SOME, into_ptr(inner_value));
                auto type = VARIANT(prog::type, OPTIONAL, into_ptr(inner_type));
                return { move(value), move(type) };
            }

            case ast::expr::NONE: {
                auto value = VARIANT(prog::constant, NONE, monostate());
                auto type = VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, NEVER, monostate())));
                return { move(value), move(type) };
            }

            case ast::expr::REFERENCE: {
                auto& name = GET(ast, REFERENCE);
                auto& global_name = get_global_name(ast, name, global_name::VARIABLE);
                auto& target_type = *program.global_vars[global_name.index]->tp;
                auto type_pointed = prog::type_pointed { make_ptr(prog::copy_type(target_type)), false };
                auto ptr_type = prog::ptr_type { prog::ptr_type::GLOBAL, into_ptr(type_pointed) };
                auto value = VARIANT(prog::constant, GLOBAL_VAR_PTR, global_name.index);
                auto type = VARIANT(prog::type, PTR, into_ptr(ptr_type));
                return { move(value), move(type) };
            }

            case ast::expr::SIZED_ARRAY: {
                auto& array_ast = *GET(ast, SIZED_ARRAY);
                auto [inner_value, inner_type] = compile_constant(*array_ast.value);
                auto size = compile_constant_size(*array_ast.size);
                auto array_type = prog::array_type { into_ptr(inner_type), size };
                auto value = VARIANT(prog::constant, SIZED_ARRAY, make_pair(into_ptr(inner_value), size));
                auto type = VARIANT(prog::type, ARRAY, into_ptr(array_type));
                return { move(value), move(type) };
            }

            case ast::expr::LENGTH: {
                auto& target = *GET(ast, LENGTH);
                auto target_type = compile_constant(target).second;
                if (!INDEX_EQ(target_type, ARRAY))
                    error(diags::invalid_type(), ast);
                auto size = GET(target_type, ARRAY)->size;
                auto value = VARIANT(prog::constant, INT, encode_number(size));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type { prog::primitive_type::U64 }));
                return { move(value), move(type) };
            }

            default:
                error(diags::expression_not_constant(), ast);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_constant_tuple(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items_ast) {
        auto size = items_ast.size();
        vector<bool> used_items(size, false);
        vector<prog::constant> values(size);
        vector<prog::type> types(size);

        for (auto& item_ast : items_ast) {
            size_t index = 0;
            ast::expr* value_ast;

            switch (INDEX(*item_ast)) {
                case ast::expr_marked::EXPR: {
                    while (index < size && used_items[index])
                        index++;
                    value_ast = GET(*item_ast, EXPR).get();
                } break;

                case ast::expr_marked::EXPR_WITH_NAME:
                    error(diags::invalid_expression(), ast);

                case ast::expr_marked::EXPR_WITH_COORD: {
                    index = GET(*item_ast, EXPR_WITH_COORD).first;
                    value_ast = GET(*item_ast, EXPR_WITH_COORD).second.get();
                } break;
            }

            if (index >= size)
                error(diags::invalid_argument(size), ast);
            if (used_items[index])
                error(diags::reused_argument(index), ast);

            auto[value, type] = compile_constant(*value_ast);
            values[index] = move(value);
            types[index] = move(type);
            used_items[index] = true;
        }

        for (size_t index = 0; index < size; index++) {
            if (!used_items[index])
                error(diags::missing_argument(index), ast);
        }

        if (size == 0) {
            auto value = VARIANT(prog::constant, UNIT, prog::monostate());
            auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type { prog::primitive_type::UNIT }));
            return { move(value), move(type) };
        }

        if (size == 1)
            return { move(values[0]), move(types[0]) };

        auto value = VARIANT(prog::constant, TUPLE, into_ptr_vector(values));
        auto type = VARIANT(prog::type, TUPLE, into_ptr_vector(types));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_constant_array(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items_ast) {
        auto size = items_ast.size();
        vector<bool> used_items(size, false);
        vector<prog::constant> values(size);
        vector<prog::type> types(size);
        prog::type common_type = VARIANT(prog::type, NEVER, monostate());

        for (auto& item_ast : items_ast) {
            size_t index = 0;
            ast::expr* value_ast;

            switch (INDEX(*item_ast)) {
                case ast::expr_marked::EXPR: {
                    while (index < size && used_items[index])
                        index++;
                    value_ast = GET(*item_ast, EXPR).get();
                } break;

                case ast::expr_marked::EXPR_WITH_NAME:
                    error(diags::invalid_expression(), ast);

                case ast::expr_marked::EXPR_WITH_COORD: {
                    index = GET(*item_ast, EXPR_WITH_COORD).first;
                    value_ast = GET(*item_ast, EXPR_WITH_COORD).second.get();
                } break;
            }

            if (index >= size)
                error(diags::invalid_argument(size), ast);
            if (used_items[index])
                error(diags::reused_argument(index), ast);

            auto[value, type] = compile_constant(*value_ast);
            common_type = common_supertype(ast, common_type, type);
            values[index] = move(value);
            types[index] = move(type);
            used_items[index] = true;
        }

        for (size_t index = 0; index < size; index++) {
            if (!used_items[index])
                error(diags::missing_argument(index), ast);
            values[index] = convert_constant(ast, move(values[index]), types[index], common_type);
        }

        auto value = VARIANT(prog::constant, ARRAY, into_ptr_vector(values));
        auto type = VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(common_type), values.size() }));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_constant_application(const ast::node& ast, const ast::expr& receiver_ast, const vector<ast::ptr<ast::expr_marked>>& args_ast) {
        auto receiver_type = compile_constant(receiver_ast).second;

        switch (INDEX(receiver_type)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(receiver_type, STRUCT_CTOR);
                auto& struct_type = *program.struct_types[struct_index];

                size_t num_args = struct_type.fields.size();
                vector<bool> used_args(num_args, false);
                vector<pair<prog::constant, prog::type>> args(num_args);

                for (auto& arg_ast : args_ast) {
                    size_t index = 0;
                    ast::expr* value_ast;

                    switch (INDEX(*arg_ast)) {
                        case ast::expr_marked::EXPR: {
                            while (index < num_args && used_args[index])
                                index++;
                            value_ast = GET(*arg_ast, EXPR).get();
                        } break;

                        case ast::expr_marked::EXPR_WITH_NAME: {
                            auto& name = GET(*arg_ast, EXPR_WITH_NAME).first;
                            if (!struct_type.field_names.count(name))
                                error(diags::invalid_struct_field(struct_type.name, name), *arg_ast);
                            index = struct_type.field_names[name];
                            value_ast = GET(*arg_ast, EXPR_WITH_NAME).second.get();
                        } break;

                        case ast::expr_marked::EXPR_WITH_COORD: {
                            index = GET(*arg_ast, EXPR_WITH_COORD).first;
                            value_ast = GET(*arg_ast, EXPR_WITH_COORD).second.get();
                            break;
                        }
                    }

                    if (index >= num_args)
                        error(diags::invalid_argument(num_args), ast);
                    if (used_args[index])
                        error(diags::reused_argument(index), ast);

                    args[index] = compile_constant(*value_ast);
                    used_args[index] = true;
                }

                vector<prog::ptr<prog::constant>> result;
                for (size_t index = 0; index < num_args; index++) {
                    if (!used_args[index])
                        error(diags::missing_argument(index), ast);
                    auto& [value, type] = args[index];
                    auto& declared_type = *struct_type.fields[index]->tp;
                    result.push_back(make_ptr(convert_constant(ast, move(value), type, declared_type)));
                }

                auto value = VARIANT(prog::constant, STRUCT, move(result));
                auto type = VARIANT(prog::type, STRUCT, struct_index);
                return { move(value), move(type) };
            }

            case prog::type::ENUM_CTOR: {
                auto [enum_index, variant_index] = GET(receiver_type, ENUM_CTOR);
                auto& enum_type = *program.enum_types[enum_index];
                auto& variant = *enum_type.variants[variant_index];

                size_t num_args = variant.tps.size();
                vector<bool> used_args(num_args, false);
                vector<pair<prog::constant, prog::type>> args(num_args);

                for (auto& arg_ast : args_ast) {
                    size_t index = 0;
                    ast::expr* value_ast;

                    switch (INDEX(*arg_ast)) {
                        case ast::expr_marked::EXPR: {
                            while (index < num_args && used_args[index])
                                index++;
                            value_ast = GET(*arg_ast, EXPR).get();
                        } break;

                        case ast::expr_marked::EXPR_WITH_NAME:
                            error(diags::invalid_expression(), ast);

                        case ast::expr_marked::EXPR_WITH_COORD: {
                            index = GET(*arg_ast, EXPR_WITH_COORD).first;
                            value_ast = GET(*arg_ast, EXPR_WITH_COORD).second.get();
                        } break;
                    }

                    if (index >= num_args)
                        error(diags::invalid_argument(num_args), ast);
                    if (used_args[index])
                        error(diags::reused_argument(index), ast);

                    args[index] = compile_constant(*value_ast);
                    used_args[index] = true;
                }

                vector<prog::ptr<prog::constant>> result;
                for (size_t index = 0; index < num_args; index++) {
                    if (!used_args[index])
                        error(diags::missing_argument(index), ast);
                    auto& [value, type] = args[index];
                    auto& declared_type = *variant.tps[index];
                    result.push_back(make_ptr(convert_constant(ast, move(value), type, declared_type)));
                }

                auto value = VARIANT(prog::constant, ENUM, make_pair(variant_index, move(result)));
                auto type = VARIANT(prog::type, ENUM, enum_index);
                return { move(value), move(type) };
            }

            case prog::type::FUNC:
            case prog::type::GLOBAL_FUNC:
            case prog::type::FUNC_WITH_PTR:
                error(diags::expression_not_constant(), ast);

            default:
                error(diags::invalid_expression(), ast);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_constant_name(const ast::node& ast, const string& name) {
        auto& global_name = get_global_name(ast, name, false);

        switch (global_name.kind) {
            case global_name::VARIABLE:
                error(diags::expression_not_constant(), ast);

            case global_name::CONSTANT: {
                auto& global_var = constants[global_name.index];
                return { prog::copy_constant(*global_var.value), prog::copy_type(*global_var.tp) };
            }

            case global_name::FUNCTION: {
                auto& global_func = *program.global_funcs[global_name.index];
                auto value = VARIANT(prog::constant, GLOBAL_FUNC_PTR, global_name.index);
                auto type = VARIANT(prog::type, GLOBAL_FUNC, make_ptr(prog::copy_func_type(*global_func.tp)));
                return { move(value), move(type) };
            }

            case global_name::STRUCT: {
                auto value = VARIANT(prog::constant, UNIT, monostate());
                auto type = VARIANT(prog::type, STRUCT_CTOR, global_name.index);
                return { move(value), move(type) };
            }

            case global_name::ENUM:
                error(diags::invalid_expression(), ast);
        }

        UNREACHABLE;
    }

    pair<prog::constant, prog::type> compiler::compile_constant_variant_name(const ast::node& ast, const string& name, const string& variant_name) {
        auto& global_name = get_global_name(ast, name, global_name::ENUM, false);
        auto& enum_type = *program.enum_types[global_name.index];

        if (!enum_type.variant_names.count(variant_name))
            error(diags::invalid_enum_variant(name, variant_name), ast);
        size_t variant_index = enum_type.variant_names[variant_name];

        if (enum_type.variants[variant_index]->tps.empty()) {
            auto value = VARIANT(prog::constant, ENUM, make_pair(variant_index, vector<prog::ptr<prog::constant>>{ }));
            auto type = VARIANT(prog::type, ENUM, global_name.index);
            return { move(value), move(type) };
        }

        auto value = VARIANT(prog::constant, UNIT, monostate());
        auto type = VARIANT(prog::type, ENUM_CTOR, make_pair(global_name.index, variant_index));
        return { move(value), move(type) };
    }

    pair<prog::constant, prog::type> compiler::compile_constant_literal(const ast::literal_expr& ast) {
        switch (INDEX(ast)) {
            case ast::literal_expr::BOOL: {
                auto value = VARIANT(prog::constant, BOOL, GET(ast, BOOL));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::BOOL}));
                return {move(value), move(type)};
            }

            case ast::literal_expr::CHAR: {
                auto value = VARIANT(prog::constant, INT, encode_number(static_cast<uint8_t>(GET(ast, CHAR))));
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
                    char_values.push_back(make_ptr(VARIANT(prog::constant, INT, encode_number(static_cast<uint8_t>(ch)))));
                auto array_value = VARIANT(prog::constant, ARRAY, move(char_values));

                auto index = program.global_vars.size();
                program.global_vars.push_back(make_ptr(prog::global_var { optional<string>(), make_ptr(copy_type(array_type)), into_ptr(array_value) }));

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
            if (opt_val) return { VARIANT(prog::constant, INT, encode_number(*opt_val)), { prog::primitive_type::type_marker } }; \
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
            case ast::float_token::F64: {
                return { VARIANT(prog::constant, FLOAT64, value), {prog::primitive_type::F64} };
            }

            case ast::float_token::F32: {
                auto single = static_cast<float>(value);

                if (value != single)
                    error(diags::single_float_overflow(value), ast);

                return { VARIANT(prog::constant, FLOAT32, single), {prog::primitive_type::F32} };
            }
        }

        UNREACHABLE;
    }

    size_t compiler::compile_constant_size(const ast::const_int& ast) {
        switch (INDEX(ast)) {
            case ast::const_int::INT:
                return GET(ast, INT);

            case ast::const_int::NAME: {
                auto& global_name = get_global_name(ast, GET(ast, NAME), global_name::CONSTANT);
                auto& global_var = constants[global_name.index];

                if (!INDEX_EQ(*global_var.value, INT))
                    error(diags::invalid_size_constant_type(), ast);

                auto value = GET(*global_var.value, INT);

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

    prog::constant compiler::convert_constant(const ast::node& ast, prog::constant value, const prog::type& type, const prog::type& new_type) {
        if (!subtype(type, new_type))
            error(diags::not_subtype(program, type, new_type), ast);

        return value; // TODO perform actual conversion
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
