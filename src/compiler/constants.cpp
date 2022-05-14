#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"
#include <string>
#include <variant>
#include <limits>

namespace sg {
    using namespace sg::utils;

    using std::to_string;
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

    pair<prog::constant, prog::type> compiler::compile_constant_expr(const ast::expr& ast) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
                return compile_constant_tuple(ast, GET(ast, TUPLE));

            case ast::expr::ARRAY:
                return compile_constant_array(ast, GET(ast, ARRAY));

            case ast::expr::APPLICATION: {
                auto& [receiver, args] = GET(ast, APPLICATION);
                return compile_constant_application(ast, *receiver, args);
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
                auto[inner_value, inner_type] = compile_constant_expr(*GET(ast, SOME));
                auto value = VARIANT(prog::constant, SOME, into_ptr(inner_value));
                auto type = VARIANT(prog::type, OPTIONAL, into_ptr(inner_type));
                return { move(value), move(type) };
            }

            case ast::expr::NONE: {
                auto value = VARIANT(prog::constant, NONE, monostate());
                auto type = VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, NEVER, monostate())));
                return { move(value), move(type) };
            }

            case ast::expr::REFERENCE:
                return compile_constant_ptr(ast, GET(ast, REFERENCE));

            case ast::expr::SIZED_ARRAY:
                return compile_constant_sized_array(*GET(ast, SIZED_ARRAY));

            case ast::expr::LENGTH:
                return compile_constant_length(ast, *GET(ast, LENGTH));

            case ast::expr::EXTRACT:
                return compile_constant_extract(*GET(ast, EXTRACT));

            case ast::expr::PTR_EXTRACT:
                return compile_constant_ptr_extract(*GET(ast, PTR_EXTRACT));

            default:
                error(diags::expression_not_constant(), ast);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_constant_tuple(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items) {
        vector<prog::constant> values;
        vector<prog::type> types;

        for (auto& item_ptr : items) {
            auto& expr_marked = *item_ptr;
            switch (INDEX(expr_marked)) {
                case ast::expr_marked::EXPR: {
                    auto[value, type] = compile_constant_expr(*GET(expr_marked, EXPR));
                    values.push_back(move(value));
                    types.push_back(move(type));
                } break;

                default:
                    error(diags::expression_not_constant(), ast);
            }
        }

        if (values.empty()) {
            auto value = VARIANT(prog::constant, UNIT, prog::monostate{ });
            auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{ prog::primitive_type::UNIT }));
            return { move(value), move(type) };
        }
        if (values.size() == 1)
            return { move(values[0]), move(types[0]) };

        return { VARIANT(prog::constant, TUPLE, into_ptr_vector(values)), VARIANT(prog::type, TUPLE, into_ptr_vector(types)) };
    }

    pair<prog::constant, prog::type> compiler::compile_constant_array(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items) {
        vector<prog::ptr<prog::constant>> values;
        prog::type type;

        for (auto& item_ptr : items) {
            auto& expr_marked = *item_ptr;
            switch (INDEX(expr_marked)) {
                case ast::expr_marked::EXPR: {
                    auto[item_value, item_type] = compile_constant_expr(*GET(expr_marked, EXPR));
                    if (values.empty())
                        type = move(item_type);
                    else
                        type = common_supertype(ast, type, item_type);
                    values.push_back(into_ptr(item_value));
                } break;

                default:
                    error(diags::expression_not_constant(), expr_marked);
            }
        }

        if (values.empty())
            type = VARIANT(prog::type, NEVER, monostate());
        return { VARIANT(prog::constant, ARRAY, move(values)), VARIANT(prog::type, ARRAY, make_ptr(prog::array_type{ into_ptr(type), values.size() })) };
    }

    pair<prog::constant, prog::type> compiler::compile_constant_application(const ast::node& ast, const ast::expr& receiver, const vector<ast::ptr<ast::expr_marked>>& args) {
        auto receiver_tp = compile_constant_expr(receiver).second;

        switch (INDEX(receiver_tp)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(receiver_tp, STRUCT_CTOR);
                auto& struct_type = program.struct_types[struct_index];
                size_t nargs = args.size(); // FIXME should be the expected number of arguments

                vector<bool> used_args(nargs, false);
                vector<pair<prog::constant, prog::type>> compiled_args(nargs);

                for (auto& arg : args) {
                    switch (INDEX(*arg)) {
                        case ast::expr_marked::EXPR: {
                            size_t coord = 0;
                            while (coord < nargs && used_args[coord])
                                coord++;

                            if (coord >= nargs)
                                error(diags::invalid_argument(nargs), ast);

                            compiled_args[coord] = compile_constant_expr(*GET(*arg, EXPR));
                            used_args[coord] = true;
                        } break;

                        case ast::expr_marked::EXPR_WITH_NAME:
                            error(diags::not_implemented(), ast); // TODO

                        case ast::expr_marked::EXPR_WITH_COORD: {
                            auto coord = GET(*arg, EXPR_WITH_COORD).first;

                            if (coord >= nargs)
                                error(diags::invalid_argument(nargs), ast);
                            if (used_args[coord])
                                error(diags::argument_reused(coord), ast);

                            compiled_args[coord] = compile_constant_expr(*GET(*arg, EXPR_WITH_COORD).second);
                            used_args[coord] = true;
                        } break;
                    }
                }

                // TODO check types and convert values

                vector<prog::ptr<prog::constant>> result;
                for (auto& compiled_arg : compiled_args)
                    result.push_back(into_ptr(compiled_arg.first));

                return { VARIANT(prog::constant, STRUCT, move(result)), VARIANT(prog::type, STRUCT, struct_index) };
            }

            case prog::type::ENUM_CTOR: {
                auto [enum_index, variant_index] = GET(receiver_tp, ENUM_CTOR);
                auto& enum_type = program.enum_types[enum_index];
                size_t nargs = args.size(); // FIXME should be the expected number of arguments

                vector<bool> used_args(nargs, false);
                vector<pair<prog::constant, prog::type>> compiled_args(nargs);

                for (auto& arg : args) {
                    switch (INDEX(*arg)) {
                        case ast::expr_marked::EXPR: {
                            size_t coord = 0;
                            while (coord < nargs && used_args[coord])
                                coord++;

                            if (coord >= nargs)
                                error(diags::invalid_argument(nargs), ast);

                            compiled_args[coord] = compile_constant_expr(*GET(*arg, EXPR));
                            used_args[coord] = true;
                        } break;

                        case ast::expr_marked::EXPR_WITH_NAME:
                            error(diags::invalid_expression(), ast);

                        case ast::expr_marked::EXPR_WITH_COORD: {
                            auto coord = GET(*arg, EXPR_WITH_COORD).first;

                            if (coord >= nargs)
                                error(diags::invalid_argument(nargs), ast);
                            if (used_args[coord])
                                error(diags::argument_reused(coord), ast);

                            compiled_args[coord] = compile_constant_expr(*GET(*arg, EXPR_WITH_COORD).second);
                            used_args[coord] = true;
                        } break;
                    }
                }

                // TODO check types and convert values

                vector<prog::ptr<prog::constant>> result;
                for (auto& compiled_arg : compiled_args)
                    result.push_back(into_ptr(compiled_arg.first));

                return { VARIANT(prog::constant, ENUM, { variant_index, move(result) }), VARIANT(prog::type, ENUM, enum_index) };
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
        if (!global_names.count(name))
            error(diags::name_not_declared(name), ast);
        auto& global_name = global_names[name];

        switch (global_name.kind) {
            case global_name::VARIABLE:
                error(diags::expression_not_constant(), ast);

            case global_name::CONSTANT: {
                auto& global_var = constants[global_name.index];
                return { prog::copy_constant(*global_var.value), prog::copy_type(*global_var.tp) };
            }

            case global_name::FUNCTION: {
                auto& global_func = *program.global_funcs[global_name.index];
                return { VARIANT(prog::constant, GLOBAL_FUNC_PTR, global_name.index), VARIANT(prog::type, GLOBAL_FUNC, make_ptr(prog::copy_func_type(*global_func.tp))) };
            }

            case global_name::STRUCT: {
                if (!global_name.compiled)
                    error(diags::name_not_compiled(name), ast);
                return { VARIANT(prog::constant, UNIT, monostate()), VARIANT(prog::type, STRUCT_CTOR, global_name.index) };
            }

            case global_name::ENUM:
                error(diags::invalid_expression(), ast);
        }

        UNREACHABLE;
    }

    pair<prog::constant, prog::type> compiler::compile_constant_variant_name(const ast::node& ast, const string& name, const string& variant_name) {
        if (!global_names.count(name))
            error(diags::name_not_declared(name), ast);
        auto& global_name = global_names[name];

        if (global_name.kind != global_name::ENUM)
            error(diags::invalid_kind(), ast);
        if (!global_name.compiled)
            error(diags::name_not_compiled(name), ast);

        auto variant_index = 0; // TODO

        return { VARIANT(prog::constant, UNIT, monostate()), VARIANT(prog::type, ENUM_CTOR, make_pair(global_name.index, variant_index)) };
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
                error(diags::not_implemented(), ast); // TODO
            }

            case ast::literal_expr::INT: {
                auto[value, type] = compile_int_token(*GET(ast, INT));
                return {move(value), VARIANT(prog::type, PRIMITIVE, into_ptr(type))};
            }

            case ast::literal_expr::FLOAT: {
                auto[value, type] = compile_float_token(*GET(ast, FLOAT));
                return {move(value), VARIANT(prog::type, PRIMITIVE, into_ptr(type))};
            }

            default:
                error(diags::not_implemented(), ast); // TODO
        }
    }

    pair<prog::constant, prog::primitive_type> compiler::compile_int_token(const ast::int_token& ast) {
        #define RETURN_IF_OK(type, type_marker) { \
            auto opt_val = try_make_number<type>(ast.value, ast.negative); \
            if (opt_val) return { VARIANT(prog::constant, INT, encode_number(*opt_val)), { prog::primitive_type::type_marker } }; \
        }

        #define RETURN_OR_ERROR(type, type_marker) { \
            RETURN_IF_OK(type, type_marker) \
            error(diags::integer_overflow((ast.negative ? "-" : "") + to_string(ast.value), is_signed<type>(), 8 * sizeof(type)), ast); \
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
        double value = ast.negative ? -ast.value : ast.value;

        switch (ast.marker) {
            case ast::float_token::NONE:
            case ast::float_token::F:
            case ast::float_token::F64: {
                return { VARIANT(prog::constant, FLOAT, value), {prog::primitive_type::F64} };
            }

            case ast::float_token::F32: {
                return { VARIANT(prog::constant, FLOAT, value), {prog::primitive_type::F32} };
            }
        }

        UNREACHABLE;
    }

    pair<prog::constant, prog::type> compiler::compile_constant_ptr(const ast::node& ast, const string& name) {
        error(diags::not_implemented(), ast); // TODO
    }

    pair<prog::constant, prog::type> compiler::compile_constant_sized_array(const ast::sized_array_expr& ast) {
        error(diags::not_implemented(), ast); // TODO
    }

    pair<prog::constant, prog::type> compiler::compile_constant_length(const ast::node& ast, const ast::expr& target) {
        error(diags::not_implemented(), ast); // TODO
    }

    pair<prog::constant, prog::type> compiler::compile_constant_extract(const ast::extract_expr& ast) {
        error(diags::not_implemented(), ast); // TODO
    }

    pair<prog::constant, prog::type> compiler::compile_constant_ptr_extract(const ast::ptr_extract_expr& ast) {
        error(diags::not_implemented(), ast); // TODO
    }

    prog::constant compiler::convert_constant(const ast::node& ast, const prog::constant& constant, const prog::type& from_tp, const prog::type& to_tp) {
        error(diags::not_implemented(), ast); // TODO
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
