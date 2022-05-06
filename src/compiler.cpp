#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "utils.hpp"
#include <utility>
#include <variant>
#include <exception>

namespace sg {
    using namespace sg::utils;

    using std::move;
    using std::get;
    using std::numeric_limits;
    using std::is_signed;
    using std::is_unsigned;
    using std::to_string;

    optional<prog::program> compiler::compile(const ast::program& ast) {
        try {
            program = { };
            compile_program(ast);
            return { move(program) };
        } catch (...) {
            return { };
        }
    }

    void compiler::compile_program(const ast::program& ast) {
        // Phase 1: Prepare struct and enum declarations

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *get<ast::global_def::STRUCT_DEF>(global_def->value);
                    auto struct_type = declare_struct_type(struct_type_ast);
                    auto name = struct_type.name;
                    auto index = program.struct_types.size();
                    program.struct_types.push_back(into_ptr(struct_type));
                    global_names[name] = { global_name::STRUCT, index, false };
                    break;
                }

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *get<ast::global_def::ENUM_DEF>(global_def->value);
                    auto enum_type = declare_enum_type(enum_type_ast);
                    auto name = enum_type.name;
                    auto index = program.enum_types.size();
                    program.enum_types.push_back(into_ptr(enum_type));
                    global_names[name] = { global_name::ENUM, index, false };
                    break;
                }

            }
        }

        // Phase 2: Compile struct, enum definitions and constants

        size_t struct_type_index = 0;
        size_t enum_type_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *get<ast::global_def::STRUCT_DEF>(global_def->value);
                    auto& struct_type = *program.struct_types[struct_type_index++];
                    compile_struct_type(struct_type_ast, struct_type);
                    global_names[struct_type.name].compiled = true;
                    break;
                }

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *get<ast::global_def::ENUM_DEF>(global_def->value);
                    auto& enum_type = *program.enum_types[enum_type_index++];
                    compile_enum_type(enum_type_ast, enum_type);
                    global_names[enum_type.name].compiled = true;
                    break;
                }

                case ast::global_def::CONST_DEF: {
                    auto& global_const_ast = *get<ast::global_def::CONST_DEF>(global_def->value);
                    auto global_const = compile_global_var(global_const_ast);
                    auto name = global_const.name;
                    auto index = constants.size();
                    constants.push_back(into_ptr(global_const));
                    global_names[name] = { global_name::CONSTANT, index, true };
                    break;
                }
            }
        }

        // Phase 3: Prepare global function declarations

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *get<ast::global_def::FUNC_DEF>(global_def->value);
                    auto global_func = declare_global_func(global_func_ast);
                    auto name = global_func.name;
                    auto index = program.global_funcs.size();
                    program.global_funcs.push_back(into_ptr(global_func));
                    global_names[name] = { global_name::FUNCTION, index, false };
                    break;
                }
            }
        }


        // Phase 4: Compile global variable definitions

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::VAR_DEF: {
                    auto& global_var_ast = *get<ast::global_def::VAR_DEF>(global_def->value);
                    auto global_var = compile_global_var(global_var_ast);
                    auto name = global_var.name;
                    auto index = program.global_vars.size();
                    program.global_vars.push_back(into_ptr(global_var));
                    global_names[name] = { global_name::VARIABLE, index, true };
                    break;
                }
            }
        }

        // Phase 5: Compile global function definitions

        size_t global_func_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *get<ast::global_def::FUNC_DEF>(global_def->value);
                    auto& global_func = *program.global_funcs[global_func_index++];
                    compile_global_func(global_func_ast, global_func);
                    global_names[global_func.name].compiled = true;
                    break;
                }
            }
        }
    }

    prog::global_var compiler::compile_global_var(const ast::var_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used_error(name));

        auto[value, value_tp] = compile_constant_expr(*ast.value);

        prog::type tp;

        if (ast.tp) {
            tp = compile_type(**ast.tp);
            value = convert_constant(value, value_tp, tp);
        } else
            tp = move(value_tp);

        return { name, into_ptr(tp), into_ptr(value) };
    }

    prog::global_func compiler::declare_global_func(const ast::func_def& ast) {
        error(diags::not_implemented_error()); // TODO
    }

    prog::struct_type compiler::declare_struct_type(const ast::struct_def& ast) {
        error(diags::not_implemented_error()); // TODO
    }

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        error(diags::not_implemented_error()); // TODO
    }

    void compiler::compile_global_func(const ast::func_def& ast, prog::global_func& global_func) {
        error(diags::not_implemented_error()); // TODO
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type) {
        error(diags::not_implemented_error()); // TODO
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type) {
        error(diags::not_implemented_error()); // TODO
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

    pair<prog::constant, prog::type> compiler::compile_constant_expr(const ast::expr& ast) {
        switch (ast.value.index()) { // TODO more constant expressions
            case ast::expr::CONST: {
                return compile_constant_literal(*get<ast::expr::CONST>(ast.value));
            }

            case ast::expr::SOME: {
                auto[inner_value, inner_type] = compile_constant_expr(*get<ast::expr::SOME>(ast.value));
                auto value = VARIANT(prog::constant, SOME, into_ptr(inner_value));
                auto type = VARIANT(prog::type, OPTIONAL, into_ptr(inner_type));
                return { move(value), move(type) };
            }

            case ast::expr::NONE: {
                auto value = VARIANT(prog::constant, NONE, std::monostate{ });
                auto type = VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{ prog::primitive_type::NEVER }))));
                return { move(value), move(type) };
            }

            default:
                error(diags::expression_not_constant());
        }
    }

    pair<prog::constant, prog::type> compiler::compile_constant_literal(const ast::const_expr& ast) {
        switch (ast.value.index()) {
            case ast::const_expr::BOOL: {
                auto value = VARIANT(prog::constant, BOOL, get<ast::const_expr::BOOL>(ast.value));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::BOOL}));
                return {move(value), move(type)};
            }

            case ast::const_expr::CHAR: {
                auto value = VARIANT(prog::constant, INT, encode_number(static_cast<uint8_t>(get<ast::const_expr::CHAR>(ast.value))));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::U8}));
                return {move(value), move(type)};
            }

            case ast::const_expr::STRING: {
                error(diags::not_implemented_error()); // TODO
            }

            case ast::const_expr::INT: {
                auto[value, type] = compile_int_token(*get<ast::const_expr::INT>(ast.value));
                return {move(value), VARIANT(prog::type, PRIMITIVE, into_ptr(type))};
            }

            case ast::const_expr::FLOAT: {
                auto[value, type] = compile_float_token(*get<ast::const_expr::FLOAT>(ast.value));
                return {move(value), VARIANT(prog::type, PRIMITIVE, into_ptr(type))};
            }

            default:
                error(diags::not_implemented_error());
        }
    }

    pair<prog::constant, prog::primitive_type> compiler::compile_int_token(const ast::int_token& ast) {
#define RETURN_IF_OK(type, type_marker) {auto opt_val = try_make_number<type>(ast.value, ast.negative); if(opt_val) return { VARIANT(prog::constant, INT, encode_number(*opt_val)), { prog::primitive_type::type_marker } }; }
#define RETURN_OR_ERROR(type, type_marker) {RETURN_IF_OK(type, type_marker) error(diags::integer_overflow_error((ast.negative ? "-" : "") + to_string(ast.value), is_signed<type>(), 8 * sizeof(type))); throw 0;}

        switch (ast.marker) {
            case ast::int_token::NONE:
            case ast::int_token::I:{
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

            case ast::int_token::U:{
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

            default:
                throw 0; // unreachable state
        }

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

            default:
                throw 0; // unreachable state
        }
    }

    prog::type compiler::compile_type(const ast::type& ast) {
        switch (ast.value.index()) {
            case ast::type::PRIMITIVE: {
                auto tp = compile_primitive_type(*get<ast::type::PRIMITIVE>(ast.value));
                return VARIANT(prog::type, PRIMITIVE, into_ptr(tp));
            }

            case ast::type::USER_TYPE: // TODO
            case ast::type::TUPLE:
            case ast::type::ARRAY:
            case ast::type::OPTIONAL:
            case ast::type::PTR:
            case ast::type::INNER_PTR:
            case ast::type::FUNC:
            case ast::type::GLOBAL_FUNC:
            case ast::type::FUNC_WITH_PTR:
            default:
                error(diags::not_implemented_error());
        }
    }

    prog::primitive_type compiler::compile_primitive_type(const ast::primitive_type& ast) {
        error(diags::not_implemented_error()); // TODO
    }

    prog::constant compiler::convert_constant(const prog::constant& constant, const prog::type& from_tp, const prog::type& to_tp) {
        error(diags::not_implemented_error()); // TODO
    }
}
