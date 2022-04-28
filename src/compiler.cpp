#include "compiler.hpp"
#include "utils.hpp"
#include <utility>
#include <variant>
#include <exception>

namespace sg {
    using namespace sg::utils;

    using std::move;
    using std::get;
    using std::exception;

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
                }

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *get<ast::global_def::ENUM_DEF>(global_def->value);
                    auto enum_type = declare_enum_type(enum_type_ast);
                    auto name = enum_type.name;
                    auto index = program.enum_types.size();
                    program.enum_types.push_back(into_ptr(enum_type));
                    global_names[name] = { global_name::ENUM, index, false };
                }
            }
        }

        // Phase 2: Prepare global function declarations

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *get<ast::global_def::FUNC_DEF>(global_def->value);
                    auto global_func = declare_global_func(global_func_ast);
                    auto name = global_func.name;
                    auto index = program.global_funcs.size();
                    program.global_funcs.push_back(into_ptr(global_func));
                    global_names[name] = { global_name::FUNCTION, index, false };
                }
            }
        }

        // Phase 3: Compile struct and enum definitions

        size_t struct_type_index = 0;
        size_t enum_type_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *get<ast::global_def::STRUCT_DEF>(global_def->value);
                    auto& struct_type = *program.struct_types[struct_type_index++];
                    compile_struct_type(struct_type_ast, struct_type);
                    global_names[struct_type.name].compiled = true;
                }

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *get<ast::global_def::ENUM_DEF>(global_def->value);
                    auto& enum_type = *program.enum_types[enum_type_index++];
                    compile_enum_type(enum_type_ast, enum_type);
                    global_names[enum_type.name].compiled = true;
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
                }
            }
        }
    }

    prog::global_var compiler::compile_global_var(const ast::var_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used_error(name));

        auto value = compile_constant(*ast.value);
        auto value_tp = constant_type(value);

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

    prog::constant compiler::compile_constant(const ast::expr& ast) {
        switch (ast.value.index()) {
            case ast::expr::TUPLE: // TODO
            case ast::expr::ARRAY:
            case ast::expr::APPLICATION:
            default:
                error(diags::not_implemented_error());
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

    prog::type compiler::constant_type(const prog::constant& constant) {
        error(diags::not_implemented_error()); // TODO
    }

    prog::constant compiler::convert_constant(const prog::constant& constant, const prog::type& from_tp, const prog::type& to_tp) {
        error(diags::not_implemented_error()); // TODO
    }
}
