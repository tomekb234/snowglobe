#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

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
            switch (INDEX(*global_def)) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *GET(*global_def, STRUCT_DEF);
                    auto struct_type = declare_struct_type(struct_type_ast);
                    auto name = struct_type.name;
                    auto index = program.struct_types.size();
                    program.struct_types.push_back(into_ptr(struct_type));
                    global_names[name] = { global_name::STRUCT, index, false };
                } break;

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                    auto enum_type = declare_enum_type(enum_type_ast);
                    auto name = enum_type.name;
                    auto index = program.enum_types.size();
                    program.enum_types.push_back(into_ptr(enum_type));
                    global_names[name] = { global_name::ENUM, index, false };
                } break;
            }
        }

        // Phase 2: Compile struct, enum definitions and constants, prepare global function declarations

        size_t struct_type_index = 0;
        size_t enum_type_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (INDEX(*global_def)) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *GET(*global_def, STRUCT_DEF);
                    auto& struct_type = *program.struct_types[struct_type_index++];
                    compile_struct_type(struct_type_ast, struct_type);
                    global_names[struct_type.name].compiled = true;
                } break;

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                    auto& enum_type = *program.enum_types[enum_type_index++];
                    compile_enum_type(enum_type_ast, enum_type);
                    global_names[enum_type.name].compiled = true;
                } break;

                case ast::global_def::CONST_DEF: {
                    auto& global_const_ast = *GET(*global_def, CONST_DEF);
                    auto global_const = compile_global_var(global_const_ast);
                    auto name = *global_const.name;
                    auto index = constants.size();
                    constants.push_back(move(global_const));
                    global_names[name] = { global_name::CONSTANT, index, true };
                } break;

                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                    auto global_func = declare_global_func(global_func_ast);
                    auto name = global_func.name;
                    auto index = program.global_funcs.size();
                    program.global_funcs.push_back(into_ptr(global_func));
                    global_names[name] = { global_name::FUNCTION, index, false };
                } break;
            }
        }

        // Phase 3: Compile global variable definitions

        for (auto& global_def : ast.global_defs) {
            switch (INDEX(*global_def)) {
                case ast::global_def::VAR_DEF: {
                    auto& global_var_ast = *GET(*global_def, VAR_DEF);
                    auto global_var = compile_global_var(global_var_ast);
                    auto name = *global_var.name;
                    auto index = program.global_vars.size();
                    program.global_vars.push_back(into_ptr(global_var));
                    global_names[name] = { global_name::VARIABLE, index, true };
                } break;
            }
        }

        // Phase 4: Compile global function definitions

        size_t global_func_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (INDEX(*global_def)) {
                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                    auto& global_func = *program.global_funcs[global_func_index++];
                    compile_global_func(global_func_ast, global_func);
                    global_names[global_func.name].compiled = true;
                } break;
            }
        }
    }
}
