#include "compiler.hpp"
#include "compiler_diagnostics.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "utils.hpp"
#include <queue>

namespace sg {
    using namespace sg::utils;
    using std::queue;

    bool compiler::compile_program(const ast::program& ast) {
        program = { };
        bool error_occured = false;

        // Phase 1: Prepare struct and enum declarations

        queue<optional<size_t>> struct_type_indices;
        queue<optional<size_t>> enum_type_indices;

        for (auto& global_def : ast.global_defs) {
            try {
                switch (INDEX(*global_def)) {
                    case ast::global_def::STRUCT_DEF: {
                        struct_type_indices.push({ });
                        auto& struct_type_ast = *GET(*global_def, STRUCT_DEF);
                        auto struct_type = declare_struct_type(struct_type_ast);
                        auto name = struct_type.name;
                        auto index = program.struct_types.size();
                        program.struct_types.push_back(into_ptr(struct_type));
                        global_names[name] = { global_name::STRUCT, index, false };
                        struct_type_indices.back() = index;
                    } break;

                    case ast::global_def::ENUM_DEF: {
                        enum_type_indices.push({ });
                        auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                        auto enum_type = declare_enum_type(enum_type_ast);
                        auto name = enum_type.name;
                        auto index = program.enum_types.size();
                        program.enum_types.push_back(into_ptr(enum_type));
                        global_names[name] = { global_name::ENUM, index, false };
                        enum_type_indices.back() = index;
                    } break;
                }
            } catch (...) {
                error_occured = true;
            }
        }

        // Phase 2: Compile struct and enum definitions. Compile constants. Prepare global function declarations

        queue<optional<size_t>> global_func_indices;

        for (auto& global_def : ast.global_defs) {
            try {
                switch (INDEX(*global_def)) {
                    case ast::global_def::STRUCT_DEF: {
                        auto& struct_type_ast = *GET(*global_def, STRUCT_DEF);
                        auto index = struct_type_indices.front();
                        struct_type_indices.pop();
                        auto& struct_type = *program.struct_types[index.value()];
                        compile_struct_type(struct_type_ast, struct_type);
                        global_names[struct_type.name].compiled = true;
                    } break;

                    case ast::global_def::ENUM_DEF: {
                        auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                        auto index = enum_type_indices.front();
                        enum_type_indices.pop();
                        auto& enum_type = *program.enum_types[index.value()];
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
                        global_func_indices.push({ });
                        auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                        auto global_func = declare_global_func(global_func_ast);
                        auto name = global_func.name;
                        auto index = program.global_funcs.size();
                        program.global_funcs.push_back(into_ptr(global_func));
                        global_names[name] = { global_name::FUNCTION, index, false };
                        global_func_indices.back() = index;
                    } break;
                }
            } catch (...) {
                error_occured = true;
            }
        }

        // Phase 3: Compile global variable definitions

        for (auto& global_def : ast.global_defs) {
            try {
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
            } catch (...) {
                error_occured = true;
            }
        }

        // Phase 4: Compile global function definitions

        for (auto& global_def : ast.global_defs) {
            try {
                switch (INDEX(*global_def)) {
                    case ast::global_def::FUNC_DEF: {
                        auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                        auto index = global_func_indices.front();
                        global_func_indices.pop();
                        auto& global_func = *program.global_funcs[index.value()];
                        compile_global_func(global_func_ast, global_func);
                        global_names[global_func.name].compiled = true;
                    } break;
                }
            } catch (...) {
                error_occured = true;
            }
        }

        return !error_occured;
    }

    compiler::global_name& compiler::get_global_name(const ast::node& ast, const string& name, bool allow_uncompiled) {
        auto it = global_names.find(name);

        if (it == global_names.end())
            error(diags::name_not_declared(name), ast);

        auto& global_name = it->second;

        if (!allow_uncompiled && !global_name.compiled)
            error(diags::name_not_compiled(name), ast);

        return global_name;
    }

    compiler::global_name& compiler::get_global_name(const ast::node& ast, const string& name, global_name::kind_t expected_kind, bool allow_uncompiled) {
        auto& global_name = get_global_name(ast, name, allow_uncompiled);

        if (global_name.kind != expected_kind)
            error(diags::invalid_kind(), ast);

        return global_name;
    }
}
