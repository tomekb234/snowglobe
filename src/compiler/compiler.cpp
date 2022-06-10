#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"
#include <queue>

namespace sg {
    using namespace sg::utils;

    using std::queue;

    bool compiler::compile(const ast::program& ast) {
        program = { };
        auto ok = true;

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
                        global_names[name] = { global_name_kind::STRUCT, index, false };
                        struct_type_indices.back() = { index };
                    } break;

                    case ast::global_def::ENUM_DEF: {
                        enum_type_indices.push({ });
                        auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                        auto enum_type = declare_enum_type(enum_type_ast);
                        auto name = enum_type.name;
                        auto index = program.enum_types.size();
                        program.enum_types.push_back(into_ptr(enum_type));
                        global_names[name] = { global_name_kind::ENUM, index, false };
                        enum_type_indices.back() = { index };
                    } break;

                    default:
                        break;
                }
            } catch (compiler_error) {
                ok = false;
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
                        if (!index)
                            break;
                        struct_type_indices.pop();
                        auto& struct_type = *program.struct_types[*index];
                        compile_struct_type(struct_type_ast, struct_type);
                        global_names[struct_type.name].compiled = true;
                    } break;

                    case ast::global_def::ENUM_DEF: {
                        auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                        auto index = enum_type_indices.front();
                        if (!index)
                            break;
                        enum_type_indices.pop();
                        auto& enum_type = *program.enum_types[*index];
                        compile_enum_type(enum_type_ast, enum_type);
                        global_names[enum_type.name].compiled = true;
                    } break;

                    case ast::global_def::CONST_DEF: {
                        auto& global_const_ast = *GET(*global_def, CONST_DEF);
                        auto global_const = compile_global_var(global_const_ast);
                        auto name = *global_const.name;
                        auto index = constants.size();
                        constants.push_back(move(global_const));
                        global_names[name] = { global_name_kind::CONSTANT, index, true };
                    } break;

                    case ast::global_def::FUNC_DEF: {
                        global_func_indices.push({ });
                        auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                        auto global_func = declare_global_func(global_func_ast);
                        auto name = global_func.name;
                        auto index = program.global_funcs.size();
                        program.global_funcs.push_back(into_ptr(global_func));
                        global_names[name] = { global_name_kind::FUNCTION, index, false };
                        global_func_indices.back() = { index };
                    } break;

                    default:
                        break;
                }
            } catch (compiler_error) {
                ok = false;
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
                        global_names[name] = { global_name_kind::VARIABLE, index, true };
                    } break;

                    default:
                        break;
                }
            } catch (compiler_error) {
                ok = false;
            }
        }

        // Phase 4: Compile global function definitions

        for (auto& global_def : ast.global_defs) {
            try {
                switch (INDEX(*global_def)) {
                    case ast::global_def::FUNC_DEF: {
                        auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                        auto index = global_func_indices.front();
                        if (!index)
                            break;
                        global_func_indices.pop();
                        auto& global_func = *program.global_funcs[*index];
                        compile_global_func(global_func_ast, global_func);
                        global_names[global_func.name].compiled = true;
                    } break;

                    default:
                        break;
                }
            } catch (compiler_error) {
                ok = false;
            }
        }

        return ok;
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

    compiler::global_name& compiler::get_global_name(const ast::node& ast, const string& name, global_name_kind expected_kind, bool allow_uncompiled) {
        auto& global_name = get_global_name(ast, name, allow_uncompiled);

        if (global_name.kind != expected_kind)
            error(diags::invalid_kind(name, global_name.kind, { expected_kind }), ast);

        return global_name;
    }

    vector<reference_wrapper<const ast::expr>> compiler::order_arguments(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& args_ast, optional<function<size_t(const ast::node&, string)>> arg_with_name, optional<size_t> expected_number) {
        auto size = args_ast.size();

        if (expected_number && size != *expected_number)
            error(diags::invalid_argument_number(size, *expected_number), ast);

        vector<bool> used(size, false);
        vector<size_t> indices(size);
        vector<const ast::expr*> value_ptrs(size);

        for (auto& arg_ast : args_ast) {
            size_t index = 0;
            const ast::expr* value_ast;

            switch (INDEX(*arg_ast)) {
                case ast::expr_marked::EXPR: {
                    while (index < size && used[index])
                        index++;
                    value_ast = GET(*arg_ast, EXPR).get();
                } break;

                case ast::expr_marked::EXPR_WITH_NAME: {
                    auto name = GET(*arg_ast, EXPR_WITH_NAME).first;
                    if (arg_with_name)
                        index = (*arg_with_name)(*arg_ast, name);
                    else
                        error(diags::invalid_argument_marker(), *arg_ast);
                } break;

                case ast::expr_marked::EXPR_WITH_COORD: {
                    index = GET(*arg_ast, EXPR_WITH_COORD).first;
                    value_ast = GET(*arg_ast, EXPR_WITH_COORD).second.get();
                } break;
            }

            if (index >= size)
                error(diags::invalid_argument_index(index, size), *arg_ast);
            if (used[index])
                error(diags::reused_argument_index(index), *arg_ast);

            value_ptrs[index] = value_ast;
            used[index] = true;
        }

        for (size_t index = 0; index < size; index++) {
            if (!used[index])
                error(diags::missing_argument(index), ast);
        }

        vector<reference_wrapper<const ast::expr>> values;

        for (auto ptr : value_ptrs)
            values.push_back(*ptr);

        return values;
    }

}
