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
        prog = { };
        auto ok = true;

        // Phase 1: Prepare struct and enum declarations

        queue<optional<prog::global_index>> struct_type_indices;
        queue<optional<prog::global_index>> enum_type_indices;

        for (auto& global_def : ast.global_defs) {
            try {
                switch (INDEX(*global_def)) {
                    case ast::global_def::STRUCT_DEF: {
                        struct_type_indices.push({ });
                        auto& struct_type_ast = *GET(*global_def, STRUCT_DEF);
                        auto struct_type = declare_struct_type(struct_type_ast);
                        auto name = struct_type.name;
                        auto index = prog.struct_types.size();
                        prog.struct_types.push_back(into_ptr(struct_type));
                        global_names[name] = { global_name_kind::STRUCT, index, false };
                        struct_type_indices.back() = { index };
                    } break;

                    case ast::global_def::ENUM_DEF: {
                        enum_type_indices.push({ });
                        auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                        auto enum_type = declare_enum_type(enum_type_ast);
                        auto name = enum_type.name;
                        auto index = prog.enum_types.size();
                        prog.enum_types.push_back(into_ptr(enum_type));
                        global_names[name] = { global_name_kind::ENUM, index, false };
                        enum_type_indices.back() = { index };
                    } break;

                    default:
                        break;
                }
            } catch (compilation_error) {
                ok = false;
            }
        }

        // Phase 2: Compile struct and enum definitions. Compile constants. Prepare global function declarations

        queue<optional<prog::global_index>> global_func_indices;

        for (auto& global_def : ast.global_defs) {
            try {
                switch (INDEX(*global_def)) {
                    case ast::global_def::STRUCT_DEF: {
                        auto& struct_type_ast = *GET(*global_def, STRUCT_DEF);
                        auto index = struct_type_indices.front();
                        if (!index)
                            break;
                        struct_type_indices.pop();
                        auto& struct_type = *prog.struct_types[*index];
                        compile_struct_type(struct_type_ast, struct_type);
                        global_names[struct_type.name].compiled = true;
                    } break;

                    case ast::global_def::ENUM_DEF: {
                        auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                        auto index = enum_type_indices.front();
                        if (!index)
                            break;
                        enum_type_indices.pop();
                        auto& enum_type = *prog.enum_types[*index];
                        compile_enum_type(enum_type_ast, enum_type);
                        global_names[enum_type.name].compiled = true;
                    } break;

                    case ast::global_def::CONST_DEF: {
                        auto& global_const_ast = *GET(*global_def, CONST_DEF);
                        auto global_const = compile_global_var(global_const_ast);
                        auto name = *global_const.name;
                        auto index = consts.size();
                        consts.push_back(move(global_const));
                        global_names[name] = { global_name_kind::CONST, index, true };
                    } break;

                    case ast::global_def::FUNC_DEF: {
                        global_func_indices.push({ });
                        auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                        auto global_func = declare_global_func(global_func_ast);
                        auto name = global_func.name;
                        auto index = prog.global_funcs.size();
                        prog.global_funcs.push_back(into_ptr(global_func));
                        global_names[name] = { global_name_kind::FUNC, index, false };
                        global_func_indices.back() = { index };
                    } break;

                    default:
                        break;
                }
            } catch (compilation_error) {
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
                        auto index = prog.global_vars.size();
                        prog.global_vars.push_back(into_ptr(global_var));
                        global_names[name] = { global_name_kind::VAR, index, true };
                    } break;

                    default:
                        break;
                }
            } catch (compilation_error) {
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
                        auto& global_func = *prog.global_funcs[*index];
                        compile_global_func(global_func_ast, global_func);
                        global_names[global_func.name].compiled = true;
                    } break;

                    default:
                        break;
                }
            } catch (compilation_error) {
                ok = false;
            }
        }

        return ok;
    }

    compiler::global_name& compiler::get_global_name(const string& name, location loc, bool allow_uncompiled_types) {
        auto it = global_names.find(name);
        if (it == global_names.end())
            error(diags::name_not_declared(name), loc);

        auto& global_name = it->second;

        auto type = global_name.kind == global_name_kind::STRUCT || global_name.kind == global_name_kind::ENUM;
        if (type && !allow_uncompiled_types && !global_name.compiled)
            error(diags::name_not_compiled(name), loc);

        return global_name;
    }

    compiler::global_name& compiler::get_global_name(const string& name, global_name_kind expected_kind, location loc, bool allow_uncompiled_types) {
        auto& global_name = get_global_name(name, loc, allow_uncompiled_types);

        if (global_name.kind != expected_kind)
            error(diags::invalid_kind(name, global_name.kind, { expected_kind }), loc);

        return global_name;
    }

    vector<ref<const ast::expr>> compiler::order_args(
            const args_ast_vector& ast_vec,
            optional<arg_with_name_function> arg_with_name,
            optional<size_t> expected_count,
            location loc) {
        auto size = ast_vec.size();

        if (expected_count && size != *expected_count)
            error(diags::invalid_argument_count(size, *expected_count), loc);

        vector<bool> used(size, false);
        vector<size_t> indices(size);
        vector<const ast::expr*> value_ptrs(size);

        for (auto& arg_ast : ast_vec) {
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
                        index = (*arg_with_name)(arg_ast->loc, name);
                    else
                        error(diags::invalid_argument_marker(), arg_ast->loc);
                    value_ast = GET(*arg_ast, EXPR_WITH_NAME).second.get();
                } break;

                case ast::expr_marked::EXPR_WITH_COORD: {
                    index = GET(*arg_ast, EXPR_WITH_COORD).first;
                    value_ast = GET(*arg_ast, EXPR_WITH_COORD).second.get();
                } break;
            }

            if (index >= size)
                error(diags::invalid_argument_index(index, size), arg_ast->loc);
            if (used[index])
                error(diags::reused_argument_index(index), arg_ast->loc);

            value_ptrs[index] = value_ast;
            used[index] = true;
        }

        for (size_t index = 0; index < size; index++) {
            if (!used[index])
                error(diags::missing_argument(index), loc);
        }

        vector<ref<const ast::expr>> values;

        for (auto ptr : value_ptrs)
            values.push_back(*ptr);

        return values;
    }
}
