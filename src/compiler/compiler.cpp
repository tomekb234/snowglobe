#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

#include <iostream>

namespace sg {
    using namespace sg::utils;

    bool compiler::compile(const ast::program& ast) {
        prog = { };
        auto ok = true;
        auto global_def_asts = as_cref_vector(ast.global_defs);

        // Phase 1: Prepare struct and enum declarations

        queue<optional<prog::global_index>> struct_indices;
        queue<optional<prog::global_index>> enum_indices;

        for (const ast::global_def& def_ast : global_def_asts) {
            try {
                switch (INDEX(def_ast)) {
                    case ast::global_def::STRUCT_DEF: {
                        struct_indices.push({ });
                        auto& struct_ast = *GET(def_ast, STRUCT_DEF);
                        auto st = declare_struct_type(struct_ast);
                        auto name = st.name;
                        auto index = prog.struct_types.size();
                        prog.struct_types.push_back(into_ptr(st));
                        global_names[name] = { global_name_kind::STRUCT, index, false };
                        struct_indices.back() = { index };
                    } break;

                    case ast::global_def::ENUM_DEF: {
                        enum_indices.push({ });
                        auto& enum_ast = *GET(def_ast, ENUM_DEF);
                        auto en = declare_enum_type(enum_ast);
                        auto name = en.name;
                        auto index = prog.enum_types.size();
                        prog.enum_types.push_back(into_ptr(en));
                        global_names[name] = { global_name_kind::ENUM, index, false };
                        enum_indices.back() = { index };
                    } break;

                    default:
                        break;
                }
            } catch (compilation_error) {
                ok = false;
            }
        }

        // Phase 2: Compile struct and enum definitions. Compile constants. Prepare global function declarations

        queue<optional<prog::global_index>> func_indices;

        for (const ast::global_def& def_ast : global_def_asts) {
            try {
                switch (INDEX(def_ast)) {
                    case ast::global_def::STRUCT_DEF: {
                        auto& struct_ast = *GET(def_ast, STRUCT_DEF);
                        auto index = struct_indices.front();
                        if (!index)
                            break;
                        struct_indices.pop();
                        auto& st = *prog.struct_types[*index];
                        compile_struct_type(struct_ast, st);
                        global_names[st.name].compiled = true;
                        auto destructor = make_struct_destructor(*index);
                        st.destructor = prog.global_funcs.size();
                        prog.global_funcs.push_back(into_ptr(destructor));
                    } break;

                    case ast::global_def::ENUM_DEF: {
                        auto& enum_ast = *GET(def_ast, ENUM_DEF);
                        auto index = enum_indices.front();
                        if (!index)
                            break;
                        enum_indices.pop();
                        auto& en = *prog.enum_types[*index];
                        compile_enum_type(enum_ast, en);
                        global_names[en.name].compiled = true;
                        auto destructor = make_enum_destructor(*index);
                        en.destructor = prog.global_funcs.size();
                        prog.global_funcs.push_back(into_ptr(destructor));
                    } break;

                    case ast::global_def::CONST_DEF: {
                        auto& var_ast = *GET(def_ast, CONST_DEF);
                        auto var = compile_global_var(var_ast);
                        auto name = *var.name;
                        auto index = consts.size();
                        consts.push_back(move(var));
                        global_names[name] = { global_name_kind::CONST, index, true };
                    } break;

                    case ast::global_def::FUNC_DEF: {
                        func_indices.push({ });
                        auto& func_ast = *GET(def_ast, FUNC_DEF);
                        auto func = declare_global_func(func_ast);
                        auto name = *func.name;
                        auto index = prog.global_funcs.size();
                        prog.global_funcs.push_back(into_ptr(func));
                        global_names[name] = { global_name_kind::FUNC, index, false };
                        func_indices.back() = { index };
                    } break;

                    default:
                        break;
                }
            } catch (compilation_error) {
                ok = false;
            }
        }

        // Phase 3: Compile global variable definitions

        for (const ast::global_def& def_ast : global_def_asts) {
            try {
                switch (INDEX(def_ast)) {
                    case ast::global_def::VAR_DEF: {
                        auto& var_ast = *GET(def_ast, VAR_DEF);
                        auto var = compile_global_var(var_ast);
                        auto name = *var.name;
                        auto index = prog.global_vars.size();
                        prog.global_vars.push_back(into_ptr(var));
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

        for (const ast::global_def& def_ast : global_def_asts) {
            try {
                switch (INDEX(def_ast)) {
                    case ast::global_def::FUNC_DEF: {
                        auto& func_ast = *GET(def_ast, FUNC_DEF);
                        auto index = func_indices.front();
                        if (!index)
                            break;
                        func_indices.pop();
                        auto& func = *prog.global_funcs[*index];
                        compile_global_func(func_ast, func);
                        global_names[*func.name].compiled = true;
                    } break;

                    default:
                        break;
                }
            } catch (compilation_error) {
                ok = false;
            }
        }

        try {
            auto main = get_global_name("main", { global_name_kind::FUNC }, whole_file(ast.loc));
            auto& func = *prog.global_funcs[main.index];

            if (!func.params.empty() || !INDEX_EQ(*func.return_tp, UNIT))
                error(diags::invalid_main_type(), whole_file(ast.loc));

            prog.entry_func = main.index;
        } catch (compilation_error) {
            ok = false;
        }

        try {
            auto cleanup = make_cleanup_func();
            prog.cleanup_func = prog.global_funcs.size();
            prog.global_funcs.push_back(into_ptr(cleanup));
        } catch (compilation_error) {
            ok = false;
        }

        return ok;
    }

    const compiler::global_name& compiler::get_global_name(string name, location loc) {
        auto iter = global_names.find(name);
        if (iter == global_names.end())
            error(diags::name_not_found(name), loc);
        return iter->second;
    }

    const compiler::global_name& compiler::get_global_name(string name, vector<global_name_kind> expected_kinds, location loc) {
        auto& gname = get_global_name(name, loc);
        if (find(expected_kinds.begin(), expected_kinds.end(), gname.kind) == expected_kinds.end())
            error(diags::invalid_kind(name, gname.kind, expected_kinds), loc);
        return gname;
    }

    const compiler::global_name& compiler::get_global_type(string name, bool allow_uncompiled, location loc) {
        auto& gname = get_global_name(name, { global_name_kind::STRUCT, global_name_kind::ENUM }, loc);
        if (!gname.compiled && !allow_uncompiled)
            error(diags::type_not_compiled(name), loc);
        return gname;
    }

    prog::global_index compiler::get_global_func_wrapper(prog::global_index func_index) {
        auto iter = func_wrappers.find(func_index);
        if (iter != func_wrappers.end())
            return iter->second;

        auto wrapper = make_func_wrapper(func_index);
        auto wrapper_index = prog.global_funcs.size();
        prog.global_funcs.push_back(into_ptr(wrapper));

        func_wrappers[func_index] = wrapper_index;
        return wrapper_index;
    }

    vector<cref<ast::expr>> compiler::order_args(
            vector<cref<ast::expr_marked>> asts,
            optional<function<size_t(string, location)>> arg_with_name,
            optional<size_t> expected_count,
            location loc) {
        auto count = asts.size();

        if (expected_count && count != *expected_count)
            error(diags::invalid_argument_count(count, *expected_count), loc);

        vector<bool> used(count, false);
        vector<const ast::expr*> value_ast_ptrs(count);

        for (const ast::expr_marked& arg_ast : asts) {
            size_t index = 0;
            const ast::expr* value_ast_ptr;

            switch (INDEX(arg_ast)) {
                case ast::expr_marked::EXPR: {
                    while (index < count && used[index])
                        index++;
                    value_ast_ptr = GET(arg_ast, EXPR).get();
                } break;

                case ast::expr_marked::EXPR_WITH_NAME: {
                    auto name = GET(arg_ast, EXPR_WITH_NAME).first;
                    if (arg_with_name)
                        index = (*arg_with_name)(name, arg_ast.loc);
                    else
                        error(diags::invalid_argument_marker(), arg_ast.loc);
                    value_ast_ptr = GET(arg_ast, EXPR_WITH_NAME).second.get();
                } break;

                case ast::expr_marked::EXPR_WITH_INDEX: {
                    index = GET(arg_ast, EXPR_WITH_INDEX).first;
                    value_ast_ptr = GET(arg_ast, EXPR_WITH_INDEX).second.get();
                } break;

                default:
                    UNREACHABLE;
            }

            if (index >= count)
                error(diags::invalid_argument_index(index, count), arg_ast.loc);
            if (used[index])
                error(diags::reused_argument_index(index), arg_ast.loc);

            value_ast_ptrs[index] = value_ast_ptr;
            used[index] = true;
        }

        for (size_t index = 0; index < count; index++) {
            if (!used[index])
                error(diags::missing_argument(index), loc);
        }

        vector<cref<ast::expr>> value_asts;

        for (auto value_ast_ptr : value_ast_ptrs)
            value_asts.push_back(*value_ast_ptr);

        return value_asts;
    }
}
