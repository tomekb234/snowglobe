#include "compiler/compiler.hpp"
#include "compiler/types.hpp"
#include "compiler/constants.hpp"
#include "compiler/functions.hpp"
#include "compiler/conversions.hpp"
#include "compiler/compiler_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void compiler::compile_builtins(const ast::program& ast, string builtin_name) {
        auto dummy_type = copy_type(prog::NEVER_TYPE);
        auto dummy_value = VARIANT(prog::constant, UNIT, monostate());
        auto dummy = prog::global_var { { builtin_name }, into_ptr(dummy_type), into_ptr(dummy_value) };

        auto dummy_index = consts.size();
        consts.push_back(move(dummy));
        global_names[builtin_name] = { global_name_kind::CONST, dummy_index, true };

        for (const ast::global_def& def_ast : as_cref_vector(ast.global_defs)) {
            switch (INDEX(def_ast)) {
                case ast::global_def::VAR_DEF: {
                    auto& var_ast = *GET(def_ast, VAR_DEF);
                    auto var = compile_global_var(var_ast);
                    auto name = *var.name;
                    auto index = prog.global_vars.size();
                    prog.global_vars.push_back(into_ptr(var));
                    global_names[name] = { global_name_kind::VAR, index, true };
                } break;

                case ast::global_def::FUNC_DEF: {
                    auto& func_ast = *GET(def_ast, FUNC_DEF);
                    auto func = declare_global_func(func_ast);
                    compile_global_func(func_ast, func);
                    auto name = *func.name;
                    auto index = prog.global_funcs.size();
                    prog.global_funcs.push_back(into_ptr(func));
                    global_names[name] = { global_name_kind::FUNC, index, true };
                } break;
            }
        }

        global_names.erase(builtin_name);
    }

    bool compiler::compile(const ast::program& ast) {
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
                error(diags::invalid_main_type(whole_file(ast.loc)));

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
            error(diags::name_not_found(name, loc));
        return iter->second;
    }

    const compiler::global_name& compiler::get_global_name(string name, vector<global_name_kind> expected_kinds, location loc) {
        auto& gname = get_global_name(name, loc);
        if (find(expected_kinds.begin(), expected_kinds.end(), gname.kind) == expected_kinds.end())
            error(diags::invalid_kind(name, gname.kind, expected_kinds, loc));
        return gname;
    }

    const compiler::global_name& compiler::get_global_type(string name, bool allow_uncompiled, location loc) {
        auto& gname = get_global_name(name, { global_name_kind::STRUCT, global_name_kind::ENUM }, loc);
        if (!gname.compiled && !allow_uncompiled)
            error(diags::type_not_compiled(name, loc));
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

    prog::global_var compiler::compile_global_var(const ast::var_def& ast) {
        auto name = ast.name;

        if (name == ast::IGNORED_PLACEHOLDER)
            error(diags::invalid_variable_name(name, ast.name_loc));

        if (global_names.count(name))
            error(diags::name_used(name, ast.name_loc));

        auto [value, value_type] = constant_compiler(*this).compile(*ast.value);

        prog::type type;

        if (ast.tp) {
            type = type_compiler(*this, false).compile(**ast.tp);
            value = compiler_utils(*this).convert_const(move(value), value_type, type, ast.loc);
        } else
            type = move(value_type);

        return { { name }, into_ptr(type), into_ptr(value) };
    }

    prog::global_func compiler::declare_global_func(const ast::func_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::name_used(name, ast.name_loc));

        vector<prog::func_param> params;
        unordered_map<string, prog::param_index> param_names;
        auto confined = false;

        for (const ast::func_param& param_ast : as_cref_vector(ast.params)) {
            param_names[param_ast.name] = params.size();

            auto type = type_compiler(*this, true).compile_local(*param_ast.tp);

            if (type.confined)
                confined = true;
            else if (confined)
                error(diags::invalid_parameter_order(param_ast.loc));

            params.push_back(prog::func_param { { param_ast.name }, into_ptr(type) });
        }

        prog::type return_type;
        if (ast.return_tp)
            return_type = type_compiler(*this, true).compile(**ast.return_tp);
        else
            return_type = VARIANT(prog::type, UNIT, monostate());

        return { { name }, into_ptr_vector(params), move(param_names), into_ptr(return_type), { }, { } };
    }

    prog::struct_type compiler::declare_struct_type(const ast::struct_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::name_used(name, ast.name_loc));

        return { name, ast.copyable, ast.copyable, { }, { }, 0 };
    }

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::name_used(name, ast.name_loc));

        return { ast.name, ast.copyable, ast.copyable, { }, { }, 0 };
    }

    void compiler::compile_global_func(const ast::func_def& ast, prog::global_func& func) {
        function_compiler(*this, func).compile(ast);
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& st) {
        vector<prog::struct_field> fields;
        unordered_map<string, size_t> field_names;

        for (const ast::struct_field& field_ast : as_cref_vector(ast.fields)) {
            if (field_names.count(field_ast.name))
                error(diags::field_name_used(field_ast.name, field_ast.loc));

            auto type = type_compiler(*this, false).compile(*field_ast.tp);
            if (st.copyable && !type_copyable(prog, type))
                error(diags::type_not_copyable(prog, move(type), field_ast.tp->loc));
            if (!type_trivial(prog, type))
                st.trivial = false;

            auto field = prog::struct_field { field_ast.name, into_ptr(type) };
            field_names[field.name] = fields.size();
            fields.push_back(move(field));
        }

        st.fields = into_ptr_vector(fields);
        st.field_names = move(field_names);
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& en) {
        vector<prog::enum_variant> variants;
        unordered_map<string, size_t> variant_names;

        for (const ast::enum_variant& variant_ast : as_cref_vector(ast.variants)) {
            if (variant_names.count(variant_ast.name))
                error(diags::variant_name_used(variant_ast.name, variant_ast.loc));

            vector<prog::type> types;
            for (const ast::type& type_ast : as_cref_vector(variant_ast.tps)) {
                auto type = type_compiler(*this, false).compile(type_ast);
                if (en.copyable && !type_copyable(prog, type))
                    error(diags::type_not_copyable(prog, move(type), type_ast.loc));
                if (!type_trivial(prog, type))
                    en.trivial = false;
                types.push_back(move(type));
            }

            auto variant = prog::enum_variant { variant_ast.name, into_ptr_vector(types) };
            variant_names[variant.name] = variants.size();
            variants.push_back(move(variant));
        }

        en.variants = into_ptr_vector(variants);
        en.variant_names = move(variant_names);
    }

    prog::global_func compiler::make_func_wrapper(prog::global_index func_index) {
        auto& func = *prog.global_funcs[func_index];

        vector<prog::func_param> params;
        params.push_back({ { }, make_ptr(copy_type_local(prog::UNIT_PTR_TYPE_LOCAL)) });
        for (const prog::func_param& param : as_cref_vector(func.params))
            params.push_back({ { }, make_ptr(copy_type_local(*param.tp)) });

        auto return_type = copy_type(*func.return_tp);

        auto wrapper = prog::global_func { { }, into_ptr_vector(params), { }, into_ptr(return_type), { }, { } };
        function_compiler(*this, wrapper).make_func_wrapper(func_index);
        return wrapper;
    }

    prog::global_func compiler::make_struct_destructor(prog::global_index struct_index) {
        vector<prog::func_param> params;
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, STRUCT, struct_index)), false };
        params.push_back({ { }, into_ptr(type) });

        auto return_type = VARIANT(prog::type, UNIT, monostate());

        auto destructor = prog::global_func { { }, into_ptr_vector(params), { }, into_ptr(return_type), { }, { } };
        function_compiler(*this, destructor).make_struct_destructor(struct_index);
        return destructor;
    }

    prog::global_func compiler::make_enum_destructor(prog::global_index enum_index) {
        vector<prog::func_param> params;
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM, enum_index)), false };
        params.push_back({ { }, into_ptr(type) });

        auto return_type = VARIANT(prog::type, UNIT, monostate());

        auto destructor = prog::global_func { { }, into_ptr_vector(params), { }, into_ptr(return_type), { }, { } };
        function_compiler(*this, destructor).make_enum_destructor(enum_index);
        return destructor;
    }

    prog::global_func compiler::make_cleanup_func() {
        auto return_type = VARIANT(prog::type, UNIT, monostate());
        auto func = prog::global_func { { }, { }, { }, into_ptr(return_type), { }, { } };
        function_compiler(*this, func).make_cleanup_func();
        return func;
    }
}
