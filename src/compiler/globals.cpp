#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::global_var compiler::compile_global_var(const ast::var_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used(name), ast.name_loc);

        auto [value, value_type] = compile_const(*ast.value);

        prog::type type;

        if (ast.tp) {
            type = compile_type(**ast.tp, false);
            value = convert_const(move(value), value_type, type, ast.loc);
        } else
            type = move(value_type);

        return { { name }, into_ptr(type), into_ptr(value) };
    }

    prog::global_func compiler::declare_global_func(const ast::func_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used(name), ast.name_loc);

        if (ast.copying)
            error(diags::global_function_copyable(), ast.name_loc);

        vector<prog::func_param> params;
        unordered_map<string, prog::param_index> param_names;

        for (auto& param_ast_ptr : ast.params) {
            param_names[param_ast_ptr->name] = params.size();
            params.push_back(prog::func_param { param_ast_ptr->name, make_ptr(compile_type_local(*param_ast_ptr->tp, true)) });
        }

        prog::type return_type;
        if (ast.return_tp)
            return_type = compile_type(**ast.return_tp, true);
        else
            return_type = VARIANT(prog::type, UNIT, monostate());

        return { name, into_ptr_vector(params), move(param_names), into_ptr(return_type), { }, { } };
    }

    prog::struct_type compiler::declare_struct_type(const ast::struct_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used(name), ast.name_loc);

        return { name, ast.copyable, ast.copyable, { }, { } };
    }

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used(name), ast.name_loc);

        return { ast.name, ast.copyable, ast.copyable, { }, { } };
    }

    void compiler::compile_global_func(const ast::func_def& ast, prog::global_func& func) {
        function_compiler(*this, func).compile(ast);
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& st) {
        vector<prog::struct_field> fields;
        unordered_map<string, size_t> field_names;

        for (auto& field_ast_ptr : ast.fields) {
            if (field_names.count(field_ast_ptr->name))
                error(diags::field_name_used(field_ast_ptr->name), field_ast_ptr->loc);

            auto type = compile_type(*field_ast_ptr->tp, false);
            if (st.copyable && !type_copyable(type))
                error(diags::type_not_copyable(prog, copy_type(type)), field_ast_ptr->tp->loc);
            if (!type_trivially_copyable(type))
                st.trivially_copyable = false;

            auto field = prog::struct_field { field_ast_ptr->name, into_ptr(type) };
            field_names[field.name] = fields.size();
            fields.push_back(move(field));
        }

        st.fields = into_ptr_vector(fields);
        st.field_names = move(field_names);
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& en) {
        vector<prog::enum_variant> variants;
        unordered_map<string, size_t> variant_names;

        for (auto& variant_ast_ptr : ast.variants) {
            if (variant_names.count(variant_ast_ptr->name))
                error(diags::variant_name_used(variant_ast_ptr->name), variant_ast_ptr->loc);

            vector<prog::type> types;
            for (auto& type_ast_ptr : variant_ast_ptr->tps) {
                auto type = compile_type(*type_ast_ptr, false);
                if (en.copyable && !type_copyable(type))
                    error(diags::type_not_copyable(prog, copy_type(type)), type_ast_ptr->loc);
                if (!type_trivially_copyable(type))
                    en.trivially_copyable = false;
                types.push_back(move(type));
            }

            auto variant = prog::enum_variant { variant_ast_ptr->name, into_ptr_vector(types) };
            variant_names[variant.name] = variants.size();
            variants.push_back(move(variant));
        }

        en.variants = into_ptr_vector(variants);
        en.variant_names = move(variant_names);
    }
}
