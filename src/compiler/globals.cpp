#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    using std::monostate;

    prog::global_var compiler::compile_global_var(const ast::var_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used(name), ast.name_loc);

        auto[value, value_type] = compile_const(*ast.value);

        prog::type type;

        if (ast.tp) {
            type = compile_type(**ast.tp);
            value = convert_const(ast, move(value), value_type, type);
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

        vector<prog::ptr<prog::func_param>> params;
        unordered_map<string, prog::param_index> param_names;

        for (auto& param_ast : ast.params) {
            param_names[param_ast->name] = params.size();
            params.push_back(make_ptr(prog::func_param { param_ast->name, make_ptr(compile_type_local(*param_ast->tp, true)) }));
        }

        prog::ptr<prog::type> return_type;
        if (ast.return_tp)
            return_type = make_ptr(compile_type(**ast.return_tp, true));
        else
            return_type = make_ptr(VARIANT(prog::type, UNIT, monostate()));

        return { name, move(params), move(param_names), move(return_type), { }, { } };
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
        vector<prog::ptr<prog::struct_field>> fields;
        unordered_map<string, size_t> field_names;

        for (auto& field_ast : ast.fields) {
            if (field_names.count(field_ast->name))
                error(diags::field_name_used(field_ast->name), *field_ast);

            auto type = compile_type(*field_ast->tp);
            if (st.copyable && !type_copyable(type))
                error(diags::type_not_copyable(prog, copy_type(type)), *field_ast->tp);
            if (!type_trivially_copyable(type))
                st.trivially_copyable = false;

            auto field = prog::struct_field { field_ast->name, into_ptr(type) };
            field_names[field.name] = fields.size();
            fields.push_back(into_ptr(field));
        }

        st.fields = move(fields);
        st.field_names = move(field_names);
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& en) {
        vector<prog::ptr<prog::enum_variant>> variants;
        unordered_map<string, size_t> variant_names;

        for (auto& variant_ast : ast.variants) {
            if (variant_names.count(variant_ast->name))
                error(diags::variant_name_used(variant_ast->name), *variant_ast);

            vector<prog::ptr<prog::type>> types;
            for (const auto& type_ast : variant_ast->tps) {
                auto type = compile_type(*type_ast);
                if (en.copyable && !type_copyable(type))
                    error(diags::type_not_copyable(prog, copy_type(type)), *type_ast);
                if (!type_trivially_copyable(type))
                    en.trivially_copyable = false;
                types.push_back(into_ptr(type));
            }

            auto variant = prog::enum_variant { variant_ast->name, move(types) };
            variant_names[variant.name] = variants.size();
            variants.push_back(into_ptr(variant));
        }

        en.variants = move(variants);
        en.variant_names = move(variant_names);
    }
}
