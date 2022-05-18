#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::global_var compiler::compile_global_var(const ast::var_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::name_used(name, diags::name_used::GLOBAL), ast);

        auto[value, value_type] = compile_constant(*ast.value);

        prog::type type;

        if (ast.tp) {
            type = compile_type(**ast.tp);
            value = convert_constant(ast, move(value), value_type, type);
        } else
            type = move(value_type);

        return { { name }, into_ptr(type), into_ptr(value) };
    }

    prog::struct_type compiler::declare_struct_type(const ast::struct_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::name_used(name, diags::name_used::GLOBAL), ast);

        return { name, ast.copyable, true, { }, { } };
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type) {
        vector<prog::ptr<prog::struct_field>> fields;
        unordered_map<string, size_t> field_names;

        for (auto& ast_field : ast.fields) {
            if (field_names.count(ast_field->name))
                error(diags::name_used(ast_field->name, diags::name_used::FIELD), *ast_field);

            auto type = compile_type(*ast_field->tp);
            if (struct_type.copyable && !type_copyable(type))
                error(diags::type_not_copyable(), *ast_field->tp);
            if (!type_trivially_copyable(type))
                struct_type.trivially_copyable = false;

            auto field = prog::struct_field { ast_field->name, into_ptr(type) };
            field_names[field.name] = fields.size();
            fields.push_back(into_ptr(field));
        }

        struct_type.fields = move(fields);
        struct_type.field_names = move(field_names);
    }

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::name_used(name, diags::name_used::GLOBAL), ast);

        return { ast.name, ast.copyable, true, { }, { } };
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type) {
        vector<prog::ptr<prog::enum_variant>> variants;
        unordered_map<string, size_t> variant_names;

        for (auto& variant_ast : ast.variants) {
            if (variant_names.count(variant_ast->name))
                error(diags::name_used(variant_ast->name, diags::name_used::VARIANT), *variant_ast);

            vector<prog::ptr<prog::type>> types;
            for (const auto& type_ast : variant_ast->tps) {
                auto type = compile_type(*type_ast);
                if (enum_type.copyable && !type_copyable(type))
                    error(diags::type_not_copyable(), *type_ast);
                if (!type_trivially_copyable(type))
                    enum_type.trivially_copyable = false;
                types.push_back(into_ptr(type));
            }

            auto variant = prog::enum_variant { variant_ast->name, move(types) };
            variant_names[variant.name] = variants.size();
            variants.push_back(into_ptr(variant));
        }

        enum_type.variants = move(variants);
        enum_type.variant_names = move(variant_names);
    }
}
