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
            error(diags::global_name_used(name), ast);

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
            error(diags::global_name_used(name), ast);

        return { name, ast.copyable, { } };
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type) {
        vector<prog::ptr<prog::struct_field>> fields;

        for (auto& ast_field : ast.fields) {
            auto type = compile_type(*ast_field->tp);
            auto field = prog::struct_field { ast_field->name, into_ptr(type) };
            fields.push_back(into_ptr(field));
        }

        struct_type.fields = move(fields);
    }

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used(name), ast);

        return { ast.name, ast.copyable, { } };
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type) {
        vector<prog::ptr<prog::enum_variant>> variants;

        for (auto& variant_ast : ast.variants) {
            vector<prog::ptr<prog::type>> types;
            for (const auto& type_ast : variant_ast->tps) {
                auto type = compile_type(*type_ast);
                types.push_back(into_ptr(type));
            }

            auto variant = prog::enum_variant { variant_ast->name, move(types) };
            variants.push_back(into_ptr(variant));
        }

        enum_type.variants = move(variants);
    }
}
