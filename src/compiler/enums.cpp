#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        return { ast.name, ast.copyable };
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
