#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        return{ ast.name, ast.copyable };
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type) {
        vector<prog::ptr<prog::enum_variant>> variants;
        
        for(auto& ast_variant : ast.variants) {
            vector<prog::ptr<prog::type>> tps;

            for(const auto& ast_type : ast_variant->tps) {
                auto&& tp = compile_type(*ast_type);
                tps.push_back(into_ptr(tp));
            }

            prog::enum_variant variant = { ast_variant->name, move(tps) };
            variants.push_back(into_ptr(variant));
        }

        enum_type.variants = move(variants);
    }
}
