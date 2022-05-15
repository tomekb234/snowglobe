#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::struct_type compiler::declare_struct_type(const ast::struct_def& ast) {
        return { ast.name, ast.copyable };
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type) {
        vector<prog::ptr<prog::struct_field>> fields;
        
        for (auto& ast_field : ast.fields) {
            auto&& tp = compile_type(*ast_field->tp);
            
            prog::struct_field field = { ast_field->name, into_ptr(tp) };
            fields.push_back(into_ptr(field));
        }

        struct_type.fields = move(fields);
    }
}
