#include "compiler.hpp"
#include "compiler_diagnostics.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::global_func compiler::declare_global_func(const ast::func_def& ast) {
        error(diags::not_implemented(), ast); // TODO
    }

    void compiler::compile_global_func(const ast::func_def& ast, prog::global_func& global_func) {
        error(diags::not_implemented(), ast); // TODO
    }
}
