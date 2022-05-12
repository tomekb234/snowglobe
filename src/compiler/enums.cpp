#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        error(diags::not_implemented(), ast); // TODO
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type) {
        error(diags::not_implemented(), ast); // TODO
    }
}
