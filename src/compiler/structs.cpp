#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::struct_type compiler::declare_struct_type(const ast::struct_def& ast) {
        error(diags::not_implemented(), ast); // TODO
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type) {
        error(diags::not_implemented(), ast); // TODO
    }
}
