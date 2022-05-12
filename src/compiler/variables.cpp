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

        auto[value, value_tp] = compile_constant_expr(*ast.value);

        prog::type tp;

        if (ast.tp) {
            tp = compile_type(**ast.tp);
            value = convert_constant(ast, value, value_tp, tp);
        } else
            tp = move(value_tp);

        return { name, into_ptr(tp), into_ptr(value) };
    }
}
