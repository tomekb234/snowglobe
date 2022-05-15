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
}
