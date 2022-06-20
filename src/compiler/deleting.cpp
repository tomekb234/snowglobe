#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void function_compiler::add_deletion(prog::reg_index value, const prog::type& type) {
        if (clr.type_trivial(type))
            return;

        // TODO
        (void)value;
        (void)type;
    }
}
