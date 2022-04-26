#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include <utility>
#include <unordered_set>

namespace sg {
    using std::pair;
    using std::unordered_set;

    class compiler {
        diagnostic_collector& diags;
        prog::program program;
        unordered_set<string> global_names;

        prog::global_var compile_global_var(const ast::var_def& input);
        pair<prog::constant, prog::type> compile_constant(const ast::expr& input);
        prog::type compile_type(const ast::type& input);
        prog::primitive_type compile_primitive_type(const ast::primitive_type& input);

        public:

        compiler(diagnostic_collector& diags) : diags(diags) { }

        prog::program compile(const ast::program& input);
    };
}
