#include "input.hpp"
#include "diagcol.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include "compiler.hpp"
#include "program.hpp"
#include "codegen.hpp"
#include <fstream>

using std::string;
using std::cerr;
using std::endl;
using std::ifstream;

int main(int argc, const char** argv) {
    if (argc < 2)
        return 1;

    string file_name(argv[1]);
    ifstream file(file_name);

    if (!file.is_open()) {
        cerr << "Could not open file " << file_name << endl;
        return 1;
    }

    auto ok = true;

    sg::lexer_input input(file, file_name);
    sg::diagnostic_collector diags;
    sg::ast::program ast;
    sg::prog::program prog;

    yy::parser parser(input, diags, ast);

    if (parser.parse() == 0) {
        sg::compiler compiler(prog, diags);

        if (compiler.compile(ast)) {
            sg::code_generator codegen(prog, diags, "test_module");

            if (codegen.generate())
                codegen.write(std::cout);
            else
                ok = false;
        } else
            ok = false;
    } else
        ok = false;

    file.clear();
    file.seekg(0);
    diags.report_all(cerr, true, file);

    return ok ? 0 : 1;
}
