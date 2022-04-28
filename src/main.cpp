#include "ast.hpp"
#include "input.hpp"
#include "diagnostics.hpp"
#include "parser.hpp"
#include "program.hpp"
#include "compiler.hpp"
#include <string>
#include <iostream>
#include <fstream>

using std::string;
using std::cerr;
using std::endl;
using std::ifstream;

int main(int argc, const char** argv) {
    if (argc < 2)
        return 1;

    string file_name = argv[1];
    ifstream file(file_name);

    if (!file.is_open()) {
        cerr << "Could not open file " << file_name << endl;
        return 1;
    }

    bool ok = true;

    sg::lexer_input input(file, file_name);
    sg::diagnostic_collector diags;
    sg::ast::program ast;

    yy::parser parser(input, diags, ast);

    if (parser.parse() == 0) {
        sg::compiler compiler(diags);
        auto program = compiler.compile(ast);

        if (program) {
            // TODO
        } else
            ok = false;
    } else
        ok = false;

    diags.report_all(cerr, true);

    return ok ? 0 : 1;
}
