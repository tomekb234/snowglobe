#include "ast.hpp"
#include "input.hpp"
#include "diagnostic.hpp"
#include "parser.hpp"
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main(int argc, const char** argv) {
    if (argc < 2)
        return 1;

    string file_name = argv[1];
    ifstream file(file_name);

    if (!file.is_open()) {
        cerr << "Could not open file " << file_name << endl;
        return 1;
    }

    sg::ast::program ast;
    sg::lexer_input input(file, file_name);
    sg::diagnostic_collector diags;

    yy::parser parser(ast, input, diags);
    parser.parse();

    diags.report_all(cerr, true);

    return 0;
}
