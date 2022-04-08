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

    sg::lexer_input input(file, file_name);
    sg::diagnostic_collector diag(cerr, true);

    yy::parser parser(input, diag);
    parser.parse();

    return 0;
}
