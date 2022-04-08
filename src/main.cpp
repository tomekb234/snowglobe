#include "input.hpp"
#include "parser.hpp"
#include "diagnostic.hpp"
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main(int argc, const char** argv) {
    if (argc < 2)
        return 1;

    ifstream file(argv[1]);
    string fname = argv[1];
    sg::lexer_input input(file, &fname);

	sg::diagnostic_collector diag(cerr, true);

    yy::parser parser(input, diag);
    parser.parse();

    return 0;
}
