#include "input.hpp"
#include "parser.hpp"
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

void yy::parser::error(const parser::location_type& loc, const string& err) {
    cerr << loc << ":" << err << endl;
}

int main(int argc, const char** argv) {
    if (argc < 2)
        return 1;

    ifstream file(argv[1]);
    string fname = argv[1];
    sg::lexer_input input(file, &fname);

    yy::parser parser(input);
    parser.parse();

    return 0;
}
