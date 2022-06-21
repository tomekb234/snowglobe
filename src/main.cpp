#include "input.hpp"
#include "diagcol.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include "compiler.hpp"
#include "program.hpp"
#include <fstream>
#include <vector>
#include <optional>
#include <iostream>
#include <unordered_map>

using std::string;
using std::cout;
using std::cerr;
using std::endl;
using std::ifstream;
using std::vector;
using std::optional;
using std::unordered_map;
using std::find;

optional<string> get_flag_value(const char** begin, const char** end, vector<string> flags) {
    for (auto& flag : flags) {
        const char** iter = std::find(begin, end, flag);
        if(iter != end && iter + 1 != end) {
            return { *(iter + 1) };
        }
    }
    return { };
}

bool flag_present(const char** begin, const char** end, const string& flag) {
    return std::find(begin, end, flag) != end;
}

unordered_map<string, size_t> sizes = {
    { "--help", 0 },
    { "-o", 1 },
    { "--output", 1 },
    { "--ir-code", 0 },
    { "--ir-bitcode", 0 },
    { "--check", 0 }
};

optional<string> input_name(const char** begin, const char** end) {
    const char** it = begin;
    while (it != end) {
        if (sizes.count(*it) == 0) {
            return { *it };
        }

        it += 1 + sizes[*it];
    }
    return { };
}

bool validate_flags(int argc, const char** argv) {
    bool seen_unmatched = false;
    const char** it = argv + 1;
    int arg_left = argc - 1;
    while(arg_left > 0) {
        arg_left--;

        if (sizes.count(*it) == 0) {
            if (seen_unmatched) {
                cerr << "Unrecognized option " << *it << endl;
                return false;
            }
            else {
                seen_unmatched = true;
            }
        }
        else {
            arg_left -= sizes[*it];
            if (arg_left < 0) {
                cerr << "Flag " << *it << " has no value" << endl;
                return false;
            }
            it += sizes[*it];
        }
        it++;
    }
    return true;
}

int main(int argc, const char** argv) {
    if (!validate_flags(argc, argv)) {
        return 1;
    }

    if (flag_present(argv + 1, argv + argc, "--help")) {
        cout << "Usage " << argv[0] << " file.snow [options]" << endl;
        cout << "Options:" << endl;
        cout << "  --help                     - Display this message" << endl;
        cout << "  -o <file>, --output <file> - Set the output to <file>" << endl;
        cout << "  --ir-code                  - Output LLVM IR in human-readable format" << endl;
        cout << "  --ir-bitcode               - Output LLVM IR in bitcode format" << endl;
        cout << "  --check                    - Only check for errors" << endl;
        return 0;
    }

    optional<string> maybe_name = input_name(argv + 1, argv + argc);
    if (!maybe_name) {
        cerr << "Input file name not provided" << endl;
        return 1;
    }

    string file_name(*maybe_name);
    ifstream file(file_name);

    if (!file.is_open()) {
        cerr << "Could not open file " << file_name << endl;
        return 1;
    }

    optional<string> output_file_name = get_flag_value(argv + 1, argv + argc, { "-o", "--output" });
    if (!output_file_name) {
        output_file_name = { "out" };
    }
    ifstream outfile(*output_file_name);
    
    auto ok = true;

    sg::lexer_input input(file, file_name);
    sg::diagnostic_collector diags;
    sg::ast::program ast;
    sg::prog::program prog;

    yy::parser parser(input, diags, ast);

    if (parser.parse() == 0) {
        sg::compiler compiler(prog, diags);

        if (compiler.compile(ast)) {
            // TODO
        } else
            ok = false;
    } else
        ok = false;

    diags.report_all(cerr, true, { { file_name, file } });

    return ok ? 0 : 1;
}
