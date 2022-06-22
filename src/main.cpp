#include "input.hpp"
#include "diagcol.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include "compiler.hpp"
#include "program.hpp"
#include <fstream>
#include <sstream>
#include <vector>
#include <optional>
#include <iostream>
#include <unordered_map>

using std::string;
using std::cout;
using std::cerr;
using std::endl;
using std::istringstream;
using std::ifstream;
using std::ofstream;
using std::vector;
using std::optional;
using std::unordered_map;
using std::find;

static void print_help(const char* name) {
    cout << "Usage: " << name << " file.snow [options]" << endl;
    cout << "Options:" << endl;
    cout << "  -h, --help                 - Display this message" << endl;
    cout << "  -o <file>, --output <file> - Set the output to <file>" << endl;
    cout << "  -k, --check                - Only check for errors" << endl;
    cout << "  --ir-code                  - Output LLVM IR in human-readable format" << endl;
    cout << "  --ir-bitcode               - Output LLVM IR in bitcode format" << endl;
}

const unordered_map<string, size_t> OPTIONS = {
    { "-h", 0 }, { "--help", 0 },
    { "-o", 1 }, { "--output", 1 },
    { "-k", 0 }, { "--check", 0 },
    { "--ir-code", 0 },
    { "--ir-bitcode", 0 },
};

static bool validate_args(int argc, const char** argv);
static bool option_present(int argc, const char** argv, vector<string> options);
static optional<string> get_option_value(int argc, const char** argv, vector<string> options);
static optional<string> get_arg(int argc, const char** argv);

const string BUILTIN_NAME = "builtin";

const string BUILTINS = R"CODE(

var argv: $[$[u8]] = builtin;

func read(count: u64) -> @[u8] { builtin }
func read_word() -> @[u8] { builtin }
func read_line() -> @[u8] { builtin }
func read_int() -> i64 { builtin }
func read_uint() -> u64 { builtin }
func read_float() -> f64 { builtin }

func print(output: &[u8]) { builtin }
func print_word(output: &[u8]) { builtin }
func print_line(output: &[u8]) { builtin }
func print_int(output: i64) { builtin }
func print_uint(output: u64) { builtin }
func print_float(output: f64) { builtin }

func exit(status: i32) -> never { builtin }
func error(message: &[u8]) -> never { builtin }
func unreachable() -> never { builtin }

)CODE";

int main(int argc, const char** argv) {
    if (!validate_args(argc, argv))
        return 1;

    if (option_present(argc, argv, { "-h", "--help" })) {
        print_help(argv[0]);
        return 0;
    }

    auto opt_file_name = get_arg(argc, argv);
    if (!opt_file_name) {
        cerr << "No input file name" << endl;
        return 1;
    }

    auto file_name = *opt_file_name;
    ifstream file(file_name);

    if (!file.is_open()) {
        cerr << "Could not open file '" << file_name << "'" << endl;
        return 1;
    }

    auto ok = true;

    sg::prog::program prog;
    sg::diagnostic_collector diags;

    istringstream builtins(BUILTINS);

    sg::lexer_input builtins_input(builtins, nullptr);
    sg::lexer_input source_input(file, &file_name);

    sg::ast::program builtins_ast;
    sg::ast::program source_ast;

    yy::parser(builtins_input, diags, builtins_ast).parse();

    if (yy::parser(source_input, diags, source_ast).parse() == 0) {
        sg::compiler compiler(prog, diags);

        compiler.compile_builtins(builtins_ast, BUILTIN_NAME);

        if (compiler.compile(source_ast)) {
            if (!option_present(argc, argv, { "-k", "--check" })) {
                auto opt_output_name = get_option_value(argc, argv, { "-o", "--output" });
                auto output_name = opt_output_name ? *opt_output_name : "a.out";
                ofstream output(output_name);

                if (!output.is_open()) {
                    cerr << "Could not open file '" << output_name << "' for writing" << endl;
                    return 1;
                }
            }
        } else
            ok = false;
    } else
        ok = false;

    diags.report_all(cerr, true, { { file_name, file } });

    return ok ? 0 : 1;
}

static bool validate_args(int argc, const char** argv) {
    auto seen_unmatched = false;
    auto iter = argv + 1;
    auto args_left = argc - 1;

    while (args_left > 0) {
        args_left--;

        if (OPTIONS.count(*iter) == 0) {
            if (seen_unmatched) {
                cerr << "Unknown option '" << *iter << "'" << endl;
                return false;
            } else
                seen_unmatched = true;
        } else {
            args_left -= OPTIONS.at(*iter);
            if (args_left < 0) {
                cerr << "The option '" << *iter << "' requires a value" << endl;
                return false;
            }
            iter += OPTIONS.at(*iter);
        }

        iter++;
    }

    return true;
}

static bool option_present(int argc, const char** argv, vector<string> options) {
    auto begin = argv + 1;
    auto end = argv + argc;

    for (auto& option : options) {
        if (find(begin, end, option) != end)
            return true;
    }
    return false;
}

static optional<string> get_option_value(int argc, const char** argv, vector<string> options) {
    auto begin = argv + 1;
    auto end = argv + argc;

    for (auto& option : options) {
        const char** iter = find(begin, end, option);
        if (iter != end && iter + 1 != end)
            return { *(iter + 1) };
    }
    return { };
}

static optional<string> get_arg(int argc, const char** argv) {
    auto iter = argv + 1;
    auto end = argv + argc;

    while (iter != end) {
        if (OPTIONS.count(*iter) == 0)
            return { *iter };
        iter += 1 + OPTIONS.at(*iter);
    }
    return { };
}
