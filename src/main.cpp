#include "input.hpp"
#include "diagcol.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include "compiler/compiler.hpp"
#include "program.hpp"
#include "codegen.hpp"
#include <fstream>
#include <sstream>
#include <vector>
#include <optional>
#include <iostream>
#include <unordered_map>
#include <algorithm>
#include <filesystem>

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
using std::filesystem::permissions;
using std::filesystem::perms;
using std::filesystem::perm_options;

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

const string BUILTIN_NAME = "builtin";

const string BUILTINS = R"CODE(
    /*var argv: $[$[u8]] = builtin;

    func read(count: u64) -> @[u8] { builtin }
    func read_word() -> @[u8] { builtin }
    func read_line() -> @[u8] { builtin }
    func read_int() -> i64 { builtin }
    func read_uint() -> u64 { builtin }
    func read_float() -> f64 { builtin }*/

    func print(output: &[u8]) { builtin }
    func print_word(output: &[u8]) { builtin }
    func print_line(output: &[u8]) { builtin }
    func print_int(output: i64) { builtin }
    func print_uint(output: u64) { builtin }
    func print_float(output: f64) { builtin }

    func exit(status: i32) -> never { builtin }
    /*func error(message: &[u8]) -> never { builtin }
    func unreachable() -> never { builtin }*/
)CODE";

static bool validate_args(int argc, const char** argv);
static bool option_present(int argc, const char** argv, vector<string> options);
static optional<string> get_option_value(int argc, const char** argv, vector<string> options);
static optional<string> get_arg(int argc, const char** argv);
static string get_executable_name(string input_name);
static string get_ir_code_name(string input_name);
static string get_ir_bitcode_name(string input_name);

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

    sg::prog::program prog;
    sg::diagnostic_collector diags;
    sg::compiler compiler(prog, diags);

    auto compile_builtins = [&] () {
        istringstream code(BUILTINS);
        sg::lexer_input input(code, nullptr);
        sg::ast::program ast;
        yy::parser(input, diags, ast).parse();
        compiler.compile_builtins(ast, BUILTIN_NAME);
    };

    auto compile = [&] () -> bool {
        sg::lexer_input input(file, &file_name);
        sg::ast::program ast;

        if (yy::parser(input, diags, ast).parse() != 0)
            return false;

        if (!compiler.compile(ast))
            return false;

        if (option_present(argc, argv, { "-k", "--check" }))
            return true;

        auto gen_code = option_present(argc, argv, { "--ir-code" });
        auto gen_bitcode = option_present(argc, argv, { "--ir-bitcode" });

        if (gen_code && gen_bitcode) {
            cerr << "The options '--ir-code' and '--ir-bitcode' are mutually exclusive" << endl;
            return false;
        }

        auto opt_output_name = get_option_value(argc, argv, { "-o", "--output" });
        string output_name;

        if (opt_output_name)
            output_name = *opt_output_name;
        else if (gen_code)
            output_name = get_ir_code_name(file_name);
        else if (gen_bitcode)
            output_name = get_ir_bitcode_name(file_name);
        else
            output_name = get_executable_name(file_name);

        ofstream output(output_name);

        if (!output.is_open()) {
            cerr << "Could not open file '" << output_name << "' for writing" << endl;
            return false;
        }

        sg::code_generator codegen(prog, diags, file_name);

        try {
        if (gen_code)
            codegen.generate_code(output);
        else if (gen_bitcode)
            codegen.generate_bitcode(output);
        else {
            codegen.generate_executable(output);
            auto exec = perms::owner_exec | perms::group_exec | perms::others_exec;
            permissions(output_name, exec, perm_options::add);
        }
        } catch (...) {
            return false;
        }

        return true;
    };

    compile_builtins();
    auto ok = compile();

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

static void remove_directory(string& name) {
    size_t slash = name.find_last_of('/');
    if (slash != string::npos)
        name = name.substr(slash + 1);
}

static bool remove_extension(string& name) {
    size_t dot = name.find_last_of('.');
    if (dot == string::npos)
        return false;
    else {
        name = name.substr(0, dot);
        return true;
    }
}

static string get_executable_name(string name) {
    string result = name;
    remove_directory(result);

    if (!remove_extension(result))
        result += ".out";

    return result;
}

static string get_ir_code_name(string name) {
    string result = name;
    remove_directory(result);
    remove_extension(result);

    return result + ".ll";
}

static string get_ir_bitcode_name(string name) {
    string result = name;
    remove_directory(result);
    remove_extension(result);

    return result + ".bc";
}
