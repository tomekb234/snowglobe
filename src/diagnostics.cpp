#include "diagnostics.hpp"
#include "lexer_diagnostics.hpp"
#include "parser_diagnostics.hpp"
#include "compiler_diagnostics.hpp"
#include <vector>
#include <ostream>
#include <istream>
#include <sstream>

namespace sg {
    using std::ostringstream;
    using std::vector;
    using std::endl;
    using std::string_view;

    const string LEVELS[] = { "Note", "Warning", "Error" };

    struct color_map {
        string levels[3];
        string location;
        string code_context;
        string code;
        string reset;
    };

    const color_map ACTIVE_COLORS = {
        .levels = { "\e[1;36m", "\e[1;33m", "\e[1;31m" },
        .location = "\e[1;37m",
        .code_context = "\e[2m",
        .code = "\e[1;35m",
        .reset = "\e[0m"
    };

    void diagnostic_collector::report_all(ostream& stream, bool enable_colors, optional<reference_wrapper<istream>> source_file) const {
        // Select color table
        const color_map colors = enable_colors ? ACTIVE_COLORS : color_map { };

        // Prepare line buffer
        vector<string> source_lines;
        if (source_file) {
            string source_line_buffer;
            while (!getline(source_file->get(), source_line_buffer).eof())
                source_lines.push_back(move(source_line_buffer));
        }

        for (auto& diag : diags) {
            // Header
            stream << colors.levels[diag->level] << LEVELS[diag->level] << colors.reset;

            // Location
            if (diag->loc) {
                stream << " at ";
                stream << colors.location;
                stream << *diag->loc->begin.file_name << ":";
                stream << diag->loc->begin.line << ":";
                stream << diag->loc->begin.column;
                stream << colors.reset;
            }

            stream << ":" << endl;

            // Code fragment
            if (diag->loc && source_file) {
                // TODO check if filename matches

                stream << colors.code_context;
                auto& [begin, end] = *diag->loc;

                for (size_t it = begin.line; it <= end.line; it++) {
                    auto line = string_view(source_lines[it - 1]);
                    auto fragment_begin = it==begin.line ? begin.column-1 : 0;
                    auto fragment_end = it==end.line ? end.column-1 : line.length();

                    stream << "\t| " << line.substr(0, fragment_begin);
                    stream << colors.reset << colors.code << line.substr(fragment_begin, fragment_end-fragment_begin);
                    stream << colors.reset << colors.code_context << line.substr(fragment_end) << endl;
                }

                stream << colors.reset;
            }

            // Indented message text
            ostringstream buf;
            diag->write(buf);
            string text = buf.str();
            size_t it = 0, nit;
            while ((nit = text.find_first_of('\n', it)) != string::npos) {
                stream << "\t" << text.substr(it, nit - it + 1);
                it = nit + 1;
            }

            // Extra line feed / stream flush
            stream << endl;
        }
    }

    namespace diags {
        void not_implemented::write(ostream& stream) const {
            stream << "Not implemented yet" << endl;
        }

        void int_token_overflow::write(ostream& stream) const {
            stream << "The number '" << number << "' does not fit in 64-bit integer type" << endl;
        }

        void float_token_overflow::write(ostream& stream) const {
            stream << "The number '" << number << "' does not fit in double-precision floating-point type" << endl;
        }

        void invalid_escape_sequence::write(ostream& stream) const {
            stream << "Invalid character escape sequence \\" << string(1, ch) << endl;
        }

        void multibyte_character_literal::write(ostream& stream) const {
            stream << "The character literal " << literal << " contains a multibyte character" << endl;
        }

        void parser_error::write(ostream& stream) const {
            stream << message << endl;
        }

        void syntax_error::write(ostream& stream) const {
            stream << "Syntax error" << endl;
            if (unexpected)
                stream << "Unexpected token: " << *unexpected << endl;
            if (!expected.empty()) {
                stream << "Expected tokens:";
                for (auto exp : expected)
                    stream << " " << exp;
                stream << endl;
            }
        }

        void name_used::write(ostream& stream) const {
            string kind_str;
            switch (kind) {
                case GLOBAL:
                    kind_str = "global";
                    break;
                case FIELD:
                    kind_str = "field";
                    break;
                case VARIANT:
                    kind_str = "variant";
                    break;
            }
            stream << "The " << kind_str << " name '" << name << "' is already used" << endl;
        }

        void name_not_declared::write(ostream& stream) const {
            stream << "The name '" << name << "' was not declared" << endl;
        }

        void name_not_compiled::write(ostream& stream) const {
            stream << "The name '" << name << "' was declared but is not yet compiled" << endl;
        }

        void invalid_expression::write(ostream& stream) const {
            stream << "Invalid expression" << endl;
        }

        void invalid_kind::write(ostream& stream) const {
            stream << "Invalid kind" << endl;
        }

        void invalid_type::write(ostream& stream) const {
            stream << "Invalid type" << endl;
        }

        void not_subtype::write(ostream& stream) const {
            stream << "Type '";
            prog::print_type(stream, program, subtype);
            stream << "' is not subtype of '";
            prog::print_type(stream, program, supertype);
            stream << "'" << endl;
        }

        void expression_not_constant::write(ostream& stream) const {
            stream << "Expression not constant" << endl;
        }

        void expression_not_left::write(ostream& stream) const {
            stream << "Expression is not left-expression" << endl;
        }

        void expression_not_right::write(ostream& stream) const {
            stream << "Expression is not right-expression" << endl;
        }

        void int_overflow::write(ostream& stream) const {
            stream << "The number '" << (negative ? "-" : "") << value << "' does not fit in " << (signed_type ? "signed" : "unsigned") << " " << bits << "-bit integer type" << endl;
        }

        void single_float_overflow::write(ostream& stream) const {
            stream << "The number '" << value << "' does not fit in single-precision floating-point type" << endl;
        }

        void no_common_supertype::write(ostream& stream) const {
            stream << "Types '";
            prog::print_type(stream, program, type1);
            stream << "' and '";
            prog::print_type(stream, program, type2);
            stream << "' have no common supertype" << endl;
        }

        void invalid_argument::write(ostream& stream) const {
            stream << "Invalid argument. Expected " << num_args << " arguments" << endl;
        }

        void reused_argument::write(ostream& stream) const {
            stream << "Reused argument with index " << index << endl;
        }

        void missing_argument::write(ostream& stream) const {
            stream << "Missing argument with index " << index << endl;
        }

        void invalid_enum_variant::write(ostream& stream) const {
            stream << "The enum '" << enum_name << "' does not have a variant named '" << variant_name << "'" << endl;
        }

        void invalid_struct_field::write(ostream& stream) const {
            stream << "The struct '" << struct_name << "' does not have a field named '" << field_name << "'" << endl;
        }

        void invalid_size_constant_type::write(ostream& stream) const {
            stream << "Size constant is not of unsigned integer type" << endl;
        }

        void type_not_copyable::write(ostream& stream) const {
            stream << "Type '";
            prog::print_type(stream, program, type);
            stream << "' is not copyable" << endl;
        }

        void global_func_copyable::write(ostream& stream) const {
            stream << "Global function marked as copyable" << endl;
        }

        void variable_without_type::write(ostream& stream) const {
            stream << "Declared variable doesn't have a type" << endl;
        }
    }
}
