#include "diagnostics.hpp"
#include <utility>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>

namespace sg {
    using std::ostream;
    using std::ostringstream;
    using std::endl;
    using std::string;
    using std::string_view;

    const string LEVEL_TEXTS[] = { "Note", "Warning", "Error" };

    struct COLORS {
        string LEVELS[3];
        string LOCATION;
        string CODE_OUTSIDE, CODE_INSIDE;
        string RESET;
    };
    const COLORS INACTIVE_COLORS = { { "", "", "" }, "", "", "", "" };
    const COLORS ACTIVE_COLORS = {
        .LEVELS = { "\e[1;36m", "\e[1;33m", "\e[1;31m" },
        .LOCATION = "\e[1;37m",
        .CODE_OUTSIDE = "\e[2m",
        .CODE_INSIDE = "\e[1;35m",
        .RESET = "\e[0m"
    };

    void diagnostic_collector::report_all(ostream& stream, bool enable_colors, optional<reference_wrapper<istream>> source_file) const {
        // Select color table
        const COLORS colors = enable_colors ? ACTIVE_COLORS : INACTIVE_COLORS;

        // Prepare line buffer
        vector<string> source_lines;
        if (source_file) {
            string source_line_buffer;
            while (!getline(source_file->get(), source_line_buffer).eof())
                source_lines.push_back(move(source_line_buffer));
        }

        for (auto& diag : diags) {
            // Header
            stream << colors.LEVELS[diag->level] << LEVEL_TEXTS[diag->level] << colors.RESET;

            // Location
            if (diag->loc) {
                stream << " at ";
                stream << colors.LOCATION;
                stream << *diag->loc->begin.file_name << ":";
                stream << diag->loc->begin.line << ":";
                stream << diag->loc->begin.column;
                stream << colors.RESET;
            }

            stream << ":" << endl;

            // Code fragment
            if (diag->loc && source_file) {
                stream << colors.CODE_OUTSIDE;
                auto& [begin, end] = *diag->loc;

                for (size_t it = begin.line; it <= end.line; it++) {
                    string_view line = source_lines[it - 1];
                    size_t fragment_begin = it==begin.line ? begin.column-1 : 0;
                    size_t fragment_end = it==end.line ? end.column-1 : line.length();

                    stream << "\t| " << line.substr(0, fragment_begin);
                    stream << colors.RESET << colors.CODE_INSIDE << line.substr(fragment_begin, fragment_end-fragment_begin);
                    stream << colors.RESET << colors.CODE_OUTSIDE << line.substr(fragment_end) << endl;
                }

                stream << colors.RESET;
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

        void invalid_escape_sequence::write(ostream& stream) const {
            stream << "Invalid character escape sequence \\" << string(1, ch) << endl;
        }

        void multibyte_character_literal::write(ostream& stream) const {
            stream << "Character literal " << literal << " contains multibyte character" << endl;
        }

        void integer_overflow::write(ostream& stream) const {
            stream << "The " << (signed_type ? "signed" : "unsigned") << " integer '" << number << "' does not fit in " << bits << " bits" << endl;
        }

        void float_overflow::write(ostream& stream) const {
            stream << "The number '" << number << "' is out of range of " << (double_precision ? "double" : "single") << "-precision format" << endl;
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
            stream << "No subtype relation" << endl;
        }

        void expression_not_constant::write(ostream& stream) const {
            stream << "Expression not constant" << endl;
        }

        void no_common_supertype::write(ostream& stream) const {
            stream << "No common supertype" << endl;
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
            stream << "Invalid enum variant" << endl;
        }

        void invalid_struct_field::write(ostream& stream) const {
            stream << "Invalid struct field" << endl;
        }

        void invalid_size_constant_type::write(ostream& stream) const {
            stream << "Size constant is not of unsigned integer type" << endl;
        }

        void type_not_copyable::write(ostream& stream) const {
            stream << "Type is not copyable" << endl;
        }
    }
}
