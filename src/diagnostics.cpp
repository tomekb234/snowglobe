#include "diagnostics.hpp"
#include <utility>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>

namespace sg {
    using std::move;
    using std::make_unique;
    using std::ostream;
    using std::ostringstream;
    using std::endl;
    using std::string;

    const string LEVEL_TEXTS[] = { "Note", "Warning", "Error" };

    #ifdef __unix__
    const string LEVEL_COLORS[] = { "\e[1;36m", "\e[1;33m", "\e[1;31m" };
    const string LOCATION_COLOR = "\e[1;37m";
    const string RESET_COLOR = "\e[0m";
    #else
    const string LEVEL_COLORS[] = { "", "" };
    const string LOCATION_COLOR = "";
    const string RESET_COLOR = "";
    #endif

    void diagnostic_collector::report_all(ostream& stream, bool enable_colors) const {
        for (auto& diag : diags) {
            // Header
            if (enable_colors)
                stream << LEVEL_COLORS[diag->level];
            stream << LEVEL_TEXTS[diag->level];
            if (enable_colors)
                stream << RESET_COLOR;

            // Location
            if (diag->loc) {
                stream << " at ";
                if (enable_colors)
                    stream << LOCATION_COLOR;
                stream << diag->loc->file_name << ":";
                stream << diag->loc->line << ":";
                stream << diag->loc->column;
                if (enable_colors)
                    stream << RESET_COLOR;
            }

            stream << ":" << endl;

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
        void not_implemented_error::write(ostream& stream) const {
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

        void invalid_escape_sequence_error::write(ostream& stream) const {
            stream << "Invalid character escape sequence \\" << string(1, ch) << endl;
        }

        void integer_overflow_error::write(ostream& stream) const {
            stream << "The integer '" << number << "' does not fit in 64 bits" << endl;
        }

        void float_overflow_error::write(ostream& stream) const {
            stream << "The number '" << number << "' is out of range of double-precision format" << endl;
        }

        void global_name_used_error::write(ostream& stream) const {
            stream << "The global name '" << name << " is already used" << endl;
        }
    }
}
