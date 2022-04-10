#include "diagnostic.hpp"
#include <sstream>

using std::string;

const string LEVEL_TEXTS[] = { "Error", "Warning" };

#ifdef __unix__
const string LEVEL_COLORS[] = { "\e[1;31m", "\e[1;33m" };
const string LOCATION_COLOR = "\e[1;37m";
const string RESET_COLOR = "\e[0m";
#else
const string LEVEL_COLORS[] = { "", "" };
const string LOCATION_COLOR = "";
const string RESET_COLOR = "";
#endif

namespace sg {
    using std::move;
    using std::endl;
    using std::ostringstream;

    void diagnostic_collector::add(unique_ptr<diagnostic> diag) {
        diags.push_back(move(diag));
    }

    void diagnostic_collector::add(unique_ptr<diagnostic> diag, yy::location location) {
        diag->location = { location };
        diags.push_back(move(diag));
    }

    void diagnostic_collector::report_all(ostream& stream, bool enable_colors) const {
        for (auto& diag : diags) {
            // header
            if (enable_colors)
                stream << LEVEL_COLORS[diag->level];
            stream << LEVEL_TEXTS[diag->level];
            if (enable_colors)
                stream << RESET_COLOR;

            // location
            if (diag->location) {
                stream << " at ";
                if (enable_colors)
                    stream << LOCATION_COLOR;
                stream << *diag->location;
                if (enable_colors)
                    stream << RESET_COLOR;
            }

            stream << ":" << endl;

            // indented message text
            ostringstream buf;
            diag->write(buf);
            string text = buf.str();
            size_t it = 0, nit;
            while ((nit = text.find_first_of('\n', it)) != string::npos) {
                stream << "\t" << text.substr(it, nit - it + 1);
                it = nit + 1;
            }

            // extre line feed / stream flush
            stream << endl;
        }
    }

    void parser_error::write(ostream& stream) const {
        stream << error << endl;
    }

    void syntax_error::write(ostream& stream) const {
        stream << "Syntax error" << endl;
        if (unexpected)
            stream << "Unexpected token: " << *unexpected << endl;
        if (!expected.empty()) {
            stream << (unexpected ? "Expected token:" : "Sample expected tokens:");
            for (auto exp : expected)
                stream << " " << exp;
            stream << endl;
        }
    }

    void invalid_escape_sequence_error::write(ostream& stream) const {
        stream << "Invalid character escape sequence \\" << string(1, ch) << endl;
    }

    void integer_overflow_error::write(ostream& stream) const {
        stream << "Integer " << number << " does not fit in 64 bits" << endl;
    }

    void float_overflow_error::write(ostream& stream) const {
        stream << "Number " << number << " is out of range of double-precision format" << endl;
    }
}
