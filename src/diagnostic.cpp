#include "diagnostic.hpp"

using std::string;
using std::endl;

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
    diagnostic_collector::diagnostic_collector(ostream& stream, bool enable_colors) : stream(stream), enable_colors(enable_colors) { }

    void diagnostic_collector::report(size_t level, yy::location location, const string& text) {
        // header
        if (enable_colors)
            stream << LEVEL_COLORS[level];
        stream << LEVEL_TEXTS[level];
        if (enable_colors)
            stream << RESET_COLOR;

        // location
        stream << " at ";
        if (enable_colors)
            stream << LOCATION_COLOR;
        stream << location;
        if (enable_colors)
            stream << RESET_COLOR;
        stream << ":\n";

        // indented message text
        size_t it = 0, nit;
        while ((nit = text.find_first_of('\n', it)) != string::npos) {
            stream << "\t" << text.substr(it, nit - it + 1);
            it = nit + 1;
        }

        // extre line feed / stream flush
        stream << endl;
    }

    namespace msg {
        string syntax_error(optional<string> unexpected, vector<string> expected) {
            string text = "Syntax error\n";
            if (unexpected)
                text += "Unexpected token: " + *unexpected + "\n";
            if (!expected.empty()) {
                text += (unexpected ? "Expected token:" : "Sample expected tokens:");
                for (auto exp : expected)
                    text += " " + exp;
                text += "\n";
            }
            return text;
        }

        string invalid_escape_sequence(char ch) {
            return "Invalid character escape sequence \\" + string(1, ch) + "\n";
        }

        string integer_overflow(string num) {
            return "Integer " + num + " does not fit in 64 bits\n";
        }
    }
}
