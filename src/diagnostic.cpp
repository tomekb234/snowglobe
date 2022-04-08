#include "diagnostic.hpp"

using std::endl;
using std::string;
using std::vector;
using std::optional;

const string severity_texts[] = {"Error", "Warning"};

namespace ANSI_codes {
	const string severity_styles[] = {"\e[1;31m", "\e[1;33m"};
	const string location_style = "\e[1;37m";
	const string reset_style = "\e[0m";

#ifdef __unix__
	const bool supported = true;
#else
	const bool supported = false;
#endif
}

namespace sg {
	diagnostic_collector::diagnostic_collector(std::ostream &stream, bool enable_colors) : stream(stream), enable_colors(enable_colors && ANSI_codes::supported) {}

	void diagnostic_collector::report (Severity severity, yy::location location, string text) {
		// header
		if (enable_colors)
			stream << ANSI_codes::severity_styles[severity];
		stream << severity_texts[severity];
		if (enable_colors)
			stream << ANSI_codes::reset_style;
		
		stream << " at ";
		if (enable_colors)
			stream << ANSI_codes::location_style;
		stream << location;
		if (enable_colors)
			stream << ANSI_codes::reset_style;
		stream << ":\n";

		// indented message text
		size_t it = 0, nit;
		while ((nit = text.find_first_of('\n', it)) != std::string::npos) {
			stream << "\t" << text.substr(it, nit - it + 1);
			it = nit + 1;
		}

		// extre line feed / stream flush
		stream << endl;
	}

	namespace messages {
		string syntax_error(optional<string> unexpected, vector<string> expected) {
			string text = (unexpected ? "Syntax error - read token: " + *unexpected + "\n" : "Syntax error\n");
			if (!expected.empty()) {
				text += (unexpected ? "while expecting:" : "sample expected tokens:");
				for (auto exp : expected)
					text += " " + exp;
				text += "\n";
			}
			return text;
		}

		string invalid_escape_sequence(char c) {
			return "Invalid escape sequence \\" + string(1, c) + "\n";
		}

		string integer_overflow(string num) {
			return "Integer " + num + " overflows 64-bit variable\n";
		}
	}
}
