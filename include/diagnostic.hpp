#ifndef DIAGNOSTIC_HPP
#define DIAGNOSTIC_HPP

#include "location.hh"
#include <ostream>
#include <string>
#include <vector>
#include <optional>

namespace sg {
	class diagnostic_collector {
		private:
		std::ostream &stream;
		bool enable_colors;

		public:
		enum Severity {ERROR = 0, WARNING};

		diagnostic_collector(std::ostream &stream, bool enable_colors);

		void report(Severity severity, yy::location location, std::string text);
	};

	namespace messages {
		std::string syntax_error(std::optional<std::string> unexpected, std::vector<std::string> expected);
		std::string invalid_escape_sequence(char c);
		std::string integer_overflow(std::string num);
	}
}

#endif
