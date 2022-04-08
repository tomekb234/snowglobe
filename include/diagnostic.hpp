#ifndef DIAGNOSTIC_HPP
#define DIAGNOSTIC_HPP

#include "location.hpp"
#include <ostream>
#include <string>
#include <optional>
#include <vector>

namespace sg {
    using std::ostream;
    using std::string;
    using std::optional;
    using std::vector;

    const size_t ERROR = 0;
    const size_t WARNING = 1;

    class diagnostic_collector {
        ostream& stream;
        bool enable_colors;

        public:

        diagnostic_collector(ostream& stream, bool enable_colors);

        void report(size_t level, yy::location location, const string& text);
    };

    namespace msg {
        string syntax_error(optional<string> unexpected, vector<string> expected);
        string invalid_escape_sequence(char ch);
        string integer_overflow(string num);
    }
}

#endif
