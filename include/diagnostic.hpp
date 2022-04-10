#ifndef DIAGNOSTIC_HPP
#define DIAGNOSTIC_HPP

#include "location.hpp"
#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace sg {
    using std::optional;
    using std::ostream;
    using std::string;
    using std::vector;

    struct diagnostic {
        enum level_t {
            ERROR = 0,
            WARNING = 1
        } level;

        optional<yy::location> location;

        diagnostic(level_t level) : level(level) { }
        virtual void write(ostream& stream) const = 0;
    };

    ostream& operator<<(ostream& stream, const diagnostic& diag);

    class diagnostic_reporter {
        ostream& stream;
        bool enable_colors;

        public:

        diagnostic_reporter(ostream& stream, bool enable_colors) : stream(stream), enable_colors(enable_colors) { }
        void report(const diagnostic& diag);
    };

    struct parser_error : diagnostic {
        string error;

        parser_error(string error) : diagnostic(ERROR), error(error) { }
        void write(ostream& stream) const override;
    };

    struct syntax_error : diagnostic {
        optional<string> unexpected;
        vector<string> expected;

        syntax_error(optional<string> unexpected, vector<string> expected) : diagnostic(ERROR), unexpected(unexpected), expected(expected) { }
        void write(ostream& stream) const override;
    };

    struct invalid_escape_sequence_error : diagnostic {
        char ch;

        invalid_escape_sequence_error(char ch) : diagnostic(ERROR), ch(ch) { }
        void write(ostream& stream) const override;
    };

    struct integer_overflow_error : diagnostic {
        string number;

        integer_overflow_error(string number) : diagnostic(ERROR), number(number) { }
        void write(ostream& stream) const override;
    };
}

#endif
