#ifndef DIAGNOSTIC_HPP
#define DIAGNOSTIC_HPP

#include "location.hpp"
#include <optional>
#include <vector>
#include <memory>
#include <ostream>
#include <string>

namespace sg {
    using std::optional;
    using std::vector;
    using std::unique_ptr;
    using std::ostream;
    using std::string;

    struct diagnostic {
        enum level_t {
            ERROR = 0,
            WARNING = 1
        } level;

        optional<yy::location> location;

        diagnostic(level_t level) : level(level) { }
        virtual void write(ostream& stream) const = 0;
    };

    class diagnostic_collector {
        vector<unique_ptr<diagnostic>> diags;

        public:

        void add(unique_ptr<diagnostic> diag);
        void add(unique_ptr<diagnostic> diag, yy::location location);
        void report_all(ostream& stream, bool enable_colors) const;
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
