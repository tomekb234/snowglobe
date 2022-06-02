#ifndef PARSER_DIAGNOSTICS_HPP
#define PARSER_DIAGNOSTICS_HPP

#include "diagnostics.hpp"
#include <string>
#include <optional>
#include <vector>
#include <ostream>

namespace sg::diags {
    using std::string;
    using std::optional;
    using std::vector;
    using std::ostream;

    struct parser_error : error {
        string message;

        parser_error(string message) : message(message) { }
        void write(ostream& stream) const override;
    };

    struct syntax_error : error {
        optional<string> unexpected;
        vector<string> expected;

        syntax_error(optional<string> unexpected, vector<string> expected) : unexpected(unexpected), expected(expected) { }
        void write(ostream& stream) const override;
    };
}

#endif
