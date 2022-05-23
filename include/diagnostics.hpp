#ifndef DIAGNOSTICS_HPP
#define DIAGNOSTICS_HPP

#include "location.hpp"
#include <string>
#include <memory>
#include <optional>
#include <iosfwd>
#include <vector>

namespace sg {
    using std::string;
    using std::unique_ptr;
    using std::optional;
    using std::ostream;
    using std::vector;

    struct diagnostic {
        enum level_t {
            NOTE = 0,
            WARNING = 1,
            ERROR = 2,
        } level;

        optional<location> loc;

        diagnostic(level_t level) : level(level) { }
        diagnostic(level_t level, location loc) : level(level), loc(loc) { }
        virtual ~diagnostic() { }

        virtual void write(ostream& stream) const = 0;
    };

    class diagnostic_collector {
        public:
        virtual void add(unique_ptr<diagnostic> diag) = 0;
    };


    namespace diags {
        // base/common diagnostics

        struct error : diagnostic {
            error() : diagnostic(ERROR) { }
        };

        struct warning : diagnostic {
            warning() : diagnostic(WARNING) { }
        };

        struct not_implemented : error {
            void write(ostream& stream) const override;
        };

        struct integer_overflow : error {
            string number;
            bool signed_type;
            int bits;

            integer_overflow(string number, bool signed_type, int bits) : number(number), signed_type(signed_type), bits(bits) { }
            void write(ostream& stream) const override;
        };

        // lexer diagnostics

        struct invalid_escape_sequence : error {
            char ch;

            invalid_escape_sequence(char ch) : ch(ch) { }
            void write(ostream& stream) const override;
        };

        struct multibyte_character_literal : error {
            string literal;

            multibyte_character_literal(string literal) : literal(literal) { }
            void write(ostream& stream) const override;
        };

        struct float_overflow : error {
            string number;
            bool double_precision;

            float_overflow(string number, bool double_precision) : number(number), double_precision(double_precision) { }
            void write(ostream& stream) const override;
        };

        // parser diagnostics

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
}

#endif
