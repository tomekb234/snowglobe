#ifndef DIAGNOSTICS_HPP
#define DIAGNOSTICS_HPP

#include "location.hpp"
#include <utility>
#include <optional>
#include <vector>
#include <memory>
#include <ostream>
#include <string>

namespace sg {
    using std::move;
    using std::optional;
    using std::vector;
    using std::unique_ptr;
    using std::ostream;
    using std::string;

    struct diagnostic {
        enum level_t {
            NOTE = 0,
            WARNING = 1,
            ERROR = 2,
        } level;

        optional<location> loc;

        diagnostic(level_t level) : level(level) { }
        diagnostic(level_t level, location loc) : level(level), loc(loc) { }

        virtual void write(ostream& stream) const = 0;
    };

    class diagnostic_collector {
        vector<unique_ptr<diagnostic>> diags;

        public:

        inline void add(unique_ptr<diagnostic> diag) {
            diags.push_back(move(diag));
        }

        void report_all(ostream& stream, bool enable_colors) const;
    };

    namespace diags {
        struct error : diagnostic {
            error() : diagnostic(ERROR) { }
        };

        struct not_implemented_error : error {
            void write(ostream& stream) const override;
        };

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

        struct invalid_escape_sequence_error : error {
            char ch;

            invalid_escape_sequence_error(char ch) : ch(ch) { }
            void write(ostream& stream) const override;
        };

        struct multibyte_character_literal : error {
            string literal;

            multibyte_character_literal(string literal) : literal(literal) { }
            void write(ostream& stream) const override;
        };

        struct integer_overflow_error : error {
            string number;
            bool signed_type;
            int bits;

            integer_overflow_error(string number, bool signed_type, int bits) : number(number), signed_type(signed_type), bits(bits) { }
            void write(ostream& stream) const override;
        };

        struct float_overflow_error : error {
            string number;
            bool double_precision;

            float_overflow_error(string number, bool double_precision) : number(number), double_precision(double_precision) { }
            void write(ostream& stream) const override;
        };

        struct global_name_used_error : error {
            string name;

            global_name_used_error(string name) : name(name) { }
            void write(ostream& stream) const override;
        };

        struct name_not_declared_error : error {
            string name;

            name_not_declared_error(string name) : name(name) { }
            void write(ostream& stream) const override;
        };

        struct expression_not_constant : error {
            void write(ostream& stream) const override;
        };

        struct no_common_supertype : error {
            void write(ostream& stream) const override;
        };
    }
}

#endif
