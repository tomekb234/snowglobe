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
    using std::istream;
    using std::ostream;
    using std::string;
    using std::reference_wrapper;

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

        void report_all(ostream& stream, bool enable_colors, optional<reference_wrapper<istream>> source_file) const;
    };

    namespace diags { // TODO make some of these messages more detailed
        struct error : diagnostic {
            error() : diagnostic(ERROR) { }
        };

        struct not_implemented : error {
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

        struct integer_overflow : error {
            string number;
            bool signed_type;
            int bits;

            integer_overflow(string number, bool signed_type, int bits) : number(number), signed_type(signed_type), bits(bits) { }
            void write(ostream& stream) const override;
        };

        struct float_overflow : error {
            string number;
            bool double_precision;

            float_overflow(string number, bool double_precision) : number(number), double_precision(double_precision) { }
            void write(ostream& stream) const override;
        };

        struct global_name_used : error {
            string name;

            global_name_used(string name) : name(name) { }
            void write(ostream& stream) const override;
        };

        struct name_not_declared : error {
            string name;

            name_not_declared(string name) : name(name) { }
            void write(ostream& stream) const override;
        };

        struct name_not_compiled : error {
            string name;

            name_not_compiled(string name) : name(name) { }
            void write(ostream& stream) const override;
        };

        struct invalid_expression : error {
            void write(ostream& stream) const override;
        };

        struct invalid_kind : error {
            void write(ostream& stream) const override;
        };

        struct invalid_type : error {
            void write(ostream& stream) const override;
        };

        struct not_subtype : error {
            void write(ostream& stream) const override;
        };

        struct expression_not_constant : error {
            void write(ostream& stream) const override;
        };

        struct no_common_supertype : error {
            void write(ostream& stream) const override;
        };

        struct invalid_argument : error {
            size_t num_args;

            invalid_argument(size_t num_args) : num_args(num_args) { }
            void write(ostream& stream) const override;
        };

        struct reused_argument : error {
            size_t index;

            reused_argument(size_t index) : index(index) { }
            void write(ostream& stream) const override;
        };

        struct missing_argument : error {
            size_t index;

            missing_argument(size_t index) : index(index) { }
            void write(ostream& stream) const override;
        };

        struct invalid_enum_variant : error {
            void write(ostream& stream) const override;
        };

        struct invalid_size_constant_type : error {
            void write(ostream& stream) const override;
        };
    }
}

#endif
