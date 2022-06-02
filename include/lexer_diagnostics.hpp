#ifndef LEXER_DIAGNOSTICS_HPP
#define LEXER_DIAGNOSTICS_HPP

#include "diagnostics.hpp"
#include <string>
#include <ostream>

namespace sg::diags {
    using std::string;
    using std::ostream;

    struct int_token_overflow : error {
        string number;

        int_token_overflow(string number) : number(number) { }
        void write(ostream& stream) const override;
    };

    struct float_token_overflow : error {
        string number;

        float_token_overflow(string number) : number(number) { }
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
}

#endif
