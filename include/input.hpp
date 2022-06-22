#ifndef INPUT_HPP
#define INPUT_HPP

#include "location.hpp"
#include <istream>
#include <string>

namespace sg {
    using std::istream;
    using std::string;

    class lexer_input {
        istream& stream;
        string buffer;
        string next_buffer;
        size_t token;
        size_t marker;
        size_t cursor;
        position token_pos;
        position marker_pos;
        position cursor_pos;

        public:

        lexer_input(istream& stream, const string& file_name) : stream(stream), cursor(0), cursor_pos { &file_name, false, 1, 1 } { }

        void start();
        char peek() const;
        void skip();
        void backup();
        void restore();
        bool less_than(size_t len) const;
        bool fill();
        string text() const;
        location loc() const;
    };
}

#endif
