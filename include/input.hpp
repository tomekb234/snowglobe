#ifndef INPUT_HPP
#define INPUT_HPP

#include <istream>
#include <string>

namespace sg {
    using std::istream;
    using std::string;

    class lexer_input {
        struct location {
            size_t line;
            size_t column;
        };

        istream& stream;
        string buffer;
        string next_buffer;
        size_t token;
        size_t marker;
        size_t cursor = 0;
        location token_loc;
        location marker_loc;
        location cursor_loc = { 1, 1 };

        public:

        const string file_name;

        lexer_input(istream& stream, string file_name) : stream(stream), file_name(file_name) { }

        void start();
        char peek() const;
        void skip();
        void backup();
        void restore();
        bool less_than(size_t len) const;
        bool fill();
        string text() const;
        size_t line() const;
        size_t column() const;
    };
}

#endif
