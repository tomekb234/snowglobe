#ifndef INPUT_HPP
#define INPUT_HPP

#include <istream>
#include <string>

namespace sg {
    using std::istream;
    using std::string;

    class lexer_input {
        istream& stream;
        string buffer;
        string next_buffer;
        string::size_type token = 0;
        string::size_type marker = 0;
        string::size_type cursor = 0;

        public:

        lexer_input(istream& stream);

        void start();
        char peek() const;
        void skip();
        void backup();
        void restore();
        void shift(string::difference_type shift);
        bool less_than(string::size_type len) const;
        bool fill();
        string text() const;
    };
}

#endif
