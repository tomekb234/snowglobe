#ifndef INPUT_HPP
#define INPUT_HPP

#include <istream>
#include <string>
#include <vector>

namespace sg {
    using std::istream;
    using std::string;
    using std::vector;

    class lexer_input {
        istream& stream;
        string* fname;
        string buffer;
        string next_buffer;
        string::size_type token = 0;
        string::size_type marker = 0;
        string::size_type cursor = 0;

        vector<int> lengths = { 0 };
        vector<int> stored_lengths = { 0 };
        public:

        lexer_input(istream& stream, string* fname);

        void start();
        char peek() const;
        void skip();
        void backup();
        void restore();
        void shift(string::difference_type shift);
        bool less_than(string::size_type len) const;
        bool fill();
        string text() const;
        long line() const;
        long column() const;
        string* file_name() const;
    };
}

#endif
