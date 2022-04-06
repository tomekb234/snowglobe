#include "input.hpp"
#include <utility>

using std::move;

namespace sg {
    lexer_input::lexer_input(istream& stream, string* fname) : stream(stream), fname(fname) { }

    void lexer_input::start() {
        token = cursor;

        auto size = buffer.size();

        if (token >= size) {
            buffer = move(next_buffer);
            next_buffer.clear();
            token -= size;
            marker -= size;
            cursor -= size;
        }
    }

    char lexer_input::peek() const {
        auto size = buffer.size();

        if (cursor < size)
            return buffer[cursor];

        if (cursor < size + next_buffer.size())
            return next_buffer[cursor - size];

        return 0;
    }

    void lexer_input::skip() {
        char cur = peek();

        lengths.back()++;
        if(cur == '\n') {
            lengths.push_back(0);
        }
        cursor++;
    }

    void lexer_input::backup() {
        marker = cursor;
        stored_lengths = lengths;
    }

    void lexer_input::restore() {
        cursor = marker;
        lengths = stored_lengths;
    }

    void lexer_input::shift(string::difference_type shift) {
        if(shift > 0) {
            while(shift--) {
                skip();
            }
        }
        else {
            while(-shift >= lengths.back()) {
                cursor -= lengths.back();
                shift += lengths.back();
                lengths.pop_back();
            }
            cursor -= shift;
        }
    }

    bool lexer_input::less_than(string::size_type len) const {
        return cursor + len > buffer.size() + next_buffer.size();
    }

    bool lexer_input::fill() {
        auto ch = stream.get();

        if (stream.eof())
            return false;

        next_buffer.push_back(ch);
        return true;
    }

    string lexer_input::text() const {
        auto size = buffer.size();

        if (cursor <= size)
            return buffer.substr(token, cursor - token);

        return buffer.substr(token) + next_buffer.substr(0, cursor - size);
    }

    long lexer_input::line() const {
        return lengths.size();
    }
    long lexer_input::column() const {
        return lengths.back();
    }
    string* lexer_input::file_name() const {
        return fname;
    }
}
