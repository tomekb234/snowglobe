#include "input.hpp"
#include <utility>

using std::move;

namespace sg {
    lexer_input::lexer_input(istream& stream) : stream(stream) { }

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
        cursor++;
    }

    void lexer_input::backup() {
        marker = cursor;
    }

    void lexer_input::restore() {
        cursor = marker;
    }

    void lexer_input::shift(string::difference_type shift) {
        cursor += shift;
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
}
