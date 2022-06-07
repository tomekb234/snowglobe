#include "input.hpp"

namespace sg {
    using std::string;
    using std::move;

    void lexer_input::start() {
        token = cursor;
        token_pos = cursor_pos;

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
        if (peek() == '\n') {
            cursor_pos.line++;
            cursor_pos.column = 1;
        } else
            cursor_pos.column++;

        cursor++;
    }

    void lexer_input::backup() {
        marker = cursor;
        marker_pos = cursor_pos;
    }

    void lexer_input::restore() {
        cursor = marker;
        cursor_pos = marker_pos;
    }

    bool lexer_input::less_than(size_t len) const {
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

    location lexer_input::loc() const {
        return { token_pos, cursor_pos };
    }
}
