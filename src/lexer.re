#include "input.hpp"
#include "parser.hpp"
#include <string>
#include <optional>
#include <cstdint>

#define SKIP { continue; }
#define TOKEN(token) { return yy::parser::make_##token(); }
#define TOKEN_WITH(token, value) { return value ? yy::parser::make_##token(*value) : yy::parser::make_YYUNDEF(); }
#define END TOKEN(YYEOF)
#define INVALID TOKEN(YYUNDEF)
#define TEXT (input.text())

/*!rules:re2c

[ \t] { SKIP }

"as" { TOKEN(AS) }
"bool" { TOKEN(BOOL) }
"break" { TOKEN(BREAK) }
"continue" { TOKEN(CONTINUE) }
"copyable" { TOKEN(COPYABLE) }
"elif" { TOKEN(ELIF) }
"else" { TOKEN(ELSE) }
"end" { TOKEN(END) }
"enum" { TOKEN(ENUM) }
"f32" { TOKEN(F32) }
"f64" { TOKEN(F64) }
"false" { TOKEN(FALSE) }
"for" { TOKEN(FOR) }
"func" { TOKEN(FUNC) }
"i8" { TOKEN(I8) }
"i16" { TOKEN(I16) }
"i32" { TOKEN(I32) }
"i64" { TOKEN(I64) }
"if" { TOKEN(IF) }
"in" { TOKEN(IN) }
"locally" { TOKEN(LOCALLY) }
"match" { TOKEN(MATCH) }
"never" { TOKEN(NEVER) }
"none" { TOKEN(NONE) }
"ref"  { TOKEN(REF) }
"return" { TOKEN(RETURN) }
"reversed" { TOKEN(REVERSED) }
"some" { TOKEN(SOME) }
"struct" { TOKEN(STRUCT) }
"then" { TOKEN(THEN) }
"true" { TOKEN(TRUE) }
"u8" { TOKEN(U8) }
"u16" { TOKEN(U16) }
"u32" { TOKEN(U32) }
"u64" { TOKEN(U64) }
"var" { TOKEN(VAR) }
"while" { TOKEN(WHILE) }
"with" { TOKEN(WITH) }

[a-zA-Z_][a-zA-Z_0-9]* { TOKEN_WITH(NAME, optional<string>(TEXT)) }

[0-9][0-9_]*([iu]("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_dec(TEXT)) }
"0b"[01][01_]*([iu]("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_bin(TEXT)) }
"0o"[0-7][0-7_]*([iu]("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_oct(TEXT)) }
"0x"[0-9a-fA-F][0-9a-fA-F_]*([iu]("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_hex(TEXT)) }
[0-9][0-9_]*[.][0-9][0-9_]*([f]("32"|"64"))? { TOKEN_WITH(INTEGER, parse_float(TEXT)) }

[']([\\].|[^'\\])['] { TOKEN_WITH(CHAR, parse_char(TEXT)) }
["]([\\].|[^"\\])*["] { TOKEN_WITH(STRING, parse_string(TEXT)) }

"->" { TOKEN(ARROW) }
".." { TOKEN(RANGE) }
"::" { TOKEN(SCOPE) }

"==" { TOKEN(EQ) }
"!=" { TOKEN(NEQ) }
"<=" { TOKEN(LSEQ) }
">=" { TOKEN(GTEQ) }

"&&" { TOKEN(AND) }
"||" { TOKEN(OR) }

"<<" { TOKEN(LSHIFT) }
">>" { TOKEN(RSHIFT) }

"+=" { TOKEN(PLUS_ASSIGN) }
"-=" { TOKEN(MINUS_ASSIGN) }
"*=" { TOKEN(STAR_ASSIGN) }
"/=" { TOKEN(SLASH_ASSIGN) }
"%=" { TOKEN(PERC_ASSIGN) }

"&=" { TOKEN(AMP_ASSIGN) }
"|=" { TOKEN(BAR_ASSIGN) }
"^=" { TOKEN(CARET_ASSIGN) }
"<<=" { TOKEN(LSHIFT_ASSIGN) }
">>=" { TOKEN(RSHIFT_ASSIGN) }

"(" { TOKEN(LPAREN) }
")" { TOKEN(RPAREN) }
"[" { TOKEN(LBRACK) }
"]" { TOKEN(RBRACK) }
"{" { TOKEN(LBRACE) }
"}" { TOKEN(RBRACE) }

"=" { TOKEN(ASSIGN) }

"+" { TOKEN(PLUS) }
"-" { TOKEN(MINUS) }
"*" { TOKEN(STAR) }
"/" { TOKEN(SLASH) }
"%" { TOKEN(PERC) }

"<" { TOKEN(LS) }
">" { TOKEN(GT) }

"!" { TOKEN(EXCL) }

"~" { TOKEN(TILDE) }
"&" { TOKEN(AMP) }
"|" { TOKEN(BAR) }
"^" { TOKEN(CARET) }

"@" { TOKEN(AT) }
"$" { TOKEN(DOLLAR) }
"#" { TOKEN(HASH) }
"." { TOKEN(DOT) }
"," { TOKEN(COMMA) }
";" { TOKEN(SCOLON) }
":" { TOKEN(COLON) }
"?" { TOKEN(QMARK) }

$ { END }
* { INVALID }

*/

using std::string;
using std::optional;
using std::stoull;
using std::stod;

string clean_number(const string& s, int base = 10) {
    string res;
    int start = 0;
    if(base != 10) {
        start = 2;
    }
    for (int i = start; i < s.size(); i++) {
        if (s[i] != '_') {
            res += s[i];
        }
    }
    return res;
}

optional<uint64_t> parse_dec(const string& s) {
    try {
        return stoull(clean_number(s, 10));
    } catch(...) {
        return { };
    }
}

optional<uint64_t> parse_bin(const string& s) {
    try {
        return stoull(clean_number(s, 2), nullptr, 2);
    } catch(...) {
        return { };
    }
}

optional<uint64_t> parse_oct(const string& s) {
    try {
        return stoull(clean_number(s, 8), nullptr, 8);
    } catch(...) {
        return { };
    }
}

optional<uint64_t> parse_hex(const string& s) {
    try {
        return stoull(clean_number(s, 16), nullptr, 16);
    } catch(...) {
        return { };
    }
}

optional<double> parse_float(const string& s) {
    try {
        return stod(clean_number(s));
    } catch(...) {
        return { };
    }
}

optional<char> resolve_escape_sequence(char c) {
    switch(c) {
        case '\'': return '\'';
        case '"': return '"';
        case '0': return '\0';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case '\\': return '\\';
        default: return { };
    }
}

optional<char> parse_char(const string& s) {
    return (s[1] == '\\') ? resolve_escape_sequence(s[2]) : s[1];
}

optional<string> parse_string(const string& s) {
    string result;
    for (int i = 1; i < s.length() - 1; )
        if (s[i] == '\\') {
            auto esc_seq = resolve_escape_sequence(s[i+1]);
            if (!esc_seq)
                return { };
            result.push_back(esc_seq.value());
            i += 2;
        }
        else
            result.push_back(s[i++]);
    return result;
}

yy::parser::symbol_type yylex(snow::lexer_input& input) {
    while (true) {
        input.start();

        /*!use:re2c

        re2c:eof = 0;
        re2c:api = custom;
        re2c:api:style = free-form;

        re2c:define:YYCTYPE = "char";
        re2c:define:YYPEEK = "input.peek()";
        re2c:define:YYSKIP = "input.skip();";
        re2c:define:YYBACKUP = "input.backup();";
        re2c:define:YYRESTORE = "input.restore();";
        re2c:define:YYSHIFT = "input.shift(@@);";
        re2c:define:YYLESSTHAN = "input.less_than(@@)";
        re2c:define:YYFILL = "input.fill()";

        */
    }
}
