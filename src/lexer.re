#include "input.hpp"
#include "parser.hpp"
#include "location.hpp"
#include "diagnostic.hpp"
#include <string>
#include <cstdint>

using std::string;

static uint64_t parse_integer(const string& text, int base);
static double parse_float(const string& text);
static char parse_char(const string& text);
static string parse_string(const string& text);

#define TEXT (input.text())
#define LOCATION (yy::location(&input.file_name, input.line(), input.column()))

#define SKIP { continue; }
#define TOKEN(token) { return yy::parser::make_##token(LOCATION); }
#define END TOKEN(YYEOF)
#define INVALID TOKEN(YYUNDEF)

#define TOKEN_WITH(token, value) { \
    try { \
        return yy::parser::make_##token(value, LOCATION); \
    } catch (string err) { \
        diag.report(sg::diagnostic_collector::ERROR, LOCATION, err); \
        return yy::parser::make_YYerror(LOCATION); \
    } \
}

/*!rules:re2c

[ \t\n\r] { SKIP }

"//" .* { SKIP }
"/" "*" "/"* "*"* (([^/]\[*])+ "/"* "*"*)* "*" "/" { SKIP }

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
"swap" { TOKEN(SWAP) }
"then" { TOKEN(THEN) }
"true" { TOKEN(TRUE) }
"u8" { TOKEN(U8) }
"u16" { TOKEN(U16) }
"u32" { TOKEN(U32) }
"u64" { TOKEN(U64) }
"var" { TOKEN(VAR) }
"while" { TOKEN(WHILE) }
"with" { TOKEN(WITH) }

[a-zA-Z_][a-zA-Z_0-9]* { TOKEN_WITH(NAME, TEXT) }

[0-9][0-9_]* (("i"|"u")("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_integer(TEXT, 10)) }
"0b" [01][01_]* (("i"|"u")("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_integer(TEXT, 2)) }
"0o" [0-7][0-7_]* (("i"|"u")("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_integer(TEXT, 8)) }
"0x" [0-9a-fA-F][0-9a-fA-F_]* (("i"|"u")("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_integer(TEXT, 16)) }

[0-9][0-9_]* "." [0-9][0-9_]* ("e"("+"|"-")?[0-9][0-9_]*)? ("f"("32"|"64")?)? { TOKEN_WITH(FLOAT, parse_float(TEXT)) }

['] ([\\].|[^'\\]) ['] { TOKEN_WITH(CHAR, parse_char(TEXT)) }
["] ([\\].|[^"\\])* ["] { TOKEN_WITH(STRING, parse_string(TEXT)) }

"<<=" { TOKEN(LSHIFT_ASSIGN) }
">>=" { TOKEN(RSHIFT_ASSIGN) }

"&&" { TOKEN(AND) }
"||" { TOKEN(OR) }
"<<" { TOKEN(LSHIFT) }
">>" { TOKEN(RSHIFT) }
"==" { TOKEN(EQ) }
"!=" { TOKEN(NEQ) }
"<=" { TOKEN(LSEQ) }
">=" { TOKEN(GTEQ) }
"+=" { TOKEN(PLUS_ASSIGN) }
"-=" { TOKEN(MINUS_ASSIGN) }
"*=" { TOKEN(STAR_ASSIGN) }
"/=" { TOKEN(SLASH_ASSIGN) }
"%=" { TOKEN(PERC_ASSIGN) }
"&=" { TOKEN(AMP_ASSIGN) }
"|=" { TOKEN(BAR_ASSIGN) }
"^=" { TOKEN(CARET_ASSIGN) }
"->" { TOKEN(ARROW) }
".." { TOKEN(RANGE) }
"::" { TOKEN(SCOPE) }

"(" { TOKEN(LPAREN) }
")" { TOKEN(RPAREN) }
"[" { TOKEN(LBRACK) }
"]" { TOKEN(RBRACK) }
"{" { TOKEN(LBRACE) }
"}" { TOKEN(RBRACE) }
"." { TOKEN(DOT) }
"," { TOKEN(COMMA) }
";" { TOKEN(SCOLON) }
"!" { TOKEN(EXCL) }
":" { TOKEN(COLON) }
"+" { TOKEN(PLUS) }
"-" { TOKEN(MINUS) }
"*" { TOKEN(STAR) }
"/" { TOKEN(SLASH) }
"%" { TOKEN(PERC) }
"~" { TOKEN(TILDE) }
"&" { TOKEN(AMP) }
"|" { TOKEN(BAR) }
"^" { TOKEN(CARET) }
"<" { TOKEN(LS) }
">" { TOKEN(GT) }
"=" { TOKEN(ASSIGN) }
"@" { TOKEN(AT) }
"#" { TOKEN(HASH) }
"$" { TOKEN(DOLLAR) }
"?" { TOKEN(QMARK) }

$ { END }
* { INVALID }

*/

yy::parser::symbol_type yylex(sg::lexer_input& input, sg::diagnostic_collector& diag) {
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
        re2c:define:YYLESSTHAN = "input.less_than(@@)";
        re2c:define:YYFILL = "input.fill()";

        */
    }
}

#include <stdexcept>

using std::stoull;
using std::stod;
using std::out_of_range;

static string remove_underscores(const string& text) {
    string result;

    for (auto ch : text) {
        if (ch != '_')
            result.push_back(ch);
    }

    return result;
}

static uint64_t parse_integer(const string& text, int base) {
    try {
        return stoull(remove_underscores(text), nullptr, base);
    } catch (out_of_range) {
        throw sg::messages::integer_overflow(text);
    }
}

static double parse_float(const string& text) {
    return stod(remove_underscores(text));
    // TODO float out-of-range
}

static char resolve_escape_sequence(char ch) {
    switch (ch) {
        case '\'': return '\'';
        case '"': return '"';
        case '0': return '\0';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case '\\': return '\\';

        default:
            throw sg::messages::invalid_escape_sequence(ch);
    }
}

static char parse_char(const string& text) {
    return (text[1] == '\\') ? resolve_escape_sequence(text[2]) : text[1];
}

static string parse_string(const string& text) {
    string result;

    for (size_t index = 1; index < text.length() - 1; index++) {
        if (text[index] == '\\')
            result.push_back(resolve_escape_sequence(text[++index]));
        else
            result.push_back(text[index]);
    }

    return result;
}
