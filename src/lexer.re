#include "ast.hpp"
#include "input.hpp"
#include "diagnostic.hpp"
#include "parser.hpp"
#include "location.hpp"
#include <string>
#include <memory>
#include <unordered_map>

using sg::ast::int_token;
using sg::ast::float_token;
using std::string;
using std::unique_ptr;

static int_token parse_int(const string& text, int base);
static float_token parse_float(const string& text);
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
    } catch (sg::diagnostic* diag) { \
        diags.add(unique_ptr<sg::diagnostic>(diag), LOCATION); \
        return yy::parser::make_YYerror(LOCATION); \
    } \
}

/*!rules:re2c

[ \t\n\r] { SKIP }

"\ufeff" { SKIP }

"//" .* { SKIP }
"/" "*" "/"* "*"* (([^/]\[*])+ "/"* "*"*)* "*" "/" { SKIP }

"as" { TOKEN(AS) }
"bool" { TOKEN(BOOL) }
"break" { TOKEN(BREAK) }
"continue" { TOKEN(CONTINUE) }
"copyable" { TOKEN(COPYABLE) }
"elif" { TOKEN(ELIF) }
"else" { TOKEN(ELSE) }
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
"true" { TOKEN(TRUE) }
"u8" { TOKEN(U8) }
"u16" { TOKEN(U16) }
"u32" { TOKEN(U32) }
"u64" { TOKEN(U64) }
"var" { TOKEN(VAR) }
"while" { TOKEN(WHILE) }
"with" { TOKEN(WITH) }

[a-zA-Z_][a-zA-Z_0-9]* { TOKEN_WITH(NAME, TEXT) }

[0-9][0-9_]* (("i"|"u")("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_int(TEXT, 10)) }
"0b" [01][01_]* (("i"|"u")("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_int(TEXT, 2)) }
"0o" [0-7][0-7_]* (("i"|"u")("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_int(TEXT, 8)) }
"0x" [0-9a-fA-F][0-9a-fA-F_]* (("i"|"u")("8"|"16"|"32"|"64")?)? { TOKEN_WITH(INTEGER, parse_int(TEXT, 16)) }

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

yy::parser::symbol_type yylex(sg::lexer_input& input, sg::diagnostic_collector& diags) {
    while (true) {
        input.start();

        /*!use:re2c

        re2c:eof = 0;
        re2c:api = custom;
        re2c:api:style = free-form;
        re2c:encoding:utf8 = 1;

        re2c:define:YYCTYPE = "unsigned char";
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
using std::unordered_map;

static string remove_underscores(const string& text) {
    string result;

    for (auto ch : text) {
        if (ch != '_')
            result.push_back(ch);
    }

    return result;
}

static const unordered_map<string, int> marker_map = {
    // int tokens
    {"i", int_token::I},
    {"i8", int_token::I8},
    {"i16", int_token::I16},
    {"i32", int_token::I32},
    {"i64", int_token::I64},
    {"u", int_token::U},
    {"u8", int_token::U8},
    {"u16", int_token::U16},
    {"u32", int_token::U32},
    {"u64", int_token::U64},
    // float tokens
    {"f", float_token::F},
    {"f32", float_token::F32},
    {"f64", float_token::F64}
};

static string get_str_marker(const string& text) {
    size_t n = text.size();
    decltype(marker_map)::const_iterator it;

    // i / u / f
    if((it = marker_map.find(text.substr(n - 1))) != marker_map.end()) {
        return it->first;
    }
    else if(n < 2) {
        return "";
    }
    // i_, u_
    else if((it = marker_map.find(text.substr(n - 2))) != marker_map.end()) {
        return it->first;
    }
    else if(n < 3) {
        return "";
    }
    // i__, u__, f__
    else if((it = marker_map.find(text.substr(n - 3))) != marker_map.end()) {
        return it->first;
    }
    else {
        return "";
    }
}

static auto get_int_marker(const string& text) {
    string str_marker = get_str_marker(text);
    
    if(str_marker == "") {
        return int_token::NONE;
    }
    else {
        return static_cast< decltype(int_token::marker) >(marker_map.at(str_marker));
    }
}

static auto get_float_marker(const string& text) {
    string str_marker = get_str_marker(text);
    
    if(str_marker == "") {
        return float_token::NONE;
    }
    else {
        return static_cast< decltype(float_token::marker) >(marker_map.at(str_marker));
    }
}

static int_token parse_int(const string& text, int base) {
    try {
        return { stoull(remove_underscores(text), nullptr, base), get_int_marker(text) };
    } catch (out_of_range) {
        throw new sg::integer_overflow_error(text);
    }
}

static float_token parse_float(const string& text) {
    try {
        return { stod(remove_underscores(text)), get_float_marker(text) };
    } catch (out_of_range) {
        throw new sg::float_overflow_error(text);
    }
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
            throw new sg::invalid_escape_sequence_error(ch);
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
