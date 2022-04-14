%require "3.8"
%language "c++"

%header

%define api.value.type variant
%define api.token.constructor

%locations
%define api.location.file "location.hpp"
%define parse.error custom

%code requires {
    #include "input.hpp"
    #include "diagnostic.hpp"
    #include "ast.hpp"
    #include <string>

    namespace yy {
        using std::string;
        using namespace sg::ast;
    }
}

%param {sg::lexer_input& input}
%param {sg::diagnostic_collector& diags}

%code provides {
    yy::parser::symbol_type yylex(sg::lexer_input& input, sg::diagnostic_collector& diags);
}

%token <string> NAME
%token <int_token> INTEGER
%token <float_token> FLOAT
%token <char> CHAR
%token <string> STRING

%token AS "as"
%token BOOL "bool"
%token BREAK "break"
%token CONTINUE "continue"
%token COPYABLE "copyable"
%token ELIF "elif"
%token ELSE "else"
%token ENUM "enum"
%token F32 "f32"
%token F64 "f64"
%token FALSE "false"
%token FOR "for"
%token FUNC "func"
%token I8 "i8"
%token I16 "i16"
%token I32 "i32"
%token I64 "i64"
%token IF "if"
%token IN "in"
%token LOCALLY "locally"
%token MATCH "match"
%token NEVER "never"
%token NONE "none"
%token REF "ref"
%token RETURN "return"
%token REVERSED "reversed"
%token SOME "some"
%token STRUCT "struct"
%token SWAP "swap"
%token TRUE "true"
%token U8 "u8"
%token U16 "u16"
%token U32 "u32"
%token U64 "u64"
%token VAR "var"
%token WHILE "while"
%token WITH "with"

%token LPAREN "("
%token RPAREN ")"
%token LBRACK "["
%token RBRACK "]"
%token LBRACE "{"
%token RBRACE "}"

%token DOT "."
%token COMMA ","
%token SCOLON ";"
%token COLON ":"

%token EXCL "!"
%token AND "&&"
%token OR "||"

%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token PERC "%"

%token TILDE "~"
%token AMP "&"
%token BAR "|"
%token CARET "^"
%token LSHIFT "<<"
%token RSHIFT ">>"

%token EQ "=="
%token LS "<"
%token GT ">"
%token NEQ "!="
%token LSEQ "<="
%token GTEQ ">="

%token ASSIGN "="
%token PLUS_ASSIGN "+="
%token MINUS_ASSIGN "-="
%token STAR_ASSIGN "*="
%token SLASH_ASSIGN "/="
%token PERC_ASSIGN "%="
%token AMP_ASSIGN "&="
%token BAR_ASSIGN "|="
%token CARET_ASSIGN "^="
%token LSHIFT_ASSIGN "<<="
%token RSHIFT_ASSIGN ">>="

%token AT "@"
%token HASH "#"
%token DOLLAR "$"
%token QMARK "?"
%token ARROW "->"
%token RANGE ".."
%token SCOPE "::"

%precedence ":"
%precedence "return" "some"
%left "||"
%left "&&"
%left "==" "!="
%left "<" "<=" ">=" ">"
%left "|"
%left "^"
%left "&"
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"
%left "as"
%right "!" "~" "@" "$" "#" UNARY_MINUS UNARY_STAR UNARY_AMP UNARY_CARET
%left "." "->"
%precedence "(" "["


%%


program:
    global_def_seq

global_def_seq:
    %empty
    | global_def_seq global_def

global_def:
    var_def
    | func_def
    | struct_def
    | enum_def


var_def:
    "var" NAME ":" type "=" expr ";"
    | "var" NAME "=" expr ";"


func_def:
    "func" optional_copying NAME "(" func_param_seq ")" optional_return_type "{" func_body "}"

optional_copying:
    %empty
    | "@"

func_param:
    NAME ":" type_local

func_param_seq:
    %empty
    | func_param_seq_nempty

func_param_seq_nempty:
    func_param
    | func_param_seq_nempty "," func_param

optional_return_type:
    %empty
    | "->" type

func_body:
    stmt_seq
    | stmt_seq expr


struct_def:
    "struct" NAME optional_copyable "{" struct_field_seq "}"

optional_copyable:
    %empty
    | "copyable"

struct_field:
    NAME ":" type

struct_field_seq:
    struct_field_seq_inner
    | struct_field_seq_inner struct_field

struct_field_seq_inner:
    %empty
    | struct_field_seq_inner struct_field ","


enum_def:
    "enum" NAME optional_copyable "{" enum_variant_seq "}"

enum_variant:
    NAME
    | NAME "(" type_seq ")"

enum_variant_seq:
    enum_variant_seq_inner
    | enum_variant_seq_inner enum_variant

enum_variant_seq_inner:
    %empty
    | enum_variant_seq_inner enum_variant ","


stmt:
    expr ";"

    | expr "=" expr ";"
    | expr "+=" expr ";"
    | expr "-=" expr ";"
    | expr "*=" expr ";"
    | expr "/=" expr ";"
    | expr "%=" expr ";"
    | expr "&=" expr ";"
    | expr "|=" expr ";"
    | expr "^=" expr ";"
    | expr "<<=" expr ";"
    | expr ">>=" expr ";"

    | locally_stmt
    | swap_stmt
    | if_stmt
    | match_stmt
    | while_stmt
    | for_stmt

    | func_def

stmt_seq:
    %empty
    | stmt_seq stmt

locally_stmt:
    "locally" name_seq_nempty "{" stmt_seq "}"

name_seq_nempty:
    NAME
    | name_seq_nempty "," NAME

swap_stmt:
    "swap" expr "with" expr ";"
    | "swap" expr "with" expr_or_name_locally "{" stmt_seq "}"

expr_or_name_locally:
    expr
    | NAME "locally"

if_stmt:
    "if" condition "{" stmt_seq "}" elif_seq optional_else

condition:
    expr
    | expr "in" expr_or_name_locally

elif:
    "elif" condition "{" stmt_seq "}"

elif_seq:
    %empty
    | elif_seq elif

optional_else:
    %empty
    | "else" "{" stmt_seq "}"

match_stmt:
    "match" expr_or_name_locally with_seq_nempty optional_else

with:
    "with" expr "{" stmt_seq "}"

with_seq_nempty:
    with
    | with_seq_nempty with

while_stmt:
    "while" condition "{" stmt_seq "}" optional_else

for_stmt:
    "for" expr "in" expr ".." expr for_stmt_tail
    | "for" expr "in" expr_or_name_locally for_stmt_tail
    | "for" expr "," expr "in" expr_or_name_locally for_stmt_tail
    | "for" expr "ref" expr_or_name_locally for_stmt_tail
    | "for" expr "," expr "ref" expr_or_name_locally for_stmt_tail

for_stmt_tail:
    optional_reversed "{" stmt_seq "}" optional_else

optional_reversed:
    %empty
    | "reversed"


expr:
    "(" expr_marked_seq ")"
    | "[" expr_marked_seq "]"
    | expr "(" expr_marked_seq ")"

    | NAME
    | NAME "::" NAME

    | "var" NAME ":" type_local
    | "var" NAME

    | "true"
    | "false"
    | CHAR
    | STRING
    | INTEGER
    | FLOAT

    | "!" expr
    | expr "&&" expr
    | expr "||" expr

    | "-" expr %prec UNARY_MINUS
    | expr "+" expr
    | expr "-" expr
    | expr "*" expr
    | expr "/" expr
    | expr "%" expr

    | expr "as" type_local

    | "~" expr
    | expr "&" expr
    | expr "|" expr
    | expr "^" expr
    | expr "<<" expr
    | expr ">>" expr

    | expr "==" expr
    | expr "!=" expr
    | expr "<" expr
    | expr "<=" expr
    | expr ">" expr
    | expr ">=" expr

    | "none"
    | "some" expr
    | "return"
    | "return" expr
    | "break"
    | "continue"

    | "&" NAME %prec UNARY_AMP
    | "@" expr
    | "*" expr %prec UNARY_STAR

    | "[" expr ";" INTEGER "]"
    | "@" "[" expr "#" expr "]"
    | "#" expr

    | expr "." NAME
    | expr "." INTEGER
    | expr "[" expr "]"

    | "^" expr %prec UNARY_CARET
    | expr "->" NAME
    | expr "->" INTEGER
    | expr "[" "ref" expr "]"
    | expr "[" "ref" optional_expr ".." optional_expr "]"

    | "func" optional_copying "(" func_param_seq ")" optional_return_type "{" func_body "}"

optional_expr:
    %empty
    | expr

expr_marked:
    expr
    | NAME ":" expr
    | INTEGER ":" expr

expr_marked_seq:
    %empty
    | expr_marked_seq_nempty

expr_marked_seq_nempty:
    expr_marked
    | expr_marked_seq_nempty "," expr_marked


type:
    NAME

    | "bool"
    | "i8" | "i16" | "i32" | "i64"
    | "u8" | "u16" | "u32" | "u64"
    | "f32" | "f64"
    | "never"

    | "(" type_seq ")"
    | "[" type ";" INTEGER "]"
    | "?" type

    | "$" type_pointed
    | "&" type_pointed
    | "*" type_pointed
    | "~" type_pointed
    | "@" type_pointed

    | "$" type_pointed "." type_pointed
    | "&" type_pointed "." type_pointed
    | "*" type_pointed "." type_pointed
    | "~" type_pointed "." type_pointed
    | "@" type_pointed "." type_pointed

    | "func" "(" type_local_seq ")" "->" type
    | "func" "$" "(" type_local_seq ")" "->" type
    | "func" "&" type_pointed "(" type_local_seq ")" "->" type
    | "func" "*" type_pointed "(" type_local_seq ")" "->" type
    | "func" "~" type_pointed "(" type_local_seq ")" "->" type
    | "func" "@" type_pointed "(" type_local_seq ")" "->" type

type_seq:
    %empty
    | type_seq_nempty

type_seq_nempty:
    type
    | type_seq_nempty "," type

type_pointed:
    type
    | "[" type "]"

type_local:
    type
    | "!" type

type_local_seq:
    %empty
    | type_local_seq_nempty

type_local_seq_nempty:
    type_local
    | type_local_seq_nempty "," type_local

%%

#include <vector>
#include <optional>
#include <memory>

using std::vector;
using std::optional;
using std::make_unique;

void yy::parser::report_syntax_error(const yy::parser::context& yyctx) const {
    const size_t MAX_TOKENS = 5;
    symbol_kind_type expected[MAX_TOKENS];

    // get expected token list
    size_t count = yyctx.expected_tokens(expected, MAX_TOKENS);
    if (count == 0 && expected[0] != symbol_kind::S_YYEMPTY)
        count = MAX_TOKENS;

    // extract token names
    vector<string> expected_names(count);
    for (size_t index = 0; index < count; index++)
        expected_names[index] = symbol_name(expected[index]);

    // report error message
    diags.add(make_unique<sg::syntax_error>(yyctx.lookahead().empty() ? optional<string>() : yyctx.lookahead().name(), expected_names), yyctx.location());
}

void yy::parser::error(const yy::parser::location_type& loc, const string& err) {
    diags.add(make_unique<sg::parser_error>(err), loc);
}
