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
    #include <string>
    #include <cstdint>

    namespace yy {
        using std::string;
    }
}

%param {sg::lexer_input& input}
%param {sg::diagnostic_collector& diag}

%code provides {
    yy::parser::symbol_type yylex(sg::lexer_input& input, sg::diagnostic_collector& diag);
}

%token <string> NAME
%token <uint64_t> INTEGER
%token <double> FLOAT
%token <char> CHAR
%token <string> STRING

%token AS "as"
%token BOOL "bool"
%token BREAK "break"
%token CONTINUE "continue"
%token COPYABLE "copyable"
%token ELIF "elif"
%token ELSE "else"
%token END "end"
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
%token THEN "then"
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
    NAME ":" mtype

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

    | swap_stmt
    | if_stmt
    | while_stmt
    | for_stmt
    | match_stmt
    | locally_stmt

    | func_def

stmt_seq:
    %empty
    | stmt_seq stmt

if_stmt:
    "if" condition "{" stmt_seq "}" elif_seq optional_else

condition:
    expr
    | expr "in" expr_or_name_locally

expr_or_name_locally:
    expr
    | NAME "locally"

elif:
    "elif" condition "{" stmt_seq "}"

elif_seq:
    %empty
    | elif_seq elif

optional_else:
    %empty
    | "else" "{" stmt_seq "}"

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

match_stmt:
    "match" expr_or_name_locally with_seq_nempty optional_else

with:
    "with" expr "{" stmt_seq "}"

with_seq_nempty:
    with
    | with_seq_nempty with

swap_stmt:
    "swap" expr "with" expr ";"
    | "swap" expr "with" expr_or_name_locally "{" stmt_seq "}"

locally_stmt:
    "locally" name_seq_nempty "{" stmt_seq "}"

name_seq_nempty:
    NAME
    | name_seq_nempty "," NAME


expr:
    "(" expr_indexed_seq ")"
    | "[" expr_indexed_seq "]"
    | expr "(" expr_indexed_seq ")"

    | NAME
    | NAME "::" NAME
    | var_decl_expr

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
    | expr "as" mtype

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

    | if_expr
    | match_expr
    | lambda_expr

optional_expr:
    %empty
    | expr

expr_indexed:
    expr
    | NAME ":" expr
    | INTEGER ":" expr

expr_indexed_seq:
    %empty
    | expr_indexed_seq_nempty

expr_indexed_seq_nempty:
    expr_indexed
    | expr_indexed_seq_nempty "," expr_indexed

var_decl_expr:
    "var" NAME ":" mtype
    | "var" NAME

if_expr:
    "if" condition "then" expr elif_in_expr_seq "else" expr "end"

elif_in_expr:
    "elif" condition "then" expr

elif_in_expr_seq:
    %empty
    | elif_in_expr_seq elif_in_expr

match_expr:
    "match" expr_or_name_locally with_in_expr_seq_nempty optional_else_in_expr "end"

with_in_expr:
    "with" expr "then" expr

with_in_expr_seq_nempty:
    with_in_expr
    | with_in_expr_seq_nempty with_in_expr

optional_else_in_expr:
    %empty
    | "else" expr

lambda_expr:
    "func" optional_copying "(" func_param_seq ")" optional_return_type "{" func_body "}"


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

    | "$" type
    | "&" type
    | "*" type
    | "~" type
    | "@" type

    | "$" "[" type "]"
    | "&" "[" type "]"
    | "*" "[" type "]"
    | "~" "[" type "]"
    | "@" "[" type "]"

    | "$" type "." type
    | "&" type "." type
    | "*" type "." type
    | "~" type "." type
    | "@" type "." type

    | "func" "(" mtype_seq ")" "->" type
    | "func" "$" "(" mtype_seq ")" "->" type
    | "func" "&" type "+" "(" mtype_seq ")" "->" type
    | "func" "*" type "+" "(" mtype_seq ")" "->" type
    | "func" "~" type "+" "(" mtype_seq ")" "->" type
    | "func" "@" type "+" "(" mtype_seq ")" "->" type

type_seq:
    %empty
    | type_seq_nempty

type_seq_nempty:
    type
    | type_seq_nempty "," type

mtype:
    type
    | "!" type

mtype_seq:
    %empty
    | mtype_seq_nempty

mtype_seq_nempty:
    mtype
    | mtype_seq_nempty "," mtype

%%

#include <vector>
#include <optional>

using std::vector;
using std::optional;

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
    diag.report(sg::ERROR, yyctx.location(), sg::msg::syntax_error(
        yyctx.lookahead().empty() ? optional<string>() : yyctx.lookahead().name(),
        expected_names
    ));
}

void yy::parser::error(const yy::parser::location_type& loc, const string& err) {
    diag.report(sg::ERROR, loc, err + "\n");
}
