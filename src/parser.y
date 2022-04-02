%require "3.8"
%language "c++"

%define api.value.type variant
%define api.token.constructor

%code requires {
    #include "input.hpp"
    #include <string>
    #include <cstdint>

    namespace yy {
        using std::string;
    }
}

%param {snow::lexer_input& input}

%code provides {
    yy::parser::symbol_type yylex(snow::lexer_input& input);
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
%token THEN "then"
%token TRUE "true"
%token U8 "u8"
%token U16 "u16"
%token U32 "u32"
%token U64 "u64"
%token VAR "var"
%token WHILE "while"
%token WITH "with"

%token ARROW "->"
%token RANGE ".."
%token SCOPE "::"

%token EQ "=="
%token NEQ "!="
%token LSEQ "<="
%token GTEQ ">="

%token AND "&&"
%token OR "||"

%token LSHIFT "<<"
%token RSHIFT ">>"

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

%token LPAREN "("
%token RPAREN ")"
%token LBRACK "["
%token RBRACK "]"
%token LBRACE "{"
%token RBRACE "}"

%token ASSIGN "="

%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token PERC "%"

%token LS "<"
%token GT ">"

%token EXCL "!"

%token TILDE "~"
%token AMP "&"
%token BAR "|"
%token CARET "^"

%token AT "@"
%token DOLLAR "$"
%token HASH "#"
%token DOT "."
%token COMMA ","
%token SCOLON ";"
%token COLON ":"
%token QMARK "?"

%precedence ":"
%precedence "ref" "return" "some"
%left "||"
%left "&&"
%left "|"
%left "^"
%left "&"
%left "==" "!="
%left "<" "<=" ">=" ">"
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"
%left "as"
%right "!" "~" "@" "$" "#"
%left "."
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
    var_decl "=" expr ";"

var_decl:
    "var" NAME ":" type
    | "var" NAME



func_def:
    "func" copying_decl NAME "(" func_param_seq ")" return_type "{" func_body "}"

copying_decl:
    %empty
    | "@"

func_param:
    NAME ":" type

func_param_seq:
    %empty
    | func_param_seq_nonempty

func_param_seq_nonempty:
    func_param
    | func_param_seq_nonempty "," func_param

return_type:
    %empty
    | "->" type

func_body:
    stmt_seq
    | stmt_seq expr



struct_def:
    "struct" NAME copyable_decl "{" struct_field_seq "}"

copyable_decl:
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
    "enum" NAME copyable_decl "{" enum_variant_seq "}"

enum_variant:
    NAME
    | NAME "(" type ")"

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

    | func_def
    | if_stmt
    | while_stmt
    | for_stmt
    | match_stmt
    | locally_stmt

stmt_seq:
    %empty
    | stmt_seq stmt

if_stmt:
    "if" condition_expr "{" stmt_seq "}" elif_seq_in_stmt optional_else_in_stmt

condition_expr:
    expr
    | expr "in" expr

elif_seq_in_stmt:
    %empty
    | elif_seq_in_stmt "elif" condition_expr "{" stmt_seq "}"

else_in_stmt:
    "else" "{" stmt_seq "}"

optional_else_in_stmt:
    %empty
    | else_in_stmt

while_stmt:
    "while" condition_expr "{" stmt_seq "}" optional_else_in_stmt

for_stmt:
    "for" expr "in" expr ".." expr optional_reversed "{" stmt_seq "}" optional_else_in_stmt
    | "for" expr "in" expr optional_reversed "{" stmt_seq "}" optional_else_in_stmt
    | "for" expr "," expr "in" expr optional_reversed "{" stmt_seq "}" optional_else_in_stmt

optional_reversed:
    %empty
    | "reversed"

match_stmt:
    "match" expr with_seq_in_stmt
    | "match" expr with_seq_in_stmt else_in_stmt

with_in_stmt:
    "with" expr "{" stmt_seq "}"

with_seq_in_stmt:
    with_in_stmt
    | with_seq_in_stmt with_in_stmt

locally_stmt:
    "locally" name_seq "{" stmt_seq "}"

name_seq_nonempty:
    NAME
    | name_seq_nonempty "," NAME

name_seq:
    %empty
    | name_seq_nonempty



expr:
    "(" expr_seq ")"

    | CHAR
    | STRING

    | "true"
    | "false"
    | "!" expr
    | expr "&&" expr
    | expr "||" expr

    | INTEGER
    | FLOAT
    | "-" expr
    | expr "+" expr
    | expr "-" expr
    | expr "*" expr
    | expr "/" expr
    | expr "%" expr
    | expr "as" type

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

    | NAME
    | var_decl

    | "ref" expr

    | INTEGER ":" expr
    | NAME ":" expr

    | expr "(" expr_seq ")"

    | "[" expr_seq "]"
    | "[" expr ";" expr "]"
    | NAME "::" NAME

    | "@" expr
    | "*" expr
    | "&" NAME
    | "$" NAME
    | "^" expr
    | "#" expr

    | expr "." NAME
    | expr "." INTEGER
    | expr "[" expr "]"
    | expr "[" expr ".." expr "]"

    | "return"
    | "return" expr
    | "break"
    | "continue"

    | if_expr
    | match_expr
    | closure_expr

expr_seq:
    %empty
    | expr
    | expr_seq "," expr

if_expr:
    "if" condition_expr "then" expr elif_seq_in_expr "else" expr "end"

elif_seq_in_expr:
    %empty
    | elif_seq_in_expr "elif" condition_expr "then" expr

match_expr:
    "match" expr with_expr_seq "end"
    | "match" expr with_expr_seq "else" expr "end"

with_expr:
    "with" expr "then" expr

with_expr_seq:
    with_expr
    | with_expr_seq with_expr

closure_expr:
    "func" copying_decl "(" func_param_seq ")" return_type "{" func_body "}"



type:
    NAME

    | "!" type

    | "bool"
    | "i8" | "i16" | "i32" | "i64"
    | "u8" | "u16" | "u32" | "u64"
    | "f32" | "f64"
    | "never"

    | "(" type_seq ")"
    | "[" type ";" INTEGER "]"
    | "?" type

    | "&" type
    | "$" type
    | "*" type
    | "~" type
    | "@" type

    | "&" "[" type "]"
    | "$" "[" type "]"
    | "*" "[" type "]"
    | "~" "[" type "]"
    | "@" "[" type "]"

    | "&" type "." type
    | "$" type "." type
    | "*" type "." type
    | "~" type "." type
    | "@" type "." type

    | "func" "(" type_seq ")" "->" type
    | "(" type_seq ")" "->" type
    | "&" type "#" "(" type_seq ")" "->" type
    | "*" type "#" "(" type_seq ")" "->" type
    | "~" type "#" "(" type_seq ")" "->" type
    | "@" type "#" "(" type_seq ")" "->" type

type_seq:
    %empty
    | type_seq_nonempty

type_seq_nonempty:
    type
    | type_seq_nonempty "," type
