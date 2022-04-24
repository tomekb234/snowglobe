// Configuration

%require "3.8"
%language "c++"

%header

%define api.value.type variant
%define api.token.constructor

%locations
%define api.location.file "location.hpp"
%define parse.error custom

// Interface

%code requires {
    #include "input.hpp"
    #include "diagnostic.hpp"
    #include "ast.hpp"

    namespace yy {
        using namespace sg::ast;
    }
}

%parse-param {sg::ast::program& result}
%param {sg::lexer_input& input}
%param {sg::diagnostic_collector& diags}

%code provides {
    yy::parser::symbol_type yylex(sg::lexer_input& input, sg::diagnostic_collector& diags);
}

// Utilities

%code {
    #include <utility>
    #include <memory>
    #include <optional>
    #include <vector>
    #include <variant>

    using std::move;
    using std::unique_ptr;
    using std::make_unique;
    using std::optional;
    using std::make_optional;
    using std::vector;
    using std::in_place_index;
    using std::make_pair;
    using std::remove_reference;

    template<typename T>
    static unique_ptr<T> into_ptr(T& value) {
        return make_unique<T>(move(value));
    }

    template<typename T>
    static unique_ptr<T> into_ptr(T&& value) {
        return make_unique<T>(move(value));
    }

    template<typename T>
    static optional<unique_ptr<T>> into_optional_ptr(optional<T>& value) {
        return value ? make_optional(make_unique<T>(move(*value))) : optional<unique_ptr<T>>();
    }

    template<typename T>
    static vector<unique_ptr<T>> into_ptr_vector(vector<T>& values) {
        vector<unique_ptr<T>> result;
        for (T& value : values)
            result.push_back(make_unique<T>(move(value)));
        return result;
    }

    #define VARIANT(type, index, val) { decltype(type::value)(in_place_index<type::index>, val) }

    #define TYPE(var) remove_reference<decltype(var)>::type
}

// Tokens

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

// Token precedence

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

// Nonterminal symbols

%nterm program
%nterm <vector<global_def>> global_def_seq
%nterm <global_def> global_def
%nterm <var_def> var_def
%nterm <func_def> func_def
%nterm <bool> optional_copying
%nterm <func_param> func_param
%nterm <vector<func_param>> func_param_seq
%nterm <vector<func_param>> func_param_seq_nempty
%nterm <optional<type>> optional_return_type
%nterm <func_body> func_body
%nterm <struct_def> struct_def
%nterm <bool> optional_copyable
%nterm <struct_field> struct_field
%nterm <vector<struct_field>> struct_field_seq
%nterm <vector<struct_field>> struct_field_seq_inner
%nterm <enum_def> enum_def
%nterm <enum_variant> enum_variant
%nterm <vector<enum_variant>> enum_variant_seq
%nterm <vector<enum_variant>> enum_variant_seq_inner

%nterm <stmt> stmt
%nterm <stmt_block> stmt_seq
%nterm <locally_block_stmt> locally_stmt
%nterm <vector<string>> name_seq_nempty
%nterm <swap_stmt> swap_stmt
%nterm <swap_block_stmt> swap_block_stmt
%nterm <expr_or_name_locally> expr_or_name_locally
%nterm <if_stmt> if_stmt
%nterm <condition> condition
%nterm <if_branch> elif
%nterm <vector<if_branch>> elif_seq
%nterm <optional<stmt_block>> optional_else
%nterm <match_stmt> match_stmt
%nterm <match_branch> with
%nterm <vector<match_branch>> with_seq_nempty
%nterm <while_stmt> while_stmt
%nterm <for_stmt> for_stmt
%nterm <for_range_stmt> for_range_stmt
%nterm <for_slice_stmt> for_slice_stmt
%nterm <for_stmt_base> for_stmt_tail
%nterm <bool> optional_reversed

%nterm <expr> expr
%nterm <optional<expr>> optional_expr;
%nterm <expr_marked> expr_marked;
%nterm <vector<expr_marked>> expr_marked_seq;
%nterm <vector<expr_marked>> expr_marked_seq_nempty;

%nterm <type> type
%nterm <vector<type>> type_seq
%nterm <vector<type>> type_seq_nempty
%nterm <type_pointed> type_pointed

%nterm <type_local> type_local
%nterm <vector<type_local>> type_local_seq
%nterm <vector<type_local>> type_local_seq_nempty

%%

// Main rules

program:
    global_def_seq { result = { into_ptr_vector($global_def_seq) }; }

global_def_seq:
    %empty { $$ = TYPE($$)(); }
    | global_def_seq[head] global_def { $$ = move($head); $$.push_back(move($global_def)); }

global_def:
    var_def { $$ = VARIANT(global_def, VAR_DEF, into_ptr($var_def)); }
    | func_def { $$ = VARIANT(global_def, FUNC_DEF, into_ptr($func_def)); }
    | struct_def { $$ = VARIANT(global_def, STRUCT_DEF, into_ptr($struct_def)); }
    | enum_def { $$ = VARIANT(global_def, ENUM_DEF, into_ptr($enum_def)); }

var_def:
    "var" NAME ":" type "=" expr ";" { $$ = { move($NAME), { into_ptr($type) }, into_ptr($expr) }; }
    | "var" NAME "=" expr ";" { $$ = { move($NAME), { }, into_ptr($expr) }; }

func_def:
    "func" optional_copying NAME "(" func_param_seq ")" optional_return_type "{" func_body "}" { $$ = { move($NAME), $optional_copying, into_ptr_vector($func_param_seq), into_optional_ptr($optional_return_type), into_ptr($func_body) }; }

optional_copying:
    %empty { $$ = false; }
    | "@" { $$ = true; }

func_param:
    NAME ":" type_local { $$ = { move($NAME), into_ptr($type_local) }; }

func_param_seq:
    %empty { $$ = TYPE($$)(); }
    | func_param_seq_nempty { $$ = move($func_param_seq_nempty); }

func_param_seq_nempty:
    func_param { $$ = TYPE($$)(); $$.push_back(move($func_param));}
    | func_param_seq_nempty[head] "," func_param { $$ = move($head); $$.push_back(move($func_param)); }

optional_return_type:
    %empty { $$ = TYPE($$)(); }
    | "->" type { $$ = { move($type) }; }

func_body:
    stmt_seq { $$ = { into_ptr($stmt_seq), { } }; }
    | stmt_seq expr { $$ = { into_ptr($stmt_seq), { into_ptr($expr) } }; }

struct_def:
    "struct" NAME optional_copyable "{" struct_field_seq "}" { $$ = { move($NAME), $optional_copyable, into_ptr_vector($struct_field_seq) }; }

optional_copyable:
    %empty { $$ = false; }
    | "copyable" { $$ = true; }

struct_field:
    NAME ":" type { $$ = { move($NAME), into_ptr($type) }; }

struct_field_seq:
    struct_field_seq_inner[head] { $$ = move($head); }
    | struct_field_seq_inner[head] struct_field { $$ = move($head); $$.push_back(move($struct_field)); }

struct_field_seq_inner:
    %empty { $$ = TYPE($$)(); }
    | struct_field_seq_inner[head] struct_field "," { $$ = move($head); $$.push_back(move($struct_field)); }

enum_def:
    "enum" NAME optional_copyable "{" enum_variant_seq "}" { $$ = { move($NAME), $optional_copyable, into_ptr_vector($enum_variant_seq) }; }

enum_variant:
    NAME { $$ = { move($NAME), { } }; }
    | NAME "(" type_seq ")" { $$ = { move($NAME), into_ptr_vector($type_seq) }; }

enum_variant_seq:
    enum_variant_seq_inner[head] { $$ = move($head); }
    | enum_variant_seq_inner[head] enum_variant { $$ = move($head); $$.push_back(move($enum_variant)); }

enum_variant_seq_inner:
    %empty { $$ = TYPE($$)(); }
    | enum_variant_seq_inner[head] enum_variant "," { $$ = move($head); $$.push_back(move($enum_variant)); }

// Statement rules

stmt:
    expr ";" { $$ = VARIANT(stmt, EXPR_EVAL, into_ptr($expr)); }

    | expr[left] "=" expr[right] ";" { $$ = VARIANT(stmt, ASSIGNMENT, into_ptr(assignment_stmt{ into_ptr($left), into_ptr($right) })); }
    | expr[left] "+=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::ADD })); }
    | expr[left] "-=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::SUB })); }
    | expr[left] "*=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::MUL })); }
    | expr[left] "/=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::DIV })); }
    | expr[left] "%=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::MOD })); }
    | expr[left] "&=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::BIT_AND })); }
    | expr[left] "|=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::BIT_OR })); }
    | expr[left] "^=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::BIT_XOR })); }
    | expr[left] "<<=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::BIT_LSH })); }
    | expr[left] ">>=" expr[right] ";" { $$ = VARIANT(stmt, COMPOUND_ASSIGNMENT, into_ptr(compound_assignment_stmt{ /*base*/{ into_ptr($left), into_ptr($right) }, compound_assignment_stmt::BIT_RSH })); }

    | locally_stmt { $$ = VARIANT(stmt, LOCALLY_BLOCK, into_ptr($locally_stmt)); }
    | swap_stmt { $$ = VARIANT(stmt, SWAP, into_ptr($swap_stmt)); }
    | swap_block_stmt { $$ = VARIANT(stmt, SWAP_BLOCK, into_ptr($swap_block_stmt)); }
    | if_stmt { $$ = VARIANT(stmt, IF, into_ptr($if_stmt)); }
    | match_stmt { $$ = VARIANT(stmt, MATCH, into_ptr($match_stmt)); }
    | while_stmt { $$ = VARIANT(stmt, WHILE, into_ptr($while_stmt)); }
    | for_stmt { $$ = VARIANT(stmt, FOR, into_ptr($for_stmt)); }

    | func_def { $$ = VARIANT(stmt, FUNC_DEF, into_ptr($func_def)); }

stmt_seq:
    %empty { $$ = { { } }; }
    | stmt_seq[head] stmt { $$ = move($head); $$.stmts.push_back(into_ptr($stmt)); }

locally_stmt:
    "locally" name_seq_nempty "{" stmt_seq "}" { $$ = { move($name_seq_nempty), into_ptr($stmt_seq) }; }

name_seq_nempty:
    NAME { $$ = TYPE($$)(); $$.push_back(move($NAME)); }
    | name_seq_nempty[head] "," NAME { $$ = move($head); $$.push_back(move($NAME)); }

swap_stmt:
    "swap" expr[left] "with" expr[right] ";" { $$ = { into_ptr($left), into_ptr($right) }; }

swap_block_stmt:
    "swap" expr "with" expr_or_name_locally "{" stmt_seq "}" { $$ = { into_ptr($expr), into_ptr($expr_or_name_locally), into_ptr($stmt_seq) }; }

expr_or_name_locally:
    expr { $$ = VARIANT(expr_or_name_locally, EXPR, into_ptr($expr)); }
    | NAME "locally" { $$ = VARIANT(expr_or_name_locally, NAME_LOCALLY, move($NAME)); }

if_stmt:
    "if" condition "{" stmt_seq "}" elif_seq optional_else { $elif_seq.insert($elif_seq.begin(), { into_ptr($condition), into_ptr($stmt_seq) }); $$ = { into_ptr_vector($elif_seq), into_optional_ptr($optional_else) }; }

condition:
    expr { $$ = VARIANT(condition, CHECK_IF_TRUE, into_ptr($expr)); }
    | expr "in" expr_or_name_locally { $$ = VARIANT(condition, CHECK_IF_PRESENT, make_pair(into_ptr($expr), into_ptr($expr_or_name_locally))); }

elif:
    "elif" condition "{" stmt_seq "}" { $$ = { into_ptr($condition), into_ptr($stmt_seq) }; }

elif_seq:
    %empty { $$ = TYPE($$)(); }
    | elif_seq[head] elif { $$ = move($head); $$.push_back(move($elif)); }

optional_else:
    %empty { $$ = TYPE($$)(); }
    | "else" "{" stmt_seq "}" { $$ = { move($stmt_seq) }; }

match_stmt:
    "match" expr_or_name_locally with_seq_nempty optional_else { $$ = { into_ptr($expr_or_name_locally), into_ptr_vector($with_seq_nempty), into_optional_ptr($optional_else) }; }

with:
    "with" expr "{" stmt_seq "}" { $$ = { into_ptr($expr), into_ptr($stmt_seq) }; }

with_seq_nempty:
    with { $$ = TYPE($$)(); $$.push_back(move($with)); }
    | with_seq_nempty[head] with { $$ = move($head); $$.push_back(move($with)); }

while_stmt:
    "while" condition "{" stmt_seq "}" optional_else { $$ = { into_ptr($condition), into_ptr($stmt_seq), into_optional_ptr($optional_else) }; }

for_stmt:
    for_range_stmt { $$ = VARIANT(for_stmt, RANGE, into_ptr($for_range_stmt)); }
    | for_slice_stmt { $$ = VARIANT(for_stmt, SLICE, into_ptr($for_slice_stmt)); }

for_range_stmt:
    "for" expr[lval] "in" expr[begin] ".." expr[end] for_stmt_tail[tail] { $tail.lvalue = into_ptr($lval); $$ = { /*base*/move($tail), into_ptr($begin), into_ptr($end) }; }  

for_slice_stmt:
    "for" expr[lval] "in" expr_or_name_locally for_stmt_tail[tail] { $tail.lvalue = into_ptr($lval); $$ = { /*base*/move($tail), { }, false, into_ptr($expr_or_name_locally) }; }
    | "for" expr[lval] "," expr[idx] "in" expr_or_name_locally for_stmt_tail[tail] { $tail.lvalue = into_ptr($lval); $$ = { /*base*/move($tail), into_ptr($idx), false, into_ptr($expr_or_name_locally) }; }
    | "for" expr[lval] "ref" expr_or_name_locally for_stmt_tail[tail] { $tail.lvalue = into_ptr($lval); $$ = { /*base*/move($tail), { }, true, into_ptr($expr_or_name_locally) }; }
    | "for" expr[lval] "," expr[idx] "ref" expr_or_name_locally for_stmt_tail[tail] { $tail.lvalue = into_ptr($lval); $$ = { /*base*/move($tail), into_ptr($idx), true, into_ptr($expr_or_name_locally) }; }

for_stmt_tail:
    optional_reversed "{" stmt_seq "}" optional_else { $$ = { {/*null pointer*/}, $optional_reversed, into_ptr($stmt_seq), into_optional_ptr($optional_else) }; }

optional_reversed:
    %empty { $$ = false; }
    | "reversed" { $$ = true; }

// Expression rules

expr:
    "(" expr_marked_seq ")" { $$ = VARIANT(expr, TUPLE, into_ptr_vector($expr_marked_seq)); }
    | "[" expr_marked_seq "]" { $$ = VARIANT(expr, ARRAY, into_ptr_vector($expr_marked_seq)); }
    | expr[exp] "(" expr_marked_seq ")" { $$ = VARIANT(expr, APPLICATION, make_pair(into_ptr($exp), into_ptr_vector($expr_marked_seq))); }

    | NAME { $$ = VARIANT(expr, NAME, move($NAME)); }
    | NAME[left] "::" NAME[right] { $$ = VARIANT(expr, QUALIFIED_NAME, make_pair(move($left), move($right))); }

    | "var" NAME ":" type_local { $$ = VARIANT(expr, VAR_DECL, into_ptr(var_decl_expr{ $NAME, into_ptr(move($type_local)) })); }
    | "var" NAME { $$ = VARIANT(expr, VAR_DECL, into_ptr(var_decl_expr{ $NAME })); }

    | "true" { $$ = VARIANT(expr, CONST, into_ptr(const_expr{ VARIANT(const_expr, BOOL, true) } )); }
    | "false" { $$ = VARIANT(expr, CONST, into_ptr(const_expr{ VARIANT(const_expr, BOOL, false) } )); }
    | CHAR { $$ = VARIANT(expr, CONST, into_ptr(const_expr{ VARIANT(const_expr, CHAR, $CHAR) } )); }
    | STRING { $$ = VARIANT(expr, CONST, into_ptr(const_expr{ VARIANT(const_expr, STRING, move($STRING)) } )); }
    | INTEGER { $$ = VARIANT(expr, CONST, into_ptr(const_expr{ VARIANT(const_expr, INT, into_ptr($INTEGER)) } )); }
    | FLOAT { $$ = VARIANT(expr, CONST, into_ptr(const_expr{ VARIANT(const_expr, FLOAT, into_ptr($FLOAT)) } )); }

    | "!" expr[exp] { $$ = VARIANT(expr, UNARY_OPERATION, into_ptr(unary_operation_expr{ unary_operation_expr::NOT, into_ptr($exp)} )); }
    | expr[left] "&&" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::AND, into_ptr($left), into_ptr($right) })); }
    | expr[left] "||" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::OR, into_ptr($left), into_ptr($right) })); }

    | "-" expr[exp] %prec UNARY_MINUS { $$ = VARIANT(expr, UNARY_OPERATION, into_ptr(unary_operation_expr{ unary_operation_expr::MINUS, into_ptr($exp)} )); }
    | expr[left] "+" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::ADD, into_ptr($left), into_ptr($right) })); }
    | expr[left] "-" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::SUB, into_ptr($left), into_ptr($right) })); }
    | expr[left] "*" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::MUL, into_ptr($left), into_ptr($right) })); }
    | expr[left] "/" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::DIV, into_ptr($left), into_ptr($right) })); }
    | expr[left] "%" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::MOD, into_ptr($left), into_ptr($right) })); }

    | expr[exp] "as" type_local { $$ = VARIANT(expr, NUMERIC_CAST, into_ptr(numeric_cast_expr{ into_ptr($exp), into_ptr($type_local) })); }

    | "~" expr[exp] { $$ = VARIANT(expr, UNARY_OPERATION, into_ptr(unary_operation_expr{ unary_operation_expr::BIT_NEG, into_ptr($exp)} )); }
    | expr[left] "&" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::BIT_AND, into_ptr($left), into_ptr($right) })); }
    | expr[left] "|" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::BIT_OR, into_ptr($left), into_ptr($right) })); }
    | expr[left] "^" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::BIT_XOR, into_ptr($left), into_ptr($right) })); }
    | expr[left] "<<" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::BIT_LSH, into_ptr($left), into_ptr($right) })); }    
    | expr[left] ">>" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::BIT_RSH, into_ptr($left), into_ptr($right) })); }

    | expr[left] "==" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::EQ, into_ptr($left), into_ptr($right) })); }
    | expr[left] "!=" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::NEQ, into_ptr($left), into_ptr($right) })); }
    | expr[left] "<" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::LS, into_ptr($left), into_ptr($right) })); }
    | expr[left] "<=" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::LSEQ, into_ptr($left), into_ptr($right) })); }
    | expr[left] ">" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::GT, into_ptr($left), into_ptr($right) })); }
    | expr[left] ">=" expr[right] { $$ = VARIANT(expr, BINARY_OPERATION, into_ptr(binary_operation_expr{ binary_operation_expr::GTEQ, into_ptr($left), into_ptr($right) })); }

    | "none" { $$ = VARIANT(expr, NONE, monostate()); }
    | "some" expr[exp] { $$ = VARIANT(expr, SOME, into_ptr($exp)); }
    | "return" { $$ = VARIANT(expr, RETURN, optional<ptr<expr>>()); }
    | "return" expr[exp] { $$ = VARIANT(expr, RETURN, into_ptr($exp)); }
    | "break" { $$ = VARIANT(expr, BREAK, monostate()); }
    | "continue" { $$ = VARIANT(expr, CONTINUE, monostate()); }

    | "&" NAME %prec UNARY_AMP { $$ = VARIANT(expr, LOCAL_PTR, move($NAME)); }
    | "@" expr[exp] { $$ = VARIANT(expr, HEAP_ALLOC, into_ptr($exp)); }
    | "*" expr[exp] %prec UNARY_STAR { $$ = VARIANT(expr, DEREF, into_ptr($exp)); }

    | "[" expr[exp] ";" INTEGER "]" { $$ = VARIANT(expr, SIZED_ARRAY, into_ptr(sized_array_expr{ into_ptr($exp), $INTEGER.value })); }
    | "@" "[" expr[value] "#" expr[size] "]" { $$ = VARIANT(expr, HEAP_SLICE_ALLOC, into_ptr(heap_slice_alloc_expr{ into_ptr($value), into_ptr($size) })); }
    | "#" expr[exp] { $$ = VARIANT(expr, LENGTH, into_ptr($exp)); }

    | expr[exp] "." NAME { $$ = VARIANT(expr, EXTRACT, into_ptr(extract_expr{ VARIANT(extract_expr, FIELD, make_pair(into_ptr($exp), move($NAME)))} )); }
    | expr[exp] "." INTEGER { $$ = VARIANT(expr, EXTRACT, into_ptr(extract_expr{ VARIANT(extract_expr, COORD, make_pair(into_ptr($exp), $INTEGER.value ))} )); }
    | expr[left] "[" expr[right] "]" { $$ = VARIANT(expr, EXTRACT, into_ptr(extract_expr{ VARIANT(extract_expr, INDEX, make_pair(into_ptr($left), into_ptr($right) ))} )); }

    | "^" expr[exp] %prec UNARY_CARET { $$ = VARIANT(expr, PTR_EXTRACT, into_ptr(ptr_extract_expr{ VARIANT(ptr_extract_expr, OWNER, into_ptr($exp)) })); }
    | expr[exp] "->" NAME { $$ = VARIANT(expr, PTR_EXTRACT, into_ptr(ptr_extract_expr{ VARIANT(ptr_extract_expr, FIELD, make_pair(into_ptr($exp), move($NAME))) })); }
    | expr[exp] "->" INTEGER { $$ = VARIANT(expr, PTR_EXTRACT, into_ptr(ptr_extract_expr{ VARIANT(ptr_extract_expr, COORD, make_pair(into_ptr($exp), $INTEGER.value)) })); }
    | expr[left] "[" "ref" expr[right] "]" { $$ = VARIANT(expr, PTR_EXTRACT, into_ptr(ptr_extract_expr{ VARIANT(ptr_extract_expr, INDEX, make_pair(into_ptr($left), into_ptr($right))) })); }
    | expr[arr] "[" "ref" optional_expr[lrange] ".." optional_expr[rrange] "]" { $$ = VARIANT(expr, PTR_EXTRACT, into_ptr(ptr_extract_expr{ VARIANT(ptr_extract_expr, RANGE, make_pair(into_ptr($arr), make_pair(into_ptr($lrange), into_ptr($rrange)))) })); }

    | "func" optional_copying "(" func_param_seq ")" optional_return_type "{" func_body "}" { $$ = VARIANT(expr, LAMBDA, into_ptr(lambda_expr{ $optional_copying, into_ptr_vector($func_param_seq), into_optional_ptr($optional_return_type), into_ptr($func_body) })); }

optional_expr:
    %empty { $$ = TYPE($$)(); }
    | expr { $$ = { move($expr) }; }

expr_marked:
    expr { $$ = expr_marked{ VARIANT(expr_marked, EXPR, into_ptr($expr)) }; }
    | NAME ":" expr { $$ = expr_marked{ VARIANT(expr_marked, EXPR_WITH_NAME, make_pair(move($NAME), into_ptr($expr))) }; }
    | INTEGER ":" expr { $$ = expr_marked{ VARIANT(expr_marked, EXPR_WITH_COORD, make_pair($INTEGER.value, into_ptr($expr))) }; }

expr_marked_seq:
    %empty { $$ = TYPE($$)(); }
    | expr_marked_seq_nempty[head] { $$ = move($head); }

expr_marked_seq_nempty:
    expr_marked { $$ = TYPE($$)(); $$.push_back(move($expr_marked)); }
    | expr_marked_seq_nempty[head] "," expr_marked { $$ = move($head); $$.push_back(move($expr_marked)); }

// Type rules

type:
    NAME { $$ = VARIANT(type, USER_TYPE, move($NAME)); }

    | "bool" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::BOOL })); }
    | "i8" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::I8 })); }
    | "i16" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::I16 })); }
    | "i32" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::I32 })); }
    | "i64" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::I64 })); }
    | "u8" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::U8 })); }
    | "u16" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::U16 })); }
    | "u32" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::U32 })); }
    | "u64" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::U64 })); }
    | "f32" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::F32 })); }
    | "f64" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::F64 })); }
    | "never" { $$ = VARIANT(type, PRIMITIVE, into_ptr(primitive_type{ primitive_type::NEVER })); }

    | "(" type_seq ")" { $$ = VARIANT(type, TUPLE, into_ptr_vector($type_seq)); }
    | "[" type[item_type] ";" INTEGER "]" { $$ = VARIANT(type, ARRAY, into_ptr(array_type{ into_ptr($item_type), $INTEGER.value })); }
    | "?" type[val_type] { $$ = VARIANT(type, OPTIONAL, into_ptr($val_type)); }

    | "$" type_pointed { $$ = VARIANT(type, PTR, into_ptr(ptr_type{ ptr_type::GLOBAL, into_ptr($type_pointed) })); }
    | "&" type_pointed { $$ = VARIANT(type, PTR, into_ptr(ptr_type{ ptr_type::BASIC, into_ptr($type_pointed) })); }
    | "*" type_pointed { $$ = VARIANT(type, PTR, into_ptr(ptr_type{ ptr_type::SHARED, into_ptr($type_pointed) })); }
    | "~" type_pointed { $$ = VARIANT(type, PTR, into_ptr(ptr_type{ ptr_type::WEAK, into_ptr($type_pointed) })); }
    | "@" type_pointed { $$ = VARIANT(type, PTR, into_ptr(ptr_type{ ptr_type::UNIQUE, into_ptr($type_pointed) })); }

    | "$" type_pointed[outer] "." type_pointed[inner] { $$ = VARIANT(type, INNER_PTR, into_ptr(inner_ptr_type{ /*base*/{ ptr_type::GLOBAL, into_ptr($inner) }, into_ptr($outer) })); }
    | "&" type_pointed[outer] "." type_pointed[inner] { $$ = VARIANT(type, INNER_PTR, into_ptr(inner_ptr_type{ /*base*/{ ptr_type::BASIC, into_ptr($inner) }, into_ptr($outer) })); }
    | "*" type_pointed[outer] "." type_pointed[inner] { $$ = VARIANT(type, INNER_PTR, into_ptr(inner_ptr_type{ /*base*/{ ptr_type::SHARED, into_ptr($inner) }, into_ptr($outer) })); }
    | "~" type_pointed[outer] "." type_pointed[inner] { $$ = VARIANT(type, INNER_PTR, into_ptr(inner_ptr_type{ /*base*/{ ptr_type::WEAK, into_ptr($inner) }, into_ptr($outer) })); }
    | "@" type_pointed[outer] "." type_pointed[inner] { $$ = VARIANT(type, INNER_PTR, into_ptr(inner_ptr_type{ /*base*/{ ptr_type::UNIQUE, into_ptr($inner) }, into_ptr($outer) })); }

    | "func" "(" type_local_seq ")" "->" type[ret_type] { $$ = VARIANT(type, FUNC, into_ptr(func_type{ into_ptr_vector($type_local_seq), into_ptr($ret_type) })); }
    | "func" "$" "(" type_local_seq ")" "->" type[ret_type] { $$ = VARIANT(type, GLOBAL_FUNC, into_ptr(func_type{ into_ptr_vector($type_local_seq), into_ptr($ret_type) })); }
    | "func" "&" type_pointed "(" type_local_seq ")" "->" type[ret_type] { $$ = VARIANT(type, FUNC_WITH_PTR, into_ptr(func_with_ptr_type{ /*base*/{ into_ptr_vector($type_local_seq), into_ptr($ret_type) }, func_with_ptr_type::BASIC, into_ptr($type_pointed) })); }
    | "func" "*" type_pointed "(" type_local_seq ")" "->" type[ret_type] { $$ = VARIANT(type, FUNC_WITH_PTR, into_ptr(func_with_ptr_type{ /*base*/{ into_ptr_vector($type_local_seq), into_ptr($ret_type) }, func_with_ptr_type::SHARED, into_ptr($type_pointed) })); }
    | "func" "~" type_pointed "(" type_local_seq ")" "->" type[ret_type] { $$ = VARIANT(type, FUNC_WITH_PTR, into_ptr(func_with_ptr_type{ /*base*/{ into_ptr_vector($type_local_seq), into_ptr($ret_type) }, func_with_ptr_type::WEAK, into_ptr($type_pointed) })); }
    | "func" "@" type_pointed "(" type_local_seq ")" "->" type[ret_type] { $$ = VARIANT(type, FUNC_WITH_PTR, into_ptr(func_with_ptr_type{ /*base*/{ into_ptr_vector($type_local_seq), into_ptr($ret_type) }, func_with_ptr_type::UNIQUE, into_ptr($type_pointed) })); }

type_seq:
    %empty { $$ = TYPE($$)(); }
    | type_seq_nempty[head] { $$ = move($head); }

type_seq_nempty:
    type { $$ = TYPE($$)(); $$.push_back(move($type)); }
    | type_seq_nempty[head] "," type { $$ = move($head); $$.push_back(move($type)); }

type_pointed:
    type { $$ = { into_ptr($type), false }; }
    | "[" type "]" { $$ = { into_ptr($type), true }; }

type_local:
    type { $$ = { into_ptr($type), true }; }
    | "!" type { $$ = {into_ptr($type), false }; }

type_local_seq:
    %empty { $$ = TYPE($$)(); }
    | type_local_seq_nempty[head] { $$ = move($head); }

type_local_seq_nempty:
    type_local { $$ = TYPE($$)(); $$.push_back(move($type_local)); }
    | type_local_seq_nempty[head] "," type_local { $$ = move($head); $$.push_back(move($type_local)); }

%%

// Error reporting

#include <string>
#include <vector>
#include <optional>
#include <memory>

using std::string;
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
