#ifndef AST_HPP
#define AST_HPP

#include "location.hpp"
#include <memory>
#include <vector>
#include <optional>
#include <variant>
#include <string>
#include <utility>

namespace sg::ast {
    template<typename T>
    using ptr = std::unique_ptr<T>;

    using std::vector;
    using std::optional;
    using std::variant;
    using std::monostate;
    using std::string;
    using std::pair;

    struct program;
    struct global_def;
    struct var_def;
    struct const_integer;
    struct func_def;
    struct func_body;
    struct func_param;
    struct struct_def;
    struct struct_field;
    struct enum_def;
    struct enum_variant;
    struct stmt;
    struct stmt_block;
    struct assignment_stmt;
    struct compound_assignment_stmt;
    struct locally_block_stmt;
    struct swap_stmt;
    struct swap_block_stmt;
    struct expr_or_name_locally;
    struct if_stmt;
    struct if_branch;
    struct condition;
    struct match_stmt;
    struct match_branch;
    struct while_stmt;
    struct for_stmt;
    struct for_stmt_base;
    struct for_range_stmt;
    struct for_slice_stmt;
    struct expr;
    struct expr_marked;
    struct var_decl_expr;
    struct literal_expr;
    struct int_token;
    struct float_token;
    struct unary_operation_expr;
    struct binary_operation_expr;
    struct numeric_cast_expr;
    struct sized_array_expr;
    struct heap_slice_alloc_expr;
    struct extract_expr;
    struct ptr_extract_expr;
    struct lambda_expr;
    struct type;
    struct primitive_type;
    struct array_type;
    struct ptr_type;
    struct inner_ptr_type;
    struct func_type;
    struct func_with_ptr_type;
    struct type_pointed;
    struct type_local;

    struct node {
        location loc;
    };

    struct program : node {
        vector<ptr<global_def>> global_defs;
    };

    struct global_def : node {
        enum {
            VAR_DEF,
            CONST_DEF,
            FUNC_DEF,
            STRUCT_DEF,
            ENUM_DEF
        };

        variant<
            ptr<var_def>,
            ptr<var_def>,
            ptr<func_def>,
            ptr<struct_def>,
            ptr<enum_def>
        > value;
    };

    struct var_def : node {
        string name;
        optional<ptr<type>> tp;
        ptr<expr> value;
    };

    struct const_integer : node {
        enum {
            INTEGER,
            NAME
        };

        variant<
            size_t,
            string
        > value;
    };

    struct func_def : node {
        string name;
        bool copying;
        vector<ptr<func_param>> params;
        optional<ptr<type>> return_tp;
        ptr<func_body> body;
    };

    struct func_body : node {
        ptr<stmt_block> block;
        optional<ptr<expr>> return_value;
    };

    struct func_param : node {
        string name;
        ptr<type_local> tp;
    };

    struct struct_def : node {
        string name;
        bool copyable;
        vector<ptr<struct_field>> fields;
    };

    struct struct_field : node {
        string name;
        ptr<type> tp;
    };

    struct enum_def : node {
        string name;
        bool copyable;
        vector<ptr<enum_variant>> variants;
    };

    struct enum_variant : node {
        string name;
        vector<ptr<type>> tps;
    };

    struct stmt : node {
        enum {
            EXPR_EVAL,
            ASSIGNMENT,
            COMPOUND_ASSIGNMENT,
            LOCALLY_BLOCK,
            SWAP,
            SWAP_BLOCK,
            IF,
            MATCH,
            WHILE,
            FOR,
            FUNC_DEF
        };

        variant<
            ptr<expr>,
            ptr<assignment_stmt>,
            ptr<compound_assignment_stmt>,
            ptr<locally_block_stmt>,
            ptr<swap_stmt>,
            ptr<swap_block_stmt>,
            ptr<if_stmt>,
            ptr<match_stmt>,
            ptr<while_stmt>,
            ptr<for_stmt>,
            ptr<func_def>
        > value;
    };

    struct stmt_block : node {
        vector<ptr<stmt>> stmts;
    };

    struct assignment_stmt : node {
        ptr<expr> lvalue;
        ptr<expr> value;
    };

    struct compound_assignment_stmt : assignment_stmt {
        enum {
            ADD,
            SUB,
            MUL,
            DIV,
            MOD,
            BIT_AND,
            BIT_OR,
            BIT_XOR,
            BIT_LSH,
            BIT_RSH
        } operation;
    };

    struct locally_block_stmt : node {
        vector<string> var_names;
        ptr<stmt_block> block;
    };

    struct swap_stmt : node {
        ptr<expr> left;
        ptr<expr> right;
    };

    struct swap_block_stmt : node {
        ptr<expr> left;
        ptr<expr_or_name_locally> right;
        ptr<stmt_block> block;
    };

    struct expr_or_name_locally : node {
        enum {
            EXPR,
            NAME_LOCALLY
        };

        variant<
            ptr<expr>,
            string
        > value;
    };

    struct if_stmt : node {
        vector<ptr<if_branch>> branches;
        optional<ptr<stmt_block>> else_branch;
    };

    struct if_branch : node {
        ptr<condition> cond;
        ptr<stmt_block> block;
    };

    struct condition : node {
        enum {
            CHECK_IF_TRUE,
            CHECK_IF_PRESENT
        };

        variant<
            ptr<expr>,
            pair<ptr<expr>, ptr<expr_or_name_locally>>
        > value;
    };

    struct match_stmt : node {
        ptr<expr_or_name_locally> value;
        vector<ptr<match_branch>> branches;
        optional<ptr<stmt_block>> else_branch;
    };

    struct match_branch : node {
        ptr<expr> lvalue;
        ptr<stmt_block> block;
    };

    struct while_stmt : node {
        ptr<condition> cond;
        ptr<stmt_block> block;
        optional<ptr<stmt_block>> else_block;
    };

    struct for_stmt : node {
        enum {
            RANGE,
            SLICE
        };

        variant<
            ptr<for_range_stmt>,
            ptr<for_slice_stmt>
        > value;
    };

    struct for_stmt_base : node {
        ptr<expr> lvalue;
        bool reversed;
        ptr<stmt_block> block;
        optional<ptr<stmt_block>> else_block;
    };

    struct for_range_stmt : for_stmt_base {
        ptr<expr> begin;
        ptr<expr> end;
    };

    struct for_slice_stmt : for_stmt_base {
        optional<ptr<expr>> index;
        bool by_ref;
        ptr<expr_or_name_locally> value;
    };

    struct expr : node {
        enum {
            TUPLE,
            ARRAY,
            APPLICATION,
            NAME,
            VARIANT_NAME,
            VAR_DECL,
            LITERAL,
            UNARY_OPERATION,
            BINARY_OPERATION,
            NUMERIC_CAST,
            NONE,
            SOME,
            RETURN,
            BREAK,
            CONTINUE,
            REFERENCE,
            HEAP_ALLOC,
            DEREFERENCE,
            SIZED_ARRAY,
            HEAP_SLICE_ALLOC,
            LENGTH,
            EXTRACT,
            PTR_EXTRACT,
            LAMBDA
        };

        variant<
            vector<ptr<expr_marked>>, // TUPLE
            vector<ptr<expr_marked>>, // ARRAY
            pair<ptr<expr>, vector<ptr<expr_marked>>>, // APPLICATION
            string, // NAME
            pair<string, string>, // VARIANT_NAME
            ptr<var_decl_expr>, // VAR_DECL
            ptr<literal_expr>, // LITERAL
            ptr<unary_operation_expr>, // UNARY_OPERATION
            ptr<binary_operation_expr>, // BINARY_OPERATION
            ptr<numeric_cast_expr>, // NUMERIC_CAST
            monostate, // NONE
            ptr<expr>, // SOME
            optional<ptr<expr>>, // RETURN
            monostate, // BREAK
            monostate, // CONTINUE
            string, // REFERENCE
            ptr<expr>, // HEAP_ALLOC
            ptr<expr>, // DEREFERENCE
            ptr<sized_array_expr>, // SIZED_ARRAY
            ptr<heap_slice_alloc_expr>, // HEAP_SLICE_ALLOC
            ptr<expr>, // LENGTH
            ptr<extract_expr>, // EXTRACT
            ptr<ptr_extract_expr>, // PTR_EXTRACT
            ptr<lambda_expr> // LAMBDA
        > value;
    };

    struct expr_marked : node {
        enum {
            EXPR,
            EXPR_WITH_NAME,
            EXPR_WITH_COORD
        };

        variant<
            ptr<expr>,
            pair<string, ptr<expr>>,
            pair<size_t, ptr<expr>>
        > value;
    };

    struct var_decl_expr : node {
        string name;
        optional<ptr<type_local>> tp;
    };

    struct literal_expr : node {
        enum {
            BOOL,
            CHAR,
            STRING,
            INT,
            FLOAT
        };

        variant<
            bool,
            char,
            string,
            ptr<int_token>,
            ptr<float_token>
        > value;
    };

    struct int_token : node {
        unsigned long long value;
        bool negative;

        enum marker_t {
            NONE,
            I, I8, I16, I32, I64,
            U, U8, U16, U32, U64
        } marker;
    };

    struct float_token : node {
        double value;
        bool negative;

        enum marker_t {
            NONE,
            F, F32, F64
        } marker;
    };

    struct unary_operation_expr : node {
        enum {
            NOT,
            MINUS,
            BIT_NEG
        } operation;

        ptr<expr> value;
    };

    struct binary_operation_expr : node {
        enum {
            AND,
            OR,
            ADD,
            SUB,
            MUL,
            DIV,
            MOD,
            BIT_AND,
            BIT_OR,
            BIT_XOR,
            BIT_LSH,
            BIT_RSH,
            EQ,
            NEQ,
            LS,
            LSEQ,
            GT,
            GTEQ
        } operation;

        ptr<expr> left;
        ptr<expr> right;
    };

    struct numeric_cast_expr : node {
        ptr<expr> value;
        ptr<type_local> tp;
    };

    struct sized_array_expr : node {
        ptr<expr> value;
        ptr<const_integer> size;
    };

    struct heap_slice_alloc_expr : node {
        ptr<expr> value;
        ptr<expr> size;
    };

    struct extract_expr : node {
        enum {
            FIELD,
            COORD,
            INDEX
        };

        variant<
            pair<ptr<expr>, string>,
            pair<ptr<expr>, size_t>,
            pair<ptr<expr>, ptr<expr>>
        > value;
    };

    struct ptr_extract_expr : node {
        enum {
            OWNER,
            FIELD,
            COORD,
            INDEX,
            RANGE
        };

        variant<
            ptr<expr>,
            pair<ptr<expr>, string>,
            pair<ptr<expr>, size_t>,
            pair<ptr<expr>, ptr<expr>>,
            pair<ptr<expr>, pair<optional<ptr<expr>>, optional<ptr<expr>>>>
        > value;
    };

    struct lambda_expr : node {
        bool copying;
        vector<ptr<func_param>> params;
        optional<ptr<type>> return_tp;
        ptr<func_body> body;
    };

    struct type : node {
        enum {
            NEVER,
            PRIMITIVE,
            USER_TYPE,
            TUPLE,
            ARRAY,
            OPTIONAL,
            PTR,
            INNER_PTR,
            FUNC,
            GLOBAL_FUNC,
            FUNC_WITH_PTR
        };

        variant<
            monostate, // NEVER
            ptr<primitive_type>, // PRIMITIVE
            string, // USER_TYPE
            vector<ptr<type>>, // TUPLE
            ptr<array_type>, // ARRAY
            ptr<type>, // OPTIONAL
            ptr<ptr_type>, // PTR
            ptr<inner_ptr_type>, // INNER_PTR
            ptr<func_type>, // FUNC
            ptr<func_type>, // GLOBAL_FUNC
            ptr<func_with_ptr_type> // FUNC_WITH_PTR
        > value;
    };

    struct primitive_type : node {
        enum {
            BOOL,
            I8,
            I16,
            I32,
            I64,
            U8,
            U16,
            U32,
            U64,
            F32,
            F64,
        } tp;
    };

    struct array_type : node {
        ptr<type> tp;
        ptr<const_integer> size;
    };

    struct ptr_type : node {
        enum {
            GLOBAL,
            BASIC,
            SHARED,
            WEAK,
            UNIQUE
        } kind;

        ptr<type_pointed> target_tp;
    };

    struct inner_ptr_type : ptr_type {
        ptr<type_pointed> owner_tp;
    };

    struct func_type : node {
        vector<ptr<type_local>> param_tps;
        ptr<type> return_tp;
    };

    struct func_with_ptr_type : func_type, ptr_type { };

    struct type_pointed : node {
        ptr<type> tp;
        bool slice;
    };

    struct type_local : node {
        ptr<type> tp;
        bool confined;
    };
}

#endif
