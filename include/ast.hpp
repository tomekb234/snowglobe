#ifndef AST_HPP
#define AST_HPP

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
    struct compund_assignment_stmt;
    struct locally_stmt;
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
    struct for_base_stmt;
    struct for_range_stmt;
    struct for_slice_stmt;
    struct expr;
    struct expr_indexed;
    struct var_decl_expr;
    struct const_expr;
    struct int_token;
    struct float_token;
    struct unary_operator_expr;
    struct binary_operator_expr;
    struct as_expr;
    struct array_with_size_expr;
    struct heap_array_expr;
    struct extract_expr;
    struct ptr_extract_expr;
    struct lambda_expr;
    struct type;
    struct primitive_type;
    struct pointer_base;
    struct pointer_type;
    struct inner_pointer_type;
    struct func_type;
    struct func_with_ptr_type;
    struct ptype;
    struct mtype;

    struct program {
        vector<ptr<global_def>> global_defs;
    };

    struct global_def {
        enum {
            VAR_DEF,
            FUNC_DEF,
            STRUCT_DEF,
            ENUM_DEF
        };

        variant<
            ptr<var_def>,
            ptr<func_def>,
            ptr<struct_def>,
            ptr<enum_def>
        > value;
    };

    struct var_def {
        string name;
        optional<ptr<type>> var_type;
        ptr<expr> value;
    };

    struct func_def {
        string name;
        bool copying;
        vector<ptr<func_param>> params;
        optional<ptr<type>> return_type;
        ptr<func_body> body;
    };

    struct func_body {
        ptr<stmt_block> block;
        optional<ptr<expr>> return_expr;
    };

    struct func_param {
        string name;
        ptr<mtype> param_type;
    };

    struct struct_def {
        string name;
        bool copyable;
        vector<ptr<struct_field>> fields;
    };

    struct struct_field {
        string name;
        ptr<type> field_type;
    };

    struct enum_def {
        string name;
        bool copyable;
        vector<ptr<enum_variant>> variants;
    };

    struct enum_variant {
        string name;
        vector<ptr<type>> variant_types;
    };

    struct stmt {
        enum {
            EXPR_EVAL,
            ASSIGNMENT,
            COMPOUND_ASSIGNMENT,
            LOCALLY,
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
            ptr<compund_assignment_stmt>,
            ptr<locally_stmt>,
            ptr<swap_stmt>,
            ptr<swap_block_stmt>,
            ptr<if_stmt>,
            ptr<match_stmt>,
            ptr<while_stmt>,
            ptr<for_stmt>,
            ptr<func_def>
        > value;
    };

    struct stmt_block {
        vector<ptr<stmt>> stmts;
    };

    struct assignment_stmt {
        ptr<expr> lvalue;
        ptr<expr> value;
    };

    struct compund_assignment_stmt : assignment_stmt {
        enum {
            PLUS,
            MINUS,
            MUL,
            DIV,
            MOD,
            AND,
            OR,
            XOR,
            LSH,
            RSH
        } operation;
    };

    struct locally_stmt {
        vector<string> var_names;
        ptr<stmt_block> block;
    };

    struct swap_stmt {
        ptr<expr> left;
        ptr<expr> right;
    };

    struct swap_block_stmt {
        ptr<expr> left;
        ptr<expr_or_name_locally> right;
        ptr<stmt_block> block;
    };

    struct expr_or_name_locally {
        enum {
            EXPR,
            NAME_LOCALLY
        };

        variant<
            ptr<expr>,
            string
        > value;
    };

    struct if_stmt {
        vector<ptr<if_branch>> branches;
        optional<ptr<stmt_block>> else_branch;
    };

    struct if_branch {
        ptr<condition> cond;
        ptr<stmt_block> block;
    };

    struct condition {
        enum {
            CHECK_IF_TRUE,
            CHECK_IF_PRESENT
        };

        variant<
            ptr<expr>,
            pair<ptr<expr>, ptr<expr_or_name_locally>>
        > value;
    };

    struct match_stmt {
        ptr<expr_or_name_locally> value;
        vector<ptr<match_branch>> branches;
        optional<ptr<stmt_block>> else_branch;
    };

    struct match_branch {
        ptr<expr> lvalue;
        ptr<stmt_block> block;
    };

    struct while_stmt {
        ptr<condition> cond;
        ptr<stmt_block> block;
        optional<ptr<stmt_block>> else_block;
    };

    struct for_stmt {
        enum {
            RANGE,
            SLICE
        };

        variant<
            ptr<for_range_stmt>,
            ptr<for_slice_stmt>
        > value;
    };

    struct for_base_stmt {
        ptr<expr> lvalue;
        bool reversed;
        ptr<stmt_block> block;
        optional<ptr<stmt_block>> else_block;
    };

    struct for_range_stmt : for_base_stmt {
        ptr<expr> begin;
        ptr<expr> end;
    };

    struct for_slice_stmt : for_base_stmt {
        optional<ptr<expr>> index;
        bool by_ref;
        ptr<expr_or_name_locally> value;
    };

    struct expr {
        enum {
            TUPPLE,
            ARRAY,
            FUNCTION_CALL,
            NAME,
            SCOPED_NAME,
            VAR_DECL,
            CONST,
            UNARY_OPERATOR,
            BINARY_OPERATOR,
            AS,
            NONE,
            SOME,
            RETURN,
            BREAK,
            CONTINUE,
            LOCAL_PTR,
            HEAP_ALLOC,
            DEREF,
            ARRAY_WITH_SIZE,
            HEAP_ARRAY,
            LENGTH,
            EXTRACT,
            PTR_EXTRACT,
            LAMBDA
        };

        variant<
            vector<ptr<expr_indexed>>, //TUPPLE
            vector<ptr<expr_indexed>>, //ARRAY
            pair<ptr<expr>, vector<ptr<expr_indexed>>>, //FUNCTION_CALL
            string, // NAME
            pair<string, string>, // SCOPED_NAME
            ptr<var_decl_expr>, // VAR_DECL
            ptr<const_expr>, // CONST
            ptr<unary_operator_expr>, // UNARY_OPERATOR
            ptr<binary_operator_expr>, // BINARY_OPERATOR
            ptr<as_expr>, // AS
            monostate, // NONE
            ptr<expr>, // SOME
            optional<ptr<expr>>, // RETURN
            monostate, // BREAK
            monostate, // CONTINUE
            string, // LOCAL_PTR
            ptr<expr>, // HEAP_ALLOC
            ptr<expr>, // DEREF
            ptr<array_with_size_expr>, // ARRAY_WITH_SIZE,
            ptr<heap_array_expr>, // HEAP_ARRAY
            ptr<expr>, // LENGTH
            ptr<extract_expr>, // EXTRACT
            ptr<ptr_extract_expr>, // PTR_EXTRACT
            ptr<lambda_expr> // LAMBDA
        > value;
    };

    struct expr_indexed {
        enum {
            EXPR,
            NAMED,
            INDEXED
        };

        variant<
            ptr<expr>,
            pair<string, ptr<expr>>,
            pair<size_t, ptr<expr>>
        > value;
    };

    struct var_decl_expr {
        string name;
        optional<ptr<mtype>> type;
    };

    struct const_expr {
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

    struct int_token {
        unsigned long long value;

        enum {
            NONE,
            I, I8, I16, I32, I64,
            U, U8, U16, U32, U64
        } type_marker;
    };

    struct float_token {
        double value;

        enum {
            NONE,
            F, F32, F64
        } type_marker;
    };

    struct unary_operator_expr {
        enum {
            NOT,
            MINUS,
            BIT_NEG
        } operation;

        ptr<expr> value;
    };

    struct binary_operator_expr {
        enum {
            AND,
            OR,
            PLUS,
            MINUS,
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
            LEQ,
            GT,
            GEQ
        } operation;

        ptr<expr> left;
        ptr<expr> right;
    };

    struct as_expr {
        ptr<expr> value;
        ptr<mtype> as_type;
    };

    struct array_with_size_expr {
        ptr<expr> value;
        size_t size;
    };

    struct heap_array_expr {
        ptr<expr> value;
        ptr<expr> size;
    };

    struct extract_expr {
        enum {
            NAME,
            COORD,
            INDEX
        };

        variant<
            pair<ptr<expr>, string>,
            pair<ptr<expr>, size_t>,
            pair<ptr<expr>, ptr<expr>>
        > value;
    };

    struct ptr_extract_expr {
        enum {
            OUTER,
            NAME,
            COORD,
            INDEX,
            RANGE
        };

        variant<
            ptr<expr>,
            pair<ptr<expr>, string>,
            pair<ptr<expr>, size_t>,
            pair<ptr<expr>, ptr<expr>>,
            pair<ptr<expr>, pair<optional<expr>, optional<expr>>>
        > value;
    };

    struct lambda_expr {
        bool copying;
        vector<ptr<func_param>> params;
        optional<ptr<type>> return_type;
        ptr<func_body> body;
    };

    struct type {
        enum {
            NAME,
            PRIMITIVE,
            TUPLE,
            ARRAY,
            MAYBE,
            POINTER,
            INNER_POINTER,
            FUNC,
            GLOBAL_FUNC,
            FUNC_WITH_PTR
        };

        variant<
            string, //NAME
            ptr<primitive_type>, //PRIMITIVE
            vector<ptr<type>>, //TUPLE
            pair<ptr<type>, size_t>, //ARRAY
            ptr<type>, //MAYBE
            ptr<pointer_type>, //POINTER
            ptr<inner_pointer_type>, //INNER_POINTER
            ptr<func_type>, //FUNC
            ptr<func_type>, //GLOBAL_FUNC
            ptr<func_with_ptr_type> //FUNC_WITH_PTR
        > value;
    };

    struct primitive_type {
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
            NEVER
        } p_type;
    };

    struct pointer_base {
        enum {
            GLOBAL,
            BASIC,
            SHARED,
            WEAK,
            UNIQUE
        } kind;
    };

    struct pointer_type : pointer_base {
        ptr<ptype> target_type;
    };

    struct inner_pointer_type : pointer_base {
        ptr<ptype> outer_type;
        ptr<ptype> inner_type;
    };

    struct func_type {
        vector<ptr<mtype>> arg_types;
        ptr<type> ret_type;
    };

    struct func_with_ptr_type : func_type {
        enum {
            BASIC,
            SHARED,
            WEAK,
            UNIQUE
        } kind;

        ptr<ptype> ptr_type;
    };

    struct ptype {
        ptr<type> base_type;
        bool slice;
    };

    struct mtype {
        ptr<type> base_type;
        bool leakable;
    };
}

#endif
