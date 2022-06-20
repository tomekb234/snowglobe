#ifndef PROGRAM_HPP
#define PROGRAM_HPP

#include <vector>
#include <memory>
#include <optional>
#include <variant>
#include <string>
#include <utility>
#include <tuple>
#include <ostream>
#include <unordered_map>

namespace sg::prog {
    using std::vector;
    using std::optional;
    using std::variant;
    using std::monostate;
    using std::string;
    using std::pair;
    using std::tuple;
    using std::ostream;
    using std::unordered_map;

    template<typename T>
    using ptr = std::unique_ptr<T>;

    struct program;
    struct global_var;
    struct global_func;
    struct func_param;
    struct struct_type;
    struct struct_field;
    struct enum_type;
    struct enum_variant;
    struct constant;
    struct type;
    struct number_type;
    struct array_type;
    struct ptr_type;
    struct inner_ptr_type;
    struct func_type;
    struct func_with_ptr_type;
    struct type_pointed;
    struct type_local;

    struct instr;
    struct instr_block;
    struct read_var_instr;
    struct read_global_var_instr;
    struct write_var_instr;
    struct write_global_var_instr;
    struct return_instr;
    struct func_call_instr;
    struct func_ptr_call_instr;
    struct from_never_instr;
    struct make_const_instr;
    struct make_tuple_instr;
    struct make_array_instr;
    struct make_sized_array_instr;
    struct make_optional_instr;
    struct make_struct_instr;
    struct make_enum_variant_instr;
    struct get_global_ptr_instr;
    struct ptr_conversion_instr;
    struct unary_operation_instr;
    struct binary_operation_instr;
    struct numeric_binary_operation_instr;
    struct make_inner_ptr_instr;
    struct make_joint_func_ptr_instr;
    struct test_optional_instr;
    struct test_variant_instr;
    struct extract_item_instr;
    struct extract_field_instr;
    struct extract_optional_value_instr;
    struct extract_variant_field_instr;
    struct numeric_conversion_instr;
    struct transform_instr;
    struct alloc_instr;
    struct alloc_slice_instr;
    struct ptr_read_instr;
    struct ptr_read_item_instr;
    struct ptr_write_instr;
    struct ptr_write_item_instr;
    struct get_slice_length_instr;
    struct test_ref_count_instr;
    struct branch_instr;
    struct value_branch_instr;
    struct repeat_instr;
    struct repeat_static_instr;

    typedef size_t global_index;
    typedef size_t param_index;
    typedef size_t field_index;
    typedef size_t variant_index;
    typedef size_t var_index;
    typedef size_t reg_index;

    struct program {
        vector<ptr<global_var>> global_vars;
        vector<ptr<global_func>> global_funcs;
        vector<ptr<struct_type>> struct_types;
        vector<ptr<enum_type>> enum_types;
        global_index cleanup_func;
    };

    struct global_var {
        optional<string> name;
        ptr<type> tp;
        ptr<constant> value;
    };

    struct global_func {
        optional<string> name;
        vector<ptr<func_param>> params;
        unordered_map<string, param_index> param_names;
        ptr<type> return_tp;
        vector<ptr<type_local>> vars;
        ptr<instr_block> instrs;
    };

    struct func_param {
        optional<string> name;
        ptr<type_local> tp;
    };

    struct struct_field {
        string name;
        ptr<type> tp;
    };

    struct struct_type {
        string name;
        bool copyable;
        bool trivial;
        unordered_map<string, field_index> field_names;
        vector<ptr<struct_field>> fields;
        global_index destructor;
    };

    struct enum_type {
        string name;
        bool copyable;
        bool trivial;
        unordered_map<string, variant_index> variant_names;
        vector<ptr<enum_variant>> variants;
        global_index destructor;
    };

    struct enum_variant {
        string name;
        vector<ptr<type>> tps;
    };

    struct constant {
        enum {
            UNIT,
            NUMBER,
            STRUCT,
            ENUM,
            TUPLE,
            ARRAY,
            SIZED_ARRAY,
            OPTIONAL,
            GLOBAL_VAR_PTR,
            GLOBAL_VAR_SLICE,
            GLOBAL_FUNC_PTR,
            GLOBAL_FUNC_FAKE_JOINT_PTR
        };

        variant<
            monostate, // UNIT
            unsigned long long, // NUMBER
            vector<ptr<constant>>, // STRUCT
            pair<variant_index, vector<ptr<constant>>>, // ENUM
            vector<ptr<constant>>, // TUPLE
            vector<ptr<constant>>, // ARRAY
            pair<ptr<constant>, size_t>, // SIZED_ARRAY
            optional<ptr<constant>>, // OPTIONAL
            global_index, // GLOBAL_VAR_PTR
            global_index, // GLOBAL_VAR_SLICE
            global_index, // GLOBAL_FUNC_PTR
            global_index // GLOBAL_FUNC_FAKE_JOINT_PTR
        > value;
    };

    struct type {
        enum {
            NEVER,
            UNIT,
            NUMBER,
            STRUCT,
            ENUM,
            TUPLE,
            ARRAY,
            OPTIONAL,
            PTR,
            INNER_PTR,
            FUNC,
            FUNC_WITH_PTR,
            GLOBAL_FUNC,
            KNOWN_FUNC,
            STRUCT_CTOR,
            ENUM_CTOR
        };

        variant<
            monostate, // NEVER
            monostate, // UNIT
            ptr<number_type>, // NUMBER
            global_index, // STRUCT
            global_index, // ENUM
            vector<ptr<type>>, // TUPLE
            ptr<array_type>, // ARRAY
            ptr<type>, // OPTIONAL
            ptr<ptr_type>, // PTR
            ptr<inner_ptr_type>, // INNER_PTR
            ptr<func_type>, // FUNC
            ptr<func_with_ptr_type>, // FUNC_WITH_PTR
            ptr<func_type>, // GLOBAL_FUNC
            global_index, // KNOWN_FUNC
            global_index, // STRUCT_CTOR
            pair<global_index, variant_index> // ENUM_CTOR
        > value;
    };

    struct number_type {
        enum type_t {
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
            F64
        } tp;
    };

    struct array_type {
        ptr<type> tp;
        size_t size;
    };

    struct ptr_type {
        enum kind_t {
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

    struct func_type {
        vector<ptr<type_local>> param_tps;
        ptr<type> return_tp;
    };

    struct func_with_ptr_type : func_type, ptr_type { };

    struct type_pointed {
        ptr<type> tp;
        bool slice;
    };

    struct type_local {
        ptr<type> tp;
        bool confined;
    };

    struct instr_block {
        vector<ptr<instr>> instrs;
    };

    struct instr {
        enum {
            READ_VAR,
            READ_GLOBAL_VAR,
            WRITE_VAR,
            WRITE_GLOBAL_VAR,
            RETURN,
            FUNC_CALL,
            FUNC_PTR_CALL,

            MAKE_UNIT,
            MAKE_CONST,
            MAKE_TUPLE,
            MAKE_ARRAY,
            MAKE_SIZED_ARRAY,
            MAKE_OPTIONAL,
            MAKE_STRUCT,
            MAKE_ENUM_VARIANT,
            MAKE_INNER_PTR,
            MAKE_JOINT_FUNC_PTR,
            MAKE_FAKE_JOINT_FUNC_PTR,
            GET_GLOBAL_VAR_PTR,
            GET_GLOBAL_FUNC_PTR,
            FROM_NEVER,

            TEST_OPTIONAL,
            TEST_VARIANT,
            EXTRACT_ITEM,
            EXTRACT_FIELD,
            EXTRACT_OPTIONAL_VALUE,
            EXTRACT_VARIANT_FIELD,
            EXTRACT_INNER_PTR,
            EXTRACT_OUTER_PTR,
            EXTRACT_PTR,
            EXTRACT_FUNC_PTR,

            BOOL_NOT,
            INT_NEG,
            FLOAT_NEG,
            BIT_NEG,
            INCR,
            DECR,
            EQ,
            NEQ,
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
            LS,
            LSEQ,
            GT,
            GTEQ,

            TRUNC,
            ZERO_EXT,
            SIGNED_EXT,
            UINT_TO_FLOAT,
            SINT_TO_FLOAT,
            FLOAT_TRUNC,
            FLOAT_EXT,
            FLOAT_TO_UINT,
            FLOAT_TO_SINT,

            TRANSFORM_ARRAY,
            TRANSFORM_OPTIONAL,

            ALLOC,
            ALLOC_SLICE,
            ARRAY_PTR_INTO_SLICE,
            GET_SLICE_LENGTH,
            ALLOC_REF_COUNTER,
            ADD_FAKE_REF_COUNTER,
            FORGET_REF_COUNTER,
            PTR_READ,
            PTR_READ_ITEM,
            PTR_WRITE,
            PTR_WRITE_ITEM,
            INCR_REF_COUNT,
            INCR_WEAK_REF_COUNT,
            DECR_REF_COUNT,
            DECR_WEAK_REF_COUNT,
            TEST_REF_COUNT,
            TEST_ANY_REF_COUNT,
            DELETE,
            DELETE_REF_COUNTER,

            BRANCH,
            VALUE_BRANCH,
            LOOP,
            REPEAT,
            REPEAT_STATIC,
            CONTINUE_LOOP,
            BREAK_LOOP,
            ABORT
        };

        variant<
            ptr<read_var_instr>, // READ_VAR
            ptr<read_global_var_instr>, // READ_GLOBAL_VAR
            ptr<write_var_instr>, // WRITE_VAR
            ptr<write_global_var_instr>, // WRITE_GLOBAL_VAR
            ptr<return_instr>, // RETURN
            ptr<func_call_instr>, // FUNC_CALL
            ptr<func_ptr_call_instr>, // FUNC_PTR_CALL

            reg_index, // MAKE_UNIT
            ptr<make_const_instr>, // MAKE_CONST
            ptr<make_tuple_instr>, // MAKE_TUPLE
            ptr<make_array_instr>, // MAKE_ARRAY
            ptr<make_sized_array_instr>, // MAKE_SIZED_ARRAY
            ptr<make_optional_instr>, // MAKE_OPTIONAL
            ptr<make_struct_instr>, // MAKE_STRUCT
            ptr<make_enum_variant_instr>, // MAKE_ENUM_VARIANT
            ptr<make_inner_ptr_instr>, // MAKE_INNER_PTR
            ptr<make_joint_func_ptr_instr>, // MAKE_JOINT_FUNC_PTR
            ptr<ptr_conversion_instr>, // MAKE_FAKE_JOINT_FUNC_PTR
            ptr<get_global_ptr_instr>, // GET_GLOBAL_VAR_PTR
            ptr<get_global_ptr_instr>, // GET_GLOBAL_FUNC_PTR
            ptr<from_never_instr>, // FROM_NEVER

            ptr<test_optional_instr>, // TEST_OPTIONAL
            ptr<test_variant_instr>, // TEST_ENUM_VARIANT
            ptr<extract_item_instr>, // EXTRACT_ITEM
            ptr<extract_field_instr>, // EXTRACT_FIELD
            ptr<extract_optional_value_instr>, // EXTRACT_OPTIONAL_VALUE
            ptr<extract_variant_field_instr>, // EXTRACT_VARIANT_FIELD
            ptr<ptr_conversion_instr>, // EXTRACT_INNER_PTR
            ptr<ptr_conversion_instr>, // EXTRACT_OUTER_PTR
            ptr<ptr_conversion_instr>, // EXTRACT_PTR
            ptr<ptr_conversion_instr>, // EXTRACT_FUNC_PTR

            ptr<unary_operation_instr>, // BOOL_NOT
            ptr<unary_operation_instr>, // INT_NEG
            ptr<unary_operation_instr>, // FLOAT_NEG
            ptr<unary_operation_instr>, // BIT_NEG
            ptr<unary_operation_instr>, // INCR
            ptr<unary_operation_instr>, // DECR
            ptr<binary_operation_instr>, // EQ
            ptr<binary_operation_instr>, // NEQ
            ptr<numeric_binary_operation_instr>, // ADD
            ptr<numeric_binary_operation_instr>, // SUB
            ptr<numeric_binary_operation_instr>, // MUL
            ptr<numeric_binary_operation_instr>, // DIV
            ptr<numeric_binary_operation_instr>, // MOD
            ptr<numeric_binary_operation_instr>, // BIT_AND
            ptr<numeric_binary_operation_instr>, // BIT_OR
            ptr<numeric_binary_operation_instr>, // BIT_XOR
            ptr<numeric_binary_operation_instr>, // BIT_LSH
            ptr<numeric_binary_operation_instr>, // BIT_RSH
            ptr<numeric_binary_operation_instr>, // LS
            ptr<numeric_binary_operation_instr>, // LSEQ
            ptr<numeric_binary_operation_instr>, // GT
            ptr<numeric_binary_operation_instr>, // GTEQ

            ptr<numeric_conversion_instr>, // TRUNC
            ptr<numeric_conversion_instr>, // ZERO_EXT
            ptr<numeric_conversion_instr>, // SIGNED_EXT
            ptr<numeric_conversion_instr>, // UINT_TO_FLOAT
            ptr<numeric_conversion_instr>, // SINT_TO_FLOAT
            ptr<numeric_conversion_instr>, // FLOAT_TRUNC
            ptr<numeric_conversion_instr>, // FLOAT_EXT
            ptr<numeric_conversion_instr>, // FLOAT_TO_UINT
            ptr<numeric_conversion_instr>, // FLOAT_TO_SINT

            ptr<transform_instr>, // TRANSFORM_ARRAY
            ptr<transform_instr>, // TRANSFORM_OPTIONAL

            ptr<alloc_instr>, // ALLOC
            ptr<alloc_slice_instr>, // ALLOC_SLICE
            ptr<ptr_conversion_instr>, // ARRAY_PTR_INTO_SLICE
            ptr<get_slice_length_instr>, // GET_SLICE_LENGTH
            ptr<ptr_conversion_instr>, // ALLOC_REF_COUNTER
            ptr<ptr_conversion_instr>, // ADD_FAKE_REF_COUNTER
            ptr<ptr_conversion_instr>, // FORGET_REF_COUNTER
            ptr<ptr_read_instr>, // PTR_READ
            ptr<ptr_read_item_instr>, // PTR_READ_ITEM
            ptr<ptr_write_instr>, // PTR_WRITE
            ptr<ptr_write_item_instr>, // PTR_WRITE_ITEM
            reg_index, // INCR_REF_COUNT
            reg_index, // INCR_WEAK_REF_COUNT
            reg_index, // DECR_REF_COUNT
            reg_index, // DECR_WEAK_REF_COUNT
            ptr<test_ref_count_instr>, // TEST_REF_COUNT
            ptr<test_ref_count_instr>, // TEST_ANY_REF_COUNT
            reg_index, // DELETE
            reg_index, // DELETE_REF_COUNTER

            ptr<branch_instr>, // BRANCH
            ptr<value_branch_instr>, // VALE_BRANCH
            ptr<instr_block>, // LOOP
            ptr<repeat_instr>, // REPEAT
            ptr<repeat_static_instr>, // REPEAT_STATIC
            monostate, // CONTINUE_LOOP
            monostate, // BREAK_LOOP
            monostate // ABORT
        > value;
    };

    struct read_var_instr {
        var_index var;
        reg_index result;
    };

    struct read_global_var_instr {
        global_index var;
        reg_index result;
    };

    struct write_var_instr {
        var_index var;
        reg_index value;
    };

    struct write_global_var_instr {
        global_index var;
        reg_index value;
    };

    struct return_instr {
        reg_index value;
    };

    struct func_call_instr {
        global_index func;
        vector<reg_index> args;
        reg_index result;
    };

    struct func_ptr_call_instr {
        reg_index func_ptr;
        vector<reg_index> args;
        reg_index result;
    };

    struct make_const_instr {
        ptr<constant> value;
        ptr<type> tp;
        reg_index result;
    };

    struct make_tuple_instr {
        vector<reg_index> values;
        reg_index result;
    };

    struct make_array_instr {
        vector<reg_index> values;
        reg_index result;
    };

    struct make_sized_array_instr {
        reg_index value;
        size_t size;
        reg_index result;
    };

    struct make_optional_instr {
        optional<reg_index> value;
        reg_index result;
    };

    struct make_struct_instr {
        global_index st;
        vector<reg_index> args;
        reg_index result;
    };

    struct make_enum_variant_instr {
        global_index en;
        variant_index variant;
        vector<reg_index> args;
        reg_index result;
    };

    struct from_never_instr {
        ptr<type> tp;
        reg_index result;
    };

    struct get_global_ptr_instr {
        global_index index;
        reg_index result;
    };

    struct ptr_conversion_instr {
        reg_index value;
        reg_index result;
    };

    struct unary_operation_instr {
        reg_index value;
        reg_index result;
    };

    struct binary_operation_instr {
        reg_index left;
        reg_index right;
        reg_index result;
    };

    struct numeric_binary_operation_instr : binary_operation_instr {
        enum kind_t {
            UNSIGNED,
            SIGNED,
            FLOAT
        } kind;
    };

    struct make_inner_ptr_instr {
        reg_index outer;
        reg_index inner;
        reg_index result;
    };

    struct make_joint_func_ptr_instr {
        reg_index ptr;
        reg_index func_ptr;
        reg_index result;
    };

    struct test_optional_instr {
        reg_index value;
        reg_index result;
    };

    struct test_variant_instr {
        reg_index value;
        variant_index variant;
        reg_index result;
    };

    struct extract_item_instr {
        reg_index value;
        size_t item;
        reg_index result;
    };

    struct extract_field_instr {
        reg_index value;
        field_index field;
        reg_index result;
    };

    struct extract_optional_value_instr {
        reg_index value;
        reg_index result;
    };

    struct extract_variant_field_instr {
        reg_index value;
        variant_index variant;
        field_index field;
        reg_index result;
    };

    struct numeric_conversion_instr {
        reg_index value;
        ptr<number_type> new_type;
        reg_index result;
    };

    struct transform_instr {
        reg_index value;
        reg_index extracted;
        ptr<instr_block> block;
        reg_index transformed;
        reg_index result;
    };

    struct alloc_instr {
        reg_index value;
        reg_index result;
    };

    struct alloc_slice_instr {
        reg_index value;
        reg_index size;
        reg_index result;
    };

    struct ptr_read_instr {
        reg_index ptr;
        reg_index result;
    };

    struct ptr_read_item_instr {
        reg_index ptr;
        size_t item;
        reg_index result;
    };

    struct ptr_write_instr {
        reg_index ptr;
        reg_index value;
    };

    struct ptr_write_item_instr {
        reg_index ptr;
        size_t item;
        reg_index value;
    };

    struct get_slice_length_instr {
        reg_index ptr;
        reg_index result;
    };

    struct test_ref_count_instr {
        reg_index ptr;
        reg_index result;
    };

    struct branch_instr {
        reg_index cond;
        ptr<instr_block> true_block;
        ptr<instr_block> false_block;
    };

    struct value_branch_instr : branch_instr {
        reg_index true_value;
        reg_index false_value;
        reg_index result;
    };

    struct repeat_instr {
        reg_index count;
        reg_index index;
        ptr<instr_block> block;
    };

    struct repeat_static_instr {
        size_t count;
        reg_index index;
        ptr<instr_block> block;
    };

    extern const type NEVER_TYPE;
    extern const type UNIT_TYPE;
    extern const type BOOL_TYPE;
    extern const type CHAR_TYPE;
    extern const type SIZE_TYPE;
    extern const type UNIT_PTR_TYPE;

    extern const type_local NEVER_TYPE_LOCAL;
    extern const type_local UNIT_TYPE_LOCAL;
    extern const type_local BOOL_TYPE_LOCAL;
    extern const type_local CHAR_TYPE_LOCAL;
    extern const type_local SIZE_TYPE_LOCAL;
    extern const type_local UNIT_PTR_TYPE_LOCAL;

    type make_ptr_type(type&& tp, ptr_type::kind_t kind, bool slice);

    constant copy_const(const constant& con);
    type copy_type(const type& tp);
    type_local copy_type_local(const type_local& tp);

    bool types_equal(const type& type_a, const type& type_b);
    bool types_local_equal(const type_local& type_a, const type_local& type_b);
    bool func_types_equal(const func_type& type_a, const func_type& type_b);

    func_type get_func_type(const global_func& func);

    void print_type(ostream& stream, const program& prog, const type& tp);
}

#endif
