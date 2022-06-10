#ifndef PROGRAM_HPP
#define PROGRAM_HPP

#include <vector>
#include <memory>
#include <optional>
#include <variant>
#include <string>
#include <utility>
#include <ostream>
#include <unordered_map>

namespace sg::prog {
    template<typename T>
    using ptr = std::unique_ptr<T>;

    using std::vector;
    using std::optional;
    using std::variant;
    using std::monostate;
    using std::string;
    using std::pair;
    using std::ostream;
    using std::unordered_map;

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
    struct primitive_type;
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
    struct make_const_instr;
    struct make_tuple_instr;
    struct make_array_instr;
    struct make_optional_instr;
    struct make_struct_instr;
    struct make_enum_variant_instr;
    struct make_global_ptr_instr;
    struct ptr_conversion_instr;
    struct make_inner_ptr_instr;
    struct extract_instr;
    struct test_optional_instr;
    struct primitive_conversion_instr;
    struct transform_instr;

    typedef size_t global_index;
    typedef size_t field_index;
    typedef size_t variant_index;
    typedef size_t var_index;
    typedef size_t reg_index;

    struct program {
        vector<ptr<global_var>> global_vars;
        vector<ptr<global_func>> global_funcs;
        vector<ptr<struct_type>> struct_types;
        vector<ptr<enum_type>> enum_types;
    };

    struct global_var {
        optional<string> name;
        ptr<type> tp;
        ptr<constant> value;
    };

    struct global_func {
        string name;
        vector<ptr<func_param>> params;
        unordered_map<string, size_t> param_names;
        ptr<type> return_tp;
        vector<ptr<type_local>> vars;
        ptr<instr_block> instrs;
    };

    struct func_param {
        string name;
        ptr<type_local> tp;
    };

    struct struct_field {
        string name;
        ptr<type> tp;
    };

    struct struct_type {
        string name;
        bool copyable;
        bool trivially_copyable;
        unordered_map<string, field_index> field_names;
        vector<ptr<struct_field>> fields;
    };

    struct enum_variant {
        string name;
        vector<ptr<type>> tps;
    };

    struct enum_type {
        string name;
        bool copyable;
        bool trivially_copyable;
        unordered_map<string, variant_index> variant_names;
        vector<ptr<enum_variant>> variants;
    };

    struct constant {
        enum {
            UNIT,
            BOOL,
            INT,
            FLOAT32,
            FLOAT64,
            STRUCT,
            ENUM,
            TUPLE,
            ARRAY,
            SIZED_ARRAY,
            SOME,
            NONE,
            GLOBAL_VAR_PTR,
            GLOBAL_FUNC_PTR
        };

        variant<
            monostate, // UNIT
            bool, // BOOL
            unsigned long long, // INT
            float, // FLOAT32
            double, // FLOAT64
            vector<ptr<constant>>, // STRUCT
            pair<variant_index, vector<ptr<constant>>>, // ENUM
            vector<ptr<constant>>, // TUPLE
            vector<ptr<constant>>, // ARRAY
            pair<ptr<constant>, size_t>, // SIZED_ARRAY
            ptr<constant>, // SOME
            monostate, // NONE
            global_index, // GLOBAL_VAR_PTR
            global_index // GLOBAL_FUNC_PTR
        > value;
    };

    struct type {
        enum {
            NEVER,
            UNIT,
            PRIMITIVE,
            STRUCT,
            ENUM,
            TUPLE,
            ARRAY,
            OPTIONAL,
            PTR,
            INNER_PTR,
            FUNC,
            GLOBAL_FUNC,
            FUNC_WITH_PTR,
            KNOWN_FUNC,
            STRUCT_CTOR,
            ENUM_CTOR
        };

        variant<
            monostate, // NEVER
            monostate, // UNIT
            ptr<primitive_type>, // PRIMITIVE
            global_index, // STRUCT
            global_index, // ENUM
            vector<ptr<type>>, // TUPLE
            ptr<array_type>, // ARRAY
            ptr<type>, // OPTIONAL
            ptr<ptr_type>, // PTR
            ptr<inner_ptr_type>, // INNER_PTR
            ptr<func_type>, // FUNC
            ptr<func_type>, // GLOBAL_FUNC
            ptr<func_with_ptr_type>, // FUNC_WITH_PTR
            global_index, // KNOWN_FUNC
            global_index, // STRUCT_CTOR
            pair<global_index, variant_index> // ENUM_CTOR
        > value;
    };

    struct primitive_type {
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
            MAKE_OPTIONAL,
            MAKE_STRUCT,
            MAKE_ENUM_VARIANT,
            MAKE_GLOBAL_FUNC_PTR,
            MAKE_SLICE,
            MAKE_INNER_PTR,
            MAKE_SHARED_PTR,
            MAKE_EMPTY_WEAK_PTR,
            MAKE_FAKE_SHARED_PTR,

            EXTRACT,
            TEST_OPTIONAL,
            EXTRACT_INNER_PTR,
            EXTRACT_OUTER_PTR,
            EXTRACT_PTR,
            EXTRACT_FUNC,

            ZERO_EXT,
            SIGNED_EXT,
            FLOAT_EXT,
            TRANSFORM_ARRAY,
            TRANSFORM_OPTIONAL,
            ADD_WEAK_REF,
            FORGET_REF_COUNTER
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
            ptr<make_optional_instr>, // MAKE_OPTIONAL
            ptr<make_struct_instr>, // MAKE_STRUCT
            ptr<make_enum_variant_instr>, // MAKE_ENUM_VARIANT
            ptr<make_global_ptr_instr>, // MAKE_GLOBAL_FUNC_PTR
            ptr<ptr_conversion_instr>, // MAKE_SLICE
            ptr<make_inner_ptr_instr>, // MAKE_INNER_PTR
            ptr<ptr_conversion_instr>, // MAKE_SHARED_PTR
            ptr<ptr_conversion_instr>, // MAKE_EMPTY_WEAK_PTR
            ptr<ptr_conversion_instr>, // MAKE_FAKE_SHARED_PTR

            ptr<extract_instr>, // EXTRACT
            ptr<test_optional_instr>, // TEST_OPTIONAL
            ptr<ptr_conversion_instr>, // EXTRACT_INNER_PTR
            ptr<ptr_conversion_instr>, // EXTRACT_OUTER_PTR
            ptr<ptr_conversion_instr>, // EXTRACT_PTR
            ptr<ptr_conversion_instr>, // EXTRACT_FUNC

            ptr<primitive_conversion_instr>, // ZERO_EXT
            ptr<primitive_conversion_instr>, // SIGNED_EXT
            ptr<primitive_conversion_instr>, // FLOAT_EXT
            ptr<transform_instr>, // TRANSFORM_ARRAY
            ptr<transform_instr>, // TRANSFORM_OPTIONAL
            ptr<ptr_conversion_instr>, // ADD_WEAK_REF
            ptr<ptr_conversion_instr> // FORGET_REF_COUNTER
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
        reg_index ptr;
        vector<reg_index> args;
        reg_index result;
    };

    struct make_const_instr {
        ptr<constant> value;
        reg_index result;
    };

    struct make_tuple_instr {
        vector<reg_index> items;
        reg_index result;
    };

    struct make_array_instr {
        vector<reg_index> items;
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

    struct make_global_ptr_instr {
        global_index var;
        reg_index result;
    };

    struct ptr_conversion_instr {
        reg_index value;
        reg_index result;
    };

    struct make_inner_ptr_instr {
        reg_index outer;
        reg_index inner;
        reg_index result;
    };

    struct extract_instr {
        reg_index value;
        size_t index;
        reg_index result;
    };

    struct test_optional_instr {
        reg_index value;
        reg_index result;
    };

    struct primitive_conversion_instr {
        reg_index value;
        ptr<primitive_type> tp;
        reg_index result;
    };

    struct transform_instr {
        reg_index value;
        reg_index extracted;
        vector<ptr<instr>> instrs;
        reg_index inner_result;
        reg_index result;
    };

    constant copy_constant(const constant& source);
    type copy_type(const type& source);
    type_local copy_type_local(const type_local& source);

    bool types_equal(const type& type1, const type& type2);
    bool types_local_equal(const type_local& type1, const type_local& type2);
    bool func_types_equal(const func_type& type1, const func_type& type2);

    func_type get_func_type(const global_func& func);

    void print_type(ostream& stream, const program& prog, const type& tp);
}

#endif
