#ifndef PROGRAM_HPP
#define PROGRAM_HPP

#include <vector>
#include <memory>
#include <optional>
#include <variant>
#include <string>
#include <utility>

namespace sg::prog {
    template<typename T>
    using ptr = std::unique_ptr<T>;

    using std::vector;
    using std::optional;
    using std::variant;
    using std::monostate;
    using std::string;
    using std::pair;

    struct program;
    struct global_var;
    struct global_func;
    struct struct_type;
    struct enum_type;
    struct constant;
    struct int_constant;
    struct float_constant;
    struct struct_constant;
    struct enum_constant;
    struct tuple_constant;
    struct array_constant;
    struct sized_array_constant;
    struct type;
    struct primitive_type;
    struct array_type;
    struct ptr_type;
    struct inner_ptr_type;
    struct func_type;
    struct func_with_ptr_type;
    struct type_pointed;
    struct type_local;

    struct program {
        vector<ptr<global_var>> global_vars;
        vector<ptr<global_func>> global_funcs;
        vector<ptr<struct_type>> struct_types;
        vector<ptr<enum_type>> enum_types;
    };

    struct global_var {
        string name;
        ptr<type> tp;
        ptr<constant> value;
    };

    struct global_func {
        // TODO
    };

    struct struct_type {
        // TODO
    };

    struct enum_type {
        // TODO
    };

    struct constant {
        enum {
            BOOL,
            INT,
            FLOAT,
            STRUCT,
            ENUM,
            TUPLE,
            ARRAY,
            SIZED_ARRAY,
            SOME,
            NONE,
            GLOBAL_PTR,
            GLOBAL_INNER_PTR,
            GLOBAL_FUNC_PTR
        };

        variant<
            bool, // BOOL
            ptr<int_constant>, // INT
            ptr<float_constant>, // FLOAT
            ptr<struct_constant>, // STRUCT
            ptr<enum_constant>, // ENUM
            ptr<tuple_constant>, // TUPLE
            ptr<array_constant>, // ARRAY
            ptr<sized_array_constant>, // SIZED_ARRAY
            ptr<constant>, // SOME
            monostate, // NONE
            size_t, // GLOBAL_PTR
            pair<size_t, vector<size_t>>, // GLOBAL_INNER_PTR
            size_t // GLOBAL_FUNC_PTR
        > value;
    };

    struct int_constant {
        enum {
            I8,
            I16,
            I32,
            I64
        } tp;

        unsigned long long value;
    };

    struct float_constant {
        enum {
            F32,
            F64
        } tp;

        double value;
    };

    struct struct_constant {
        size_t struct_id;
        vector<ptr<constant>> fields;
    };

    struct enum_constant {
        size_t enum_id;
        size_t variant_id;
        vector<ptr<constant>> values;
    };

    struct tuple_constant {
        vector<ptr<constant>> values;
    };

    struct array_constant {
        vector<ptr<constant>> values;
    };

    struct sized_array_constant {
        ptr<constant> value;
        size_t size;
    };

    struct type {
        enum {
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
            FUNC_WITH_PTR
        };

        variant<
            ptr<primitive_type>, // PRIMITIVE
            size_t, // STRUCT
            size_t, // ENUM
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
        } tp;
    };

    struct array_type {
        ptr<type> tp;
        size_t size;
    };

    struct ptr_type {
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

    struct func_type {
        vector<ptr<type_local>> param_tps;
        ptr<type> return_tp;
    };

    struct func_with_ptr_type : func_type {
        enum {
            BASIC,
            SHARED,
            WEAK,
            UNIQUE
        } kind;

        ptr<type_pointed> target_tp;
    };

    struct type_pointed {
        ptr<type> tp;
        bool slice;
    };


    struct type_local {
        ptr<type> tp;
        bool confined;
    };
}

#endif
