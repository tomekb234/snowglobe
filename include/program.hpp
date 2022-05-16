#ifndef PROGRAM_HPP
#define PROGRAM_HPP

#include <vector>
#include <memory>
#include <optional>
#include <variant>
#include <string>
#include <utility>
#include <ostream>

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

    struct program;
    struct global_var;
    struct global_func;
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
        ptr<func_type> tp;
        // TODO
    };

    struct struct_field {
        string name;
        ptr<type> tp;
    };

    struct struct_type {
        string name;
        bool copyable;
        vector<ptr<struct_field>> fields;
    };

    struct enum_variant {
        string name;
        vector<ptr<type>> tps;
    };

    struct enum_type {
        string name;
        bool copyable;
        vector<ptr<enum_variant>> variants;
    };

    struct constant {
        enum {
            UNIT,
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
            GLOBAL_VAR_PTR,
            GLOBAL_FUNC_PTR
        };

        variant<
            monostate, // UNIT
            bool, // BOOL
            unsigned long long, // INT
            double, // FLOAT
            vector<ptr<constant>>, // STRUCT
            pair<size_t, vector<ptr<constant>>>, // ENUM
            vector<ptr<constant>>, // TUPLE
            vector<ptr<constant>>, // ARRAY
            pair<ptr<constant>, size_t>, // SIZED_ARRAY
            ptr<constant>, // SOME
            monostate, // NONE
            size_t, // GLOBAL_VAR_PTR
            size_t // GLOBAL_FUNC_PTR
        > value;
    };

    struct type {
        enum {
            NEVER,
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
            STRUCT_CTOR,
            ENUM_CTOR
        };

        variant<
            monostate, // NEVER
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
            ptr<func_with_ptr_type>, // FUNC_WITH_PTR
            size_t, // STRUCT_CTOR
            pair<size_t, size_t> // ENUM_CTOR
        > value;
    };

    struct primitive_type {
        enum {
            UNIT,
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

    constant copy_constant(const constant& source);
    type copy_type(const type& source);
    array_type copy_array_type(const array_type& source);
    ptr_type copy_ptr_type(const ptr_type& source);
    inner_ptr_type copy_inner_ptr_type(const inner_ptr_type& source);
    func_type copy_func_type(const func_type& source);
    func_with_ptr_type copy_func_with_ptr_type(const func_with_ptr_type& source);
    type_pointed copy_type_pointed(const type_pointed& source);
    type_local copy_type_local(const type_local& source);

    void print_type(ostream& stream, const program& prog, const type& tp);
}

#endif
