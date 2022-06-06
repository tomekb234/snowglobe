#ifndef COMPILER_DIAGNOSTICS
#define COMPILER_DIAGNOSTICS

#include "diagnostics.hpp"
#include "program.hpp"
#include <string>
#include <ostream>

namespace sg::diags {
    using std::string;
    using std::ostream;

    struct name_used : error {
        string name;
        enum kind_t {GLOBAL, FIELD, VARIANT} kind;

        name_used(string name, kind_t kind) : name(name), kind(kind) { }
        void write(ostream& stream) const override;
    };

    struct name_not_declared : error {
        string name;

        name_not_declared(string name) : name(name) { }
        void write(ostream& stream) const override;
    };

    struct name_not_compiled : error {
        string name;

        name_not_compiled(string name) : name(name) { }
        void write(ostream& stream) const override;
    };

    struct invalid_expression : error {
        void write(ostream& stream) const override;
    };

    struct invalid_kind : error {
        void write(ostream& stream) const override;
    };

    struct invalid_type : error {
        void write(ostream& stream) const override;
    };

    struct not_subtype : error {
        const prog::program& program;
        prog::type subtype, supertype;

        not_subtype(const prog::program& program, const prog::type& subtype, const prog::type& supertype) : program(program), subtype(copy_type(subtype)), supertype(copy_type(supertype)) { }
        void write(ostream& stream) const override;
    };

    struct expression_not_constant : error {
        void write(ostream& stream) const override;
    };

    struct expression_not_left : error {
        void write(ostream& stream) const override;
    };

    struct expression_not_right : error {
        void write(ostream& stream) const override;
    };

    struct int_overflow : error {
        unsigned long long value;
        bool negative;
        bool signed_type;
        size_t bits;

        int_overflow(unsigned long long value, bool negative, bool signed_type, size_t bits) : value(value), negative(negative), signed_type(signed_type), bits(bits) { }
        void write(ostream& stream) const override;
    };

    struct single_float_overflow : error {
        double value;

        single_float_overflow(double value) : value(value) { }
        void write(ostream& stream) const override;
    };

    struct no_common_supertype : error {
        const prog::program& program;
        prog::type type1, type2;
        bool confined;

        no_common_supertype(const prog::program& program, const prog::type& type1, const prog::type& type2, bool confined) : program(program), type1(copy_type(type1)), type2(copy_type(type2)), confined(confined) { }
        void write(ostream& stream) const override;
    };

    struct not_convertible : error {
        const prog::program& program;
        prog::type type1, type2;
        bool confined;

        not_convertible(const prog::program& program, const prog::type& type1, const prog::type& type2, bool confined) : program(program), type1(copy_type(type1)), type2(copy_type(type2)), confined(confined) { }
        void write(ostream& stream) const override;
    };

    struct invalid_argument : error {
        size_t num_args;

        invalid_argument(size_t num_args) : num_args(num_args) { }
        void write(ostream& stream) const override;
    };

    struct reused_argument : error {
        size_t index;

        reused_argument(size_t index) : index(index) { }
        void write(ostream& stream) const override;
    };

    struct missing_argument : error {
        size_t index;

        missing_argument(size_t index) : index(index) { }
        void write(ostream& stream) const override;
    };

    struct invalid_enum_variant : error {
        string enum_name, variant_name;

        invalid_enum_variant(string enum_name, string variant_name) : enum_name(enum_name), variant_name(variant_name) { }
        void write(ostream& stream) const override;
    };

    struct invalid_struct_field : error {
        string struct_name, field_name;

        invalid_struct_field(string struct_name, string field_name) : struct_name(struct_name), field_name(field_name) { }
        void write(ostream& stream) const override;
    };

    struct invalid_size_constant_type : error {
        void write(ostream& stream) const override;
    };

    struct type_not_copyable : error {
        const prog::program& program;
        prog::type type;

        type_not_copyable(const prog::program& program, const prog::type& type) : program(program), type(copy_type(type)) { }
        void write(ostream& stream) const override;
    };

    struct global_func_copyable : error {
        void write(ostream& stream) const override;
    };

    struct variable_without_type : error {
        void write(ostream& stream) const override;
    };

    struct no_return : error {
        void write(ostream& stream) const override;
    };

    struct dead_code : warning {
        void write(ostream& stream) const override;
    };

    struct restrictive_ptr_type : warning {
        void write(ostream& stream) const override;
    };
}

#endif
