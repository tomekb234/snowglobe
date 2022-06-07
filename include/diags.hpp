#ifndef DIAGS_HPP
#define DIAGS_HPP

#include "diagcol.hpp"
#include "program.hpp"
#include "compiler.hpp"
#include <ostream>
#include <string>
#include <optional>
#include <vector>

namespace sg::diags {
    using std::ostream;
    using std::string;
    using std::optional;
    using std::vector;

    struct error : diagnostic {
        error() : diagnostic(ERROR) { }
    };

    struct warning : diagnostic {
        warning() : diagnostic(WARNING) { }
    };

    struct not_implemented : error {
        void write(ostream& stream) const override;
    };

    // Lexer diagnostics

    struct int_token_overflow : error {
        string number;

        int_token_overflow(string number) : number(number) { }
        void write(ostream& stream) const override;
    };

    struct float_token_overflow : error {
        string number;

        float_token_overflow(string number) : number(number) { }
        void write(ostream& stream) const override;
    };

    struct invalid_escape_sequence : error {
        char ch;

        invalid_escape_sequence(char ch) : ch(ch) { }
        void write(ostream& stream) const override;
    };

    struct multibyte_character_literal : error {
        string literal;

        multibyte_character_literal(string literal) : literal(literal) { }
        void write(ostream& stream) const override;
    };

    // Parser diagnostics

    struct parser_error : error {
        string message;

        parser_error(string message) : message(message) { }
        void write(ostream& stream) const override;
    };

    struct syntax_error : error {
        optional<string> unexpected;
        vector<string> expected;

        syntax_error(optional<string> unexpected, vector<string> expected) : unexpected(unexpected), expected(expected) { }
        void write(ostream& stream) const override;
    };

    // Compiler diagnostics

    // Name resolution related

    struct name_used : error {
        string name;

        enum kind_t {
            GLOBAL,
            FIELD,
            VARIANT
        } kind;

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

    struct invalid_kind : error {
        string name;
        global_name_kind kind;
        optional<global_name_kind> expected_kind;

        invalid_kind(string name, global_name_kind kind, optional<global_name_kind> expected_kind) : name(name), kind(kind), expected_kind(expected_kind) { }
        void write(ostream& stream) const override;
    };

    // Expression compilation related

    struct invalid_expression : error {
        void write(ostream& stream) const override;
    };

    struct expression_not_constant : error {
        void write(ostream& stream) const override;
    };

    struct expression_not_assignable : error {
        void write(ostream& stream) const override;
    };

    struct invalid_expression_marker : error {
        void write(ostream& stream) const override;
    };

    struct invalid_argument : error {
        size_t index;
        size_t num_args;

        invalid_argument(size_t index, size_t num_args) : index(index), num_args(num_args) { }
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

    struct expected_variant_name : error {
        void write(ostream& stream) const override;
    };

    struct invalid_struct_field : error {
        prog::struct_type& struct_type;
        string field_name;

        invalid_struct_field(prog::struct_type& struct_type, string field_name) : struct_type(struct_type), field_name(field_name) { }
        void write(ostream& stream) const override;
    };

    struct invalid_enum_variant : error {
        prog::enum_type& enum_type;
        string variant_name;

        invalid_enum_variant(prog::enum_type& enum_type, string variant_name) : enum_type(enum_type), variant_name(variant_name) { }
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

    // Typing related

    struct invalid_expression_type : error {
        const prog::program& program;
        prog::type type;

        enum kind_t {
            ARRAY
        } expected;

        invalid_expression_type(const prog::program& program, const prog::type& type, kind_t expected) : program(program), type(copy_type(type)), expected(expected) { }
        void write(ostream& stream) const override;
    };

    struct no_common_supertype : error {
        const prog::program& program;
        prog::type type1, type2;

        no_common_supertype(const prog::program& program, const prog::type& type1, const prog::type& type2) : program(program), type1(copy_type(type1)), type2(copy_type(type2)) { }
        void write(ostream& stream) const override;
    };

    struct not_convertible : error {
        const prog::program& program;
        prog::type type1, type2;

        not_convertible(const prog::program& program, const prog::type& type1, const prog::type& type2) : program(program), type1(copy_type(type1)), type2(copy_type(type2)) { }
        void write(ostream& stream) const override;
    };

    struct type_not_copyable : error {
        const prog::program& program;
        prog::type type;

        type_not_copyable(const prog::program& program, const prog::type& type) : program(program), type(copy_type(type)) { }
        void write(ostream& stream) const override;
    };

    struct invalid_size_constant_type : error {
        void write(ostream& stream) const override;
    };

    struct restrictive_ptr_type : warning {
        void write(ostream& stream) const override;
    };

    // Function compilation related

    struct global_func_copyable : error {
        void write(ostream& stream) const override;
    };

    struct variable_without_type : error {
        void write(ostream& stream) const override;
    };

    struct missing_return : error {
        void write(ostream& stream) const override;
    };

    struct dead_code : warning {
        void write(ostream& stream) const override;
    };
}

#endif
