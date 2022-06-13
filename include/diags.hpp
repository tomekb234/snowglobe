#ifndef DIAGS_HPP
#define DIAGS_HPP

#include "diagcol.hpp"
#include "ast.hpp"
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

    #define DIAG0(name, base) \
        struct name : base { \
            void write(ostream& stream) const override; \
        }

    #define DIAG1(name, base, type1, name1) \
        struct name : base { \
            type1 name1; \
            name(type1 name1) : name1(move(name1)) { } \
            void write(ostream& stream) const override; \
        }

    #define DIAG2(name, base, type1, name1, type2, name2) \
        struct name : base { \
            type1 name1; \
            type2 name2; \
            name(type1 name1, type2 name2) : name1(move(name1)), name2(move(name2)) { } \
            void write(ostream& stream) const override; \
        }

    #define DIAG3(name, base, type1, name1, type2, name2, type3, name3) \
        struct name : base { \
            type1 name1; \
            type2 name2; \
            type3 name3; \
            name(type1 name1, type2 name2, type3 name3) : name1(move(name1)), name2(move(name2)), name3(move(name3)) { } \
            void write(ostream& stream) const override; \
        }

    #define DIAG4(name, base, type1, name1, type2, name2, type3, name3, type4, name4) \
        struct name : base { \
            type1 name1; \
            type2 name2; \
            type3 name3; \
            type4 name4; \
            name(type1 name1, type2 name2, type3 name3, type4 name4) : name1(move(name1)), name2(move(name2)), name3(move(name3)), name4(move(name4)) { } \
            void write(ostream& stream) const override; \
        }

    DIAG0(not_implemented, error);

    // Lexer diagnostics

    DIAG1(int_token_overflow, error, string, number);
    DIAG1(float_token_overflow, error, string, number);
    DIAG1(invalid_escape_sequence, error, char, ch);
    DIAG1(multibyte_character_literal, error, string, literal);

    // Parser diagnostics

    DIAG1(parser_error, error, string, message);
    DIAG2(syntax_error, error, optional<string>, unexpected, vector<string>, expected);

    // Compiler diagnostics

    // Name resolution related

    DIAG1(name_not_declared, error, string, name);
    DIAG1(name_not_compiled, error, string, name);
    DIAG1(global_name_used, error, string, name);
    DIAG1(field_name_used, error, string, name);
    DIAG1(variant_name_used, error, string, name);
    DIAG3(invalid_kind, error, string, name, global_name_kind, kind, optional<global_name_kind>, expected_kind);

    // Expression compilation related

    DIAG0(invalid_expression, error);
    DIAG0(expression_not_constant, error);
    DIAG0(expression_not_assignable, error);
    DIAG2(invalid_argument_count, error, size_t, count, size_t, expected);
    DIAG0(invalid_argument_marker, error);
    DIAG2(invalid_argument_index, error, size_t, index, size_t, argument_count);
    DIAG1(reused_argument_index, error, size_t, index);
    DIAG1(missing_argument, error, size_t, index);
    DIAG2(invalid_struct_field, error, const prog::struct_type&, st, string, name);
    DIAG2(invalid_enum_variant, error, const prog::enum_type&, en, string, name);
    DIAG2(invalid_function_parameter, error, const prog::global_func&, func, string, name);
    DIAG0(expected_variant_name, error);
    DIAG4(int_overflow, error, unsigned long long, value, bool, negative, bool, signed_type, size_t, bits);
    DIAG1(single_float_overflow, error, double, value);
    DIAG3(invalid_unary_operation, error, const prog::program&, prog, ast::unary_operation_expr::operation_t, operation, prog::type, type);
    DIAG4(invalid_binary_operation, error, const prog::program&, prog, ast::binary_operation_expr::operation_t, operation, prog::type, left_type, prog::type, right_type);

    // Typing related

    DIAG3(not_convertible, error, const prog::program&, prog, prog::type, type, prog::type, new_type);
    DIAG1(confinement_mismatch, error, bool, confined);
    DIAG0(confinement_ambiguous, error);
    DIAG0(function_call_in_confined_context, error);
    DIAG3(no_common_supertype, error, const prog::program&, prog, prog::type, type_a, prog::type, type_b);
    DIAG2(type_not_copyable, error, const prog::program&, prog, prog::type, type);
    DIAG2(expected_array_type, error, const prog::program&, prog, prog::type, type);
    DIAG0(invalid_size_constant_type, error);
    DIAG0(restrictive_pointer_type, warning);

    // Function compilation related

    DIAG0(global_function_copyable, error);
    DIAG0(variable_without_type, error);
    DIAG4(variable_not_usable, error, string, name, bool, initialized, bool, uninitialized, bool, moved_out);
    DIAG1(variable_moved_inside_loop, error, string, name);
    DIAG1(global_variable_moved, error, string, name);
    DIAG0(missing_return, error);
    DIAG0(dead_code, warning);

    // Code generator diagnostics
    
    DIAG1(code_generator_fail, error, string, text);
}

#endif
