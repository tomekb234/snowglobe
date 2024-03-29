#ifndef DIAGS_HPP
#define DIAGS_HPP

#include "diagcol.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "compiler/compiler.hpp"
#include "compiler/functions.hpp"
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
        error(location loc) : diagnostic(ERROR, loc) { }
    };

    struct warning : diagnostic {
        warning(location loc) : diagnostic(WARNING, loc) { }
    };

    #define DIAG0(name, base) \
        struct name : base { \
            name(location loc) : base(loc) { } \
            void write(ostream& stream) const override; \
        }

    #define DIAG1(name, base, type1, name1) \
        struct name : base { \
            type1 name1; \
            name(type1 name1, location loc) : base(loc), name1(move(name1)) { } \
            void write(ostream& stream) const override; \
        }

    #define DIAG2(name, base, type1, name1, type2, name2) \
        struct name : base { \
            type1 name1; \
            type2 name2; \
            name(type1 name1, type2 name2, location loc) : base(loc), name1(move(name1)), name2(move(name2)) { } \
            void write(ostream& stream) const override; \
        }

    #define DIAG3(name, base, type1, name1, type2, name2, type3, name3) \
        struct name : base { \
            type1 name1; \
            type2 name2; \
            type3 name3; \
            name(type1 name1, type2 name2, type3 name3, location loc) : base(loc), name1(move(name1)), name2(move(name2)), name3(move(name3)) { } \
            void write(ostream& stream) const override; \
        }

    #define DIAG4(name, base, type1, name1, type2, name2, type3, name3, type4, name4) \
        struct name : base { \
            type1 name1; \
            type2 name2; \
            type3 name3; \
            type4 name4; \
            name(type1 name1, type2 name2, type3 name3, type4 name4, location loc) \
                    : base(loc), name1(move(name1)), name2(move(name2)), name3(move(name3)), name4(move(name4)) { } \
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

    DIAG1(name_not_found, error, string, name);
    DIAG1(type_not_compiled, error, string, name);
    DIAG1(name_used, error, string, name);
    DIAG1(field_name_used, error, string, name);
    DIAG1(variant_name_used, error, string, name);
    DIAG3(invalid_kind, error, string, name, global_name_kind, kind, vector<global_name_kind>, expected_kinds);

    // Expression compilation related

    DIAG0(invalid_expression, error);
    DIAG0(expression_not_constant, error);
    DIAG0(expression_not_assignable, error);
    DIAG2(invalid_argument_count, error, size_t, count, size_t, expected);
    DIAG0(invalid_argument_marker, error);
    DIAG2(invalid_argument_index, error, size_t, index, size_t, argument_count);
    DIAG1(duplicate_argument_index, error, size_t, index);
    DIAG1(missing_argument, error, size_t, index);
    DIAG2(unknown_struct_field, error, const prog::struct_type&, st, string, name);
    DIAG2(unknown_enum_variant, error, const prog::enum_type&, en, string, name);
    DIAG2(unknown_function_parameter, error, const prog::global_func&, func, string, name);
    DIAG2(invalid_tuple_index, error, size_t, index, size_t, count);
    DIAG0(expected_enum_name, error);
    DIAG0(expression_not_swappable, error);

    // Typing related

    enum struct type_kind {
        NUMBER,
        INTEGER,
        OPTIONAL,
        TUPLE,
        ARRAY,
        STRUCT,
        ENUM,
        POINTER,
        SLICE_OR_ARRAY_POINTER,
        WEAK_POINTER,
        INNER_POINTER
    };

    DIAG3(not_convertible, error, const prog::program&, prog, prog::type, type, prog::type, new_type);
    DIAG3(no_common_supertype, error, const prog::program&, prog, prog::type, type_a, prog::type, type_b);
    DIAG2(type_not_copyable, error, const prog::program&, prog, prog::type, type);
    DIAG3(invalid_type, error, const prog::program&, prog, prog::type, type, type_kind, expected);
    DIAG4(int_overflow, error, unsigned long long, value, bool, negative, bool, signed_type, size_t, bits);
    DIAG3(invalid_unary_operation, error, const prog::program&, prog, ast::unary_operation_expr::operation_t, operation, prog::type, type);

    DIAG4(
        invalid_binary_operation,
        error,
        const prog::program&,
        prog,
        ast::binary_operation_expr::operation_t,
        operation,
        prog::type,
        left_type,
        prog::type,
        right_type
    );

    DIAG2(invalid_tuple_size, error, size_t, size, size_t, expected);
    DIAG2(invalid_array_size, error, size_t, size, size_t, expected);
    DIAG2(invalid_struct, error, const prog::struct_type&, st, const prog::struct_type&, expected);
    DIAG2(invalid_size_constant_type, error, const prog::program&, prog, prog::type, type);
    DIAG0(weak_pointer_dereference, error);
    DIAG0(slice_dereference, error);
    DIAG0(trivial_type_with_confinement_marker, warning);
    DIAG0(restrictive_pointer_type, warning);

    // Value confinement related

    enum struct value_kind {
        FUNCTION_RESULT,
        DEREFERENCE,
        ALLOCATION
    };

    DIAG1(confinement_mismatch, error, bool, confined);
    DIAG0(confinement_ambiguous, error);
    DIAG1(not_allowed_in_confined_context, error, value_kind, kind);

    // Function compilation related

    DIAG0(invalid_main_type, error);
    DIAG0(invalid_parameter_order, error);
    DIAG1(invalid_variable_name, error, string, name);
    DIAG1(variable_not_found, error, string, name);
    DIAG0(variable_without_type, error);
    DIAG2(variable_not_usable, error, optional<string>, name, var_state, state);
    DIAG2(variable_not_deletable, error, optional<string>, name, var_state, state);
    DIAG1(variable_moved_out_inside_loop, error, optional<string>, name);
    DIAG1(variable_outside_confinement, error, optional<string>, name);
    DIAG0(global_variable_moved_out, error);
    DIAG0(break_outside_loop, error);
    DIAG0(continue_outside_loop, error);
    DIAG0(missing_return, error);
    DIAG0(unreachable_code, warning);

    DIAG1(code_generator_fail, error, string, text); // TODO the code generator should not report any diagnostics
}

#endif
