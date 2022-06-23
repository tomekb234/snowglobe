#include "diags.hpp"
#include "program.hpp"
#include <ostream>

namespace sg::diags {
    using std::endl;

    void not_implemented::write(ostream& stream) const {
        stream << "Not implemented yet" << endl;
    }

    void int_token_overflow::write(ostream& stream) const {
        stream << "The number '" << number << "' does not fit in 64-bit integer type" << endl;
    }

    void float_token_overflow::write(ostream& stream) const {
        stream << "The number '" << number << "' does not fit in double-precision floating-point type" << endl;
    }

    void invalid_escape_sequence::write(ostream& stream) const {
        stream << "Invalid character escape sequence \\" << string(1, ch) << endl;
    }

    void multibyte_character_literal::write(ostream& stream) const {
        stream << "The character literal " << literal << " contains a multibyte character" << endl;
    }

    void parser_error::write(ostream& stream) const {
        stream << message << endl;
    }

    void syntax_error::write(ostream& stream) const {
        stream << "Syntax error" << endl;
        if (unexpected)
            stream << "Unexpected token: " << *unexpected << endl;
        if (!expected.empty()) {
            stream << "Example expected tokens:";
            for (auto exp : expected)
                stream << " " << exp;
            stream << endl;
        }
    }

    void name_not_found::write(ostream& stream) const {
        stream << "Name '" << name << "' not found" << endl;
    }

    void type_not_compiled::write(ostream& stream) const {
        stream << "The type '" << name << "' cannot be used before its definition unless inside a pointer type" << endl;
    }

    void name_used::write(ostream& stream) const {
        stream << "The name '" << name << "' is already used" << endl;
    }

    void field_name_used::write(ostream& stream) const {
        stream << "The field name '" << name << "' is already used" << endl;
    }

    void variant_name_used::write(ostream& stream) const {
        stream << "The variant name '" << name << "' is already used" << endl;
    }

    void invalid_kind::write(ostream& stream) const {
        stream << "The name '" << name << "' may not refer to ";

        switch (kind) {
            case global_name_kind::VAR: stream << "a variable"; break;
            case global_name_kind::CONST: stream << "a constant"; break;
            case global_name_kind::FUNC: stream << "a function"; break;
            case global_name_kind::STRUCT: stream << "a struct"; break;
            case global_name_kind::ENUM: stream << "an enum"; break;
        }

        stream << endl;

        if (!expected_kinds.empty()) {
            stream << "Expected ";

            auto count = expected_kinds.size();

            for (size_t index = 0; index < count; index++) {
                auto expected_kind = expected_kinds[index];

                switch (expected_kind) {
                    case global_name_kind::VAR: stream << "a variable"; break;
                    case global_name_kind::CONST: stream << "a constant"; break;
                    case global_name_kind::FUNC: stream << "a function"; break;
                    case global_name_kind::STRUCT: stream << "a struct"; break;
                    case global_name_kind::ENUM: stream << "an enum"; break;
                }

                if (index < count - 2)
                    stream << ", ";
                else if (index == count - 2)
                    stream << " or ";
            }

            stream << endl;
        }
    }

    void invalid_expression::write(ostream& stream) const {
        stream << "Invalid expression" << endl;
    }

    void expression_not_constant::write(ostream& stream) const {
        stream << "Expression not constant" << endl;
    }

    void expression_not_assignable::write(ostream& stream) const {
        stream << "Expression not assignable" << endl;
    }

    void invalid_argument_count::write(ostream& stream) const {
        stream << "Expected " << expected << " arguments instead of " << count << endl;
    }

    void invalid_argument_marker::write(ostream& stream) const {
        stream << "Invalid argument marker" << endl;
    }

    void invalid_argument_index::write(ostream& stream) const {
        stream << "Invalid argument with index " << index << endl;
        stream << "Expected " << argument_count << " arguments" << endl;
    }

    void duplicate_argument_index::write(ostream& stream) const {
        stream << "Duplicate argument with index " << index << endl;
    }

    void missing_argument::write(ostream& stream) const {
        stream << "Missing argument with index " << index << endl;
    }

    void unknown_struct_field::write(ostream& stream) const {
        stream << "The struct '" << st.name << "' does not have a field '" << name << "'" << endl;
    }

    void unknown_enum_variant::write(ostream& stream) const {
        stream << "The enum '" << en.name << "' does not have a variant '" << name << "'" << endl;
    }

    void unknown_function_parameter::write(ostream& stream) const {
        stream << "The function ";
        if (func.name)
            stream << "'" << *func.name << "' ";
        stream << "does not have a parameter '" << name << "'" << endl;
    }

    void invalid_tuple_index::write(ostream& stream) const {
        stream << "Invalid field index " << index << " for tuple with " << count << " fields" << endl;
    }

    void expected_enum_name::write(ostream& stream) const {
        stream << "Expected enum name" << endl;
    };

    void expression_not_swappable::write(ostream& stream) const {
        stream << "Expression not swappable" << endl;
    }

    void not_convertible::write(ostream& stream) const {
        stream << "Cannot convert from type '";
        print_type(stream, prog, type);
        stream << "' to type '";
        print_type(stream, prog, new_type);
        stream << "'" << endl;
    }

    void no_common_supertype::write(ostream& stream) const {
        stream << "The types '";
        print_type(stream, prog, type_a);
        stream << "' and '";
        print_type(stream, prog, type_b);
        stream << "' have no common supertype" << endl;
    }

    void type_not_copyable::write(ostream& stream) const {
        stream << "The type '";
        print_type(stream, prog, type);
        stream << "' is not copyable" << endl;
    }

    void invalid_type::write(ostream& stream) const {
        stream << "Expected ";

        switch (expected) {
            case type_kind::NUMBER: stream << "a number "; break;
            case type_kind::INTEGER: stream << "an integer "; break;
            case type_kind::OPTIONAL: stream << "an optional value "; break;
            case type_kind::TUPLE: stream << "a tuple "; break;
            case type_kind::ARRAY: stream << "an array "; break;
            case type_kind::STRUCT: stream << "a struct "; break;
            case type_kind::ENUM: stream << "an enum "; break;
            case type_kind::POINTER: stream << "a pointer "; break;
            case type_kind::SLICE_OR_ARRAY_POINTER: stream << "a slice or an array pointer "; break;
            case type_kind::WEAK_POINTER: stream << "a weak shared pointer "; break;
            case type_kind::INNER_POINTER: stream << "an inner pointer "; break;
        }

        stream << "instead of value with type '";
        print_type(stream, prog, type);
        stream << "'" << endl;
    }

    void int_overflow::write(ostream& stream) const {
        stream << "The number '" << (negative ? "-" : "") << value << "' does not fit in " << (signed_type ? "signed" : "unsigned") << " " << bits << "-bit integer type" << endl;
    }

    void invalid_unary_operation::write(ostream& stream) const {
        stream << "The unary operator '";

        switch (operation) {
            case ast::unary_operation_expr::NOT: stream << "!"; break;
            case ast::unary_operation_expr::MINUS: stream << "-"; break;
            case ast::unary_operation_expr::BIT_NEG: stream << "~"; break;
        }

        stream << "' is not applicable to type '";
        print_type(stream, prog, type);
        stream << "'" << endl;
    }

    void invalid_binary_operation::write(ostream& stream) const {
        stream << "The binary operator '";

        switch (operation) {
            case ast::binary_operation_expr::AND: stream << "&&"; break;
            case ast::binary_operation_expr::OR: stream << "||"; break;
            case ast::binary_operation_expr::ADD: stream << "+"; break;
            case ast::binary_operation_expr::SUB: stream << "-"; break;
            case ast::binary_operation_expr::MUL: stream << "*"; break;
            case ast::binary_operation_expr::DIV: stream << "/"; break;
            case ast::binary_operation_expr::MOD: stream << "%"; break;
            case ast::binary_operation_expr::BIT_AND: stream << "&"; break;
            case ast::binary_operation_expr::BIT_OR: stream << "|"; break;
            case ast::binary_operation_expr::BIT_XOR: stream << "^"; break;
            case ast::binary_operation_expr::BIT_LSH: stream << "<<"; break;
            case ast::binary_operation_expr::BIT_RSH: stream << ">>"; break;
            case ast::binary_operation_expr::EQ: stream << "=="; break;
            case ast::binary_operation_expr::NEQ: stream << "!="; break;
            case ast::binary_operation_expr::LS: stream << "<"; break;
            case ast::binary_operation_expr::LSEQ: stream << "<="; break;
            case ast::binary_operation_expr::GT: stream << ">"; break;
            case ast::binary_operation_expr::GTEQ: stream << ">="; break;
        }

        stream << "' is not applicable to types '";
        print_type(stream, prog, left_type);
        stream << "' and '";
        print_type(stream, prog, right_type);
        stream << "'" << endl;
    }

    void invalid_tuple_size::write(ostream& stream) const {
        stream << "Expected tuple type with size " << expected << " instead of " << size << endl;
    }

    void invalid_array_size::write(ostream& stream) const {
        stream << "Expected array type with size " << expected << " instead of " << size << endl;
    }

    void invalid_struct::write(ostream& stream) const {
        stream << "Expected struct '" << expected.name << "' instead of '" << st.name << "'" << endl;
    }

    void invalid_size_constant_type::write(ostream& stream) const {
        stream << "Expected a constant with unsigned integer type instead of '";
        print_type(stream, prog, type);
        stream << "'" << endl;
    }

    void weak_pointer_dereference::write(ostream& stream) const {
        stream << "Cannot dereference a weak pointer" << endl;
    }

    void slice_dereference::write(ostream& stream) const {
        stream << "Cannot dereference a slice" << endl;
    }

    void trivial_type_with_confinement_marker::write(ostream& stream) const {
        stream << "Redundant confinement marker for trivial type" << endl;
    }

    void restrictive_pointer_type::write(ostream& stream) const {
        stream << "Restrictive pointer type for confined value" << endl;
        stream << "Use '&' instead" << endl;
    }

    void confinement_mismatch::write(ostream& stream) const {
        if (confined)
            stream << "Cannot leak a confined value" << endl;
        else
            stream << "Can only confine a value by passing it as an argument to a function or with a 'locally' block" << endl;
    }

    void confinement_ambiguous::write(ostream& stream) const {
        stream << "Either all or none non-trivial arguments must be confined" << endl;
    }

    void not_allowed_in_confined_context::write(ostream& stream) const {
        stream << "Cannot receive ";

        switch (kind) {
            case value_kind::FUNCTION_RESULT: stream << "a non-trivial function result "; break;
            case value_kind::DEREFERENCE: stream << "a non-trivial dereference result "; break;
            case value_kind::ALLOCATION: stream << "a pointer to newly allocated memory "; break;
        }

        stream << "in a confined context" << endl;
    }

    void invalid_main_type::write(ostream& stream) const {
        stream << "Invalid type for main function" << endl;
    }

    void invalid_parameter_order::write(ostream& stream) const {
        stream << "All function parameters with non-confined types must be ordered after parameters with confined types" << endl;
    }

    void invalid_variable_name::write(ostream& stream) const {
        stream << "The name '" << name << "' cannot be used for a variable" << endl;
    }

    void variable_not_found::write(ostream& stream) const {
        stream << "Local variable with name '" << name << "' not found" << endl;
    }

    void variable_without_type::write(ostream& stream) const {
        stream << "Type annotation required" << endl;
    }

    void variable_not_usable::write(ostream& stream) const {
        if (name)
            stream << "The variable '" << *name << "' ";
        else
            stream << "An internal variable ";
        stream << "was ";
        if (state & VAR_INITIALIZED)
            stream << "potentially ";
        if (state & VAR_UNINITIALIZED)
            stream << "unitialized ";
        if ((state & VAR_UNINITIALIZED) && (state & VAR_MOVED_OUT))
            stream << "or ";
        if (state & VAR_MOVED_OUT)
            stream << "moved out";
        stream << endl;
    }

    void variable_not_deletable::write(ostream& stream) const {
        stream << "The value of ";
        if (name)
            stream << "the variable '" << *name << "' ";
        else
            stream << "an internal variable ";
        stream << "cannot be deleted because it is either initialized";
        if ((state & VAR_UNINITIALIZED) && (state & VAR_MOVED_OUT))
            stream << ", uninitialized or moved out";
        else if (state & VAR_UNINITIALIZED)
            stream << " or uninitialized ";
        else if (state & VAR_MOVED_OUT)
            stream << " or moved out";
        stream << endl;
    }

    void variable_moved_out_inside_loop::write(ostream& stream) const {
        stream << "Cannot move out ";
        if (name)
            stream << "the variable '" << *name << "' ";
        else
            stream << "an internal variable ";
        stream << "multiple times with a loop" << endl;
    }

    void variable_outside_confinement::write(ostream& stream) const {
        stream << "Cannot assign a confined value to ";
        if (name)
            stream << "the variable '" << *name << "' ";
        else
            stream << "an internal variable ";
       stream << "from outside of current 'locally' block" << endl;
    }

    void global_variable_moved_out::write(ostream& stream) const {
        stream << "Cannot move out a global variable" << endl;
    }

    void break_outside_loop::write(ostream& stream) const {
        stream << "Cannot use 'break' statement outside of a loop" << endl;
    }

    void continue_outside_loop::write(ostream& stream) const {
        stream << "Cannot use 'continue' statement outside of a loop" << endl;
    }

    void missing_return::write(ostream& stream) const {
        stream << "Missing 'return' statement" << endl;
    }

    void unreachable_code::write(ostream& stream) const {
        stream << "Unreachable code" << endl;
    }

    void code_generator_fail::write(ostream& stream) const {
        stream << "Code generator verification failed:" << endl << text << endl;
    }
}
