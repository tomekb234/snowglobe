#include "diags.hpp"
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
            stream << "Expected tokens:";
            for (auto exp : expected)
                stream << " " << exp;
            stream << endl;
        }
    }

    void name_not_declared::write(ostream& stream) const {
        stream << "The name '" << name << "' was not declared" << endl;
    }

    void name_not_compiled::write(ostream& stream) const {
        stream << "The name '" << name << "' was declared but is not yet compiled" << endl;
    }

    void global_name_used::write(ostream& stream) const {
        stream << "The global name '" << name << "' is already used" << endl;
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

        if (expected_kind) {
            stream << "Expected ";

            switch (*expected_kind) {
                case global_name_kind::VAR: stream << "a variable"; break;
                case global_name_kind::CONST: stream << "a constant"; break;
                case global_name_kind::FUNC: stream << "a function"; break;
                case global_name_kind::STRUCT: stream << "a struct"; break;
                case global_name_kind::ENUM: stream << "an enum"; break;
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
        stream << "Applied " << count << " arguments but expected " << expected << endl;
    }

    void invalid_argument_marker::write(ostream& stream) const {
        stream << "Invalid argument marker" << endl;
    }

    void invalid_argument_index::write(ostream& stream) const {
        stream << "Invalid argument with index " << index << endl;
        stream << "Expected " << argument_count << " arguments" << endl;
    }

    void reused_argument_index::write(ostream& stream) const {
        stream << "Reused argument with index " << index << endl;
    }

    void missing_argument::write(ostream& stream) const {
        stream << "Missing argument with index " << index << endl;
    }

    void invalid_struct_field::write(ostream& stream) const {
        stream << "The struct '" << st.name << "' does not have a field named '" << name << "'" << endl;
    }

    void invalid_enum_variant::write(ostream& stream) const {
        stream << "The enum '" << en.name << "' does not have a variant named '" << name << "'" << endl;
    }

    void invalid_function_parameter::write(ostream& stream) const {
        stream << "The function '" << func.name << "' has no parameter named '" << name << "'" << endl;
    }

    void expected_variant_name::write(ostream& stream) const {
        stream << "Expected variant name" << endl;
    };

    void int_overflow::write(ostream& stream) const {
        stream << "The number '" << (negative ? "-" : "") << value << "' does not fit in " << (signed_type ? "signed" : "unsigned") << " " << bits << "-bit integer type" << endl;
    }

    void single_float_overflow::write(ostream& stream) const {
        stream << "The number '" << value << "' does not fit in single-precision floating-point type" << endl;
    }

    void invalid_unary_operation::write(ostream& stream) const {
        stream << "The unary operator '";

        switch (operation) {
            case ast::unary_operation_expr::NOT: stream << "!"; break;
            case ast::unary_operation_expr::MINUS: stream << "-"; break;
            case ast::unary_operation_expr::BIT_NEG: stream << "~"; break;
        }

        stream << "' is not applicable to type ";
        prog::print_type(stream, prog, type);
        stream << endl;
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

        stream << "' is not applicable to types ";
        prog::print_type(stream, prog, left_type);
        stream << " and ";
        prog::print_type(stream, prog, right_type);
        stream << endl;
    }

    void not_convertible::write(ostream& stream) const {
        stream << "A value with type '";
        prog::print_type(stream, prog, type);
        stream << "' cannot be converted to a value with type '";
        prog::print_type(stream, prog, new_type);
        stream << "'" << endl;
    }

    void confinement_mismatch::write(ostream& stream) const {
        if (confined)
            stream << "Leaked confined value" << endl;
        else
            stream << "A value can only be confined when passed as an argument to a function" << endl;
    }

    void confinement_ambiguous::write(ostream& stream) const {
        stream << "Either all or none arguments can have confined types" << endl;
    }

    void function_call_in_confined_context::write(ostream& stream) const {
        stream << "Cannot receive a function result with non-trivial destructor in a confined context" << endl;
    }

    void no_common_supertype::write(ostream& stream) const {
        stream << "The types '";
        prog::print_type(stream, prog, type_a);
        stream << "' and '";
        prog::print_type(stream, prog, type_b);
        stream << "' have no common supertype" << endl;
    }

    void type_not_copyable::write(ostream& stream) const {
        stream << "The type '";
        prog::print_type(stream, prog, type);
        stream << "' is not copyable" << endl;
    }

    void expected_array_type::write(ostream& stream) const {
        stream << "Expression with invalid type '";
        prog::print_type(stream, prog, type);
        stream << "'" << endl;
        stream << "Expected an array type";
    }

    void invalid_size_constant_type::write(ostream& stream) const {
        stream << "Size constant without unsigned integer type" << endl;
    }

    void restrictive_pointer_type::write(ostream& stream) const {
        stream << "Restrictive pointer type" << endl;
        stream << "Use '&' instead" << endl;
    }

    void global_function_copyable::write(ostream& stream) const {
        stream << "Global function marked as copyable" << endl;
    }

    void variable_without_type::write(ostream& stream) const {
        stream << "Variable declared without type" << endl;
    }

    void variable_not_usable::write(ostream& stream) const {
        stream << "Variable '" << name << "' is ";
        if (initialized)
            stream << "potentially ";
        if (uninitialized)
            stream << "unitialized ";
        if (uninitialized && moved_out)
            stream << "or ";
        if (moved_out)
            stream << "moved out";
        stream << endl;
    }

    void missing_return::write(ostream& stream) const {
        stream << "Missing return statement" << endl;
    }

    void dead_code::write(ostream& stream) const {
        stream << "Dead code" << endl;
    }
}
