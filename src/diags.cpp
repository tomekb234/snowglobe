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

    void name_used::write(ostream& stream) const {
        stream << "The ";

        switch (kind) {
            case GLOBAL: stream << "global"; break;
            case FIELD: stream << "field"; break;
            case VARIANT: stream << "variant"; break;
        }

        stream << " name '" << name << "' is already used" << endl;
    }

    void name_not_declared::write(ostream& stream) const {
        stream << "The name '" << name << "' was not declared" << endl;
    }

    void name_not_compiled::write(ostream& stream) const {
        stream << "The name '" << name << "' was declared but is not yet compiled" << endl;
    }

    void invalid_kind::write(ostream& stream) const {
        stream << "The name '" << name << "' may not refer to a";

        switch (kind) {
            case global_name_kind::VARIABLE: stream << " variable"; break;
            case global_name_kind::CONSTANT: stream << " constant"; break;
            case global_name_kind::FUNCTION: stream << " function"; break;
            case global_name_kind::STRUCT: stream << " struct"; break;
            case global_name_kind::ENUM: stream << "n enum"; break;
        }

        stream << endl;

        if (expected_kind) {
            stream << "Expected a";

            switch (*expected_kind) {
                case global_name_kind::VARIABLE: stream << " variable"; break;
                case global_name_kind::CONSTANT: stream << " constant"; break;
                case global_name_kind::FUNCTION: stream << " function"; break;
                case global_name_kind::STRUCT: stream << " struct"; break;
                case global_name_kind::ENUM: stream << "n enum"; break;
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

    void invalid_expression_marker::write(ostream& stream) const {
        stream << "Invalid expression marker" << endl;
    }

    void invalid_argument::write(ostream& stream) const {
        stream << "Invalid argument with index " << index << endl;
        stream << "Expected " << num_args << " arguments" << endl;
    }

    void reused_argument::write(ostream& stream) const {
        stream << "Reused argument with index " << index << endl;
    }

    void missing_argument::write(ostream& stream) const {
        stream << "Missing argument with index " << index << endl;
    }

    void expected_variant_name::write(ostream& stream) const {
        stream << "Expected variant name" << endl;
    };

    void invalid_struct_field::write(ostream& stream) const {
        stream << "The struct '" << struct_type.name << "' does not have a field named '" << field_name << "'" << endl;
    }

    void invalid_enum_variant::write(ostream& stream) const {
        stream << "The enum '" << enum_type.name << "' does not have a variant named '" << variant_name << "'" << endl;
    }

    void int_overflow::write(ostream& stream) const {
        stream << "The number '" << (negative ? "-" : "") << value << "' does not fit in " << (signed_type ? "signed" : "unsigned") << " " << bits << "-bit integer type" << endl;
    }

    void single_float_overflow::write(ostream& stream) const {
        stream << "The number '" << value << "' does not fit in single-precision floating-point type" << endl;
    }

    void invalid_expression_type::write(ostream& stream) const {
        stream << "Expression with invalid type '";
        prog::print_type(stream, program, type);
        stream << "'" << endl;

        stream << "Expected a";

        switch (expected) {
            case ARRAY: stream << "n array"; break;
        }

        stream << endl;
    }

    void no_common_supertype::write(ostream& stream) const {
        stream << "The types '";
        prog::print_type(stream, program, type1);
        stream << "' and '";
        prog::print_type(stream, program, type2);
        stream << "' have no common supertype" << endl;
    }

    void not_convertible::write(ostream& stream) const {
        stream << "A value with type '";
        prog::print_type(stream, program, type1);
        stream << "' cannot be converted to a value of type '";
        prog::print_type(stream, program, type2);
        stream << "'" << endl;
    }

    void type_not_copyable::write(ostream& stream) const {
        stream << "The type '";
        prog::print_type(stream, program, type);
        stream << "' is not copyable" << endl;
    }

    void invalid_size_constant_type::write(ostream& stream) const {
        stream << "Size constant without unsigned integer type" << endl;
    }

    void restrictive_ptr_type::write(ostream& stream) const {
        stream << "Restrictive pointer type" << endl;
        stream << "Use '&' instead" << endl;
    }

    void global_func_copyable::write(ostream& stream) const {
        stream << "Global function marked as copyable" << endl;
    }

    void variable_without_type::write(ostream& stream) const {
        stream << "Variable declared without type" << endl;
    }

    void missing_return::write(ostream& stream) const {
        stream << "Missing return statement" << endl;
    }

    void dead_code::write(ostream& stream) const {
        stream << "Dead code" << endl;
    }
}
