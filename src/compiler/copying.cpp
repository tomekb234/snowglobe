#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void function_compiler::add_copy(prog::reg_index value, const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::STRUCT: {
                auto struct_index = GET(type, STRUCT);
                auto& st = *clr.prog.struct_types[struct_index];
                add_struct_copy(value, st);
            } break;

            case prog::type::ENUM: {
                auto enum_index = GET(type, ENUM);
                auto& en = *clr.prog.enum_types[enum_index];
                add_enum_variants_copy(value, en, 0);
            } break;

            case prog::type::TUPLE: {
                auto types = as_cref_vector(GET(type, TUPLE));
                add_tuple_copy(value, types);
            } break;

            case prog::type::ARRAY: {
                auto& array_type = *GET(type, ARRAY);
                add_array_copy(value, array_type);
            } break;

            case prog::type::OPTIONAL: {
                auto& inner_type = *GET(type, OPTIONAL);
                add_optional_copy(value, inner_type);
            } break;

            case prog::type::PTR: {
                auto& ptr_type = *GET(type, PTR);
                add_ptr_copy(value, ptr_type.kind);
            } break;

            case prog::type::INNER_PTR: {
                auto& inner_ptr_type = *GET(type, INNER_PTR);
                auto extracted = new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, extracted };
                add_instr(VARIANT(prog::instr, EXTRACT_OUTER_PTR, into_ptr(extract_instr)));
                add_ptr_copy(extracted, inner_ptr_type.kind);
            } break;

            case prog::type::FUNC_WITH_PTR: {
                auto& func_with_ptr_type = *GET(type, FUNC_WITH_PTR);
                auto extracted = new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, extracted };
                add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(extract_instr)));
                add_ptr_copy(extracted, func_with_ptr_type.kind);
            } break;

            default:
                break;
        }
    }

    void function_compiler::add_struct_copy(prog::reg_index value, const prog::struct_type& st) {
        auto count = st.fields.size();

        for (size_t index = 0; index < count; index++) {
            auto extracted = new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            add_copy(extracted, *st.fields[index]->tp);
        }
    }

    void function_compiler::add_enum_variants_copy(prog::reg_index value, const prog::enum_type& en, prog::variant_index variant_index) {
        auto variant_count = en.variants.size();

        auto test_result = new_reg();
        auto test_instr = prog::test_variant_instr { value, variant_index, test_result };
        add_instr(VARIANT(prog::instr, TEST_VARIANT, into_ptr(test_instr)));

        auto true_branch = [&] () {
            auto& variant = *en.variants[variant_index];
            auto count = variant.tps.size();

            for (size_t index = 0; index < count; index++) {
                auto extracted = new_reg();
                auto extract_instr = prog::extract_variant_field_instr { value, variant_index, index, extracted };
                add_instr(VARIANT(prog::instr, EXTRACT_VARIANT_FIELD, into_ptr(extract_instr)));
                add_copy(extracted, *variant.tps[index]);
            }
        };

        auto false_branch = [&] () {
            if (variant_index < variant_count - 1)
                add_enum_variants_copy(value, en, variant_index + 1);
        };

        add_branch(test_result, true_branch, false_branch);
    }

    void function_compiler::add_tuple_copy(prog::reg_index value, vector<cref<prog::type>> types) {
        auto count = types.size();

        for (size_t index = 0; index < count; index++) {
            auto extracted = new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            add_copy(extracted, types[index]);
        }
    }

    void function_compiler::add_array_copy(prog::reg_index value, const prog::array_type& array_type) {
        push_frame();

        auto index = new_reg();
        auto extracted = new_reg();
        auto extract_instr = prog::extract_item_instr { value, index, extracted };
        add_instr(VARIANT(prog::instr, EXTRACT_ITEM, into_ptr(extract_instr)));
        add_copy(extracted, *array_type.tp);

        auto block = pop_frame();

        auto repeat_instr = prog::repeat_static_instr { array_type.size, index, into_ptr(block) };
        add_instr(VARIANT(prog::instr, REPEAT_STATIC, into_ptr(repeat_instr)));
    }

    void function_compiler::add_optional_copy(prog::reg_index value, const prog::type& inner_type) {
        auto test_result = new_reg();
        auto test_instr = prog::test_optional_instr { value, test_result };
        add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

        auto true_branch = [&] () {
            auto extracted = new_reg();
            auto extract_instr = prog::extract_optional_value_instr { value, extracted };
            add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));
            add_copy(extracted, inner_type);
        };

        add_branch(test_result, true_branch, [] { });
    }

    void function_compiler::add_ptr_copy(prog::reg_index value, prog::ptr_type::kind_t kind) {
        if (kind == prog::ptr_type::SHARED)
            add_instr(VARIANT(prog::instr, INCR_REF_COUNT, value));
        else if (kind == prog::ptr_type::WEAK)
            add_instr(VARIANT(prog::instr, INCR_WEAK_REF_COUNT, value));
    }
}
