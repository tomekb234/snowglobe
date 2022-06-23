#include "compiler/copying.hpp"
#include "compiler/function_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void copy_generator::add(prog::reg_index value, const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::STRUCT: {
                auto struct_index = GET(type, STRUCT);
                auto& st = *prog.struct_types[struct_index];
                add_for_struct(value, st);
            } break;

            case prog::type::ENUM: {
                auto enum_index = GET(type, ENUM);
                auto& en = *prog.enum_types[enum_index];
                add_for_enum_variants(value, en, 0);
            } break;

            case prog::type::TUPLE: {
                auto types = as_cref_vector(GET(type, TUPLE));
                add_for_tuple(value, types);
            } break;

            case prog::type::ARRAY: {
                auto& array_type = *GET(type, ARRAY);
                add_for_array(value, array_type);
            } break;

            case prog::type::OPTIONAL: {
                auto& inner_type = *GET(type, OPTIONAL);
                add_for_optional(value, inner_type);
            } break;

            case prog::type::PTR: {
                auto& ptr_type = *GET(type, PTR);
                add_for_ptr(value, ptr_type.kind);
            } break;

            case prog::type::INNER_PTR: {
                auto& inner_ptr_type = *GET(type, INNER_PTR);
                auto extracted = fclr.new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, extracted };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_OUTER_PTR, into_ptr(extract_instr)));
                add_for_ptr(extracted, inner_ptr_type.kind);
            } break;

            case prog::type::FUNC_WITH_PTR: {
                auto& func_with_ptr_type = *GET(type, FUNC_WITH_PTR);
                auto extracted = fclr.new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, extracted };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(extract_instr)));
                add_for_ptr(extracted, func_with_ptr_type.kind);
            } break;

            default:
                break;
        }
    }

    void copy_generator::add_for_struct(prog::reg_index value, const prog::struct_type& st) {
        auto count = st.fields.size();

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            add(extracted, *st.fields[index]->tp);
        }
    }

    void copy_generator::add_for_enum_variants(prog::reg_index value, const prog::enum_type& en, prog::variant_index variant_index) {
        auto variant_count = en.variants.size();

        auto test_result = fclr.new_reg();
        auto test_instr = prog::test_variant_instr { value, variant_index, test_result };
        fclr.add_instr(VARIANT(prog::instr, TEST_VARIANT, into_ptr(test_instr)));

        auto true_branch = [&] () {
            auto& variant = *en.variants[variant_index];
            auto count = variant.tps.size();

            for (size_t index = 0; index < count; index++) {
                auto extracted = fclr.new_reg();
                auto extract_instr = prog::extract_variant_field_instr { value, variant_index, index, extracted };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_VARIANT_FIELD, into_ptr(extract_instr)));
                add(extracted, *variant.tps[index]);
            }
        };

        auto false_branch = [&] () {
            if (variant_index < variant_count - 1)
                add_for_enum_variants(value, en, variant_index + 1);
        };

        function_utils(fclr).add_branch(test_result, true_branch, false_branch);
    }

    void copy_generator::add_for_tuple(prog::reg_index value, vector<cref<prog::type>> types) {
        auto count = types.size();

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            add(extracted, types[index]);
        }
    }

    void copy_generator::add_for_array(prog::reg_index value, const prog::array_type& array_type) {
        auto count = array_type.size;
        auto& type = *array_type.tp;

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            add(extracted, type);
        }
    }

    void copy_generator::add_for_optional(prog::reg_index value, const prog::type& inner_type) {
        auto test_result = fclr.new_reg();
        auto test_instr = prog::test_optional_instr { value, test_result };
        fclr.add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

        auto true_branch = [&] () {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_optional_value_instr { value, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));
            add(extracted, inner_type);
        };

        function_utils(fclr).add_branch(test_result, true_branch, [] { });
    }

    void copy_generator::add_for_ptr(prog::reg_index value, prog::ptr_type::kind_t kind) {
        if (kind == prog::ptr_type::SHARED)
            fclr.add_instr(VARIANT(prog::instr, INCR_REF_COUNT, value));
        else if (kind == prog::ptr_type::WEAK)
            fclr.add_instr(VARIANT(prog::instr, INCR_WEAK_REF_COUNT, value));
    }
}
