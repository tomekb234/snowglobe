#include "compiler/copying.hpp"
#include "compiler/function_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void copy_generator::add(const prog::type& type) {
        if (type_trivial(prog, type))
            return;

        switch (INDEX(type)) {
            case prog::type::STRUCT: {
                auto struct_index = GET(type, STRUCT);
                auto& st = *prog.struct_types[struct_index];
                add_of_struct(st);
            } break;

            case prog::type::ENUM: {
                auto enum_index = GET(type, ENUM);
                auto& en = *prog.enum_types[enum_index];
                add_of_enum_variants(en, 0);
            } break;

            case prog::type::TUPLE: {
                auto types = as_cref_vector(GET(type, TUPLE));
                add_of_tuple(types);
            } break;

            case prog::type::ARRAY: {
                auto& array_type = *GET(type, ARRAY);
                add_of_array(array_type);
            } break;

            case prog::type::OPTIONAL: {
                auto& inner_type = *GET(type, OPTIONAL);
                add_of_optional(inner_type);
            } break;

            case prog::type::PTR:
            case prog::type::INNER_PTR:
            case prog::type::FUNC_WITH_PTR: {
                auto [result, kind, type_pointed] = function_utils(fclr).add_ptr_owner_extraction(value, type);
                value = result;
                add_of_ptr(kind);
            } break;

            default:
                break;
        }
    }

    void copy_generator::add_of_struct(const prog::struct_type& st) {
        auto count = st.fields.size();

        for (size_t index = 0; index < count; index++) {
            auto& type = *st.fields[index]->tp;
            if (type_trivial(prog, type))
                continue;

            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));

            copy_generator(fclr, extracted).add(type);
        }
    }

    void copy_generator::add_of_enum_variants(const prog::enum_type& en, prog::variant_index variant_index) {
        auto variant_count = en.variants.size();

        auto test_result = fclr.new_reg();
        auto test_instr = prog::test_variant_instr { value, variant_index, test_result };
        fclr.add_instr(VARIANT(prog::instr, TEST_VARIANT, into_ptr(test_instr)));

        auto true_branch = [&] () {
            auto& variant = *en.variants[variant_index];
            auto count = variant.tps.size();

            for (size_t index = 0; index < count; index++) {
                auto& type = *variant.tps[index];
                if (type_trivial(prog, type))
                    continue;

                auto extracted = fclr.new_reg();
                auto extract_instr = prog::extract_variant_field_instr { value, variant_index, index, extracted };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_VARIANT_FIELD, into_ptr(extract_instr)));

                copy_generator(fclr, extracted).add(type);
            }
        };

        auto false_branch = [&] () {
            if (variant_index < variant_count - 1)
                add_of_enum_variants(en, variant_index + 1);
        };

        function_utils(fclr).add_branch(test_result, true_branch, false_branch);
    }

    void copy_generator::add_of_tuple(vector<cref<prog::type>> types) {
        auto count = types.size();

        for (size_t index = 0; index < count; index++) {
            auto& type = types[index];
            if (type_trivial(prog, type))
                continue;

            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));

            copy_generator(fclr, extracted).add(type);
        }
    }

    void copy_generator::add_of_array(const prog::array_type& array_type) {
        auto count = array_type.size;

        auto& type = *array_type.tp;
        if (type_trivial(prog, type))
            return;

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));

            copy_generator(fclr, extracted).add(type);
        }
    }

    void copy_generator::add_of_optional(const prog::type& inner_type) {
        if (type_trivial(prog, inner_type))
            return;

        auto test_result = fclr.new_reg();
        auto test_instr = prog::test_optional_instr { value, test_result };
        fclr.add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

        auto true_branch = [&] () {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_optional_value_instr { value, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));

            copy_generator(fclr, extracted).add(inner_type);
        };

        function_utils(fclr).add_branch(test_result, true_branch, [] { });
    }

    void copy_generator::add_of_ptr(prog::ptr_type::kind_t kind) {
        if (kind == prog::ptr_type::SHARED)
            fclr.add_instr(VARIANT(prog::instr, INCR_REF_COUNT, value));
        else if (kind == prog::ptr_type::WEAK)
            fclr.add_instr(VARIANT(prog::instr, INCR_WEAK_REF_COUNT, value));
    }
}
