#include "compiler/deletion.hpp"
#include "compiler/function_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void deletion_generator::add_struct_destructor(const prog::struct_type& st) {
        auto count = st.fields.size();

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            deletion_generator(fclr, extracted).add(*st.fields[index]->tp);
        }
    }

    void deletion_generator::add_enum_variants_destructor(const prog::enum_type& en, prog::variant_index variant_index) {
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
                deletion_generator(fclr, extracted).add(*variant.tps[index]);
            }
        };

        auto false_branch = [&] () {
            if (variant_index < variant_count - 1)
                add_enum_variants_destructor(en, variant_index + 1);
        };

        function_utils(fclr).add_branch(test_result, true_branch, false_branch);
    }

    void deletion_generator::add(const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::STRUCT: {
                auto struct_index = GET(type, STRUCT);
                auto& st = *prog.struct_types[struct_index];
                add_of_struct(st);
            } break;

            case prog::type::ENUM: {
                auto enum_index = GET(type, ENUM);
                auto& en = *prog.enum_types[enum_index];
                add_of_enum(en);
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
            case prog::type::INNER_PTR: {
                auto [result, kind, type_pointed] = function_utils(fclr).add_ptr_owner_extraction(value, type);
                value = result;
                add_of_ptr(kind, type_pointed);
            } break;

            default:
                break;
        }
    }

    void deletion_generator::add_of_struct(const prog::struct_type& st) {
        auto instr = prog::func_call_instr { st.destructor, { value }, fclr.new_reg() };
        fclr.add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(instr)));
    }

    void deletion_generator::add_of_enum(const prog::enum_type& en) {
        auto instr = prog::func_call_instr { en.destructor, { value }, fclr.new_reg() };
        fclr.add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(instr)));
    }

    void deletion_generator::add_of_tuple(vector<cref<prog::type>> types) {
        auto count = types.size();

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            deletion_generator(fclr, extracted).add(types[index]);
        }
    }

    void deletion_generator::add_of_array(const prog::array_type& array_type) {
        auto count = array_type.size;
        auto& type = *array_type.tp;

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            deletion_generator(fclr, extracted).add(type);
        }
    }

    void deletion_generator::add_of_optional(const prog::type& inner_type) {
        auto test_result = fclr.new_reg();
        auto test_instr = prog::test_optional_instr { value, test_result };
        fclr.add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

        auto true_branch = [&] () {
            auto extracted = fclr.new_reg();
            auto extract_instr = prog::extract_optional_value_instr { value, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));
            deletion_generator(fclr, extracted).add(inner_type);
        };

        function_utils(fclr).add_branch(test_result, true_branch, [] { });
    }

    void deletion_generator::add_of_ptr(prog::ptr_type::kind_t kind, const prog::type_pointed& target_type) {
        auto add = [&] () {
            if (target_type.slice) {
                auto length = fclr.new_reg();
                auto length_instr = prog::get_slice_length_instr { value, length };
                fclr.add_instr(VARIANT(prog::instr, GET_SLICE_LENGTH, into_ptr(length_instr)));

                fclr.push_frame();

                auto index = fclr.new_reg();

                auto ptr = fclr.new_reg();
                auto get_instr = prog::get_item_ptr_instr { value, index, ptr };
                fclr.add_instr(VARIANT(prog::instr, GET_ITEM_PTR, into_ptr(get_instr)));

                auto result = fclr.new_reg();
                auto read_instr = prog::ptr_read_instr { ptr, result };
                fclr.add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(read_instr)));

                deletion_generator(fclr, result).add(*target_type.tp);

                auto block = fclr.pop_frame();

                auto repeat_instr = prog::repeat_instr { length, index, into_ptr(block) };
                fclr.add_instr(VARIANT(prog::instr, REPEAT, into_ptr(repeat_instr)));
            } else {
                auto result = fclr.new_reg();
                auto read_instr = prog::ptr_read_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(read_instr)));
                deletion_generator(fclr, result).add(*target_type.tp);
            }

            fclr.add_instr(VARIANT(prog::instr, DELETE, value));
        };

        if (kind == prog::ptr_type::UNIQUE)
            add();
        else if (kind == prog::ptr_type::SHARED) {
            fclr.add_instr(VARIANT(prog::instr, DECR_REF_COUNT, value));
            auto test_result = fclr.new_reg();
            auto test_instr = prog::test_ref_count_instr { value, test_result };
            fclr.add_instr(VARIANT(prog::instr, TEST_REF_COUNT, into_ptr(test_instr)));
            function_utils(fclr).add_branch(test_result, [] { }, add);
        } else if (kind == prog::ptr_type::WEAK)
            fclr.add_instr(VARIANT(prog::instr, DECR_WEAK_REF_COUNT, value));

        if (kind == prog::ptr_type::SHARED || kind == prog::ptr_type::WEAK) {
            auto test_result = fclr.new_reg();
            auto test_instr = prog::test_ref_count_instr { value, test_result };
            fclr.add_instr(VARIANT(prog::instr, TEST_ANY_REF_COUNT, into_ptr(test_instr)));
            function_utils(fclr).add_branch(test_result, [] { }, [&] () { fclr.add_instr(VARIANT(prog::instr, DELETE_REF_COUNTER, value)); });
        }
    }
}
