#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void function_compiler::add_struct_destructor(prog::reg_index value, const prog::struct_type& st) {
        auto count = st.fields.size();

        for (size_t index = 0; index < count; index++) {
            auto extracted = new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            add_delete(extracted, *st.fields[index]->tp);
        }
    }

    void function_compiler::add_enum_variants_destructor(prog::reg_index value, const prog::enum_type& en, prog::variant_index variant_index) {
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
                add_delete(extracted, *variant.tps[index]);
            }
        };

        auto false_branch = [&] () {
            if (variant_index < variant_count - 1)
                add_enum_variants_destructor(value, en, variant_index + 1);
        };

        add_branch(test_result, true_branch, false_branch);
    }

    void function_compiler::add_delete(prog::reg_index value, const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::STRUCT: {
                auto struct_index = GET(type, STRUCT);
                auto& st = *clr.prog.struct_types[struct_index];
                add_struct_delete(value, st);
            } break;

            case prog::type::ENUM: {
                auto enum_index = GET(type, ENUM);
                auto& en = *clr.prog.enum_types[enum_index];
                add_enum_delete(value, en);
            } break;

            case prog::type::TUPLE: {
                auto types = as_cref_vector(GET(type, TUPLE));
                add_tuple_delete(value, types);
            } break;

            case prog::type::ARRAY: {
                auto& array_type = *GET(type, ARRAY);
                add_array_delete(value, array_type);
            } break;

            case prog::type::OPTIONAL: {
                auto& inner_type = *GET(type, OPTIONAL);
                add_optional_delete(value, inner_type);
            } break;

            case prog::type::PTR: {
                auto& ptr_type = *GET(type, PTR);
                add_ptr_delete(value, ptr_type.kind, *ptr_type.target_tp);
            } break;

            case prog::type::INNER_PTR: {
                auto& inner_ptr_type = *GET(type, INNER_PTR);
                auto extracted = new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, extracted };
                add_instr(VARIANT(prog::instr, EXTRACT_OUTER_PTR, into_ptr(extract_instr)));
                add_ptr_delete(extracted, inner_ptr_type.kind, *inner_ptr_type.owner_tp);
            } break;

            case prog::type::FUNC_WITH_PTR: {
                auto& func_with_ptr_type = *GET(type, FUNC_WITH_PTR);
                auto extracted = new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, extracted };
                add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(extract_instr)));
                add_ptr_delete(extracted, func_with_ptr_type.kind, *func_with_ptr_type.target_tp);
            } break;

            default:
                break;
        }
    }

    void function_compiler::add_struct_delete(prog::reg_index value, const prog::struct_type& st) {
        auto instr = prog::func_call_instr { st.destructor, { value }, new_reg() };
        add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(instr)));
    }

    void function_compiler::add_enum_delete(prog::reg_index value, const prog::enum_type& en) {
        auto instr = prog::func_call_instr { en.destructor, { value }, new_reg() };
        add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(instr)));
    }

    void function_compiler::add_tuple_delete(prog::reg_index value, vector<cref<prog::type>> types) {
        auto count = types.size();

        for (size_t index = 0; index < count; index++) {
            auto extracted = new_reg();
            auto extract_instr = prog::extract_field_instr { value, index, extracted };
            add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(extract_instr)));
            add_delete(extracted, types[index]);
        }
    }

    void function_compiler::add_array_delete(prog::reg_index value, const prog::array_type& array_type) {
        push_frame();

        auto index = new_reg();
        auto extracted = new_reg();
        auto extract_instr = prog::extract_item_instr { value, index, extracted };
        add_instr(VARIANT(prog::instr, EXTRACT_ITEM, into_ptr(extract_instr)));
        add_delete(extracted, *array_type.tp);

        auto block = pop_frame();

        auto repeat_instr = prog::repeat_static_instr { array_type.size, index, into_ptr(block) };
        add_instr(VARIANT(prog::instr, REPEAT_STATIC, into_ptr(repeat_instr)));
    }

    void function_compiler::add_optional_delete(prog::reg_index value, const prog::type& inner_type) {
        auto test_result = new_reg();
        auto test_instr = prog::test_optional_instr { value, test_result };
        add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

        auto true_branch = [&] () {
            auto extracted = new_reg();
            auto extract_instr = prog::extract_optional_value_instr { value, extracted };
            add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));
            add_delete(extracted, inner_type);
        };

        add_branch(test_result, true_branch, [] { });
    }

    void function_compiler::add_ptr_delete(prog::reg_index value, prog::ptr_type::kind_t kind, const prog::type_pointed& target_type) {
        auto add = [&] () {
            if (target_type.slice) {
                auto length = new_reg();
                auto get_instr = prog::get_slice_length_instr { value, length };
                add_instr(VARIANT(prog::instr, GET_SLICE_LENGTH, into_ptr(get_instr)));

                push_frame();
                auto index = new_reg();
                auto result = new_reg();
                auto read_instr = prog::slice_read_instr { value, index, result };
                add_instr(VARIANT(prog::instr, SLICE_READ, into_ptr(read_instr)));
                add_delete(result, *target_type.tp);
                auto block = pop_frame();

                auto repeat_instr = prog::repeat_instr { length, index, into_ptr(block) };
                add_instr(VARIANT(prog::instr, REPEAT, into_ptr(repeat_instr)));
            } else {
                auto result = new_reg();
                auto read_instr = prog::ptr_read_instr { value, result };
                add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(read_instr)));
                add_delete(result, *target_type.tp);
            }

            add_instr(VARIANT(prog::instr, DELETE, value));
        };

        if (kind == prog::ptr_type::UNIQUE)
            add();
        else if (kind == prog::ptr_type::SHARED) {
            add_instr(VARIANT(prog::instr, DECR_REF_COUNT, value));
            auto test_result = new_reg();
            auto test_instr = prog::test_ref_count_instr { value, test_result };
            add_instr(VARIANT(prog::instr, TEST_REF_COUNT, into_ptr(test_instr)));
            add_branch(test_result, [] { }, add);
        } else if (kind == prog::ptr_type::WEAK)
            add_instr(VARIANT(prog::instr, DECR_WEAK_REF_COUNT, value));

        if (kind == prog::ptr_type::SHARED || kind == prog::ptr_type::WEAK) {
            auto test_result = new_reg();
            auto test_instr = prog::test_ref_count_instr { value, test_result };
            add_instr(VARIANT(prog::instr, TEST_ANY_REF_COUNT, into_ptr(test_instr)));
            add_branch(test_result, [] { }, [&] () { add_instr(VARIANT(prog::instr, DELETE_REF_COUNTER, value)); });
        }
    }
}
