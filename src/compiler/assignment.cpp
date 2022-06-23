#include "compiler/assignment.hpp"
#include "compiler/conversions.hpp"
#include "compiler/deletion.hpp"
#include "compiler/compiler_utils.hpp"
#include "compiler/function_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void assignment_generator::add() {
        switch (INDEX(lval)) {
            case lvalue::IGNORED: {
                if (!type.confined)
                    deletion_generator(fclr).add(value, *type.tp);
            } break;

            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                add_to_var(var_index);
            } break;

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                add_to_global_var(var_index);
            } break;

            case lvalue::TUPLE: {
                auto lvals = as_cref_vector(GET(lval, TUPLE));
                add_to_tuple(lvals);
            } break;

            case lvalue::ARRAY: {
                auto lvals = as_cref_vector(GET(lval, ARRAY));
                add_to_array(lvals);
            } break;

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                add_to_struct(struct_index, as_cref_vector(lval_ptrs));
            } break;

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, target_type] = GET(lval, DEREFERENCE);
                add_to_dereference(ptr_value, target_type);
            } break;
        }
    }

    void assignment_generator::add_to_var(prog::var_index var_index) {
        auto& var = fclr.vars[var_index];

        if (type.confined && !type_trivial(prog, *type.tp) && var.outside_confinement > 0)
            error(diags::variable_outside_confinement(var.name, loc));

        function_utils(fclr).add_var_deletion(var_index, loc);

        value = conversion_generator(fclr).convert(value, type, var.type, loc);

        auto write_instr = prog::write_var_instr { var_index, value };
        fclr.add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

        var.state = VAR_INITIALIZED;
        var.outside_loop = 0;
    }

    void assignment_generator::add_to_global_var(prog::global_index var_index) {
        auto& var_type = *prog.global_vars[var_index]->tp;

        auto old_value = fclr.new_reg();
        auto read_instr = prog::read_global_var_instr { var_index, old_value };
        fclr.add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(read_instr)));
        deletion_generator(fclr).add(old_value, var_type);

        value = conversion_generator(fclr).convert(value, type, var_type, loc);

        auto instr = prog::write_global_var_instr { var_index, value };
        fclr.add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, into_ptr(instr)));
    }

    void assignment_generator::add_to_tuple(vector<cref<lvalue>> lvals) {
        auto count = lvals.size();

        if (!INDEX_EQ(*type.tp, TUPLE))
            error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::TUPLE, loc));

        auto field_types = as_cref_vector(GET(*type.tp, TUPLE));
        auto confined = type.confined;

        if (field_types.size() != count)
            error(diags::invalid_tuple_size(field_types.size(), count, loc));

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

            auto field_type = prog::type_local { make_ptr(copy_type(field_types[index])), confined };
            assignment_generator(fclr, lvals[index], extracted, field_type, loc).add();
        }
    }

    void assignment_generator::add_to_array(vector<cref<lvalue>> lvals) {
        auto count = lvals.size();

        if (!INDEX_EQ(*type.tp, ARRAY))
            error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::ARRAY, loc));

        auto& array_type = *GET(*type.tp, ARRAY);
        auto item_type = prog::type_local { make_ptr(copy_type(*array_type.tp)), type.confined };

        if (array_type.size != count)
            error(diags::invalid_array_size(array_type.size, count, loc));

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

            assignment_generator(fclr, lvals[index], extracted, item_type, loc).add();
        }
    }

    void assignment_generator::add_to_struct(prog::global_index struct_index, vector<cref<lvalue>> lvals) {
        if (!INDEX_EQ(*type.tp, STRUCT))
            error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::STRUCT, loc));
        if (GET(*type.tp, STRUCT) != struct_index)
            error(diags::invalid_struct(*prog.struct_types[GET(*type.tp, STRUCT)], *prog.struct_types[struct_index], loc));

        auto& st = *prog.struct_types[struct_index];
        auto count = st.fields.size();
        auto confined = type.confined;

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

            auto field_type = prog::type_local { make_ptr(copy_type(*st.fields[index]->tp)), confined };
            assignment_generator(fclr, lvals[index], extracted, field_type, loc).add();
        }
    }

    void assignment_generator::add_to_dereference(prog::reg_index ptr_value, const prog::type& target_type) {
        auto old_value = fclr.new_reg();
        auto read_instr = prog::ptr_read_instr { ptr_value, old_value };
        fclr.add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(read_instr)));
        deletion_generator(fclr).add(old_value, target_type);

        value = conversion_generator(fclr).convert(value, type, target_type, loc);

        auto write_instr = prog::ptr_write_instr { ptr_value, value };
        fclr.add_instr(VARIANT(prog::instr, PTR_WRITE, into_ptr(write_instr)));
    }

    void assignment_generator::add_from_swap() {
        switch (INDEX(lval)) {
            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                add_to_var_from_swap(var_index);
            } break;

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                add_to_global_var_from_swap(var_index);
            } break;

            case lvalue::TUPLE: {
                auto lvals = as_cref_vector(GET(lval, TUPLE));
                add_to_tuple_from_swap(lvals);
            } break;

            case lvalue::ARRAY: {
                auto lvals = as_cref_vector(GET(lval, ARRAY));
                add_to_array_from_swap(lvals);
            } break;

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                add_to_struct_from_swap(struct_index, as_cref_vector(lval_ptrs));
            } break;

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, target_type] = GET(lval, DEREFERENCE);
                add_to_dereference_from_swap(ptr_value, target_type);
            } break;
        }
    }

    void assignment_generator::add_to_var_from_swap(prog::var_index var_index) {
        auto& var = fclr.vars[var_index];

        if (type.confined && !type_trivial(prog, *type.tp) && var.outside_confinement > 0)
            error(diags::variable_outside_confinement(var.name, loc));

        value = conversion_generator(fclr).convert(value, type, var.type, loc);

        auto write_instr = prog::write_var_instr { var_index, value };
        fclr.add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

        var.state = VAR_INITIALIZED;
        var.outside_loop = 0;
    }

    void assignment_generator::add_to_global_var_from_swap(prog::global_index var_index) {
        auto& var_type = *prog.global_vars[var_index]->tp;

        value = conversion_generator(fclr).convert(value, type, var_type, loc);

        auto instr = prog::write_global_var_instr { var_index, value };
        fclr.add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, into_ptr(instr)));
    }

    void assignment_generator::add_to_tuple_from_swap(vector<cref<lvalue>> lvals) {
        auto count = lvals.size();

        if (!INDEX_EQ(*type.tp, TUPLE))
            error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::TUPLE, loc));

        auto field_types = as_cref_vector(GET(*type.tp, TUPLE));
        auto confined = type.confined;

        if (field_types.size() != count)
            error(diags::invalid_tuple_size(field_types.size(), count, loc));

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

            auto field_type = prog::type_local { make_ptr(copy_type(field_types[index])), confined };
            assignment_generator(fclr, lvals[index], extracted, field_type, loc).add_from_swap();
        }
    }

    void assignment_generator::add_to_array_from_swap(vector<cref<lvalue>> lvals) {
        auto count = lvals.size();

        if (!INDEX_EQ(*type.tp, ARRAY))
            error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::ARRAY, loc));

        auto& array_type = *GET(*type.tp, ARRAY);
        auto item_type = prog::type_local { make_ptr(copy_type(*array_type.tp)), type.confined };

        if (array_type.size != count)
            error(diags::invalid_array_size(array_type.size, count, loc));

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

            assignment_generator(fclr, lvals[index], extracted, item_type, loc).add_from_swap();
        }
    }

    void assignment_generator::add_to_struct_from_swap(prog::global_index struct_index, vector<cref<lvalue>> lvals) {
        if (!INDEX_EQ(*type.tp, STRUCT))
            error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::STRUCT, loc));
        if (GET(*type.tp, STRUCT) != struct_index)
            error(diags::invalid_struct(*prog.struct_types[GET(*type.tp, STRUCT)], *prog.struct_types[struct_index], loc));

        auto& st = *prog.struct_types[struct_index];
        auto count = st.fields.size();
        auto confined = type.confined;

        for (size_t index = 0; index < count; index++) {
            auto extracted = fclr.new_reg();
            auto instr = prog::extract_field_instr { value, index, extracted };
            fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

            auto field_type = prog::type_local { make_ptr(copy_type(*st.fields[index]->tp)), confined };
            assignment_generator(fclr, lvals[index], extracted, field_type, loc).add_from_swap();
        }
    }

    void assignment_generator::add_to_dereference_from_swap(prog::reg_index ptr_value, const prog::type& target_type) {
        value = conversion_generator(fclr).convert(value, type, target_type, loc);
        auto write_instr = prog::ptr_write_instr { ptr_value, value };
        fclr.add_instr(VARIANT(prog::instr, PTR_WRITE, into_ptr(write_instr)));
    }
}
