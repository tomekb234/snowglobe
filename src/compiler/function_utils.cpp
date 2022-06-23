#include "compiler/function_utils.hpp"
#include "compiler/conversions.hpp"
#include "compiler/copying.hpp"
#include "compiler/deletion.hpp"
#include "compiler/compiler_utils.hpp"
#include "diags.hpp"
#include "program.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void function_utils::add_frame_cleanup(function_compiler::frame_index rev_index, location loc) {
        auto& fr = fclr.frames[fclr.frames.size() - rev_index - 1];

        for (auto var_index : fr.vars)
            function_utils(*this).add_var_deletion(var_index, loc);

        for (auto iter = fr.cleanup_actions.rbegin(); iter != fr.cleanup_actions.rend(); iter++)
            (*iter)();
    }

    void function_utils::add_return(prog::reg_index value, location loc) {
        auto frame_count = fclr.frames.size();

        for (size_t index = 0; index < frame_count; index++)
            add_frame_cleanup(index, loc);

        auto instr = prog::return_instr { value };
        fclr.add_instr(VARIANT(prog::instr, RETURN, into_ptr(instr)));

        fclr.returned = true;
    }

    void function_utils::add_break(location loc) {
        auto frame_count = fclr.frames.size();
        size_t index;

        for (index = 0; index < frame_count; index++) {
            add_frame_cleanup(index, loc);
            if (fclr.frames[frame_count - index - 1].loop)
                break;
        }

        if (index == frame_count)
            error(diags::break_outside_loop(loc));

        fclr.add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
    }

    void function_utils::add_continue(location loc) {
        auto frame_count = fclr.frames.size();
        size_t index;

        for (index = 0; index < frame_count; index++) {
            if (fclr.frames[frame_count - index - 1].loop)
                break;
            add_frame_cleanup(index, loc);
        }

        if (index == frame_count)
            error(diags::continue_outside_loop(loc));

        fclr.add_instr(VARIANT(prog::instr, CONTINUE_LOOP, monostate()));
    }

    prog::branch_instr function_utils::make_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch) {
        auto init_var_states = fclr.backup_var_states();
        auto init_returned = fclr.returned;

        fclr.push_frame();
        true_branch();
        auto true_block = fclr.pop_frame();

        auto branch_var_states = fclr.backup_var_states();
        auto branch_returned = fclr.returned;

        fclr.restore_var_states(init_var_states);
        fclr.returned = init_returned;

        fclr.push_frame();
        false_branch();
        auto false_block = fclr.pop_frame();

        fclr.merge_var_states(branch_var_states);
        fclr.returned &= branch_returned;

        return prog::branch_instr { cond, into_ptr(true_block), into_ptr(false_block) };
    }

    void function_utils::add_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch) {
        fclr.add_instr(VARIANT(prog::instr, BRANCH, make_ptr(make_branch(cond, true_branch, false_branch))));
    }

    void function_utils::add_loop(function<prog::reg_index()> head, function<void()> true_branch, function<void()> false_branch, function<void()> end) {
        fclr.push_loop_frame();

        auto cond = head();

        auto init_var_states = fclr.backup_var_states();
        auto init_returned = fclr.returned;

        fclr.push_frame();
        true_branch();
        auto true_block = fclr.pop_frame();

        auto branch_var_states = fclr.backup_var_states();
        auto branch_returned = fclr.returned;

        fclr.merge_var_states(init_var_states);
        fclr.returned &= init_returned;

        fclr.push_frame();
        false_branch();
        fclr.add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
        auto false_block = fclr.pop_frame();

        auto branch_instr = prog::branch_instr { cond, into_ptr(true_block), into_ptr(false_block) };
        fclr.add_instr(VARIANT(prog::instr, BRANCH, into_ptr(branch_instr)));

        end();

        auto loop_block = fclr.pop_loop_frame();
        fclr.add_instr(VARIANT(prog::instr, LOOP, into_ptr(loop_block)));

        fclr.merge_var_states(init_var_states);
        fclr.merge_var_states(branch_var_states);
        fclr.returned &= init_returned & branch_returned;
    }

    pair<prog::reg_index, prog::type_local> function_utils::add_var_read(prog::var_index var_index, bool confined, location loc) {
        auto& var = fclr.vars[var_index];
        auto& state = var.state;
        auto& type = *var.type.tp;
        auto var_confined = var.type.confined;

        if (state != VAR_INITIALIZED)
            error(diags::variable_not_usable(var.name, state, loc));

        auto result = fclr.new_reg();
        auto instr = prog::read_var_instr { var_index, result };
        fclr.add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(instr)));

        if (!confined && !var_confined) {
            if (type_copyable(prog, type))
                copy_generator(fclr, result).add(type);
            else
                fclr.move_out_var(var_index, loc);
        }

        auto type_local = prog::type_local { make_ptr(copy_type(type)), var_confined || confined };

        return { result, move(type_local) };
    }

    pair<prog::reg_index, prog::type_local> function_utils::add_var_confinement(prog::var_index var_index, location loc) {
        auto [value, type] = add_var_read(var_index, true, loc);

        auto var_name = *fclr.vars[var_index].name;
        auto new_var_index = fclr.add_var(var_name, copy_type_local(type));

        auto write_instr = prog::write_var_instr { new_var_index, value };
        fclr.add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

        fclr.vars[new_var_index].state = VAR_INITIALIZED;

        return { value, move(type) };
    }

    void function_utils::add_var_deletion(prog::var_index index, location loc) {
        auto& var = fclr.vars[index];

        if (!var.type.confined && !type_trivial(prog, *var.type.tp)) {
            if (var.state == VAR_INITIALIZED) {
                auto value = fclr.new_reg();
                auto read_instr = prog::read_var_instr { index, value };
                fclr.add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(read_instr)));
                deletion_generator(fclr).add(value, *var.type.tp);
            } else if (var.state & VAR_INITIALIZED)
                error(diags::variable_not_deletable(var.name, var.state, loc));
        }
    }

    pair<prog::reg_index, prog::ptr_type> function_utils::add_ptr_extraction(prog::reg_index value, const prog::type& type, location loc) {
        switch (INDEX(type)) {
            case prog::type::PTR: {
                auto& ptr_type = *GET(type, PTR);
                return { value, copy_ptr_type(ptr_type) };
            }

            case prog::type::INNER_PTR: {
                auto result = fclr.new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_INNER_PTR, into_ptr(extract_instr)));
                auto& ptr_type = *GET(type, INNER_PTR);
                return { result, copy_ptr_type(ptr_type) };
            }

            case prog::type::FUNC_WITH_PTR: {
                auto result = fclr.new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(extract_instr)));
                auto& ptr_type = *GET(type, FUNC_WITH_PTR);
                return { result, copy_ptr_type(ptr_type) };
            }

            default:
                error(diags::invalid_type(prog, copy_type(type), diags::type_kind::POINTER, loc));
        }
    }

    tuple<prog::reg_index, prog::ptr_type::kind_t, prog::type_pointed> function_utils::add_ptr_owner_extraction(prog::reg_index value, const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::PTR: {
                auto& ptr_type = *GET(type, PTR);
                return { value, ptr_type.kind, copy_type_pointed(*ptr_type.target_tp) };
            }

            case prog::type::INNER_PTR: {
                auto result = fclr.new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_OUTER_PTR, into_ptr(extract_instr)));
                auto& ptr_type = *GET(type, INNER_PTR);
                return { result, ptr_type.kind, copy_type_pointed(*ptr_type.owner_tp) };
            }

            case prog::type::FUNC_WITH_PTR: {
                auto result = fclr.new_reg();
                auto extract_instr = prog::ptr_conversion_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(extract_instr)));
                auto& ptr_type = *GET(type, FUNC_WITH_PTR);
                return { result, ptr_type.kind, copy_type_pointed(*ptr_type.target_tp) };
            }

            default:
                UNREACHABLE;
        }
    }

    pair<prog::reg_index, prog::type_local> function_utils::add_read_for_swap(const lvalue& lval, location loc) {
        auto fields = [&] (vector<cref<lvalue>> lvals) -> tuple<vector<prog::reg_index>, vector<prog::type>, bool> {
            vector<prog::reg_index> values;
            vector<prog::type> types;
            optional<bool> all_confined;

            for (const lvalue& lval : lvals) {
                auto [value, type] = add_read_for_swap(lval, loc);

                if (!type_trivial(prog, *type.tp)) {
                    if (!all_confined)
                        all_confined = { type.confined };
                    else if (type.confined != *all_confined)
                        error(diags::confinement_ambiguous(loc));
                }

                values.push_back(value);
                types.push_back(move(*type.tp));
            }

            if (!all_confined)
                all_confined = { false };

            return { move(values), move(types), *all_confined };
        };

        switch (INDEX(lval)) {
            case lvalue::IGNORED:
                error(diags::expression_not_swappable(loc));

            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                auto& var = fclr.vars[var_index];
                auto& state = var.state;

                if (state != VAR_INITIALIZED)
                    error(diags::variable_not_usable(var.name, state, loc));

                auto result = fclr.new_reg();
                auto instr = prog::read_var_instr { var_index, result };
                fclr.add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(instr)));

                return { result, copy_type_local(var.type) };
            }

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                auto& type = *prog.global_vars[var_index]->tp;
                auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

                auto result = fclr.new_reg();
                auto instr = prog::read_global_var_instr { var_index, result };
                fclr.add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(instr)));

                return { result, move(type_local) };
            }

            case lvalue::TUPLE: {
                auto [values, types, all_confined] = fields(as_cref_vector(GET(lval, TUPLE)));

                auto result = fclr.new_reg();
                auto instr = prog::make_tuple_instr { move(values), result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_TUPLE, into_ptr(instr)));

                auto type = VARIANT(prog::type, TUPLE, into_ptr_vector(types));
                auto type_local = prog::type_local { into_ptr(type), all_confined };

                return { result, move(type_local) };
            }

            case lvalue::ARRAY: {
                auto [values, types, all_confined] = fields(as_cref_vector(GET(lval, ARRAY)));
                auto count = values.size();

                prog::type common_type = copy_type(prog::NEVER_TYPE);

                for (auto& type : types)
                    common_type = compiler_utils(clr).common_supertype(common_type, type, loc);

                for (size_t index = 0; index < count; index++)
                    values[index] = conversion_generator(fclr).convert(values[index], types[index], common_type, all_confined, loc);

                auto result = fclr.new_reg();
                auto instr = prog::make_array_instr { move(values), result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_ARRAY, into_ptr(instr)));

                auto array = prog::array_type { into_ptr(common_type), count };
                auto type = VARIANT(prog::type, ARRAY, into_ptr(array));
                auto type_local = prog::type_local { into_ptr(type), all_confined };

                return { result, move(type_local) };
            }

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                auto [values, types, all_confined] = fields(as_cref_vector(lval_ptrs));

                auto& st = *prog.struct_types[struct_index];
                auto count = values.size();

                for (size_t index = 0; index < count; index++) {
                    auto& type = types[index];
                    auto& field_type = *st.fields[index]->tp;
                    values[index] = conversion_generator(fclr).convert(values[index], type, field_type, all_confined, loc);
                }

                auto result = fclr.new_reg();
                auto instr = prog::make_struct_instr { struct_index, move(values), result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_STRUCT, into_ptr(instr)));

                auto type = VARIANT(prog::type, STRUCT, struct_index);
                auto type_local = prog::type_local { into_ptr(type), all_confined };

                return { result, move(type_local) };
            }

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, type] = GET(lval, DEREFERENCE);
                auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

                auto result = fclr.new_reg();
                auto instr = prog::ptr_read_instr { ptr_value, result };
                fclr.add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(instr)));

                return { result, move(type_local) };
            }
        }

        UNREACHABLE;
    }
}
