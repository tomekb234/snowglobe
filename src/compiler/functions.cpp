#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void function_compiler::compile(const ast::func_def& ast) {
        push_frame();

        for (const prog::func_param& param : as_cref_vector(func.params)) {
            if (param.name) {
                auto var_index = add_var(*param.name, copy_type_local(*param.tp));
                auto instr = prog::write_var_instr { var_index, reg_counter++ };
                add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(instr)));
                vars[var_index].state = VAR_INITIALIZED;
            }

            reg_counter++;
        }

        compile_stmt_block(*ast.body->block, false);

        if (ast.body->return_value) {
            if (returned)
                warning(diags::unreachable_code((*ast.body->return_value)->loc));
            compile_return(as_optional_cref(ast.body->return_value), (*ast.body->return_value)->loc);
        } else if (!returned) {
            if (!INDEX_EQ(*func.return_tp, UNIT))
                error(diags::missing_return(ast.body->block->end_loc));
            add_return(new_unit_reg(), ast.body->block->end_loc);
        }

        for (auto& var : vars)
            func.vars.push_back(into_ptr(var.type));

        func.instrs = make_ptr(pop_frame());
    }

    void function_compiler::make_func_wrapper(prog::global_index func_index) {
        push_frame();

        auto param_count = func.params.size();

        vector<prog::reg_index> call_args;
        for (size_t index = 1; index < param_count; index++)
            call_args.push_back(index);

        auto result = new_reg();
        auto call_instr = prog::func_call_instr { func_index, call_args, result };
        add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(call_instr)));

        auto return_instr = prog::return_instr { result };
        add_instr(VARIANT(prog::instr, RETURN, into_ptr(return_instr)));

        func.instrs = make_ptr(pop_frame());
    }

    void function_compiler::make_struct_destructor(prog::global_index struct_index) {
        push_frame();

        auto& st = *prog.struct_types[struct_index];
        add_struct_destructor(0, st);

        func.instrs = make_ptr(pop_frame());
    }

    void function_compiler::make_enum_destructor(prog::global_index enum_index) {
        push_frame();

        auto& en = *prog.enum_types[enum_index];
        add_enum_variants_destructor(0, en, 0);

        func.instrs = make_ptr(pop_frame());
    }

    void function_compiler::make_cleanup_func() {
        push_frame();

        auto count = prog.global_vars.size();

        for (size_t index = 0; index < count; index++) {
            auto value = new_reg();
            auto instr = prog::read_global_var_instr { index, value };
            add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(instr)));
            add_delete(value, *prog.global_vars[index]->tp);
        }

        func.instrs = make_ptr(pop_frame());
    }

    prog::reg_index function_compiler::new_reg() {
        return ++reg_counter;
    }

    prog::reg_index function_compiler::new_unit_reg() {
        auto result = new_reg();
        add_instr(VARIANT(prog::instr, MAKE_UNIT, result));
        return result;
    }

    void function_compiler::add_instr(prog::instr&& instr) {
        frames.back().instrs.push_back(move(instr));
    }

    void function_compiler::add_instrs(prog::instr_block&& block) {
        for (auto& instr_ptr : block.instrs)
            add_instr(move(*instr_ptr));
    }

    void function_compiler::push_frame() {
        frames.push_back({ { }, { }, { }, false, { } });
    }

    void function_compiler::push_loop_frame() {
        push_frame();
        frames.back().loop = true;

        for (auto& var : vars)
            var.outside_loop++;
    }

    void function_compiler::push_confining_frame() {
        push_frame();

        for (auto& var : vars)
            var.outside_confinement++;
    }

    prog::instr_block function_compiler::pop_frame() {
        for (auto& name : frames.back().var_names) {
            var_names[name].pop_back();
            if (var_names[name].empty())
                var_names.erase(name);
        }

        auto block = prog::instr_block { into_ptr_vector(frames.back().instrs) };
        frames.pop_back();
        return block;
    }

    prog::instr_block function_compiler::pop_loop_frame() {
        for (auto& var : vars) {
            if (var.outside_loop > 0)
                var.outside_loop--;
        }

        return pop_frame();
    }

    prog::instr_block function_compiler::pop_confining_frame() {
        for (auto& var : vars) {
            if (var.outside_confinement > 0)
                var.outside_confinement--;
        }

        return pop_frame();
    }

    prog::var_index function_compiler::add_var(prog::type_local&& type) {
        auto index = vars.size();

        vars.push_back({ { }, move(type), VAR_UNINITIALIZED, 0, 0 });
        frames.back().vars.push_back(index);

        return index;
    }

    prog::var_index function_compiler::add_var(string name, prog::type_local&& type) {
        auto index = add_var(move(type));

        vars[index].name = { name };
        frames.back().var_names.push_back(name);
        var_names[name].push_back(index);

        return index;
    }

    optional<prog::var_index> function_compiler::try_get_var(string name) {
        auto iter = var_names.find(name);
        if (iter == var_names.end())
            return { };
        return { iter->second.back() };
    }

    prog::var_index function_compiler::get_var(string name, location loc) {
        auto index = try_get_var(name);
        if (!index)
            error(diags::variable_not_found(name, loc));
        return *index;
    }

    void function_compiler::move_out_var(prog::var_index index, location loc) {
        auto& var = vars[index];

        if (var.outside_loop > 0)
            error(diags::variable_moved_out_inside_loop(var.name, loc));

        var.state = VAR_MOVED_OUT;
    }

    vector<function_compiler::var_state> function_compiler::backup_var_states() {
        vector<var_state> states;
        for (auto& var : vars)
            states.push_back(var.state);
        return states;
    }

    void function_compiler::restore_var_states(const vector<var_state>& states) {
        auto count = states.size();
        for (size_t index = 0; index < count; index++)
            vars[index].state = states[index];
    }

    void function_compiler::merge_var_states(const vector<var_state>& states) {
        auto count = states.size();
        for (size_t index = 0; index < count; index++)
            vars[index].state |= states[index];
    }

    void function_compiler::defer_cleanup_action(function<void()> cleanup_action) {
        frames.back().cleanup_actions.push_back(cleanup_action);
    }

    void function_compiler::add_frame_cleanup(frame_index rev_index, location loc) {
        auto& fr = frames[frames.size() - rev_index - 1];

        for (auto var_index : fr.vars)
            add_var_delete(var_index, loc);

        for (auto iter = fr.cleanup_actions.rbegin(); iter != fr.cleanup_actions.rend(); iter++)
            (*iter)();
    }

    pair<prog::reg_index, prog::type_local> function_compiler::add_var_read(prog::var_index var_index, bool confined, location loc) {
        auto& var = vars[var_index];
        auto& state = var.state;
        auto& type = *var.type.tp;
        auto var_confined = var.type.confined;

        if (state != VAR_INITIALIZED)
            error(diags::variable_not_usable(var.name, state & VAR_INITIALIZED, state & VAR_UNINITIALIZED, state & VAR_MOVED_OUT, loc));

        auto result = new_reg();
        auto instr = prog::read_var_instr { var_index, result };
        add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(instr)));

        if (!confined && !var_confined) {
            if (clr.type_copyable(type))
                add_copy(result, type);
            else
                move_out_var(var_index, loc);
        }

        auto type_local = prog::type_local { make_ptr(copy_type(type)), var_confined || confined };

        return { result, move(type_local) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::add_var_confinement(prog::var_index var_index, location loc) {
        auto [value, type] = add_var_read(var_index, true, loc);

        auto var_name = *vars[var_index].name;
        auto new_var_index = add_var(var_name, copy_type_local(type));

        auto write_instr = prog::write_var_instr { new_var_index, value };
        add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

        vars[new_var_index].state = VAR_INITIALIZED;

        return { value, move(type) };
    }

    void function_compiler::add_var_delete(prog::var_index index, location loc) {
        auto& var = vars[index];

        if (!var.type.confined && !clr.type_trivial(*var.type.tp)) {
            if (var.state == VAR_INITIALIZED) {
                auto value = new_reg();
                auto read_instr = prog::read_var_instr { index, value };
                add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(read_instr)));
                add_delete(value, *var.type.tp);
            } else if (var.state & VAR_INITIALIZED)
                error(diags::variable_not_deletable(var.name, var.state & VAR_UNINITIALIZED, var.state & VAR_MOVED_OUT, loc));
        }
    }

    pair<prog::reg_index, prog::ptr_type> function_compiler::add_ptr_extraction(prog::reg_index value, prog::type&& type, location loc) {
        if (INDEX_EQ(type, PTR)) {
            auto& ptr_type = *GET(type, PTR);
            return { value, move(ptr_type) };
        } else if (INDEX_EQ(type, INNER_PTR)) {
            auto& ptr_type = *GET(type, INNER_PTR);
            auto result = new_reg();
            auto extract_instr = prog::ptr_conversion_instr { value, result };
            add_instr(VARIANT(prog::instr, EXTRACT_INNER_PTR, into_ptr(extract_instr)));
            return { result, move(ptr_type) };
        } else if (INDEX_EQ(type, FUNC_WITH_PTR)) {
            auto& ptr_type = *GET(type, FUNC_WITH_PTR);
            auto result = new_reg();
            auto extract_instr = prog::ptr_conversion_instr { value, result };
            add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(extract_instr)));
            return { result, move(ptr_type) };
        } else
            error(diags::invalid_type(prog, move(type), diags::type_kind::POINTER, loc));
    }

    void function_compiler::add_return(prog::reg_index value, location loc) {
        auto frame_count = frames.size();

        for (size_t index = 0; index < frame_count; index++)
            add_frame_cleanup(index, loc);

        auto instr = prog::return_instr { value };
        add_instr(VARIANT(prog::instr, RETURN, into_ptr(instr)));

        returned = true;
    }

    void function_compiler::add_break(location loc) {
        auto frame_count = frames.size();
        size_t index;

        for (index = 0; index < frame_count; index++) {
            add_frame_cleanup(index, loc);
            if (frames[frame_count - index - 1].loop)
                break;
        }

        if (index == frame_count)
            error(diags::break_outside_loop(loc));

        add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
    }

    void function_compiler::add_continue(location loc) {
        auto frame_count = frames.size();
        size_t index;

        for (index = 0; index < frame_count; index++) {
            if (frames[frame_count - index - 1].loop)
                break;
            add_frame_cleanup(index, loc);
        }

        if (index == frame_count)
            error(diags::continue_outside_loop(loc));

        add_instr(VARIANT(prog::instr, CONTINUE_LOOP, monostate()));
    }

    prog::branch_instr function_compiler::make_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch) {
        auto init_var_states = backup_var_states();
        auto init_returned = returned;

        push_frame();
        true_branch();
        auto true_block = pop_frame();

        auto branch_var_states = backup_var_states();
        auto branch_returned = returned;

        restore_var_states(init_var_states);
        returned = init_returned;

        push_frame();
        false_branch();
        auto false_block = pop_frame();

        merge_var_states(branch_var_states);
        returned &= branch_returned;

        return prog::branch_instr { cond, into_ptr(true_block), into_ptr(false_block) };
    }

    void function_compiler::add_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch) {
        add_instr(VARIANT(prog::instr, BRANCH, make_ptr(make_branch(cond, true_branch, false_branch))));
    }

    void function_compiler::add_loop(function<prog::reg_index()> head, function<void()> true_branch, function<void()> false_branch, function<void()> end) {
        push_loop_frame();

        auto cond = head();

        auto init_var_states = backup_var_states();
        auto init_returned = returned;

        push_frame();
        true_branch();
        auto true_block = pop_frame();

        auto branch_var_states = backup_var_states();
        auto branch_returned = returned;

        merge_var_states(init_var_states);
        returned &= init_returned;

        push_frame();
        false_branch();
        add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
        auto false_block = pop_frame();

        auto branch_instr = prog::branch_instr { cond, into_ptr(true_block), into_ptr(false_block) };
        add_instr(VARIANT(prog::instr, BRANCH, into_ptr(branch_instr)));

        end();

        auto loop_block = pop_loop_frame();
        add_instr(VARIANT(prog::instr, LOOP, into_ptr(loop_block)));

        merge_var_states(init_var_states);
        merge_var_states(branch_var_states);
        returned &= init_returned & branch_returned;
    }

    void function_compiler::add_assignment(const lvalue& lval, prog::reg_index value, const prog::type_local& type, location loc) {
        switch (INDEX(lval)) {
            case lvalue::IGNORED:
                if (!type.confined)
                    add_delete(value, *type.tp);
                break;

            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                auto& var = vars[var_index];

                if (type.confined && !clr.type_trivial(*type.tp) && var.outside_confinement > 0)
                    error(diags::variable_outside_confinement(var.name, loc));

                add_var_delete(var_index, loc);

                value = conv_clr.convert(value, type, var.type, loc);

                auto write_instr = prog::write_var_instr { var_index, value };
                add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

                var.state = VAR_INITIALIZED;
                var.outside_loop = 0;
            } break;

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                auto& var_type = *prog.global_vars[var_index]->tp;

                auto old_value = new_reg();
                auto read_instr = prog::read_global_var_instr { var_index, old_value };
                add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(read_instr)));
                add_delete(old_value, var_type);

                value = conv_clr.convert(value, type, var_type, loc);

                auto instr = prog::write_global_var_instr { var_index, value };
                add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, into_ptr(instr)));
            } break;

            case lvalue::TUPLE: {
                auto lvals = as_cref_vector(GET(lval, TUPLE));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, TUPLE))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::TUPLE, loc));

                auto field_types = as_cref_vector(GET(*type.tp, TUPLE));
                auto confined = type.confined;

                if (field_types.size() != count)
                    error(diags::invalid_tuple_size(field_types.size(), count, loc));

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(field_types[index])), confined };
                    add_assignment(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::ARRAY: {
                auto lvals = as_cref_vector(GET(lval, ARRAY));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, ARRAY))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::ARRAY, loc));

                auto& array_type = *GET(*type.tp, ARRAY);
                auto item_type = prog::type_local { make_ptr(copy_type(*array_type.tp)), type.confined };

                if (array_type.size != count)
                    error(diags::invalid_array_size(array_type.size, count, loc));

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    add_assignment(lvals[index], extracted, item_type, loc);
                }
            } break;

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                auto lvals = as_cref_vector(lval_ptrs);

                if (!INDEX_EQ(*type.tp, STRUCT))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::STRUCT, loc));
                if (GET(*type.tp, STRUCT) != struct_index)
                    error(diags::invalid_struct(*prog.struct_types[GET(*type.tp, STRUCT)], *prog.struct_types[struct_index], loc));

                auto& st = *prog.struct_types[struct_index];
                auto count = st.fields.size();
                auto confined = type.confined;

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(*st.fields[index]->tp)), confined };
                    add_assignment(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, target_type] = GET(lval, DEREFERENCE);

                auto old_value = new_reg();
                auto read_instr = prog::ptr_read_instr { ptr_value, old_value };
                add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(read_instr)));
                add_delete(old_value, target_type);

                value = conv_clr.convert(value, type, target_type, loc);

                auto write_instr = prog::ptr_write_instr { ptr_value, value };
                add_instr(VARIANT(prog::instr, PTR_WRITE, into_ptr(write_instr)));
            } break;
        }
    }

    pair<prog::reg_index, prog::type_local> function_compiler::add_read_for_swap(const lvalue& lval, location loc) {
        auto fields = [&] (vector<cref<lvalue>> lvals) -> tuple<vector<prog::reg_index>, vector<prog::type>, bool> {
            vector<prog::reg_index> values;
            vector<prog::type> types;
            optional<bool> all_confined;

            for (const lvalue& lval : lvals) {
                auto [value, type] = add_read_for_swap(lval, loc);

                if (!clr.type_trivial(*type.tp)) {
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
                auto& var = vars[var_index];
                auto& state = var.state;

                if (state != VAR_INITIALIZED)
                    error(diags::variable_not_usable(var.name, state & VAR_INITIALIZED, state & VAR_UNINITIALIZED, state & VAR_MOVED_OUT, loc));

                auto result = new_reg();
                auto instr = prog::read_var_instr { var_index, result };
                add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(instr)));

                return { result, copy_type_local(var.type) };
            }

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                auto& type = *prog.global_vars[var_index]->tp;
                auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

                auto result = new_reg();
                auto instr = prog::read_global_var_instr { var_index, result };
                add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(instr)));

                return { result, move(type_local) };
            }

            case lvalue::TUPLE: {
                auto [values, types, all_confined] = fields(as_cref_vector(GET(lval, TUPLE)));

                auto result = new_reg();
                auto instr = prog::make_tuple_instr { move(values), result };
                add_instr(VARIANT(prog::instr, MAKE_TUPLE, into_ptr(instr)));

                auto type = VARIANT(prog::type, TUPLE, into_ptr_vector(types));
                auto type_local = prog::type_local { into_ptr(type), all_confined };

                return { result, move(type_local) };
            }

            case lvalue::ARRAY: {
                auto [values, types, all_confined] = fields(as_cref_vector(GET(lval, ARRAY)));
                auto count = values.size();

                prog::type common_type = copy_type(prog::NEVER_TYPE);

                for (auto& type : types)
                    common_type = clr.common_supertype(common_type, type, loc);

                for (size_t index = 0; index < count; index++)
                    values[index] = conv_clr.convert(values[index], types[index], common_type, all_confined, loc);

                auto result = new_reg();
                auto instr = prog::make_array_instr { move(values), result };
                add_instr(VARIANT(prog::instr, MAKE_ARRAY, into_ptr(instr)));

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
                    values[index] = conv_clr.convert(values[index], type, field_type, all_confined, loc);
                }

                auto result = new_reg();
                auto instr = prog::make_struct_instr { struct_index, move(values), result };
                add_instr(VARIANT(prog::instr, MAKE_STRUCT, into_ptr(instr)));

                auto type = VARIANT(prog::type, STRUCT, struct_index);
                auto type_local = prog::type_local { into_ptr(type), all_confined };

                return { result, move(type_local) };
            }

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, type] = GET(lval, DEREFERENCE);
                auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

                auto result = new_reg();
                auto instr = prog::ptr_read_instr { ptr_value, result };
                add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(instr)));

                return { result, move(type_local) };
            }
        }

        UNREACHABLE;
    }

    void function_compiler::add_write_from_swap(const lvalue& lval, prog::reg_index value, const prog::type_local& type, location loc) {
        switch (INDEX(lval)) {
            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                auto& var = vars[var_index];

                if (type.confined && !clr.type_trivial(*type.tp) && var.outside_confinement > 0)
                    error(diags::variable_outside_confinement(var.name, loc));

                value = conv_clr.convert(value, type, var.type, loc);

                auto write_instr = prog::write_var_instr { var_index, value };
                add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

                var.state = VAR_INITIALIZED;
                var.outside_loop = 0;
            } break;

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                auto& var_type = *prog.global_vars[var_index]->tp;

                value = conv_clr.convert(value, type, var_type, loc);

                auto instr = prog::write_global_var_instr { var_index, value };
                add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, into_ptr(instr)));
            } break;

            case lvalue::TUPLE: {
                auto lvals = as_cref_vector(GET(lval, TUPLE));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, TUPLE))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::TUPLE, loc));

                auto field_types = as_cref_vector(GET(*type.tp, TUPLE));
                auto confined = type.confined;

                if (field_types.size() != count)
                    error(diags::invalid_tuple_size(field_types.size(), count, loc));

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(field_types[index])), confined };
                    add_write_from_swap(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::ARRAY: {
                auto lvals = as_cref_vector(GET(lval, ARRAY));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, ARRAY))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::ARRAY, loc));

                auto& array_type = *GET(*type.tp, ARRAY);
                auto item_type = prog::type_local { make_ptr(copy_type(*array_type.tp)), type.confined };

                if (array_type.size != count)
                    error(diags::invalid_array_size(array_type.size, count, loc));

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    add_write_from_swap(lvals[index], extracted, item_type, loc);
                }
            } break;

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                auto lvals = as_cref_vector(lval_ptrs);

                if (!INDEX_EQ(*type.tp, STRUCT))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::STRUCT, loc));
                if (GET(*type.tp, STRUCT) != struct_index)
                    error(diags::invalid_struct(*prog.struct_types[GET(*type.tp, STRUCT)], *prog.struct_types[struct_index], loc));

                auto& st = *prog.struct_types[struct_index];
                auto count = st.fields.size();
                auto confined = type.confined;

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(*st.fields[index]->tp)), confined };
                    add_write_from_swap(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, target_type] = GET(lval, DEREFERENCE);

                value = conv_clr.convert(value, type, target_type, loc);

                auto write_instr = prog::ptr_write_instr { ptr_value, value };
                add_instr(VARIANT(prog::instr, PTR_WRITE, into_ptr(write_instr)));
            } break;
        }
    }

}
