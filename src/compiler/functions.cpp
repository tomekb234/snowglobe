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
                clr.warning(diags::dead_code(), (*ast.body->return_value)->loc);
            compile_return(as_optional_cref(ast.body->return_value), (*ast.body->return_value)->loc);
        } else if (!returned) {
            if (!INDEX_EQ(*func.return_tp, UNIT))
                clr.error(diags::missing_return(), ast.body->block->end_loc);
            add_return(unit_reg(), ast.body->block->end_loc);
        }

        for (auto& var : vars)
            func.vars.push_back(into_ptr(var.type));

        func.instrs = make_ptr(pop_frame());
    }

    prog::reg_index function_compiler::new_reg() {
        return ++reg_counter;
    }

    prog::reg_index function_compiler::unit_reg() {
        auto result = new_reg();
        add_instr(VARIANT(prog::instr, MAKE_UNIT, result));
        return result;
    }

    void function_compiler::add_instr(prog::instr&& instr) {
        frames.back().instrs.push_back(move(instr));
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

    void function_compiler::add_instrs(prog::instr_block&& block) {
        for (auto& instr_ptr : block.instrs)
            add_instr(move(*instr_ptr));
    }

    void function_compiler::add_copy_instrs(prog::reg_index value, const prog::type& type) {
        // TODO
        (void)value;
        (void)type;
    }

    void function_compiler::add_delete_instrs(prog::reg_index value, const prog::type& type) {
        // TODO
        (void)value;
        (void)type;
    }

    void function_compiler::add_delete_instrs(prog::reg_index value, const prog::type_local& type) {
        if (!type.confined)
            add_delete_instrs(value, *type.tp);
    }

    prog::var_index function_compiler::get_var(string name, location loc) {
        auto index = try_get_var(name);
        if (!index)
            clr.error(diags::variable_not_found(name), loc);
        return *index;
    }

    void function_compiler::delete_var(prog::var_index index, location loc) {
        auto& var = vars[index];

        if (!var.type.confined && !clr.type_trivial(*var.type.tp)) {
            if (var.state == VAR_INITIALIZED) {
                auto value = new_reg();
                auto read_instr = prog::read_var_instr { index, value };
                add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(read_instr)));
                add_delete_instrs(value, var.type);
            } else if (var.state & VAR_INITIALIZED)
                clr.error(diags::variable_not_deletable(var.name, var.state & VAR_UNINITIALIZED, var.state & VAR_MOVED_OUT), loc);
        }
    }

    void function_compiler::add_cleanup_action(function<void()> cleanup_action) {
        frames.back().cleanup_actions.push_back(cleanup_action);
    }

    void function_compiler::cleanup_frame(frame_index index, location loc) {
        auto& fr = frames[frames.size() - index - 1];

        for (auto var_index : fr.vars)
            delete_var(var_index, loc);

        for (auto iter = fr.cleanup_actions.rbegin(); iter != fr.cleanup_actions.rend(); iter++)
            (*iter)();
    }

    void function_compiler::add_return(prog::reg_index value, location loc) {
        auto frame_count = frames.size();

        for (size_t index = 0; index < frame_count; index++)
            cleanup_frame(index, loc);

        auto instr = prog::return_instr { value };
        add_instr(VARIANT(prog::instr, RETURN, into_ptr(instr)));

        returned = true;
    }

    void function_compiler::add_break(location loc) {
        auto frame_count = frames.size();
        size_t index;

        for (index = 0; index < frame_count; index++) {
            cleanup_frame(index, loc);
            if (frames[frame_count - index - 1].loop)
                break;
        }

        if (index == frame_count)
            clr.error(diags::break_outside_loop(), loc);

        add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
    }

    void function_compiler::add_continue(location loc) {
        auto frame_count = frames.size();
        size_t index;

        for (index = 0; index < frame_count; index++) {
            if (frames[frame_count - index - 1].loop)
                break;
            cleanup_frame(index, loc);
        }

        if (index == frame_count)
            clr.error(diags::continue_outside_loop(), loc);

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
}
