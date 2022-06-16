#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    const prog::type_local function_compiler::NEVER_TYPE = { make_ptr(VARIANT(prog::type, NEVER, monostate())), false };
    const prog::type_local function_compiler::UNIT_TYPE = { make_ptr(VARIANT(prog::type, UNIT, monostate())), false };
    const prog::type_local function_compiler::BOOL_TYPE = { make_ptr(VARIANT(prog::type, NUMBER, make_ptr(prog::number_type { prog::number_type::BOOL }))), false };

    void function_compiler::compile(const ast::func_def& ast) {
        push_frame();

        for (auto& param_ptr : func.params) {
            auto var = add_var(param_ptr->name, copy_type_local(*param_ptr->tp));
            auto instr = prog::write_var_instr { var, reg_counter++ };
            add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(instr)));
            init_var(var);
        }

        compile_stmt_block(*ast.body->block, false);

        if (ast.body->return_value) {
            if (returned)
                clr.warning(diags::dead_code(), (*ast.body->return_value)->loc);
            compile_return(as_optional_cref(ast.body->return_value), (*ast.body->return_value)->loc);
        } else if (!returned) {
            if (!INDEX_EQ(*func.return_tp, UNIT))
                clr.error(diags::missing_return(), ast.body->block->end_loc);
            add_return_instr(unit_reg(), ast.body->block->end_loc);
        }

        func.vars = into_ptr_vector(var_types);
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
        frames.push_back({ { }, { }, { }, false });
    }

    void function_compiler::push_loop_frame() {
        frames.push_back({ { }, { }, { }, true });

        for (auto& state : var_states)
            state.loop_level++;
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
        auto block = pop_frame();

        for (auto& state : var_states) {
            if (state.loop_level > 0)
                state.loop_level--;
        }

        return block;
    }

    prog::var_index function_compiler::add_var(prog::type_local&& type) {
        auto var = var_types.size();

        var_types.push_back(move(type));
        var_states.emplace_back();
        frames.back().vars.push_back(var);

        return var;
    }

    prog::var_index function_compiler::add_var(string name, prog::type_local&& type) {
        auto var = add_var(move(type));

        frames.back().var_names.push_back(name);
        var_names[name].push_back(var);

        return var;
    }

    optional<prog::var_index> function_compiler::get_var(string name) {
        auto iter = var_names.find(name);
        if (iter == var_names.end())
            return { };
        return { iter->second.back() };
    }

    void function_compiler::init_var(prog::var_index var) {
        auto& state = var_states[var];
        state.initialized = true;
        state.uninitialized = false;
        state.moved_out = false;
        state.loop_level = 0;
    }

    void function_compiler::move_out_var(prog::var_index var) {
        auto& state = var_states[var];
        state.initialized = false;
        state.uninitialized = false;
        state.moved_out = true;
    }

    vector<function_compiler::var_state> function_compiler::backup_var_states() {
        return var_states;
    }

    void function_compiler::restore_var_states(const vector<var_state>& states) {
        auto count = states.size();
        for (size_t index = 0; index < count; index++)
            var_states[index] = states[index];
    }

    void function_compiler::merge_var_states(const vector<var_state>& states) {
        auto count = states.size();
        for (size_t index = 0; index < count; index++) {
            var_states[index].initialized |= states[index].initialized;
            var_states[index].uninitialized |= states[index].uninitialized;
            var_states[index].moved_out |= states[index].moved_out;
        }
    }

    void function_compiler::add_copy_instrs(prog::reg_index value, const prog::type& type) {
        // TODO
        (void)value;
        (void)type;
    }

    void function_compiler::add_delete_instrs(prog::reg_index value, const prog::type_local& type) {
        if (type.confined)
            return;

        // TODO
        (void)value;
    }

    void function_compiler::add_cleanup_instrs(const frame& fr, location loc) {
        // TODO
        (void)fr;
        (void)loc;
    }

    void function_compiler::add_return_instr(prog::reg_index value, location loc) {
        for (auto iter = frames.rbegin(); iter != frames.rend(); iter++)
            add_cleanup_instrs(*iter, loc);

        auto instr = prog::return_instr { value };
        add_instr(VARIANT(prog::instr, RETURN, into_ptr(instr)));

        returned = true;
    }

    void function_compiler::add_break_instr(location loc) {
        for (auto iter = frames.rbegin(); true; iter++) {
            if (iter == frames.rend())
                clr.error(diags::break_outside_loop(), loc);
            add_cleanup_instrs(*iter, loc);
            if (iter->loop)
                break;
        }

        add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
    }

    void function_compiler::add_continue_instr(location loc) {
        for (auto iter = frames.rbegin(); true; iter++) {
            if (iter == frames.rend())
                clr.error(diags::continue_outside_loop(), loc);
            if (iter->loop)
                break;
            add_cleanup_instrs(*iter, loc);
        }

        add_instr(VARIANT(prog::instr, CONTINUE_LOOP, monostate()));
    }

    void function_compiler::add_branch_instr(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch) {
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

        auto instr = prog::branch_instr { cond, into_ptr(true_block), into_ptr(false_block) };
        add_instr(VARIANT(prog::instr, BRANCH, into_ptr(instr)));

        merge_var_states(branch_var_states);
        returned &= branch_returned;
    }

    void function_compiler::add_loop_instr(function<prog::reg_index()> head, function<void()> body, function<void()> end) {
        push_loop_frame();

        auto cond = head();

        auto init_var_states = backup_var_states();
        auto init_returned = returned;

        push_frame();
        body();
        auto true_block = pop_frame();

        auto branch_var_states = backup_var_states();
        auto branch_returned = returned;

        merge_var_states(init_var_states);
        returned &= init_returned;

        push_frame();
        end();
        add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
        auto false_block = pop_frame();

        auto branch_instr = prog::branch_instr { cond, into_ptr(true_block), into_ptr(false_block) };
        add_instr(VARIANT(prog::instr, BRANCH, into_ptr(branch_instr)));

        auto loop_block = pop_loop_frame();
        add_instr(VARIANT(prog::instr, LOOP, into_ptr(loop_block)));

        merge_var_states(init_var_states);
        merge_var_states(branch_var_states);
        returned &= init_returned & branch_returned;
    }
}
