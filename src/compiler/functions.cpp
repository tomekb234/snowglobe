#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void function_compiler::compile(const ast::func_def& ast) {
        push_frame();

        for (auto& param_ptr : func.params) {
            auto var = add_var(param_ptr->name, copy_type_local(*param_ptr->tp));
            auto instr = prog::write_var_instr { var, reg_counter++ };
            add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(instr)));
            var_states[var] = VAR_INITIALIZED;
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

        for (auto& level : var_loop_levels)
            level++;
    }

    prog::instr_block function_compiler::pop_frame() {
        for (auto& name : frames.back().var_names) {
            var_names_map[name].pop_back();
            if (var_names_map[name].empty())
                var_names_map.erase(name);
        }

        auto block = prog::instr_block { into_ptr_vector(frames.back().instrs) };
        frames.pop_back();
        return block;
    }

    prog::instr_block function_compiler::pop_loop_frame() {
        auto block = pop_frame();

        for (auto& level : var_loop_levels) {
            if (level > 0)
                level--;
        }

        return block;
    }

    prog::var_index function_compiler::add_var(prog::type_local&& type) {
        auto var = var_types.size();

        var_types.push_back(move(type));
        var_states.push_back(VAR_UNINITIALIZED);
        var_loop_levels.push_back(0);
        var_names.emplace_back();
        frames.back().vars.push_back(var);

        return var;
    }

    prog::var_index function_compiler::add_var(string name, prog::type_local&& type) {
        auto var = add_var(move(type));

        var_names[var] = { name };
        frames.back().var_names.push_back(name);
        var_names_map[name].push_back(var);

        return var;
    }

    optional<prog::var_index> function_compiler::get_var(string name) {
        auto iter = var_names_map.find(name);
        if (iter == var_names_map.end())
            return { };
        return { iter->second.back() };
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
        for (size_t index = 0; index < count; index++)
            var_states[index] |= states[index];
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

    void function_compiler::add_var_delete_instrs(prog::var_index var, location loc) {
        auto& type = var_types[var];
        auto& state = var_states[var];
        auto& name = var_names[var];

        if (!type.confined && !clr.type_trivially_copyable(*type.tp)) {
            if (state == VAR_INITIALIZED) {
                auto value = new_reg();
                auto read_instr = prog::read_var_instr { var, value };
                add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(read_instr)));
                add_delete_instrs(value, type);
            } else if (state & VAR_INITIALIZED)
                clr.error(diags::variable_not_deletable(name, state & VAR_UNINITIALIZED, state & VAR_MOVED_OUT), loc);
        }
    }

    void function_compiler::add_frame_delete_instrs(const frame& fr, location loc) {
        for (auto var : fr.vars)
            add_var_delete_instrs(var, loc);
    }

    void function_compiler::add_return_instr(prog::reg_index value, location loc) {
        for (auto iter = frames.rbegin(); iter != frames.rend(); iter++)
            add_frame_delete_instrs(*iter, loc);

        auto instr = prog::return_instr { value };
        add_instr(VARIANT(prog::instr, RETURN, into_ptr(instr)));

        returned = true;
    }

    void function_compiler::add_break_instr(location loc) {
        for (auto iter = frames.rbegin(); true; iter++) {
            if (iter == frames.rend())
                clr.error(diags::break_outside_loop(), loc);
            add_frame_delete_instrs(*iter, loc);
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
            add_frame_delete_instrs(*iter, loc);
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
