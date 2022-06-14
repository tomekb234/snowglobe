#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    using std::monostate;

    const prog::type_local function_compiler::NEVER_TYPE = { make_ptr(VARIANT(prog::type, NEVER, monostate())), false };
    const prog::type_local function_compiler::UNIT_TYPE = { make_ptr(VARIANT(prog::type, UNIT, monostate())), false };
    const prog::type_local function_compiler::BOOL_TYPE = { make_ptr(VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type { prog::primitive_type::BOOL }))), false };

    void function_compiler::compile(const ast::func_def& ast) {
        push_frame();

        for (auto& param : func.params) {
            auto var = add_var(param->name, make_ptr(copy_type_local(*param->tp)));
            auto instr = prog::write_var_instr { var, reg_counter++ };
            add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(instr)));
            init_var(var);
        }

        compile_stmt_block(*ast.body->block, false);

        if (ast.body->return_value) {
            if (returned)
                clr.warning(diags::dead_code(), **ast.body->return_value);
            compile_return(**ast.body->return_value, ast.body->return_value);
        } else if (!returned) {
            if (!INDEX_EQ(*func.return_tp, UNIT))
                clr.error(diags::missing_return(), ast.end_loc);
            add_return_instr(unit_reg());
        }

        func.vars = move(var_types);
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
        frames.push_back({ { }, { } });
    }

    prog::instr_block function_compiler::pop_frame() {
        auto& vars = frames.back().vars;

        for (auto& name : vars) {
            var_names[name].pop_back();
            if (var_names[name].empty())
                var_names.erase(name);
        }

        auto block = prog::instr_block { into_ptr_vector(frames.back().instrs) };
        frames.pop_back();
        return block;
    }

    prog::var_index function_compiler::add_var(string name, prog::ptr<prog::type_local> type) {
        auto index = var_types.size();

        var_types.push_back(move(type));
        var_states.emplace_back();
        frames.back().vars.push_back(name);
        var_names[name].push_back(index);

        return index;
    }

    optional<prog::var_index> function_compiler::get_var(string name) {
        if (var_names.count(name) == 0)
            return { };
        return { var_names[name].back() };
    }

    void function_compiler::init_var(prog::var_index index) {
        auto& state = var_states[index];
        state.initialized = true;
        state.uninitialized = false;
        state.moved_out = false;
        state.loop_level = 0;
    }

    void function_compiler::move_out_var(prog::var_index index) {
        auto& state = var_states[index];
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

    void function_compiler::incr_loop_level() {
        for (auto& state : var_states)
            state.loop_level++;
    }

    void function_compiler::decr_loop_level() {
        for (auto& state : var_states) {
            if (state.loop_level > 0)
                state.loop_level--;
        }
    }

    prog::reg_index function_compiler::add_copy_instrs(prog::reg_index value, const prog::type& type) {
        // TODO
        (void)type;
        return value;
    }

    void function_compiler::add_delete_instrs(prog::reg_index value, const prog::type_local& type) {
        if (type.confined)
            return;

        // TODO
        (void)value;
    }

    void function_compiler::add_cleanup_instrs(bool all_frames) {
        // TODO
        (void)all_frames;
    }

    void function_compiler::add_return_instr(prog::reg_index value) {
        add_cleanup_instrs(true);

        auto instr = prog::return_instr { value };
        add_instr(VARIANT(prog::instr, RETURN, into_ptr(instr)));

        returned = true;
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
}
