#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    using std::monostate;

    void function_compiler::compile_stmt_block(const ast::stmt_block& ast, bool cleanup) {
        for (auto& stmt_ast : ast.stmts) {
            if (returned)
                clr.warning(diags::dead_code(), *stmt_ast);

            compile_stmt(*stmt_ast);
        }

        if (cleanup)
            add_cleanup_instrs();
    }

    void function_compiler::compile_stmt(const ast::stmt& ast) {
        switch (INDEX(ast)) {
            case ast::stmt::EXPR_EVAL: {
                auto& expr_ast = *GET(ast, EXPR_EVAL);
                if (INDEX_EQ(expr_ast, VAR_DECL))
                    compile_left_expr(expr_ast, { });
                else {
                    auto [value, type] = compile_expr(expr_ast, false);
                    add_delete_instrs(value, type);
                }
            } break;

            case ast::stmt::ASSIGNMENT: {
                auto& assignment_ast = *GET(ast, ASSIGNMENT);
                auto [result, type] = compile_expr(*assignment_ast.value, false);
                auto lvalue = compile_left_expr(*assignment_ast.lvalue, type);

                switch (INDEX(lvalue)) { // TODO add more assignment options, move to separate method
                    case lvalue::VAR: {
                        auto var_index = GET(lvalue, VAR);
                        auto& var_type = var_types[var_index];
                        result = conv_clr.convert(assignment_ast, result, type, var_type);
                        auto instr = prog::write_var_instr { var_index, result };
                        add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(instr)));
                        init_var(var_index);
                    } break;

                    case lvalue::GLOBAL_VAR: {
                        auto var_index = GET(lvalue, GLOBAL_VAR);
                        auto& var = *clr.prog.global_vars[var_index];
                        result = conv_clr.convert(assignment_ast, result, type, *var.tp);
                        auto instr = prog::write_global_var_instr { var_index, result };
                        add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, into_ptr(instr)));
                    } break;

                    default:
                        clr.error(diags::not_implemented(), assignment_ast);
                }
            } break;

            case ast::stmt::COMPOUND_ASSIGNMENT:
            case ast::stmt::LOCALLY_BLOCK:
            case ast::stmt::SWAP:
            case ast::stmt::SWAP_BLOCK:
                clr.error(diags::not_implemented(), ast); // TODO

            case ast::stmt::IF:
                compile_if_stmt(*GET(ast, IF));
                break;

            case ast::stmt::MATCH:
                clr.error(diags::not_implemented(), ast); // TODO

            case ast::stmt::WHILE:
                compile_while_stmt(*GET(ast, WHILE));
                break;

            case ast::stmt::FOR:
            case ast::stmt::FUNC_DEF:
                clr.error(diags::not_implemented(), ast); // TODO
        }
    }

    void function_compiler::compile_if_stmt(const ast::if_stmt& ast, size_t branch_index) {
        auto& branch_ast = *ast.branches[branch_index];
        auto& cond_ast = *branch_ast.cond;
        auto& block_ast = *branch_ast.block;

        prog::reg_index cond;

        if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
            auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
            auto [value, type] = compile_expr(expr_ast, true);
            cond = conv_clr.convert(expr_ast, value, type, BOOL_TYPE);
        } else
            clr.error(diags::not_implemented(), cond_ast); // TODO

        auto true_branch = [&] () {
            compile_stmt_block(block_ast);
        };

        auto false_branch = [&] () {
            if (branch_index < ast.branches.size() - 1)
                compile_if_stmt(ast, branch_index + 1);
            else if (ast.else_branch)
                compile_stmt_block(**ast.else_branch);
        };

        add_branch_instr(cond, true_branch, false_branch);
    }

    void function_compiler::compile_while_stmt(const ast::while_stmt& ast) {
        auto& cond_ast = *ast.cond;

        push_frame();

        prog::reg_index cond;

        if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
            auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
            auto [value, type] = compile_expr(expr_ast, true);
            cond = conv_clr.convert(expr_ast, value, type, BOOL_TYPE);
        } else
            clr.error(diags::not_implemented(), cond_ast); // TODO

        auto init_var_states = backup_var_states();
        auto init_returned = returned;

        push_frame();
        incr_loop_level();
        compile_stmt_block(*ast.block);
        decr_loop_level();
        auto true_block = pop_frame();

        auto branch_var_states = backup_var_states();
        auto branch_returned = returned;

        merge_var_states(init_var_states);
        returned &= init_returned;

        push_frame();
        if (ast.else_block)
            compile_stmt_block(**ast.else_block);
        add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
        auto false_block = pop_frame();

        auto branch_instr = prog::branch_instr { cond, into_ptr(true_block), into_ptr(false_block) };
        add_instr(VARIANT(prog::instr, BRANCH, into_ptr(branch_instr)));

        auto block = pop_frame();
        add_instr(VARIANT(prog::instr, LOOP, into_ptr(block)));

        merge_var_states(init_var_states);
        merge_var_states(branch_var_states);
        returned &= init_returned & branch_returned;
    }
}
