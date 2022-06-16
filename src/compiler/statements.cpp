#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    using std::tie;

    void function_compiler::compile_stmt_block(const ast::stmt_block& ast, bool cleanup) {
        for (auto& stmt_ast : ast.stmts) {
            if (returned)
                clr.warning(diags::dead_code(), stmt_ast->loc);

            compile_stmt(*stmt_ast);
        }

        if (cleanup)
            add_cleanup_instrs(frames.back(), ast.end_loc);
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
                auto lval = compile_left_expr(*assignment_ast.lvalue, type);
                compile_assignment(lval, result, type, ast.loc);
            } break;

            case ast::stmt::COMPOUND_ASSIGNMENT:
            case ast::stmt::LOCALLY_BLOCK:
            case ast::stmt::SWAP:
            case ast::stmt::SWAP_BLOCK:
                clr.error(diags::not_implemented(), ast.loc); // TODO

            case ast::stmt::IF:
                compile_if_stmt(*GET(ast, IF));
                break;

            case ast::stmt::MATCH:
                clr.error(diags::not_implemented(), ast.loc); // TODO

            case ast::stmt::WHILE:
                compile_while_stmt(*GET(ast, WHILE));
                break;

            case ast::stmt::FOR:
                compile_for_stmt(*GET(ast, FOR));
                break;

            case ast::stmt::FUNC_DEF:
                clr.error(diags::not_implemented(), ast.loc); // TODO
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
            cond = conv_clr.convert(value, type, BOOL_TYPE, expr_ast.loc);
        } else
            clr.error(diags::not_implemented(), cond_ast.loc); // TODO

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

        auto head = [&] () -> prog::reg_index {
            if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
                auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
                auto [value, type] = compile_expr(expr_ast, true);
                return conv_clr.convert(value, type, BOOL_TYPE, expr_ast.loc);
            } else
                clr.error(diags::not_implemented(), cond_ast.loc); // TODO
        };

        auto body = [&] () {
            compile_stmt_block(*ast.block);
        };

        auto end = [&] () {
            if (ast.else_block)
                compile_stmt_block(**ast.else_block);
        };

        add_loop_instr(head, body, end);
    }

    void function_compiler::compile_for_stmt(const ast::for_stmt& ast) {
        if (INDEX_EQ(ast, RANGE)) {
            auto& range_ast = *GET(ast, RANGE);

            prog::reg_index begin_value, end_value;
            prog::type_local begin_type, end_type;

            tie(begin_value, begin_type) = compile_expr(*range_ast.begin, true);
            tie(end_value, end_type) = compile_expr(*range_ast.end, true);
            auto incr = !range_ast.reversed;

            auto type = clr.common_supertype(*begin_type.tp, *begin_type.tp, ast.loc);
            auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

            if (!INDEX_EQ(type, NUMBER))
                clr.error(diags::expected_integer_type(clr.prog, copy_type(type)), range_ast.begin->loc);

            auto& ntype = *GET(type, NUMBER);
            prog::numeric_binary_operation_instr::kind_t op_kind;

            switch (ntype.tp) {
                case prog::number_type::BOOL:
                case prog::number_type::U8:
                case prog::number_type::U16:
                case prog::number_type::U32:
                case prog::number_type::U64:
                    op_kind = prog::numeric_binary_operation_instr::UNSIGNED;
                    break;

                case prog::number_type::I8:
                case prog::number_type::I16:
                case prog::number_type::I32:
                case prog::number_type::I64:
                    op_kind = prog::numeric_binary_operation_instr::SIGNED;
                    break;

                case prog::number_type::F32:
                case prog::number_type::F64:
                    clr.error(diags::expected_integer_type(clr.prog, copy_type(type)), range_ast.begin->loc);
            }

            begin_value = conv_clr.convert(begin_value, begin_type, type, range_ast.begin->loc);
            end_value = conv_clr.convert(end_value, end_type, type, range_ast.end->loc);

            auto var = add_var(copy_type_local(type_local));
            auto write_instr = prog::write_var_instr { var, incr ? begin_value : end_value };
            add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));
            init_var(var);

            auto value = new_reg();

            auto head = [&] () -> prog::reg_index {
                auto read_instr = prog::read_var_instr { var, value };
                add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(read_instr)));

                auto cond = new_reg();

                if (incr) {
                    auto compare_instr = prog::numeric_binary_operation_instr { { value, end_value, cond }, op_kind };
                    add_instr(VARIANT(prog::instr, LS, into_ptr(compare_instr)));
                } else {
                    auto compare_instr = prog::numeric_binary_operation_instr { { value, begin_value, cond }, op_kind };
                    add_instr(VARIANT(prog::instr, GT, into_ptr(compare_instr)));
                }

                return cond;
            };

            auto body = [&] () {
                if (!incr) {
                    auto new_value = new_reg();
                    auto decr_instr = prog::unary_operation_instr { value, new_value };
                    add_instr(VARIANT(prog::instr, DECR, into_ptr(decr_instr)));
                    value = new_value;
                }

                auto lval = compile_left_expr(*range_ast.lvalue, { type_local });

                compile_assignment(lval, value, type_local, range_ast.lvalue->loc);

                compile_stmt_block(*range_ast.block);

                if (incr) {
                    auto new_value = new_reg();
                    auto incr_instr = prog::unary_operation_instr { value, new_value };
                    add_instr(VARIANT(prog::instr, INCR, into_ptr(incr_instr)));
                    value = new_value;
                }

                auto write_instr = prog::write_var_instr { var, value };
                add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));
            };

            auto end = [&] () {
                if (range_ast.else_block)
                    compile_stmt_block(**range_ast.else_block);
            };

            add_loop_instr(head, body, end);
        } else if (INDEX_EQ(ast, SLICE)) {
            clr.error(diags::not_implemented(), ast.loc); // TODO
        }
    }
}
