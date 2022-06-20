#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void function_compiler::compile_stmt_block(const ast::stmt_block& ast, bool cleanup) {
        for (const ast::stmt& stmt_ast : as_cref_vector(ast.stmts)) {
            if (returned)
                warning(diags::dead_code(), stmt_ast.loc);

            compile_stmt(stmt_ast);
        }

        if (cleanup)
            add_frame_cleanup(0, ast.end_loc);
    }

    void function_compiler::compile_stmt(const ast::stmt& ast) {
        switch (INDEX(ast)) {
            case ast::stmt::EXPR_EVAL: {
                auto& expr_ast = *GET(ast, EXPR_EVAL);
                if (INDEX_EQ(expr_ast, VAR_DECL))
                    compile_left_expr(expr_ast, { });
                else {
                    auto [value, type] = compile_expr(expr_ast, false);
                    add_deletion(value, type);
                }
            } break;

            case ast::stmt::ASSIGNMENT: {
                auto& assignment_ast = *GET(ast, ASSIGNMENT);
                auto [value, type] = compile_expr(*assignment_ast.value, false);
                auto lval = compile_left_expr(*assignment_ast.lvalue, { type });
                add_assignment(lval, value, type, assignment_ast.value->loc);
            } break;

            case ast::stmt::COMPOUND_ASSIGNMENT: {
                auto& expr_ast = *GET(ast, COMPOUND_ASSIGNMENT)->expr;
                auto [value, type] = compile_binary_operation(expr_ast);
                auto lval = compile_left_expr(*expr_ast.left, { type });
                add_assignment(lval, value, type, expr_ast.right->loc);
            } break;

            case ast::stmt::LOCALLY_BLOCK:
                compile_locally_block_stmt(*GET(ast, LOCALLY_BLOCK));
                break;

            case ast::stmt::SWAP:
                compile_swap_stmt(*GET(ast, SWAP));
                break;

            case ast::stmt::SWAP_BLOCK:
                compile_swap_block_stmt(*GET(ast, SWAP_BLOCK));
                break;

            case ast::stmt::IF:
                compile_if_stmt_branches(*GET(ast, IF), 0);
                break;

            case ast::stmt::MATCH:
                compile_match_stmt(*GET(ast, MATCH));
                break;

            case ast::stmt::WHILE:
                compile_while_stmt(*GET(ast, WHILE));
                break;

            case ast::stmt::FOR:
                compile_for_stmt(*GET(ast, FOR));
                break;

            case ast::stmt::FUNC_DEF:
                error(diags::not_implemented(), ast.loc); // TODO
        }
    }

    void function_compiler::compile_locally_block_stmt(const ast::locally_block_stmt& ast) {
        push_confining_frame();

        for (auto name : ast.var_names) {
            auto var_index = get_var(name, ast.names_loc);
            add_var_confinement(var_index, ast.names_loc);
        }

        compile_stmt_block(*ast.block, true);

        add_instrs(pop_confining_frame());
    }

    void function_compiler::compile_swap_stmt(const ast::swap_stmt& ast) {
        auto lval_a = compile_left_expr(*ast.left, { });
        auto lval_b = compile_left_expr(*ast.right, { });

        add_lvalues_swap(lval_a, lval_b, ast.loc);
    }

    void function_compiler::compile_swap_block_stmt(const ast::swap_block_stmt& ast) {
        auto& right_ast = *ast.right;
        auto& block_ast = *ast.block;

        auto lval_a = compile_left_expr(*ast.left, { });
        lvalue lval_b;
        optional<prog::var_index> var_index;

        if (INDEX_EQ(right_ast, EXPR))
            lval_b = compile_left_expr(*GET(right_ast, EXPR), { });
        else if (INDEX_EQ(right_ast, NAME_LOCALLY)) {
            auto name = GET(right_ast, NAME_LOCALLY);
            var_index = { get_var(name, right_ast.loc) };
            lval_b = VARIANT(lvalue, VAR, *var_index);
        }

        auto swap = [&] () {
            add_lvalues_swap(lval_a, lval_b, ast.loc);
        };

        push_frame();

        swap();
        defer_cleanup_action(swap);

        if (var_index) {
            push_confining_frame();
            add_var_confinement(*var_index, right_ast.loc);
        } else
            push_frame();

        compile_stmt_block(block_ast, true);

        if (var_index)
            add_instrs(pop_confining_frame());
        else
            add_instrs(pop_frame());

        add_frame_cleanup(0, block_ast.end_loc);
        add_instrs(pop_frame());
    }

    void function_compiler::compile_if_stmt_branches(const ast::if_stmt& ast, size_t branch_index) {
        auto& branch_ast = *ast.branches[branch_index];
        auto& cond_ast = *branch_ast.cond;
        auto& block_ast = *branch_ast.block;
        auto else_branch_ast = as_optional_cref(ast.else_branch);
        auto branch_count = ast.branches.size();

        if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
            auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
            auto [value, type] = compile_expr(expr_ast, true);
            auto cond = conv_clr.convert(value, type, prog::BOOL_TYPE, expr_ast.loc);

            auto true_branch = [&] () {
                compile_stmt_block(block_ast, true);
            };

            auto false_branch = [&] () {
                if (branch_index < branch_count - 1)
                    compile_if_stmt_branches(ast, branch_index + 1);
                else if (else_branch_ast)
                    compile_stmt_block(*else_branch_ast, true);
            };

            add_branch(cond, true_branch, false_branch);
        } else if (INDEX_EQ(cond_ast, CHECK_IF_PRESENT)) {
            auto& [lvalue_ast_ptr, value_ast_ptr] = GET(cond_ast, CHECK_IF_PRESENT);
            auto& lvalue_ast = *lvalue_ast_ptr;
            auto& value_ast = *value_ast_ptr;

            prog::reg_index value;
            prog::type_local type;
            auto confining = false;

            if (INDEX_EQ(value_ast, EXPR))
                tie(value, type) = compile_expr(*GET(value_ast, EXPR), false);
            else if (INDEX_EQ(value_ast, NAME_LOCALLY)) {
                auto name = GET(value_ast, NAME_LOCALLY);
                auto var_index = get_var(name, value_ast.loc);
                push_confining_frame();
                tie(value, type) = add_var_confinement(var_index, value_ast.loc);
                confining = true;
            }

            if (!INDEX_EQ(*type.tp, OPTIONAL))
                error(diags::expected_optional_type(clr.prog, move(*type.tp)), value_ast.loc);

            auto cond = new_reg();
            auto test_instr = prog::test_optional_instr { value, cond };
            add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

            auto true_branch = [&] () {
                auto result = new_reg();
                auto extract_instr = prog::extract_optional_value_instr { value, result };
                add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));

                auto inner_type = prog::type_local { make_ptr(copy_type(*GET(*type.tp, OPTIONAL))), type.confined };
                auto lval = compile_left_expr(lvalue_ast, { inner_type });
                add_assignment(lval, result, inner_type, value_ast.loc);

                compile_stmt_block(block_ast, true);
            };

            auto false_branch = [&] () {
                add_deletion(value, type);

                if (branch_index < branch_count - 1)
                    compile_if_stmt_branches(ast, branch_index + 1);
                else if (else_branch_ast)
                    compile_stmt_block(*else_branch_ast, true);
            };

            add_branch(cond, true_branch, false_branch);

            if (confining)
                add_instrs(pop_confining_frame());
        }
    }

    void function_compiler::compile_match_stmt(const ast::match_stmt& ast) {
        auto& value_ast = *ast.value;

        prog::reg_index value;
        prog::type_local type;
        auto confining = false;

        if (INDEX_EQ(value_ast, EXPR))
            tie(value, type) = compile_expr(*GET(value_ast, EXPR), false);
        else if (INDEX_EQ(value_ast, NAME_LOCALLY)) {
            auto name = GET(value_ast, NAME_LOCALLY);
            auto var_index = get_var(name, value_ast.loc);
            push_confining_frame();
            tie(value, type) = add_var_confinement(var_index, value_ast.loc);
            confining = true;
        }

        if (!INDEX_EQ(*type.tp, ENUM))
            error(diags::expected_enum_type(clr.prog, move(*type.tp)), value_ast.loc);

        compile_match_stmt_branches(ast, value, type, 0);

        if (confining)
            add_instrs(pop_confining_frame());
    }

    void function_compiler::compile_match_stmt_branches(const ast::match_stmt& ast, prog::reg_index value, const prog::type_local& type, size_t branch_index) {
        auto& value_ast = *ast.value;
        auto& branch_ast = *ast.branches[branch_index];
        auto& lval_ast = *branch_ast.lvalue;
        auto& block_ast = *branch_ast.block;
        auto else_branch_ast = as_optional_cref(ast.else_branch);
        auto branch_count = ast.branches.size();

        auto enum_index = GET(*type.tp, ENUM);
        auto& en = *clr.prog.enum_types[enum_index];

        string variant_name;
        optional<vector<cref<ast::expr_marked>>> arg_marked_asts;
        vector<cref<ast::expr>> arg_asts;

        if (INDEX_EQ(lval_ast, NAME))
            variant_name = GET(lval_ast, NAME);
        else if (INDEX_EQ(lval_ast, APPLICATION)) {
            auto& [receiver_ast_ptr, arg_marked_ast_ptrs] = GET(lval_ast, APPLICATION);
            auto& receiver_ast = *receiver_ast_ptr;
            if (!INDEX_EQ(receiver_ast, NAME))
                error(diags::invalid_expression(), lval_ast.loc);
            variant_name = GET(receiver_ast, NAME);
            arg_marked_asts = { as_cref_vector(arg_marked_ast_ptrs) };
        } else
            error(diags::invalid_expression(), lval_ast.loc);

        auto iter = en.variant_names.find(variant_name);
        if (iter == en.variant_names.end())
            error(diags::unknown_enum_variant(en, variant_name), lval_ast.loc);

        auto variant_index = iter->second;
        auto& variant = *en.variants[variant_index];
        auto count = variant.tps.size();
        auto confined = type.confined;

        if ((count > 0) != arg_marked_asts.has_value())
            error(diags::invalid_expression(), lval_ast.loc);
        if (count > 0)
            arg_asts = clr.order_args(*arg_marked_asts, { }, { count }, lval_ast.loc);

        push_frame();

        auto test_result = new_reg();
        auto test_instr = prog::test_variant_instr { value, variant_index, test_result };
        add_instr(VARIANT(prog::instr, TEST_VARIANT, into_ptr(test_instr)));

        auto true_branch = [&] () {
            for (size_t index = 0; index < count; index++) {
                auto extracted = new_reg();
                auto instr = prog::extract_variant_field_instr { value, variant_index, index, extracted };
                add_instr(VARIANT(prog::instr, EXTRACT_VARIANT_FIELD, into_ptr(instr)));

                auto field_type = prog::type_local { make_ptr(copy_type(*variant.tps[index])), confined };
                auto lval = compile_left_expr(arg_asts[index], { field_type });
                add_assignment(lval, extracted, field_type, value_ast.loc);
            }

            compile_stmt_block(block_ast, true);
            add_frame_cleanup(1, block_ast.end_loc);
        };

        auto false_branch = [&] () {
            if (branch_index < branch_count - 1)
                compile_match_stmt_branches(ast, value, type, branch_index + 1);
            else {
                add_deletion(value, type);
                if (else_branch_ast)
                    compile_stmt_block(*else_branch_ast, true);
            }
        };

        add_branch(test_result, true_branch, false_branch);

        add_instrs(pop_frame());
    }

    void function_compiler::compile_while_stmt(const ast::while_stmt& ast) {
        auto& cond_ast = *ast.cond;
        auto& block_ast = *ast.block;
        auto else_block_ast = as_optional_cref(ast.else_block);

        if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
            auto head = [&] () -> prog::reg_index {
                auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
                auto [value, type] = compile_expr(expr_ast, true);
                return conv_clr.convert(value, type, prog::BOOL_TYPE, expr_ast.loc);
            };

            auto true_branch = [&] () {
                compile_stmt_block(block_ast, true);
            };

            auto false_branch = [&] () {
                if (else_block_ast)
                    compile_stmt_block(*else_block_ast, true);
            };

            add_loop(head, true_branch, false_branch, [] { });
        } else if (INDEX_EQ(cond_ast, CHECK_IF_PRESENT)) {
            auto& [lvalue_ast_ptr, value_ast_ptr] = GET(cond_ast, CHECK_IF_PRESENT);
            auto& lvalue_ast = *lvalue_ast_ptr;
            auto& value_ast = *value_ast_ptr;

            prog::reg_index value;
            prog::type_local type;
            auto confining = false;

            auto head = [&] () -> prog::reg_index {
                if (INDEX_EQ(value_ast, EXPR))
                    tie(value, type) = compile_expr(*GET(value_ast, EXPR), false);
                else if (INDEX_EQ(value_ast, NAME_LOCALLY)) {
                    auto name = GET(value_ast, NAME_LOCALLY);
                    auto var_index = get_var(name, value_ast.loc);
                    push_confining_frame();
                    tie(value, type) = add_var_confinement(var_index, value_ast.loc);
                    confining = true;
                }

                if (!INDEX_EQ(*type.tp, OPTIONAL))
                    error(diags::expected_optional_type(clr.prog, move(*type.tp)), value_ast.loc);

                auto cond = new_reg();
                auto test_instr = prog::test_optional_instr { value, cond };
                add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));
                return cond;
            };

            auto true_branch = [&] () {
                auto result = new_reg();
                auto extract_instr = prog::extract_optional_value_instr { value, result };
                add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));

                auto inner_type = prog::type_local { make_ptr(copy_type(*GET(*type.tp, OPTIONAL))), type.confined };
                auto lval = compile_left_expr(lvalue_ast, { inner_type });
                add_assignment(lval, result, inner_type, value_ast.loc);

                compile_stmt_block(block_ast, true);
            };

            auto false_branch = [&] () {
                add_deletion(value, type);

                if (else_block_ast)
                    compile_stmt_block(*else_block_ast, true);
            };

            auto end = [&] () {
                if (confining)
                    add_instrs(pop_confining_frame());
            };

            add_loop(head, true_branch, false_branch, end);
        }
    }

    void function_compiler::compile_for_stmt(const ast::for_stmt& ast) {
        if (INDEX_EQ(ast, RANGE)) {
            auto& range_ast = *GET(ast, RANGE);
            auto& begin_ast = *range_ast.begin;
            auto& end_ast = *range_ast.end;
            auto& lvalue_ast = *range_ast.lvalue;
            auto& block_ast = *range_ast.block;
            auto else_block_ast = as_optional_cref(range_ast.else_block);

            prog::reg_index begin_value, end_value;
            prog::type_local begin_type, end_type;

            tie(begin_value, begin_type) = compile_expr(begin_ast, true);
            tie(end_value, end_type) = compile_expr(end_ast, true);
            auto incr = !range_ast.reversed;

            auto type = clr.common_supertype(*begin_type.tp, *end_type.tp, ast.loc);
            auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

            if (!INDEX_EQ(type, NUMBER))
                error(diags::expected_integer_type(clr.prog, move(type)), begin_ast.loc);

            auto& ntype = *GET(type, NUMBER);
            prog::numeric_binary_operation_instr::kind_t op_kind;

            using num = prog::number_type;

            switch (ntype.tp) {
                case num::BOOL:
                case num::U8:
                case num::U16:
                case num::U32:
                case num::U64:
                    op_kind = prog::numeric_binary_operation_instr::UNSIGNED;
                    break;

                case num::I8:
                case num::I16:
                case num::I32:
                case num::I64:
                    op_kind = prog::numeric_binary_operation_instr::SIGNED;
                    break;

                case num::F32:
                case num::F64:
                    error(diags::expected_integer_type(clr.prog, move(type)), begin_ast.loc);
            }

            begin_value = conv_clr.convert(begin_value, begin_type, type, begin_ast.loc);
            end_value = conv_clr.convert(end_value, end_type, type, end_ast.loc);

            auto var_index = add_var(copy_type_local(type_local));
            auto write_instr = prog::write_var_instr { var_index, incr ? begin_value : end_value };
            add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));
            vars[var_index].state = VAR_INITIALIZED;

            auto value = new_reg();

            auto head = [&] () -> prog::reg_index {
                auto read_instr = prog::read_var_instr { var_index, value };
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

            auto true_branch = [&] () {
                if (!incr) {
                    auto new_value = new_reg();
                    auto decr_instr = prog::unary_operation_instr { value, new_value };
                    add_instr(VARIANT(prog::instr, DECR, into_ptr(decr_instr)));
                    value = new_value;
                }

                auto lval = compile_left_expr(lvalue_ast, { type_local });

                add_assignment(lval, value, type_local, begin_ast.loc);

                compile_stmt_block(block_ast, true);

                if (incr) {
                    auto new_value = new_reg();
                    auto incr_instr = prog::unary_operation_instr { value, new_value };
                    add_instr(VARIANT(prog::instr, INCR, into_ptr(incr_instr)));
                    value = new_value;
                }

                auto write_instr = prog::write_var_instr { var_index, value };
                add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));
            };

            auto false_branch = [&] () {
                if (else_block_ast)
                    compile_stmt_block(*else_block_ast, true);
            };

            add_loop(head, true_branch, false_branch, [] { });
        } else if (INDEX_EQ(ast, SLICE)) {
            error(diags::not_implemented(), ast.loc); // TODO
        }
    }
}
