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
                clr.warning(diags::dead_code(), stmt_ast.loc);

            compile_stmt(stmt_ast);
        }

        if (cleanup)
            cleanup_frame(0, ast.end_loc);
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
                compile_assignment(lval, result, type, assignment_ast.value->loc);
            } break;

            case ast::stmt::COMPOUND_ASSIGNMENT:
                clr.error(diags::not_implemented(), ast.loc); // TODO

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
                clr.error(diags::not_implemented(), ast.loc); // TODO
        }
    }

    void function_compiler::compile_locally_block_stmt(const ast::locally_block_stmt& ast) {
        push_confining_frame();

        for (auto name : ast.var_names) {
            auto var_index = get_var(name, ast.names_loc);
            compile_confinement(var_index, ast.names_loc);
        }

        compile_stmt_block(*ast.block, true);

        add_instrs(pop_confining_frame());
    }

    void function_compiler::compile_swap_stmt(const ast::swap_stmt& ast) {
        auto [left_val, left_type] = compile_expr(*ast.left, false);
        auto [right_val, right_type] = compile_expr(*ast.right, false);

        auto left_lval = compile_left_expr(*ast.left, { right_type });
        auto right_lval = compile_left_expr(*ast.right, { left_type });

        left_val = conv_clr.convert(left_val, left_type, right_type, ast.left->loc);
        right_val = conv_clr.convert(right_val, right_type, left_type, ast.right->loc);

        compile_assignment(left_lval, right_val, right_type, ast.right->loc);
        compile_assignment(right_lval, left_val, left_type, ast.left->loc);
    }

    void function_compiler::compile_swap_block_stmt(const ast::swap_block_stmt& ast) {
        auto confining = INDEX_EQ(*ast.right, NAME_LOCALLY);
        prog::var_index right_var_index;

        if (confining)
            right_var_index = get_var(GET(*ast.right, NAME_LOCALLY), ast.right->loc);

        auto swap = [&] () {
            prog::reg_index left_val, right_val;
            prog::type_local left_type, right_type;
            lvalue left_lval, right_lval;

            tie(left_val, left_type) = compile_expr(*ast.left, false);

            if (confining) {
                tie(right_val, right_type) = compile_var_read(right_var_index, false, ast.right->loc);
                left_lval = compile_left_expr(*ast.left, { right_type });
                right_lval = VARIANT(lvalue, VAR, right_var_index);
            } else {
                tie(right_val, right_type) = compile_expr(*GET(*ast.right, EXPR), false);
                left_lval = compile_left_expr(*ast.left, { right_type });
                right_lval = compile_left_expr(*GET(*ast.right, EXPR), { left_type });
            }

            left_val = conv_clr.convert(left_val, left_type, right_type, ast.left->loc);
            right_val = conv_clr.convert(right_val, right_type, left_type, ast.right->loc);

            compile_assignment(left_lval, right_val, right_type, ast.right->loc);
            compile_assignment(right_lval, left_val, left_type, ast.left->loc);
        };

        push_frame();

        swap();
        add_cleanup_action(swap);

        if (confining) {
            push_confining_frame();
            compile_confinement(right_var_index, ast.right->loc);
        } else
            push_frame();

        compile_stmt_block(*ast.block, true);

        if (confining)
            add_instrs(pop_confining_frame());
        else
            add_instrs(pop_frame());

        cleanup_frame(0, ast.block->end_loc);
        add_instrs(pop_frame());
    }

    void function_compiler::compile_if_stmt_branches(const ast::if_stmt& ast, size_t branch_index) {
        auto& branch_ast = *ast.branches[branch_index];
        auto& cond_ast = *branch_ast.cond;
        auto& block_ast = *branch_ast.block;

        prog::reg_index cond;

        if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
            auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
            auto [value, type] = compile_expr(expr_ast, true);
            cond = conv_clr.convert(value, type, prog::BOOL_TYPE, expr_ast.loc);
        } else
            clr.error(diags::not_implemented(), cond_ast.loc); // TODO

        auto true_branch = [&] () {
            compile_stmt_block(block_ast, true);
        };

        auto false_branch = [&] () {
            if (branch_index < ast.branches.size() - 1)
                compile_if_stmt_branches(ast, branch_index + 1);
            else if (ast.else_branch)
                compile_stmt_block(**ast.else_branch, true);
        };

        add_branch(cond, true_branch, false_branch);
    }

    void function_compiler::compile_match_stmt(const ast::match_stmt& ast) {
        prog::reg_index value;
        prog::type_local type;

        if (INDEX_EQ(*ast.value, EXPR))
            tie(value, type) = compile_expr(*GET(*ast.value, EXPR), false);
        else if (INDEX_EQ(*ast.value, NAME_LOCALLY)) {
            auto name = GET(*ast.value, NAME_LOCALLY);
            auto var_index = get_var(name, ast.value->loc);
            push_confining_frame();
            tie(value, type) = compile_confinement(var_index, ast.value->loc);
        }

        if (!INDEX_EQ(*type.tp, ENUM))
            clr.error(diags::expected_enum_type(clr.prog, copy_type(*type.tp)), ast.value->loc);

        compile_match_stmt_branches(ast, value, type, 0);

        if (INDEX_EQ(*ast.value, NAME_LOCALLY))
            add_instrs(pop_confining_frame());
    }

    void function_compiler::compile_match_stmt_branches(const ast::match_stmt& ast, prog::reg_index value, const prog::type_local& type, size_t branch_index) {
        auto& branch_ast = *ast.branches[branch_index];
        auto& lval_ast = *branch_ast.lvalue;
        auto& block_ast = *branch_ast.block;

        push_frame();

        auto lval = compile_left_expr(lval_ast, { type });

        if (!INDEX_EQ(lval, ENUM_VARIANT))
            clr.error(diags::expected_enum_variant(), lval_ast.loc);

        auto& [enum_index, variant_index, lval_ptrs] = GET(lval, ENUM_VARIANT);
        auto variant_index_copy = variant_index;
        auto lvals = as_cref_vector(lval_ptrs);

        if (enum_index != GET(*type.tp, ENUM))
            clr.error(diags::invalid_type(clr.prog, copy_type(*type.tp), VARIANT(prog::type, ENUM, enum_index)), ast.value->loc);

        auto& en = *clr.prog.enum_types[enum_index];
        auto& variant = *en.variants[variant_index];
        auto count = variant.tps.size();
        auto confined = type.confined;

        auto test_result = new_reg();
        auto test_instr = prog::test_variant_instr { value, variant_index, test_result };
        add_instr(VARIANT(prog::instr, TEST_VARIANT, into_ptr(test_instr)));

        auto true_branch = [&] () {
            for (size_t index = 0; index < count; index++) {
                auto extracted = new_reg();
                auto instr = prog::extract_variant_field_instr { value, variant_index_copy, index, extracted };
                add_instr(VARIANT(prog::instr, EXTRACT_VARIANT_FIELD, into_ptr(instr)));

                auto field_type = prog::type_local { make_ptr(copy_type(*variant.tps[index])), confined };
                compile_assignment(lvals[index], extracted, field_type, ast.value->loc);
            }

            compile_stmt_block(block_ast, true);
            cleanup_frame(1, block_ast.end_loc);
        };

        auto false_branch = [&] () {
            if (branch_index < ast.branches.size() - 1)
                compile_match_stmt_branches(ast, value, type, branch_index + 1);
            else {
                add_delete_instrs(value, type);
                if (ast.else_branch)
                    compile_stmt_block(**ast.else_branch, true);
            }
        };

        add_branch(test_result, true_branch, false_branch);

        add_instrs(pop_frame());
    }

    void function_compiler::compile_while_stmt(const ast::while_stmt& ast) {
        auto& cond_ast = *ast.cond;

        auto head = [&] () -> prog::reg_index {
            if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
                auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
                auto [value, type] = compile_expr(expr_ast, true);
                return conv_clr.convert(value, type, prog::BOOL_TYPE, expr_ast.loc);
            } else
                clr.error(diags::not_implemented(), cond_ast.loc); // TODO
        };

        auto body = [&] () {
            compile_stmt_block(*ast.block, true);
        };

        auto end = [&] () {
            if (ast.else_block)
                compile_stmt_block(**ast.else_block, true);
        };

        add_loop(head, body, end);
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

            auto body = [&] () {
                if (!incr) {
                    auto new_value = new_reg();
                    auto decr_instr = prog::unary_operation_instr { value, new_value };
                    add_instr(VARIANT(prog::instr, DECR, into_ptr(decr_instr)));
                    value = new_value;
                }

                auto lval = compile_left_expr(*range_ast.lvalue, { type_local });

                compile_assignment(lval, value, type_local, range_ast.begin->loc);

                compile_stmt_block(*range_ast.block, true);

                if (incr) {
                    auto new_value = new_reg();
                    auto incr_instr = prog::unary_operation_instr { value, new_value };
                    add_instr(VARIANT(prog::instr, INCR, into_ptr(incr_instr)));
                    value = new_value;
                }

                auto write_instr = prog::write_var_instr { var_index, value };
                add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));
            };

            auto end = [&] () {
                if (range_ast.else_block)
                    compile_stmt_block(**range_ast.else_block, true);
            };

            add_loop(head, body, end);
        } else if (INDEX_EQ(ast, SLICE)) {
            clr.error(diags::not_implemented(), ast.loc); // TODO
        }
    }

    void function_compiler::compile_assignment(const lvalue& lval, prog::reg_index value, const prog::type_local& type, location loc) {
        switch (INDEX(lval)) {
            case lvalue::IGNORED:
                add_delete_instrs(value, type);
                break;

            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                auto& var = vars[var_index];

                if (type.confined && !clr.type_trivial(*type.tp) && var.outside_confinement > 0)
                    clr.error(diags::variable_outside_confinement(var.name), loc);

                delete_var(var_index, loc);

                value = conv_clr.convert(value, type, var.type, loc);

                auto write_instr = prog::write_var_instr { var_index, value };
                add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

                var.state = VAR_INITIALIZED;
                var.outside_loop = 0;
            } break;

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                auto& var_type = *clr.prog.global_vars[var_index]->tp;

                if (!clr.type_trivial(var_type)) {
                    auto old_value = new_reg();
                    auto read_instr = prog::read_global_var_instr { var_index, old_value };
                    add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(read_instr)));
                    add_delete_instrs(old_value, var_type);
                }

                value = conv_clr.convert(value, type, var_type, loc);

                auto instr = prog::write_global_var_instr { var_index, value };
                add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, into_ptr(instr)));
            } break;

            case lvalue::TUPLE: {
                auto lvals = as_cref_vector(GET(lval, TUPLE));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, TUPLE))
                    clr.error(diags::expected_tuple_type(clr.prog, copy_type(*type.tp)), loc);

                auto field_types = as_cref_vector(GET(*type.tp, TUPLE));
                auto confined = type.confined;

                if (field_types.size() != count)
                    clr.error(diags::invalid_tuple_size(field_types.size(), count), loc);

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(field_types[index])), confined };
                    compile_assignment(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::ARRAY: {
                auto lvals = as_cref_vector(GET(lval, ARRAY));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, ARRAY))
                    clr.error(diags::expected_array_type(clr.prog, copy_type(*type.tp)), loc);

                auto& array_type = *GET(*type.tp, ARRAY);
                auto item_type = prog::type_local { make_ptr(copy_type(*array_type.tp)), type.confined };

                if (array_type.size != count)
                    clr.error(diags::invalid_array_size(array_type.size, count), loc);

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_item_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_ITEM, into_ptr(instr)));

                    compile_assignment(lvals[index], extracted, item_type, loc);
                }
            } break;

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                auto lvals = as_cref_vector(lval_ptrs);

                if (!INDEX_EQ(*type.tp, STRUCT) || GET(*type.tp, STRUCT) != struct_index)
                    clr.error(diags::invalid_type(clr.prog, copy_type(*type.tp), VARIANT(prog::type, STRUCT, struct_index)), loc);

                auto& st = *clr.prog.struct_types[struct_index];
                auto count = st.fields.size();
                auto confined = type.confined;

                for (size_t index = 0; index < count; index++) {
                    auto extracted = new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(*st.fields[index]->tp)), confined };
                    compile_assignment(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::ENUM_VARIANT: {
                auto& [enum_index, variant_index, lval_ptrs] = GET(lval, ENUM_VARIANT);
                auto variant_index_copy = variant_index;
                auto lvals = as_cref_vector(lval_ptrs);

                if (!INDEX_EQ(*type.tp, ENUM) || GET(*type.tp, ENUM) != enum_index)
                    clr.error(diags::invalid_type(clr.prog, copy_type(*type.tp), VARIANT(prog::type, ENUM, enum_index)), loc);

                auto& en = *clr.prog.enum_types[enum_index];
                auto& variant = *en.variants[variant_index];
                auto count = variant.tps.size();
                auto confined = type.confined;

                auto test_result = new_reg();
                auto test_instr = prog::test_variant_instr { value, variant_index, test_result };
                add_instr(VARIANT(prog::instr, TEST_VARIANT, into_ptr(test_instr)));

                auto true_branch = [&] () {
                    for (size_t index = 0; index < count; index++) {
                        auto extracted = new_reg();
                        auto instr = prog::extract_variant_field_instr { value, variant_index_copy, index, extracted };
                        add_instr(VARIANT(prog::instr, EXTRACT_VARIANT_FIELD, into_ptr(instr)));

                        auto field_type = prog::type_local { make_ptr(copy_type(*variant.tps[index])), confined };
                        compile_assignment(lvals[index], extracted, field_type, loc);
                    }
                };

                auto false_branch = [&] () {
                    add_instr(VARIANT(prog::instr, ABORT, monostate())); // TODO error message
                };

                add_branch(test_result, true_branch, false_branch);
            } break;
        }
    }
}
