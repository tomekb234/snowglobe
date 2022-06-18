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
                auto [value, type] = compile_expr(*assignment_ast.value, false);
                auto lval = compile_left_expr(*assignment_ast.lvalue, { type });
                compile_assignment(lval, value, type, assignment_ast.value->loc);
            } break;

            case ast::stmt::COMPOUND_ASSIGNMENT: {
                auto& expr_ast = *GET(ast, COMPOUND_ASSIGNMENT)->expr;
                auto [value, type] = compile_binary_operation(expr_ast);
                auto lval = compile_left_expr(*expr_ast.left, { type });
                compile_assignment(lval, value, type, expr_ast.right->loc);
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
        auto& left_ast = *ast.left;
        auto& right_ast = *ast.right;

        auto [left_val, left_type] = compile_expr(left_ast, false);
        auto [right_val, right_type] = compile_expr(right_ast, false);

        auto left_lval = compile_left_expr(left_ast, { right_type });
        auto right_lval = compile_left_expr(right_ast, { left_type });

        left_val = conv_clr.convert(left_val, left_type, right_type, left_ast.loc);
        right_val = conv_clr.convert(right_val, right_type, left_type, right_ast.loc);

        compile_assignment(left_lval, right_val, right_type, right_ast.loc);
        compile_assignment(right_lval, left_val, left_type, left_ast.loc);
    }

    void function_compiler::compile_swap_block_stmt(const ast::swap_block_stmt& ast) {
        auto& left_ast = *ast.left;
        auto& right_ast = *ast.right;
        auto& block_ast = *ast.block;

        prog::var_index right_var_index;
        auto confining = false;

        if (INDEX_EQ(right_ast, NAME_LOCALLY)) {
            auto name = GET(right_ast, NAME_LOCALLY);
            right_var_index = get_var(name, right_ast.loc);
            confining = true;
        }

        auto swap = [&] () {
            prog::reg_index left_val, right_val;
            prog::type_local left_type, right_type;
            lvalue left_lval, right_lval;

            tie(left_val, left_type) = compile_expr(left_ast, false);

            if (INDEX_EQ(right_ast, EXPR)) {
                tie(right_val, right_type) = compile_expr(*GET(right_ast, EXPR), false);
                left_lval = compile_left_expr(left_ast, { right_type });
                right_lval = compile_left_expr(*GET(right_ast, EXPR), { left_type });
            } else if (INDEX_EQ(right_ast, NAME_LOCALLY)) {
                tie(right_val, right_type) = compile_var_read(right_var_index, false, right_ast.loc);
                left_lval = compile_left_expr(left_ast, { right_type });
                right_lval = VARIANT(lvalue, VAR, right_var_index);
            } else
                UNREACHABLE;

            left_val = conv_clr.convert(left_val, left_type, right_type, left_ast.loc);
            right_val = conv_clr.convert(right_val, right_type, left_type, right_ast.loc);

            compile_assignment(left_lval, right_val, right_type, right_ast.loc);
            compile_assignment(right_lval, left_val, left_type, left_ast.loc);
        };

        push_frame();

        swap();
        add_cleanup_action(swap);

        if (confining) {
            push_confining_frame();
            compile_confinement(right_var_index, right_ast.loc);
        } else
            push_frame();

        compile_stmt_block(block_ast, true);

        if (confining)
            add_instrs(pop_confining_frame());
        else
            add_instrs(pop_frame());

        cleanup_frame(0, block_ast.end_loc);
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
                tie(value, type) = compile_confinement(var_index, value_ast.loc);
                confining = true;
            }

            if (!INDEX_EQ(*type.tp, OPTIONAL))
                clr.error(diags::expected_optional_type(clr.prog, copy_type(*type.tp)), value_ast.loc);

            auto cond = new_reg();
            auto test_instr = prog::test_optional_instr { value, cond };
            add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

            auto true_branch = [&] () {
                auto result = new_reg();
                auto extract_instr = prog::extract_optional_value_instr { value, result };
                add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));

                auto inner_type = prog::type_local { make_ptr(copy_type(*GET(*type.tp, OPTIONAL))), type.confined };
                auto lval = compile_left_expr(lvalue_ast, { inner_type });
                compile_assignment(lval, result, inner_type, value_ast.loc);

                compile_stmt_block(block_ast, true);
            };

            auto false_branch = [&] () {
                add_delete_instrs(value, type);

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
            tie(value, type) = compile_confinement(var_index, value_ast.loc);
            confining = true;
        }

        if (!INDEX_EQ(*type.tp, ENUM))
            clr.error(diags::expected_enum_type(clr.prog, copy_type(*type.tp)), value_ast.loc);

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

        push_frame();

        auto lval = compile_left_expr(lval_ast, { type });

        if (!INDEX_EQ(lval, ENUM_VARIANT))
            clr.error(diags::expected_enum_variant(), lval_ast.loc);

        auto& [enum_index, variant_index, lval_ptrs] = GET(lval, ENUM_VARIANT);
        auto variant_index_copy = variant_index;
        auto lvals = as_cref_vector(lval_ptrs);

        if (enum_index != GET(*type.tp, ENUM))
            clr.error(diags::invalid_type(clr.prog, copy_type(*type.tp), VARIANT(prog::type, ENUM, enum_index)), value_ast.loc);

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
                compile_assignment(lvals[index], extracted, field_type, value_ast.loc);
            }

            compile_stmt_block(block_ast, true);
            cleanup_frame(1, block_ast.end_loc);
        };

        auto false_branch = [&] () {
            if (branch_index < branch_count - 1)
                compile_match_stmt_branches(ast, value, type, branch_index + 1);
            else {
                add_delete_instrs(value, type);
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
                    tie(value, type) = compile_confinement(var_index, value_ast.loc);
                    confining = true;
                }

                if (!INDEX_EQ(*type.tp, OPTIONAL))
                    clr.error(diags::expected_optional_type(clr.prog, copy_type(*type.tp)), value_ast.loc);

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
                compile_assignment(lval, result, inner_type, value_ast.loc);

                compile_stmt_block(block_ast, true);
            };

            auto false_branch = [&] () {
                add_delete_instrs(value, type);

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
                clr.error(diags::expected_integer_type(clr.prog, copy_type(type)), begin_ast.loc);

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
                    clr.error(diags::expected_integer_type(clr.prog, copy_type(type)), begin_ast.loc);
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

                compile_assignment(lval, value, type_local, begin_ast.loc);

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
