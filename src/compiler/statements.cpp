#include "compiler/statements.hpp"
#include "compiler/expressions.hpp"
#include "compiler/conversions.hpp"
#include "compiler/assignment.hpp"
#include "compiler/deletion.hpp"
#include "compiler/compiler_utils.hpp"
#include "compiler/function_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void statement_compiler::compile_block(const ast::stmt_block& ast, bool cleanup) {
        for (const ast::stmt& stmt_ast : as_cref_vector(ast.stmts)) {
            if (fclr.returned)
                warning(diags::unreachable_code(stmt_ast.loc));

            compile(stmt_ast);
        }

        if (cleanup)
            function_utils(fclr).add_frame_cleanup(0, ast.end_loc);
    }

    void statement_compiler::compile(const ast::stmt& ast) {
        switch (INDEX(ast)) {
            case ast::stmt::EXPR_EVAL: {
                auto& expr_ast = *GET(ast, EXPR_EVAL);
                if (INDEX_EQ(expr_ast, VAR_DECL))
                    expression_compiler(fclr).compile_left(expr_ast, { });
                else {
                    auto [value, type] = expression_compiler(fclr).compile(expr_ast, false);
                    if (!type.confined)
                        deletion_generator(fclr).add(value, *type.tp);
                    if (INDEX_EQ(*type.tp, NEVER))
                        fclr.returned = true;
                }
            } break;

            case ast::stmt::ASSIGNMENT: {
                auto& assignment_ast = *GET(ast, ASSIGNMENT);
                auto [value, type] = expression_compiler(fclr).compile(*assignment_ast.value, false);
                auto lval = expression_compiler(fclr).compile_left(*assignment_ast.lvalue, { type });
                assignment_generator(fclr, lval, value, type, assignment_ast.value->loc).add();
            } break;

            case ast::stmt::COMPOUND_ASSIGNMENT: {
                auto& expr_ast = *GET(ast, COMPOUND_ASSIGNMENT)->expr;
                auto [value, type] = expression_compiler(fclr).compile_binary_operation(expr_ast);
                auto lval = expression_compiler(fclr).compile_left(*expr_ast.left, { type });
                assignment_generator(fclr, lval, value, type, expr_ast.right->loc).add();
            } break;

            case ast::stmt::LOCALLY_BLOCK:
                compile_locally_block(*GET(ast, LOCALLY_BLOCK));
                break;

            case ast::stmt::SWAP:
                compile_swap(*GET(ast, SWAP));
                break;

            case ast::stmt::SWAP_BLOCK:
                compile_swap_block(*GET(ast, SWAP_BLOCK));
                break;

            case ast::stmt::IF:
                compile_if_branches(*GET(ast, IF), 0);
                break;

            case ast::stmt::MATCH:
                compile_match(*GET(ast, MATCH));
                break;

            case ast::stmt::WHILE:
                compile_while(*GET(ast, WHILE));
                break;

            case ast::stmt::FOR:
                compile_for(*GET(ast, FOR));
                break;

            case ast::stmt::FUNC_DEF:
                error(diags::not_implemented(ast.loc)); // TODO
        }
    }

    void statement_compiler::compile_locally_block(const ast::locally_block_stmt& ast) {
        fclr.push_confining_frame();

        for (auto name : ast.var_names) {
            auto var_index = fclr.get_var(name, ast.names_loc);
            function_utils(fclr).add_var_confinement(var_index, ast.names_loc);
        }

        compile_block(*ast.block, true);

        fclr.add_instrs(fclr.pop_confining_frame());
    }

    void statement_compiler::compile_swap(const ast::swap_stmt& ast) {
        auto& left_ast = *ast.left;
        auto& right_ast = *ast.right;

        auto lval_a = expression_compiler(fclr).compile_left(left_ast, { });
        auto lval_b = expression_compiler(fclr).compile_left(right_ast, { });

        auto [value_a, type_a] = function_utils(fclr).add_read_for_swap(lval_a, left_ast.loc);
        auto [value_b, type_b] = function_utils(fclr).add_read_for_swap(lval_b, right_ast.loc);

        assignment_generator(fclr, lval_a, value_b, type_b, right_ast.loc).add_from_swap();
        assignment_generator(fclr, lval_b, value_a, type_a, left_ast.loc).add_from_swap();
    }

    void statement_compiler::compile_swap_block(const ast::swap_block_stmt& ast) {
        auto& left_ast = *ast.left;
        auto& right_ast = *ast.right;
        auto& block_ast = *ast.block;

        auto lval_a = expression_compiler(fclr).compile_left(left_ast, { });
        lvalue lval_b;
        optional<prog::var_index> var_index;

        if (INDEX_EQ(right_ast, EXPR))
            lval_b = expression_compiler(fclr).compile_left(*GET(right_ast, EXPR), { });
        else if (INDEX_EQ(right_ast, NAME_LOCALLY)) {
            auto name = GET(right_ast, NAME_LOCALLY);
            var_index = { fclr.get_var(name, right_ast.loc) };
            lval_b = VARIANT(lvalue, VAR, *var_index);
        }

        auto swap = [&] () {
            auto [value_a, type_a] = function_utils(fclr).add_read_for_swap(lval_a, left_ast.loc);
            auto [value_b, type_b] = function_utils(fclr).add_read_for_swap(lval_b, right_ast.loc);

            assignment_generator(fclr, lval_a, value_b, type_b, right_ast.loc).add_from_swap();
            assignment_generator(fclr, lval_b, value_a, type_a, left_ast.loc).add_from_swap();
        };

        fclr.push_frame();

        swap();
        fclr.defer_cleanup_action(swap);

        if (var_index) {
            fclr.push_confining_frame();
            function_utils(fclr).add_var_confinement(*var_index, right_ast.loc);
        } else
            fclr.push_frame();

        compile_block(block_ast, true);

        if (var_index)
            fclr.add_instrs(fclr.pop_confining_frame());
        else
            fclr.add_instrs(fclr.pop_frame());

        function_utils(fclr).add_frame_cleanup(0, block_ast.end_loc);
        fclr.add_instrs(fclr.pop_frame());
    }

    void statement_compiler::compile_if_branches(const ast::if_stmt& ast, size_t branch_index) {
        auto& branch_ast = *ast.branches[branch_index];
        auto& cond_ast = *branch_ast.cond;
        auto& block_ast = *branch_ast.block;
        auto else_branch_ast = as_optional_cref(ast.else_branch);
        auto branch_count = ast.branches.size();

        if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
            auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
            auto [value, type] = expression_compiler(fclr).compile(expr_ast, true);
            auto cond = conversion_generator(fclr).convert(value, type, prog::BOOL_TYPE, expr_ast.loc);

            auto true_branch = [&] () {
                compile_block(block_ast, true);
            };

            auto false_branch = [&] () {
                if (branch_index < branch_count - 1)
                    compile_if_branches(ast, branch_index + 1);
                else if (else_branch_ast)
                    compile_block(*else_branch_ast, true);
            };

            function_utils(fclr).add_branch(cond, true_branch, false_branch);
        } else if (INDEX_EQ(cond_ast, CHECK_IF_PRESENT)) {
            auto& [lvalue_ast_ptr, value_ast_ptr] = GET(cond_ast, CHECK_IF_PRESENT);
            auto& lvalue_ast = *lvalue_ast_ptr;
            auto& value_ast = *value_ast_ptr;

            prog::reg_index value;
            prog::type_local type;
            auto confining = false;

            if (INDEX_EQ(value_ast, EXPR))
                tie(value, type) = expression_compiler(fclr).compile(*GET(value_ast, EXPR), false);
            else if (INDEX_EQ(value_ast, NAME_LOCALLY)) {
                auto name = GET(value_ast, NAME_LOCALLY);
                auto var_index = fclr.get_var(name, value_ast.loc);
                fclr.push_confining_frame();
                tie(value, type) = function_utils(fclr).add_var_confinement(var_index, value_ast.loc);
                confining = true;
            }

            if (!INDEX_EQ(*type.tp, OPTIONAL))
                error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::OPTIONAL, value_ast.loc));

            auto cond = fclr.new_reg();
            auto test_instr = prog::test_optional_instr { value, cond };
            fclr.add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

            auto true_branch = [&] () {
                auto result = fclr.new_reg();
                auto extract_instr = prog::extract_optional_value_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));

                auto inner_type = prog::type_local { make_ptr(copy_type(*GET(*type.tp, OPTIONAL))), type.confined };
                auto lval = expression_compiler(fclr).compile_left(lvalue_ast, { inner_type });
                assignment_generator(fclr, lval, result, inner_type, value_ast.loc).add();

                compile_block(block_ast, true);
            };

            auto false_branch = [&] () {
                if (!type.confined)
                    deletion_generator(fclr).add(value, *type.tp);

                if (branch_index < branch_count - 1)
                    compile_if_branches(ast, branch_index + 1);
                else if (else_branch_ast)
                    compile_block(*else_branch_ast, true);
            };

            function_utils(fclr).add_branch(cond, true_branch, false_branch);

            if (confining)
                fclr.add_instrs(fclr.pop_confining_frame());
        }
    }

    void statement_compiler::compile_match(const ast::match_stmt& ast) {
        auto& value_ast = *ast.value;

        prog::reg_index value;
        prog::type_local type;
        auto confining = false;

        if (INDEX_EQ(value_ast, EXPR))
            tie(value, type) = expression_compiler(fclr).compile(*GET(value_ast, EXPR), false);
        else if (INDEX_EQ(value_ast, NAME_LOCALLY)) {
            auto name = GET(value_ast, NAME_LOCALLY);
            auto var_index = fclr.get_var(name, value_ast.loc);
            fclr.push_confining_frame();
            tie(value, type) = function_utils(fclr).add_var_confinement(var_index, value_ast.loc);
            confining = true;
        }

        if (!INDEX_EQ(*type.tp, ENUM))
            error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::ENUM, value_ast.loc));

        compile_match_branches(ast, value, type, 0);

        if (confining)
            fclr.add_instrs(fclr.pop_confining_frame());
    }

    void statement_compiler::compile_match_branches(const ast::match_stmt& ast, prog::reg_index value, const prog::type_local& type, size_t branch_index) {
        auto& value_ast = *ast.value;
        auto& branch_ast = *ast.branches[branch_index];
        auto& lval_ast = *branch_ast.lvalue;
        auto& block_ast = *branch_ast.block;
        auto else_branch_ast = as_optional_cref(ast.else_branch);
        auto branch_count = ast.branches.size();

        auto enum_index = GET(*type.tp, ENUM);
        auto& en = *prog.enum_types[enum_index];

        string variant_name;
        optional<vector<cref<ast::expr_marked>>> arg_marked_asts;
        vector<cref<ast::expr>> arg_asts;

        if (INDEX_EQ(lval_ast, NAME))
            variant_name = GET(lval_ast, NAME);
        else if (INDEX_EQ(lval_ast, APPLICATION)) {
            auto& [receiver_ast_ptr, arg_marked_ast_ptrs] = GET(lval_ast, APPLICATION);
            auto& receiver_ast = *receiver_ast_ptr;
            if (!INDEX_EQ(receiver_ast, NAME))
                error(diags::invalid_expression(lval_ast.loc));
            variant_name = GET(receiver_ast, NAME);
            arg_marked_asts = { as_cref_vector(arg_marked_ast_ptrs) };
        } else
            error(diags::invalid_expression(lval_ast.loc));

        auto iter = en.variant_names.find(variant_name);
        if (iter == en.variant_names.end())
            error(diags::unknown_enum_variant(en, variant_name, lval_ast.loc));

        auto variant_index = iter->second;
        auto& variant = *en.variants[variant_index];
        auto count = variant.tps.size();
        auto confined = type.confined;

        if ((count > 0) != arg_marked_asts.has_value())
            error(diags::invalid_expression(lval_ast.loc));
        if (count > 0)
            arg_asts = compiler_utils(clr).order_args(*arg_marked_asts, { }, { count }, lval_ast.loc);

        fclr.push_frame();

        auto test_result = fclr.new_reg();
        auto test_instr = prog::test_variant_instr { value, variant_index, test_result };
        fclr.add_instr(VARIANT(prog::instr, TEST_VARIANT, into_ptr(test_instr)));

        auto true_branch = [&] () {
            for (size_t index = 0; index < count; index++) {
                auto extracted = fclr.new_reg();
                auto instr = prog::extract_variant_field_instr { value, variant_index, index, extracted };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_VARIANT_FIELD, into_ptr(instr)));

                auto field_type = prog::type_local { make_ptr(copy_type(*variant.tps[index])), confined };
                auto lval = expression_compiler(fclr).compile_left(arg_asts[index], { field_type });
                assignment_generator(fclr, lval, extracted, field_type, value_ast.loc).add();
            }

            compile_block(block_ast, true);
            function_utils(fclr).add_frame_cleanup(1, block_ast.end_loc);
        };

        auto false_branch = [&] () {
            if (branch_index < branch_count - 1)
                compile_match_branches(ast, value, type, branch_index + 1);
            else {
                if (!type.confined)
                    deletion_generator(fclr).add(value, *type.tp);
                if (else_branch_ast)
                    compile_block(*else_branch_ast, true);
            }
        };

        function_utils(fclr).add_branch(test_result, true_branch, false_branch);

        fclr.add_instrs(fclr.pop_frame());
    }

    void statement_compiler::compile_while(const ast::while_stmt& ast) {
        auto& cond_ast = *ast.cond;
        auto& block_ast = *ast.block;
        auto else_block_ast = as_optional_cref(ast.else_block);

        if (INDEX_EQ(cond_ast, CHECK_IF_TRUE)) {
            auto head = [&] () -> prog::reg_index {
                auto& expr_ast = *GET(cond_ast, CHECK_IF_TRUE);
                auto [value, type] = expression_compiler(fclr).compile(expr_ast, true);
                return conversion_generator(fclr).convert(value, type, prog::BOOL_TYPE, expr_ast.loc);
            };

            auto true_branch = [&] () {
                compile_block(block_ast, true);
            };

            auto false_branch = [&] () {
                if (else_block_ast)
                    compile_block(*else_block_ast, true);
            };

            function_utils(fclr).add_loop(head, true_branch, false_branch, [] { });
        } else if (INDEX_EQ(cond_ast, CHECK_IF_PRESENT)) {
            auto& [lvalue_ast_ptr, value_ast_ptr] = GET(cond_ast, CHECK_IF_PRESENT);
            auto& lvalue_ast = *lvalue_ast_ptr;
            auto& value_ast = *value_ast_ptr;

            prog::reg_index value;
            prog::type_local type;
            auto confining = false;

            auto head = [&] () -> prog::reg_index {
                if (INDEX_EQ(value_ast, EXPR))
                    tie(value, type) = expression_compiler(fclr).compile(*GET(value_ast, EXPR), false);
                else if (INDEX_EQ(value_ast, NAME_LOCALLY)) {
                    auto name = GET(value_ast, NAME_LOCALLY);
                    auto var_index = fclr.get_var(name, value_ast.loc);
                    fclr.push_confining_frame();
                    tie(value, type) = function_utils(fclr).add_var_confinement(var_index, value_ast.loc);
                    confining = true;
                }

                if (!INDEX_EQ(*type.tp, OPTIONAL))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::OPTIONAL, value_ast.loc));

                auto cond = fclr.new_reg();
                auto test_instr = prog::test_optional_instr { value, cond };
                fclr.add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));
                return cond;
            };

            auto true_branch = [&] () {
                auto result = fclr.new_reg();
                auto extract_instr = prog::extract_optional_value_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));

                auto inner_type = prog::type_local { make_ptr(copy_type(*GET(*type.tp, OPTIONAL))), type.confined };
                auto lval = expression_compiler(fclr).compile_left(lvalue_ast, { inner_type });
                assignment_generator(fclr, lval, result, inner_type, value_ast.loc).add();

                compile_block(block_ast, true);
            };

            auto false_branch = [&] () {
                if (!type.confined)
                    deletion_generator(fclr).add(value, *type.tp);

                if (else_block_ast)
                    compile_block(*else_block_ast, true);
            };

            auto end = [&] () {
                if (confining)
                    fclr.add_instrs(fclr.pop_confining_frame());
            };

            function_utils(fclr).add_loop(head, true_branch, false_branch, end);
        }
    }

    void statement_compiler::compile_for(const ast::for_stmt& ast) {
        if (INDEX_EQ(ast, RANGE)) {
            auto& range_ast = *GET(ast, RANGE);
            auto& begin_ast = *range_ast.begin;
            auto& end_ast = *range_ast.end;
            auto& lvalue_ast = *range_ast.lvalue;
            auto& block_ast = *range_ast.block;
            auto else_block_ast = as_optional_cref(range_ast.else_block);

            prog::reg_index begin_value, end_value;
            prog::type_local begin_type, end_type;

            tie(begin_value, begin_type) = expression_compiler(fclr).compile(begin_ast, true);
            tie(end_value, end_type) = expression_compiler(fclr).compile(end_ast, true);
            auto incr = !range_ast.reversed;

            auto type = compiler_utils(clr).common_supertype(*begin_type.tp, *end_type.tp, begin_ast.loc);
            auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

            if (!INDEX_EQ(type, NUMBER))
                error(diags::invalid_type(prog, move(type), diags::type_kind::INTEGER, begin_ast.loc));

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
                    error(diags::invalid_type(prog, move(type), diags::type_kind::INTEGER, begin_ast.loc));
            }

            begin_value = conversion_generator(fclr).convert(begin_value, begin_type, type, begin_ast.loc);
            end_value = conversion_generator(fclr).convert(end_value, end_type, type, end_ast.loc);

            auto var_index = fclr.add_var(copy_type_local(type_local));
            auto write_instr = prog::write_var_instr { var_index, incr ? begin_value : end_value };
            fclr.add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));
            fclr.vars[var_index].state = VAR_INITIALIZED;

            auto value = fclr.new_reg();

            auto head = [&] () -> prog::reg_index {
                auto read_instr = prog::read_var_instr { var_index, value };
                fclr.add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(read_instr)));

                auto cond = fclr.new_reg();

                if (incr) {
                    auto compare_instr = prog::numeric_binary_operation_instr { { value, end_value, cond }, op_kind };
                    fclr.add_instr(VARIANT(prog::instr, LS, into_ptr(compare_instr)));
                } else {
                    auto compare_instr = prog::numeric_binary_operation_instr { { value, begin_value, cond }, op_kind };
                    fclr.add_instr(VARIANT(prog::instr, GT, into_ptr(compare_instr)));
                }

                return cond;
            };

            auto true_branch = [&] () {
                if (!incr) {
                    auto new_value = fclr.new_reg();
                    auto decr_instr = prog::unary_operation_instr { value, new_value };
                    fclr.add_instr(VARIANT(prog::instr, DECR, into_ptr(decr_instr)));
                    value = new_value;
                }

                auto lval = expression_compiler(fclr).compile_left(lvalue_ast, { type_local });

                assignment_generator(fclr, lval, value, type_local, begin_ast.loc).add();

                compile_block(block_ast, true);

                if (incr) {
                    auto new_value = fclr.new_reg();
                    auto incr_instr = prog::unary_operation_instr { value, new_value };
                    fclr.add_instr(VARIANT(prog::instr, INCR, into_ptr(incr_instr)));
                    value = new_value;
                }

                auto write_instr = prog::write_var_instr { var_index, value };
                fclr.add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));
            };

            auto false_branch = [&] () {
                if (else_block_ast)
                    compile_block(*else_block_ast, true);
            };

            function_utils(fclr).add_loop(head, true_branch, false_branch, [] { });
        } else if (INDEX_EQ(ast, SLICE)) {
            error(diags::not_implemented(ast.loc)); // TODO
        }
    }
}
