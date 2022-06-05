#include "compiler.hpp"
#include "compiler_diagnostics.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    using std::monostate;
    using std::tie;

    prog::global_func compiler::declare_global_func(const ast::func_def& ast) {
        auto name = ast.name;
        
        if (global_names.count(name))
            error(diags::name_used(name, diags::name_used::GLOBAL), ast);
        
        if (ast.copying)
            error(diags::global_func_copyable(), ast);
        
        vector<prog::ptr<prog::func_param>> params;
        for (auto& param : ast.params)
            params.push_back(make_ptr(prog::func_param { param->name, make_ptr(compile_type_local(*param->tp, true)) }));
        
        prog::ptr<prog::type> return_tp;
        if (ast.return_tp)
            return_tp = make_ptr(compile_type(**ast.return_tp, true));
        else
            return_tp = make_ptr(VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type { prog::primitive_type::UNIT })));

        return { name, move(params), move(return_tp), { }, { } };
    }

    void compiler::compile_global_func(const ast::func_def& ast, prog::global_func& global_func) {
        function_compiler(*this, global_func).compile(ast);
    }

    prog::var_index_t function_compiler::add_var(string name, prog::ptr<prog::type_local> type) {
        prog::var_index_t index = vars.size();

        vars.push_back({ move(type) });
        var_frames.back().push_back(name);
        var_names[name].push_back(index);
        
        return index;
    }

    void function_compiler::push_frame() {
        var_frames.push_back({ });
        instrs.push_back({ });
    }

    void function_compiler::pop_frame() {
        vector<string>& cur_frame = var_frames.back();
        
        for (auto& name : cur_frame) {
            var_names[name].pop_back();
            if (var_names[name].empty()) {
                var_names.erase(name);
            }
        }
        
        var_frames.pop_back();
        instrs.pop_back();
    }

    optional<prog::var_index_t> function_compiler::get_var(string name) {
        if (var_names.count(name) == 0)
            return { };
        return { var_names[name].back() };
    }

    void function_compiler::compile(const ast::func_def& ast) {
        reg_counter = 0;

        push_frame();

        for (auto& param : func.params) {
            auto var = add_var(param->name, make_ptr(copy_type_local(*param->tp)));
            instrs.back().push_back(VARIANT(prog::instr, WRITE_VAR, make_ptr(prog::write_var_instr { var, reg_counter++ })));
        }

        compile_stmt_block(*ast.body->block);

        // TODO ast->body->return_value

        func.instrs = make_ptr(prog::instr_block { into_ptr_vector(instrs.back()) });

        for (auto& var : vars)
            func.vars.push_back(move(var.tp));

        // TODO check if value returned
    }

    void function_compiler::compile_stmt_block(const ast::stmt_block& ast) {
        for (auto& stmt_ast : ast.stmts) {
            switch (INDEX(*stmt_ast)) {
                case ast::stmt::EXPR_EVAL: {
                    auto& expr = *GET(*stmt_ast, EXPR_EVAL);
                    if (INDEX(expr) == ast::expr::VAR_DECL)
                        compile_left_expr(expr, { });
                    else
                        compile_right_expr(expr);
                } break;

                case ast::stmt::ASSIGNMENT: {
                    auto& assignment = *GET(*stmt_ast, ASSIGNMENT);
                    auto[reg, type] = compile_right_expr(*assignment.value);
                    auto lval = compile_left_expr(*assignment.lvalue, type);

                    switch (INDEX(lval)) { // TODO add more assignment options, move to separate nethod
                        case lvalue::LOCAL_VAR: {
                            auto var = GET(lval, LOCAL_VAR);
                            // TODO check type correctness
                            instrs.back().push_back(VARIANT(prog::instr, WRITE_VAR, make_ptr(prog::write_var_instr{ var, reg })));
                        } break;

                        default:
                            cmplr.error(diags::not_implemented(), assignment);
                    }
                } break;

                case ast::stmt::COMPOUND_ASSIGNMENT:
                case ast::stmt::LOCALLY_BLOCK:
                case ast::stmt::SWAP:
                case ast::stmt::SWAP_BLOCK:
                case ast::stmt::IF:
                case ast::stmt::MATCH:
                case ast::stmt::WHILE:
                case ast::stmt::FOR:
                case ast::stmt::FUNC_DEF:
                    cmplr.error(diags::not_implemented(), *stmt_ast); // TODO
            }
        }
    }

    function_compiler::lvalue function_compiler::compile_left_expr(const ast::expr& ast, optional<reference_wrapper<const prog::type_local>> implicit_type) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
            case ast::expr::ARRAY:
            case ast::expr::APPLICATION:
            case ast::expr::NAME:
                cmplr.error(diags::not_implemented(), ast); // TODO

            case ast::expr::VAR_DECL: {
                auto& var_decl = *GET(ast, VAR_DECL);
                if (!var_decl.tp && !implicit_type)
                    cmplr.error(diags::variable_without_type(), var_decl);
                auto type = var_decl.tp ? cmplr.compile_type_local(**var_decl.tp) : copy_type_local(*implicit_type);
                auto var = add_var(var_decl.name, into_ptr(type));
                return VARIANT(lvalue, LOCAL_VAR, var);
            } break;

            case ast::expr::DEREFERENCE:
            case ast::expr::EXTRACT:
                cmplr.error(diags::not_implemented(), ast); // TODO

            default:
                cmplr.error(diags::expression_not_left(), ast);
        }
    }

    pair<prog::reg_index_t, prog::type_local> function_compiler::compile_right_expr(const ast::expr& ast) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
            case ast::expr::ARRAY:
            case ast::expr::APPLICATION:
            case ast::expr::NAME:
            case ast::expr::VARIANT_NAME:
                cmplr.error(diags::not_implemented(), ast); // TODO

            case ast::expr::LITERAL: {
                auto& literal_expr = *GET(ast, LITERAL);
                auto[constant, type] = cmplr.compile_constant_literal(literal_expr);
                prog::reg_index_t reg = ++reg_counter;
                instrs.back().push_back(VARIANT(prog::instr, MAKE_CONST, make_ptr(prog::make_const_instr{ into_ptr(constant), reg })));
                return { reg, prog::type_local{ into_ptr(type), false } };
            } break;

            case ast::expr::UNARY_OPERATION:
            case ast::expr::BINARY_OPERATION:
            case ast::expr::NUMERIC_CAST:
            case ast::expr::NONE:
            case ast::expr::SOME:
                cmplr.error(diags::not_implemented(), ast); // TODO

            case ast::expr::RETURN: {
                auto& return_expr = GET(ast, RETURN);
                prog::reg_index_t reg;
                prog::type_local type;
                if (return_expr)
                    tie(reg, type) = compile_right_expr(**return_expr);
                else {
                    reg = ++reg_counter;
                    type = prog::type_local{ make_ptr(VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{ prog::primitive_type::UNIT }))), false };
                    instrs.back().push_back(VARIANT(prog::instr, MAKE_CONST, make_ptr(prog::make_const_instr{ make_ptr(VARIANT(prog::constant, UNIT, monostate())), reg })));
                }
                // TODO check type, perform conversion
                instrs.back().push_back(VARIANT(prog::instr, RETURN, make_ptr(prog::return_instr{ reg })));
                return { reg, prog::type_local{ make_ptr(VARIANT(prog::type, NEVER, monostate())), false } };
            } break;

            case ast::expr::BREAK:
            case ast::expr::CONTINUE:
            case ast::expr::REFERENCE:
            case ast::expr::HEAP_ALLOC:
            case ast::expr::DEREFERENCE:
            case ast::expr::SIZED_ARRAY:
            case ast::expr::HEAP_SLICE_ALLOC:
            case ast::expr::LENGTH:
            case ast::expr::EXTRACT:
            case ast::expr::PTR_EXTRACT:
            case ast::expr::LAMBDA:
                cmplr.error(diags::not_implemented(), ast); // TODO

            default:
                cmplr.error(diags::expression_not_right(), ast);
        }
    }
}
