#include "compiler.hpp"
#include "compiler_diagnostics.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

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

    void function_compiler::push_var_frame() {
        var_frames.push_back({ });
    }

    void function_compiler::pop_var_frame() {
        vector<string>& cur_frame = var_frames.back();
        
        for (auto& name : cur_frame) {
            var_names[name].pop_back();
            if (var_names[name].empty()) {
                var_names.erase(name);
            }
        }
        
        var_frames.pop_back();
    }

    optional<prog::var_index_t> function_compiler::get_var(string name) {
        if (var_names.count(name) == 0)
            return { };
        return { var_names[name].back() };
    }

    void function_compiler::compile(const ast::func_def& ast) {
        reg_counter = 0;

        push_var_frame();

        for (auto& param : func.params) {
            auto var = add_var(param->name, make_ptr(copy_type_local(*param->tp)));
            instrs.push_back(VARIANT(prog::instr, WRITE_VAR, make_ptr(prog::write_var_instr { var, reg_counter++ })));
        }

        for (auto& stmt_ast : ast.body->block->stmts) {
            switch (INDEX(*stmt_ast)) {
                case ast::stmt::EXPR_EVAL: {
                    
                } break;
                
                case ast::stmt::ASSIGNMENT: {
                    
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
                    cmplr.error(diags::not_implemented(), ast); // TODO
            }
        }

        // TODO ast->body->return_value

        func.instrs = make_ptr(prog::instr_block { into_ptr_vector(instrs) });

        for (auto& var : vars)
            func.vars.push_back(move(var.tp));

        // TODO check if value returned
    }
}
