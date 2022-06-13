#include "codegen.hpp"
#include "utils.hpp"
#include "diags.hpp"

#include <vector>
#include <sstream>

#include <llvm-12/llvm/IR/IRBuilder.h>
#include <llvm-12/llvm/IR/Verifier.h>
#include <llvm/Support/raw_os_ostream.h>

namespace sg {
    using namespace sg::utils;
    using std::vector;
    using std::ostringstream;

    bool code_generator::generate() {
        try {
            for (auto& global_func : prog.global_funcs)
                function_code_generator(*this, *global_func).generate();

            llvm_verify([&](llvm::raw_ostream* stream){ return llvm::verifyModule(mod, stream); });
        } catch (generator_error) {
            return false;
        }

        return true;
    }

    void code_generator::write(ostream& stream) {
        llvm::raw_os_ostream s(stream);
        mod.print(s, nullptr);
    }

    void code_generator::llvm_verify(function<bool(llvm::raw_ostream*)> func) {
        ostringstream stream;
        llvm::raw_os_ostream llvm_stream(stream);
        if (func(&llvm_stream))
            error(diags::code_generator_fail(stream.str()));
    }

    llvm::Type* code_generator::get_llvm_type(const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::PRIMITIVE: {
                switch (GET(type, PRIMITIVE)->tp) {
                    case prog::primitive_type::BOOL:
                        return llvm::Type::getInt1Ty(ctx);

                    case prog::primitive_type::I8:
                    case prog::primitive_type::U8:
                        return llvm::Type::getInt8Ty(ctx);

                    case prog::primitive_type::I16:
                    case prog::primitive_type::U16:
                        return llvm::Type::getInt16Ty(ctx);

                    case prog::primitive_type::I32:
                    case prog::primitive_type::U32:
                        return llvm::Type::getInt32Ty(ctx);

                    case prog::primitive_type::I64:
                    case prog::primitive_type::U64:
                        return llvm::Type::getInt64Ty(ctx);

                    case prog::primitive_type::F32:
                        return llvm::Type::getFloatTy(ctx);

                    case prog::primitive_type::F64:
                        return llvm::Type::getDoubleTy(ctx);
                }
            }

            default:
                error(diags::not_implemented()); // TODO
        }
    }

    llvm::Value* code_generator::make_constant(const prog::constant& constant) {
        switch (INDEX(constant)) {
            case prog::constant::BOOL:
                return llvm::ConstantInt::get(llvm::Type::getInt1Ty(ctx), GET(constant, BOOL));

            case prog::constant::INT:
                return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), GET(constant, INT)); // TODO handle different integer types

            default:
                error(diags::not_implemented()); // TODO
        }
    }

    void function_code_generator::generate() {
        // get function type
        vector<llvm::Type*> arg_types;
        for (auto& param : func.params)
            arg_types.push_back(gen.get_llvm_type(*param->tp->tp));
        auto ret_type = gen.get_llvm_type(*func.return_tp);
        auto function_type = llvm::FunctionType::get(ret_type, arg_types, false);

        // create function
        llvm_function = llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, func.name, gen.mod);

        // create entry block
        auto entry_block = llvm::BasicBlock::Create(gen.ctx, "entry", llvm_function);
        llvm::IRBuilder<> builder(entry_block);

        // alloc variables
        for (size_t i = 0; i < func.vars.size(); i++)
            vars[i] = builder.CreateAlloca(gen.get_llvm_type(*func.vars[i]->tp));
        for (size_t i = 0; i < func.params.size(); i++)
            regs[i] = llvm_function->getArg(i);
        
        // generate function body
        process_instr_block(*func.instrs, entry_block, nullptr);

        // verify corectness
        gen.llvm_verify([&](llvm::raw_ostream* stream){ return llvm::verifyFunction(*llvm_function, stream); });
    }

    void function_code_generator::process_instr_block(const prog::instr_block& block, llvm::BasicBlock* init_basic_block, llvm::BasicBlock* next_basic_block) {
        llvm::IRBuilder<> builder(init_basic_block);

        for (auto& instr : block.instrs) {
            switch (INDEX(*instr)) {
                case prog::instr::READ_VAR: {
                    auto& read_var_instr = *GET(*instr, READ_VAR);
                    regs[read_var_instr.result] = builder.CreateLoad(gen.get_llvm_type(*func.vars[read_var_instr.var]->tp), vars[read_var_instr.var]);
                } break;

                case prog::instr::WRITE_VAR: {
                    auto& write_var_instr = *GET(*instr, WRITE_VAR);
                    builder.CreateStore(regs[write_var_instr.value], vars[write_var_instr.var]);
                } break;

                case prog::instr::RETURN: {
                    auto& return_instr = *GET(*instr, RETURN);
                    builder.CreateRet(regs[return_instr.value]);
                } break;

                case prog::instr::MAKE_UNIT: {
                    auto reg = GET(*instr, MAKE_UNIT);
                    regs[reg] = llvm::ConstantInt::get(llvm::Type::getInt1Ty(gen.ctx), 0);
                } break;

                case prog::instr::MAKE_CONST: {
                    auto& make_const_instr = *GET(*instr, MAKE_CONST);
                    regs[make_const_instr.result] = gen.make_constant(*make_const_instr.value);
                } break;

                case prog::instr::ADD: {
                    auto& numeric_operation_instr = *GET(*instr, ADD);
                    if (numeric_operation_instr.kind == prog::numeric_binary_operation_instr::FLOAT)
                        regs[numeric_operation_instr.result] = builder.CreateFAdd(regs[numeric_operation_instr.left], regs[numeric_operation_instr.right]);
                    else
                        regs[numeric_operation_instr.result] = builder.CreateAdd(regs[numeric_operation_instr.left], regs[numeric_operation_instr.right]);
                } break;

                case prog::instr::LS: {
                    auto& numeric_operation_instr = *GET(*instr, LS);
                    if (numeric_operation_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[numeric_operation_instr.result] = builder.CreateICmpULT(regs[numeric_operation_instr.left], regs[numeric_operation_instr.right]);
                    else if (numeric_operation_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[numeric_operation_instr.result] = builder.CreateICmpSLT(regs[numeric_operation_instr.left], regs[numeric_operation_instr.right]);
                    else
                        regs[numeric_operation_instr.result] = builder.CreateFCmpULT(regs[numeric_operation_instr.left], regs[numeric_operation_instr.right]);
                } break;

                case prog::instr::BRANCH: {
                    auto& branch_instr = *GET(*instr, BRANCH);
                    auto continuation_basic_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);

                    auto true_basic_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    process_instr_block(*branch_instr.true_instrs, true_basic_block, continuation_basic_block);
                    auto false_basic_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    process_instr_block(*branch_instr.false_instrs, false_basic_block, continuation_basic_block);

                    builder.CreateCondBr(regs[branch_instr.cond], true_basic_block, false_basic_block);
                    builder.SetInsertPoint(continuation_basic_block);
                } break;

                default:
                    gen.error(diags::not_implemented()); // TODO
            }
        }

        if (next_basic_block)
            builder.CreateBr(next_basic_block);
    }
}
