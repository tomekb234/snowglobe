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

    template<typename T>
    static T decode_number(unsigned long long number) {
        return reinterpret_cast<T&>(number);
    }

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

    string code_generator::get_code() {
        string code;
        llvm::raw_string_ostream stream(code);
        mod.print(stream, nullptr);
        return code;
    }

    void code_generator::llvm_verify(function<bool(llvm::raw_ostream*)> func) {
        string error_msg;
        llvm::raw_string_ostream stream(error_msg);
        if (func(&stream))
            error(diags::code_generator_fail(error_msg));
    }

    llvm::Type* code_generator::get_llvm_type(const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::UNIT:
                return llvm::Type::getInt1Ty(ctx);

            case prog::type::NUMBER:
                return get_llvm_number_type(*GET(type, NUMBER));

            default:
                error(diags::not_implemented()); // TODO
        }
    }

    llvm::Type* code_generator::get_llvm_number_type(const prog::number_type& ntp) {
        switch (ntp.tp) {
            case prog::number_type::BOOL:
                return llvm::Type::getInt1Ty(ctx);

            case prog::number_type::I8:
            case prog::number_type::U8:
                return llvm::Type::getInt8Ty(ctx);

            case prog::number_type::I16:
            case prog::number_type::U16:
                return llvm::Type::getInt16Ty(ctx);

            case prog::number_type::I32:
            case prog::number_type::U32:
                return llvm::Type::getInt32Ty(ctx);

            case prog::number_type::I64:
            case prog::number_type::U64:
                return llvm::Type::getInt64Ty(ctx);

            case prog::number_type::F32:
                return llvm::Type::getFloatTy(ctx);

            case prog::number_type::F64:
                return llvm::Type::getDoubleTy(ctx);
        }

        UNREACHABLE;
    }

    llvm::Value* code_generator::make_constant(const prog::constant& constant) {
        switch (INDEX(constant)) {
            case prog::constant::UNIT:
                return llvm::ConstantInt::getFalse(ctx);

            case prog::constant::NUMBER: {
                auto& [value, number_type] = GET(constant, NUMBER);
                switch (number_type->tp) {
                    case prog::number_type::BOOL:
                        return llvm::ConstantInt::getBool(ctx, decode_number<bool>(value));

                    case prog::number_type::I8:
                        return llvm::ConstantInt::getSigned(llvm::Type::getInt8Ty(ctx), decode_number<int8_t>(value));

                    case prog::number_type::I16:
                        return llvm::ConstantInt::getSigned(llvm::Type::getInt16Ty(ctx), decode_number<int16_t>(value));

                    case prog::number_type::I32:
                        return llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(ctx), decode_number<int32_t>(value));

                    case prog::number_type::I64:
                        return llvm::ConstantInt::getSigned(llvm::Type::getInt64Ty(ctx), decode_number<int64_t>(value));

                    case prog::number_type::U8:
                        return llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx), decode_number<uint8_t>(value));

                    case prog::number_type::U16:
                        return llvm::ConstantInt::get(llvm::Type::getInt16Ty(ctx), decode_number<uint16_t>(value));

                    case prog::number_type::U32:
                        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), decode_number<uint32_t>(value));

                    case prog::number_type::U64:
                        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx), decode_number<uint64_t>(value));

                    case prog::number_type::F32:
                        return llvm::ConstantFP::get(llvm::Type::getFloatTy(ctx), llvm::APFloat(decode_number<float>(value)));

                    case prog::number_type::F64:
                        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(ctx), llvm::APFloat(decode_number<double>(value)));
                }
            } break;

            default:
                error(diags::not_implemented()); // TODO
        }

        UNREACHABLE;
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
        bool terminated = false;

        for (size_t i = 0; !terminated && i < block.instrs.size(); i++) {
            auto& instr = *block.instrs[i];
            switch (INDEX(instr)) {
                case prog::instr::READ_VAR: {
                    auto& rv_instr = *GET(instr, READ_VAR);
                    regs[rv_instr.result] = builder.CreateLoad(gen.get_llvm_type(*func.vars[rv_instr.var]->tp), vars[rv_instr.var]);
                } break;

                case prog::instr::WRITE_VAR: {
                    auto& wv_instr = *GET(instr, WRITE_VAR);
                    builder.CreateStore(regs[wv_instr.value], vars[wv_instr.var]);
                } break;

                case prog::instr::RETURN: {
                    auto& r_instr = *GET(instr, RETURN);
                    builder.CreateRet(regs[r_instr.value]);
                    terminated = true;
                } break;

                case prog::instr::MAKE_UNIT: {
                    auto reg = GET(instr, MAKE_UNIT);
                    regs[reg] = llvm::ConstantInt::get(llvm::Type::getInt1Ty(gen.ctx), 0);
                } break;

                case prog::instr::MAKE_CONST: {
                    auto& mc_instr = *GET(instr, MAKE_CONST);
                    regs[mc_instr.result] = gen.make_constant(*mc_instr.value);
                } break;

                case prog::instr::BOOL_NOT: {
                    auto& uo_instr = *GET(instr, BOOL_NOT);
                    regs[uo_instr.result] = builder.CreateXor(regs[uo_instr.value], llvm::ConstantInt::get(llvm::Type::getInt1Ty(gen.ctx), 1));
                } break;

                case prog::instr::INT_NEG: {
                    auto& uo_instr = *GET(instr, INT_NEG);
                    regs[uo_instr.result] = builder.CreateSub(llvm::ConstantInt::get(regs[uo_instr.value]->getType(), 0), regs[uo_instr.value]);
                } break;

                case prog::instr::FLOAT_NEG: {
                    auto& uo_instr = *GET(instr, FLOAT_NEG);
                    regs[uo_instr.result] = builder.CreateFNeg(regs[uo_instr.value]);
                } break;

                case prog::instr::BIT_NEG: {
                    auto& uo_instr = *GET(instr, BIT_NEG);
                    regs[uo_instr.result] = builder.CreateXor(regs[uo_instr.value], llvm::ConstantInt::get(regs[uo_instr.value]->getType(), -1));
                } break;

                case prog::instr::INCR: {
                    auto& uo_instr = *GET(instr, INCR);
                    regs[uo_instr.result] = builder.CreateAdd(regs[uo_instr.value], llvm::ConstantInt::get(regs[uo_instr.value]->getType(), 1));
                } break;

                case prog::instr::DECR: {
                    auto& uo_instr = *GET(instr, DECR);
                    regs[uo_instr.result] = builder.CreateSub(regs[uo_instr.value], llvm::ConstantInt::get(regs[uo_instr.value]->getType(), 1));
                } break;

                case prog::instr::ADD: {
                    auto& nbo_instr = *GET(instr, ADD);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::FLOAT)
                        regs[nbo_instr.result] = builder.CreateFAdd(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateAdd(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::SUB: {
                    auto& nbo_instr = *GET(instr, SUB);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::FLOAT)
                        regs[nbo_instr.result] = builder.CreateFSub(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateSub(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::MUL: {
                    auto& nbo_instr = *GET(instr, MUL);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::FLOAT)
                        regs[nbo_instr.result] = builder.CreateFMul(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateMul(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::DIV: {
                    auto& nbo_instr = *GET(instr, DIV);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = builder.CreateUDiv(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = builder.CreateSDiv(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateFDiv(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::MOD: {
                    auto& nbo_instr = *GET(instr, MOD);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = builder.CreateURem(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = builder.CreateSRem(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateFRem(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::BIT_AND: {
                    auto& nbo_instr = *GET(instr, BIT_AND);
                    regs[nbo_instr.result] = builder.CreateAnd(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::BIT_OR: {
                    auto& nbo_instr = *GET(instr, BIT_OR);
                    regs[nbo_instr.result] = builder.CreateOr(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::BIT_XOR: {
                    auto& nbo_instr = *GET(instr, BIT_XOR);
                    regs[nbo_instr.result] = builder.CreateXor(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::BIT_LSH:
                case prog::instr::BIT_RSH:
                    gen.error(diags::not_implemented()); // TODO

                case prog::instr::LS: {
                    auto& nbo_instr = *GET(instr, LS);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = builder.CreateICmpULT(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = builder.CreateICmpSLT(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateFCmpULT(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::LSEQ: {
                    auto& nbo_instr = *GET(instr, LSEQ);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = builder.CreateICmpULE(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = builder.CreateICmpSLE(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateFCmpULE(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::GT: {
                    auto& nbo_instr = *GET(instr, GT);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = builder.CreateICmpUGT(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = builder.CreateICmpSGT(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateFCmpUGT(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::GTEQ: {
                    auto& nbo_instr = *GET(instr, GTEQ);
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = builder.CreateICmpUGE(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = builder.CreateICmpSGE(regs[nbo_instr.left], regs[nbo_instr.right]);
                    else
                        regs[nbo_instr.result] = builder.CreateFCmpUGE(regs[nbo_instr.left], regs[nbo_instr.right]);
                } break;

                case prog::instr::ZERO_EXT: {
                    auto& nc_instr = *GET(instr, ZERO_EXT);
                    regs[nc_instr.result] = builder.CreateZExt(regs[nc_instr.value], gen.get_llvm_number_type(*nc_instr.new_type));
                } break;

                case prog::instr::SIGNED_EXT: {
                    auto& nc_instr = *GET(instr, SIGNED_EXT);
                    regs[nc_instr.result] = builder.CreateSExt(regs[nc_instr.value], gen.get_llvm_number_type(*nc_instr.new_type));
                } break;

                case prog::instr::FLOAT_EXT: {
                    auto& nc_instr = *GET(instr, FLOAT_EXT);
                    regs[nc_instr.result] = builder.CreateFPExt(regs[nc_instr.value], gen.get_llvm_number_type(*nc_instr.new_type));
                } break;

                case prog::instr::BRANCH: {
                    auto& b_instr = *GET(instr, BRANCH);
                    bool last = i == block.instrs.size()-1;
                    auto continuation_basic_block = last ? next_basic_block : llvm::BasicBlock::Create(gen.ctx, "", llvm_function);

                    auto true_basic_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    process_instr_block(*b_instr.true_block, true_basic_block, continuation_basic_block);
                    auto false_basic_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    process_instr_block(*b_instr.false_block, false_basic_block, continuation_basic_block);

                    builder.CreateCondBr(regs[b_instr.cond], true_basic_block, false_basic_block);
                    if (!last)
                        builder.SetInsertPoint(continuation_basic_block);
                } break;

                default:
                    gen.error(diags::not_implemented()); // TODO
            }
        }

        if (!builder.GetInsertBlock()->getTerminator()) {
            if (next_basic_block)
                builder.CreateBr(next_basic_block);
            else
                builder.CreateRet(llvm::UndefValue::get(llvm_function->getReturnType()));
        }
    }
}
