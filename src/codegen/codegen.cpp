#include "codegen.hpp"
#include "utils.hpp"
#include "diags.hpp"

#include <vector>

#include <llvm-12/llvm/IR/IRBuilder.h>
#include <llvm-12/llvm/IR/Verifier.h>
#include <llvm/Support/raw_os_ostream.h>

namespace sg {
    using namespace sg::utils;
    using std::vector;

    template<typename T>
    static T decode_number(unsigned long long number) {
        return reinterpret_cast<T&>(number);
    }

    bool code_generator::generate() {
        try {
            // prepare struct types
            struct_types.resize(prog.struct_types.size(), nullptr);
            for (size_t i = 0; i < prog.struct_types.size(); i++)
                struct_types[i] = declare_struct_type(*prog.struct_types[i]);
            for (size_t i = 0; i < prog.struct_types.size(); i++)
                define_struct_type(*prog.struct_types[i], struct_types[i]);

            // declare all functions
            functions.resize(prog.global_funcs.size());
            for (size_t i = 0; i < prog.global_funcs.size(); i++)
                functions[i] = declare_function(*prog.global_funcs[i]);

            // define global variables
            global_vars.resize(prog.global_vars.size());
            for (size_t i = 0; i < prog.global_vars.size(); i++)
                global_vars[i] = define_global_variable(*prog.global_vars[i]);

            // generate function code
            for (size_t i = 0; i < prog.global_funcs.size(); i++)
                function_code_generator(*this, *prog.global_funcs[i], functions[i]).generate();

            // verify module well-formedness
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

    code_generator::typed_llvm_value<llvm::Constant> code_generator::make_constant(const prog::constant& constant) {
        switch (INDEX(constant)) {
            case prog::constant::UNIT:
                return { llvm::ConstantInt::getFalse(ctx), get_unit_type() };

            case prog::constant::NUMBER: {
                auto& [value, number_type] = GET(constant, NUMBER);
                auto named_type = get_number_type(number_type->tp);
                switch (number_type->tp) {
                    case prog::number_type::BOOL:
                        return { llvm::ConstantInt::getBool(ctx, decode_number<bool>(value)), named_type };

                    case prog::number_type::I8:
                        return { llvm::ConstantInt::getSigned(named_type.type, decode_number<int8_t>(value)), named_type };

                    case prog::number_type::I16:
                        return { llvm::ConstantInt::getSigned(named_type.type, decode_number<int16_t>(value)), named_type };

                    case prog::number_type::I32:
                        return { llvm::ConstantInt::getSigned(named_type.type, decode_number<int32_t>(value)), named_type };

                    case prog::number_type::I64:
                        return { llvm::ConstantInt::getSigned(named_type.type, decode_number<int64_t>(value)), named_type };

                    case prog::number_type::U8:
                        return { llvm::ConstantInt::get(named_type.type, decode_number<uint8_t>(value)), named_type };

                    case prog::number_type::U16:
                        return { llvm::ConstantInt::get(named_type.type, decode_number<uint16_t>(value)), named_type };

                    case prog::number_type::U32:
                        return { llvm::ConstantInt::get(named_type.type, decode_number<uint32_t>(value)), named_type };

                    case prog::number_type::U64:
                        return { llvm::ConstantInt::get(named_type.type, decode_number<uint64_t>(value)), named_type };

                    case prog::number_type::F32:
                        return { llvm::ConstantFP::get(named_type.type, llvm::APFloat(decode_number<float>(value))), named_type };

                    case prog::number_type::F64:
                        return { llvm::ConstantFP::get(named_type.type, llvm::APFloat(decode_number<double>(value))), named_type };
                }
            }

            case prog::constant::STRUCT: {
                auto& [struct_index, fields] = GET(constant, STRUCT);
                vector<llvm::Constant*> llvm_fields;
                for (auto& field : fields) {
                    auto [value, named_type] = make_constant(*field);
                    llvm_fields.push_back(value);
                }
                auto named_type = get_struct_type(struct_index);
                return { llvm::ConstantStruct::get(named_type.type, llvm_fields), named_type.to_type() };
            }

            case prog::constant::TUPLE: {
                auto& vals = GET(constant, TUPLE);
                vector<llvm::Constant*> llvm_vals;
                vector<named_llvm_type<llvm::Type>> llvm_types;
                for (auto& val : vals) {
                    auto [value, named_type] = make_constant(*val);
                    llvm_vals.push_back(value);
                    llvm_types.push_back(named_type);
                }
                auto named_type = get_tuple_type(llvm_types);
                return { llvm::ConstantStruct::get(named_type.type, llvm_vals), named_type.to_type() };
            }

            case prog::constant::ARRAY: {
                auto& vals = GET(constant, ARRAY);
                vector<llvm::Constant*> llvm_vals;
                named_llvm_type<llvm::Type> value_type = get_number_type(prog::number_type::I8);
                for (auto& val : vals) {
                    auto [value, named_type] = make_constant(*val);
                    llvm_vals.push_back(value);
                    value_type = named_type;
                }
                auto named_type = get_array_type(value_type, vals.size());
                return { llvm::ConstantArray::get(named_type.type, llvm_vals), named_type.to_type() };
            }

            default:
                error(diags::not_implemented()); // TODO
        }

        UNREACHABLE;
    }

    code_generator::typed_llvm_value<llvm::Function> code_generator::declare_function(const prog::global_func& func) {
        // get function type
        vector<llvm::Type*> arg_types;
        for (auto& param : func.params)
            arg_types.push_back(get_type_from_prog(*param->tp->tp).type);
        auto ret_type = get_type_from_prog(*func.return_tp);
        auto function_type = llvm::FunctionType::get(ret_type.type, arg_types, false);

        // create function
        return { llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, func.name.value_or(""), mod), ret_type };
    }
    
    code_generator::typed_llvm_value<> code_generator::define_global_variable(const prog::global_var& var) {
        auto value = make_constant(*var.value);
        auto type = get_type_from_prog(*var.tp);
        return { new llvm::GlobalVariable(mod, type.type, false, llvm::GlobalVariable::ExternalLinkage, value.value, var.name.value_or("")), type };
    }

    llvm::StructType* code_generator::declare_struct_type(const prog::struct_type& struct_type) {
        return llvm::StructType::create(ctx, struct_type.name);
    }

    void code_generator::define_struct_type(const prog::struct_type& struct_type, llvm::StructType* llvm_struct_type) {
        vector<llvm::Type*> field_types;
        for (auto& field : struct_type.fields)
            field_types.push_back(get_type_from_prog(*field->tp).type);
        llvm_struct_type->setBody(field_types);
    }

    void function_code_generator::generate() {
        // create entry block
        auto entry_block = llvm::BasicBlock::Create(gen.ctx, "entry", llvm_function);
        llvm::IRBuilder<> builder(entry_block);

        // alloc variables
        for (size_t i = 0; i < func.vars.size(); i++) {
            auto type = gen.get_type_from_prog(*func.vars[i]->tp);
            vars[i] = { builder.CreateAlloca(type.type), type };
        }
        for (size_t i = 0; i < func.params.size(); i++) {
            auto type = gen.get_type_from_prog(*func.params[i]->tp->tp);
            regs[i] = { llvm_function->getArg(i), type };
        }

        // prepare unreachable terminator block
        auto terminator_block = llvm::BasicBlock::Create(gen.ctx, "terminator", llvm_function);
        llvm::IRBuilder<>(terminator_block).CreateRet(llvm::UndefValue::get(return_type.type));

        // generate function body
        process_instr_block(*func.instrs, entry_block, terminator_block, nullptr, nullptr);

        // verify corectness
        gen.llvm_verify([&](llvm::raw_ostream* stream){ return llvm::verifyFunction(*llvm_function, stream); });
    }

    llvm::BasicBlock* function_code_generator::process_instr_block(const prog::instr_block& block, llvm::BasicBlock* init_block, llvm::BasicBlock* after_block, llvm::BasicBlock* loop_block, llvm::BasicBlock* after_loop_block) {
        llvm::IRBuilder<> builder(init_block);
        bool terminated = false;

        for (auto& instr : block.instrs) {
            switch (INDEX(*instr)) {
                case prog::instr::READ_VAR: {
                    auto& rv_instr = *GET(*instr, READ_VAR);
                    auto& var = vars[rv_instr.var];
                    regs[rv_instr.result] = { builder.CreateLoad(var.type.type, var.value), var.type };
                } break;

                case prog::instr::READ_GLOBAL_VAR: {
                    auto& rgv_instr = *GET(*instr, READ_GLOBAL_VAR);
                    auto& gvar = gen.global_vars[rgv_instr.var];
                    regs[rgv_instr.result] = { builder.CreateLoad(gvar.type.type, gvar.value), gvar.type };
                } break;

                case prog::instr::WRITE_VAR: {
                    auto& wv_instr = *GET(*instr, WRITE_VAR);
                    builder.CreateStore(regs[wv_instr.value].value, vars[wv_instr.var].value);
                } break;

                case prog::instr::WRITE_GLOBAL_VAR: {
                    auto& wgv_instr = *GET(*instr, WRITE_GLOBAL_VAR);
                    builder.CreateStore(regs[wgv_instr.value].value, gen.global_vars[wgv_instr.var].value);
                } break;

                case prog::instr::RETURN: {
                    auto& r_instr = *GET(*instr, RETURN);
                    builder.CreateRet(regs[r_instr.value].value);
                    terminated = true;
                } break;

                case prog::instr::FUNC_CALL: {
                    auto& fc_instr = *GET(*instr, FUNC_CALL);
                    auto& typed_func = gen.functions[fc_instr.func];
                    auto callee = llvm::FunctionCallee(typed_func.value);
                    vector<llvm::Value*> args;
                    for (auto arg : fc_instr.args)
                        args.push_back(regs[arg].value);
                    regs[fc_instr.result] = { builder.CreateCall(callee, args), typed_func.type };
                } break;

                case prog::instr::MAKE_UNIT: {
                    auto reg = GET(*instr, MAKE_UNIT);
                    regs[reg] = { llvm::ConstantInt::getFalse(gen.ctx), gen.get_unit_type() };
                } break;

                case prog::instr::MAKE_CONST: {
                    auto& mc_instr = *GET(*instr, MAKE_CONST);
                    auto constant = gen.make_constant(*mc_instr.value);
                    regs[mc_instr.result] = { constant.value, constant.type };
                } break;

                case prog::instr::MAKE_TUPLE: {
                    auto& mt_instr = *GET(*instr, MAKE_TUPLE);
                    vector<code_generator::named_llvm_type<>> field_types;
                    for (auto reg : mt_instr.values)
                        field_types.push_back(regs[reg].type);
                    auto tuple_type = gen.get_tuple_type(field_types);
                    llvm::Value* tuple_value = llvm::UndefValue::get(tuple_type.type);
                    for (size_t i = 0; i < mt_instr.values.size(); i++)
                        tuple_value = builder.CreateInsertValue(tuple_value, regs[mt_instr.values[i]].value, vector<unsigned>{(unsigned)i});
                    regs[mt_instr.result] = { tuple_value, tuple_type.to_type() };
                } break;

                case prog::instr::MAKE_ARRAY: {
                    auto& ma_instr = *GET(*instr, MAKE_ARRAY);
                    auto value_type = ma_instr.values.empty() ? gen.get_never_type() : regs[ma_instr.values.front()].type;
                    auto array_type = gen.get_array_type(value_type, ma_instr.values.size());
                    llvm::Value* array_value = llvm::UndefValue::get(array_type.type);
                    for (size_t i = 0; i < ma_instr.values.size(); i++)
                        array_value = builder.CreateInsertValue(array_value, regs[ma_instr.values[i]].value, vector<unsigned>{(unsigned)i});
                    regs[ma_instr.result] = { array_value, array_type.to_type() };
                } break;

                case prog::instr::MAKE_STRUCT: {
                    auto& ms_instr = *GET(*instr, MAKE_STRUCT);
                    auto struct_type = gen.get_struct_type(ms_instr.st);
                    llvm::Value* struct_value = llvm::UndefValue::get(struct_type.type);
                    for (size_t i = 0; i < ms_instr.args.size(); i++)
                        struct_value = builder.CreateInsertValue(struct_value, regs[ms_instr.args[i]].value, vector<unsigned>{(unsigned)i});
                    regs[ms_instr.result] = { struct_value, struct_type.to_type() };
                } break;

                case prog::instr::BOOL_NOT: {
                    auto& uo_instr = *GET(*instr, BOOL_NOT);
                    regs[uo_instr.result] = { builder.CreateXor(regs[uo_instr.value].value, llvm::ConstantInt::getTrue(gen.ctx)), regs[uo_instr.value].type };
                } break;

                case prog::instr::INT_NEG: {
                    auto& uo_instr = *GET(*instr, INT_NEG);
                    auto& val = regs[uo_instr.value];
                    regs[uo_instr.result] = { builder.CreateSub(llvm::ConstantInt::get(val.type.type, 0), val.value), val.type };
                } break;

                case prog::instr::FLOAT_NEG: {
                    auto& uo_instr = *GET(*instr, FLOAT_NEG);
                    regs[uo_instr.result] = { builder.CreateFNeg(regs[uo_instr.value].value), regs[uo_instr.value].type };
                } break;

                case prog::instr::BIT_NEG: {
                    auto& uo_instr = *GET(*instr, BIT_NEG);
                    auto& reg = regs[uo_instr.value];
                    regs[uo_instr.result] = { builder.CreateXor(reg.value, llvm::ConstantInt::get(reg.type.type, -1)), reg.type };
                } break;

                case prog::instr::INCR: {
                    auto& uo_instr = *GET(*instr, INCR);
                    auto& reg = regs[uo_instr.value];
                    regs[uo_instr.result] = { builder.CreateAdd(reg.value, llvm::ConstantInt::get(reg.type.type, 1)), reg.type };
                } break;

                case prog::instr::DECR: {
                    auto& uo_instr = *GET(*instr, DECR);
                    auto& reg = regs[uo_instr.value];
                    regs[uo_instr.result] = { builder.CreateSub(reg.value, llvm::ConstantInt::get(reg.type.type, 1)), reg.type };
                } break;

                case prog::instr::ADD: {
                    auto& nbo_instr = *GET(*instr, ADD);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::FLOAT)
                        regs[nbo_instr.result] = { builder.CreateFAdd(lreg.value, rreg.value), lreg.type };
                    else
                        regs[nbo_instr.result] = { builder.CreateAdd(lreg.value, rreg.value), lreg.type };
                } break;

                case prog::instr::SUB: {
                    auto& nbo_instr = *GET(*instr, SUB);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::FLOAT)
                        regs[nbo_instr.result] = { builder.CreateFSub(lreg.value, rreg.value), lreg.type };
                    else
                        regs[nbo_instr.result] = { builder.CreateSub(lreg.value, rreg.value), lreg.type };
                } break;

                case prog::instr::MUL: {
                    auto& nbo_instr = *GET(*instr, MUL);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::FLOAT)
                        regs[nbo_instr.result] = { builder.CreateFMul(lreg.value, rreg.value), lreg.type };
                    else
                        regs[nbo_instr.result] = { builder.CreateMul(lreg.value, rreg.value), lreg.type };
                } break;

                case prog::instr::DIV: {
                    auto& nbo_instr = *GET(*instr, DIV);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateUDiv(lreg.value, rreg.value), lreg.type };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateSDiv(lreg.value, rreg.value), lreg.type };
                    else
                        regs[nbo_instr.result] = { builder.CreateFDiv(lreg.value, rreg.value), lreg.type };
                } break;

                case prog::instr::MOD: {
                    auto& nbo_instr = *GET(*instr, MOD);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateURem(lreg.value, rreg.value), lreg.type };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateSRem(lreg.value, rreg.value), lreg.type };
                    else
                        regs[nbo_instr.result] = { builder.CreateFRem(lreg.value, rreg.value), lreg.type };
                } break;

                case prog::instr::BIT_AND: {
                    auto& nbo_instr = *GET(*instr, BIT_AND);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    regs[nbo_instr.result] = { builder.CreateAnd(lreg.value, rreg.value), lreg.type };
                } break;

                case prog::instr::BIT_OR: {
                    auto& nbo_instr = *GET(*instr, BIT_OR);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    regs[nbo_instr.result] = { builder.CreateOr(lreg.value, rreg.value), lreg.type };
                } break;

                case prog::instr::BIT_XOR: {
                    auto& nbo_instr = *GET(*instr, BIT_XOR);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    regs[nbo_instr.result] = { builder.CreateXor(lreg.value, rreg.value), lreg.type };
                } break;

                case prog::instr::BIT_LSH:
                case prog::instr::BIT_RSH:
                    gen.error(diags::not_implemented()); // TODO

                case prog::instr::LS: {
                    auto& nbo_instr = *GET(*instr, LS);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpULT(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpSLT(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                    else
                        regs[nbo_instr.result] = { builder.CreateFCmpULT(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                } break;

                case prog::instr::LSEQ: {
                    auto& nbo_instr = *GET(*instr, LSEQ);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpULE(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpSLE(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                    else
                        regs[nbo_instr.result] = { builder.CreateFCmpULE(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                } break;

                case prog::instr::GT: {
                    auto& nbo_instr = *GET(*instr, GT);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpUGT(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpSGT(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                    else
                        regs[nbo_instr.result] = { builder.CreateFCmpUGT(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                } break;

                case prog::instr::GTEQ: {
                    auto& nbo_instr = *GET(*instr, GTEQ);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpUGE(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpSGE(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                    else
                        regs[nbo_instr.result] = { builder.CreateFCmpUGE(lreg.value, rreg.value), gen.get_number_type(prog::number_type::BOOL) };
                } break;

                case prog::instr::ZERO_EXT: {
                    auto& nc_instr = *GET(*instr, ZERO_EXT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateZExt(regs[nc_instr.value].value, type.type), type };
                } break;

                case prog::instr::SIGNED_EXT: {
                    auto& nc_instr = *GET(*instr, SIGNED_EXT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateSExt(regs[nc_instr.value].value, type.type), type };
                } break;

                case prog::instr::FLOAT_EXT: {
                    auto& nc_instr = *GET(*instr, FLOAT_EXT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateFPExt(regs[nc_instr.value].value, type.type), type };
                } break;

                case prog::instr::BRANCH:
                case prog::instr::VALUE_BRANCH: {
                    auto& b_instr = INDEX_EQ(*instr, BRANCH) ? *GET(*instr, BRANCH) : *GET(*instr, VALUE_BRANCH);
                    auto continuation_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);

                    auto true_init_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    auto true_return_block = process_instr_block(*b_instr.true_block, true_init_block, continuation_block, loop_block, after_loop_block);
                    auto false_init_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    auto false_return_block = process_instr_block(*b_instr.false_block, false_init_block, continuation_block, loop_block, after_loop_block);

                    builder.CreateCondBr(regs[b_instr.cond].value, true_init_block, false_init_block);
                    builder.SetInsertPoint(continuation_block);

                    if (INDEX_EQ(*instr, VALUE_BRANCH)) {
                        auto& vb_instr = *GET(*instr, VALUE_BRANCH);
                        auto& type = regs[vb_instr.true_value].type;
                        auto phi_node = llvm::PHINode::Create(type.type, 2, "", continuation_block);
                        phi_node->addIncoming(regs[vb_instr.true_value].value, true_return_block);
                        phi_node->addIncoming(regs[vb_instr.false_value].value, false_return_block);
                        regs[vb_instr.result] = { phi_node, type };
                    }
                } break;

                case prog::instr::LOOP: {
                    auto& l_instr = *GET(*instr, LOOP);
                    auto loop_body_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    auto continuation_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    process_instr_block(l_instr, loop_body_block, loop_body_block, loop_body_block, continuation_block);

                    builder.CreateBr(loop_body_block);
                    builder.SetInsertPoint(continuation_block);
                } break;

                case prog::instr::CONTINUE_LOOP: {
                    builder.CreateBr(loop_block);
                    terminated = true;
                } break;

                case prog::instr::BREAK_LOOP: {
                    builder.CreateBr(after_loop_block);
                    terminated = true;
                } break;

                default:
                    gen.error(diags::not_implemented()); // TODO
            }
            if (terminated)
                break;
        }

        if (!builder.GetInsertBlock()->getTerminator())
            builder.CreateBr(after_block);
        return builder.GetInsertBlock();
    }
}
