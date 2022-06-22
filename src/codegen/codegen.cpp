#include "codegen.hpp"
#include "utils.hpp"
#include "diags.hpp"

#include <llvm-12/llvm/IR/IRBuilder.h>
#include <llvm-12/llvm/IR/Verifier.h>
#include <llvm/Support/raw_os_ostream.h>

namespace sg {
    using namespace sg::utils;

    const string internal_func_prefix = "if.";
    const string internal_var_prefix = "iv.";

    template<typename T>
    static T decode_number(unsigned long long number) {
        auto ptr = reinterpret_cast<T*>(&number);
        return *ptr;
    }

    bool code_generator::generate() {
        try {
            // prepare struct/enum types
            for (size_t i = 0; i < prog.struct_types.size(); i++)
                struct_types.push_back(declare_struct_type(i));
            for (size_t i = 0; i < prog.enum_types.size(); i++) {
                enum_types.push_back(declare_enum_type(i));
                variant_types.push_back({ });
                for (size_t j = 0; j < prog.enum_types[i]->variants.size(); j++)
                    variant_types[i].push_back(declare_enum_variant_type(i, j));
            }

            for (size_t i = 0; i < prog.struct_types.size(); i++)
                define_struct_type(*prog.struct_types[i], &GET(*struct_types[i], STRUCT));
            for (size_t i = 0; i < prog.enum_types.size(); i++) {
                vector<ll_enum_variant_type*> enum_variants;
                for (auto enum_variant : variant_types[i])
                    enum_variants.push_back(&GET(*enum_variant, ENUM_VARIANT));
                define_enum_type_with_variants(*prog.enum_types[i], &GET(*enum_types[i], ENUM), enum_variants);
            }

            // declare all functions
            functions.resize(prog.global_funcs.size());
            for (size_t i = 0; i < prog.global_funcs.size(); i++)
                functions[i] = declare_function(*prog.global_funcs[i]);

            // define global variables
            global_vars.resize(prog.global_vars.size());
            for (size_t i = 0; i < prog.global_vars.size(); i++)
                global_vars[i] = define_global_variable(*prog.global_vars[i]);

            // generate internal variables / functions
            auto init_func = define_init_function();
            
            // generate function code
            for (size_t i = 0; i < prog.global_funcs.size(); i++) {
                auto func_codegen = function_code_generator(*this, *prog.global_funcs[i], functions[i]);
                if (prog.global_funcs[i]->name.value_or("") == "main")
                    func_codegen.generate({ init_func });
                else
                    func_codegen.generate({ });
            }

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

    code_generator::typed_llvm_value<> code_generator::make_constant(const prog::constant& constant, const prog::type& type, llvm::IRBuilderBase& builder) {
        switch (INDEX(constant)) {
            case prog::constant::UNIT:
                return { builder.getFalse(), get_unit_type() };

            case prog::constant::NUMBER: {
                auto value = GET(constant, NUMBER);
                auto number_type = GET(type, NUMBER)->tp;
                auto ll_num_type = get_number_type(number_type);
                switch (number_type) {
                    case prog::number_type::BOOL:
                        return { builder.getInt1(decode_number<bool>(value)), ll_num_type };

                    case prog::number_type::I8:
                        return { builder.getInt8(decode_number<int8_t>(value)), ll_num_type };

                    case prog::number_type::I16:
                        return { builder.getInt16(decode_number<int16_t>(value)), ll_num_type };

                    case prog::number_type::I32:
                        return { builder.getInt32(decode_number<int32_t>(value)), ll_num_type };

                    case prog::number_type::I64:
                        return { builder.getInt64(decode_number<int64_t>(value)), ll_num_type };

                    case prog::number_type::U8:
                        return { builder.getInt8(decode_number<uint8_t>(value)), ll_num_type };

                    case prog::number_type::U16:
                        return { builder.getInt16(decode_number<uint16_t>(value)), ll_num_type };

                    case prog::number_type::U32:
                        return { builder.getInt32(decode_number<uint32_t>(value)), ll_num_type };

                    case prog::number_type::U64:
                        return { builder.getInt64(decode_number<uint64_t>(value)), ll_num_type };

                    case prog::number_type::F32:
                        return { llvm::ConstantFP::get(ll_num_type->get_type(), llvm::APFloat(decode_number<float>(value))), ll_num_type };

                    case prog::number_type::F64:
                        return { llvm::ConstantFP::get(ll_num_type->get_type(), llvm::APFloat(decode_number<double>(value))), ll_num_type };
                }

                UNREACHABLE;
            }

            case prog::constant::STRUCT: {
                auto struct_index = GET(type, STRUCT);
                auto& fields = GET(constant, STRUCT);
                vector<typed_llvm_value<>> field_values;
                for (size_t i = 0; i < fields.size(); i++) {
                    auto& field_type = *prog.struct_types[struct_index]->fields[i]->tp;
                    field_values.push_back(make_constant(*fields[i], field_type, builder));
                }
                return make_struct_value(struct_index, field_values, builder);
            }

            case prog::constant::ENUM: {
                auto enum_index = GET(type, ENUM);
                auto& [variant_index, fields] = GET(constant, ENUM);
                vector<typed_llvm_value<>> field_values;
                for (size_t i = 0; i < fields.size(); i++) {
                    auto& field_type = *prog.enum_types[enum_index]->variants[variant_index]->tps[i];
                    field_values.push_back(make_constant(*fields[i], field_type, builder));
                }
                return make_enum_variant_value(enum_index, variant_index, field_values, builder);
            }

            case prog::constant::TUPLE: {
                auto& field_types = GET(type, TUPLE);
                auto& vals = GET(constant, TUPLE);
                vector<typed_llvm_value<>> fields;
                for (size_t i = 0; i < vals.size(); i++)
                    fields.push_back(make_constant(*vals[i], *field_types[i], builder));
                return make_tuple_value(fields, builder);
            }

            case prog::constant::ARRAY: {
                auto& value_type = *GET(type, ARRAY)->tp;
                auto& vals = GET(constant, ARRAY);
                vector<typed_llvm_value<>> values;
                for (auto& val : vals)
                    values.push_back(make_constant(*val, value_type, builder));
                return make_array_value(values, builder);
            }

            case prog::constant::OPTIONAL: {
                auto& value_type = *GET(type, OPTIONAL);
                auto& maybe_value = GET(constant, OPTIONAL);
                if (maybe_value)
                    return make_filled_optional_value(make_constant(**maybe_value, value_type, builder), builder);
                else
                    return make_empty_optional_value(get_type_from_prog(value_type), builder);
            }

            default:
                error(diags::not_implemented()); // TODO
        }

        UNREACHABLE;
    }

    code_generator::typed_llvm_value<> code_generator::make_struct_value(prog::global_index struct_index, vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder) {
        auto struct_type = struct_types[struct_index];
        llvm::Value* struct_value = llvm::UndefValue::get(struct_type->get_type());
        for (size_t i = 0; i < fields.size(); i++)
            struct_value = builder.CreateInsertValue(struct_value, fields[i].value, vector<unsigned>{(unsigned)i});
        return { struct_value, struct_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_enum_variant_value(prog::global_index enum_index, prog::variant_index variant_index, vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder) {
        // make variant
        auto variant_type = variant_types[enum_index][variant_index];
        llvm::Value* variant_value = llvm::UndefValue::get(variant_type->get_type());
        variant_value = builder.CreateInsertValue(variant_value, builder.getInt64(variant_index), vector<unsigned>{0});
        for (size_t i = 0; i < fields.size(); i++)
            variant_value = builder.CreateInsertValue(variant_value, fields[i].value, vector<unsigned>{(unsigned)i + 1});

        // bitcast to generic enum type
        auto enum_type = enum_types[enum_index];
        auto llvm_mem_ptr = builder.CreateAlloca(variant_type->get_type());
        builder.CreateStore(variant_value, llvm_mem_ptr);
        auto llvm_mem_ptr_casted = builder.CreateBitCast(llvm_mem_ptr, llvm::PointerType::getUnqual(enum_type->get_type()));
        auto enum_value = builder.CreateLoad(enum_type->get_type(), llvm_mem_ptr_casted);

        return { enum_value, enum_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_tuple_value(vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder) {
        vector<ll_type*> field_types;
        for (auto field : fields)
            field_types.push_back(field.type);
        auto tuple_type = get_tuple_type(field_types);
        llvm::Value* tuple_value = llvm::UndefValue::get(tuple_type->get_type());
        for (size_t i = 0; i < fields.size(); i++)
            tuple_value = builder.CreateInsertValue(tuple_value, fields[i].value, vector<unsigned>{(unsigned)i});
        return { tuple_value, tuple_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_array_value(vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder) {
        auto value_type = fields.empty() ? get_never_type() : fields.front().type;
        auto array_type = get_array_type(value_type, fields.size());
        llvm::Value* array_value = llvm::UndefValue::get(array_type->get_type());
        for (size_t i = 0; i < fields.size(); i++)
            array_value = builder.CreateInsertValue(array_value, fields[i].value, vector<unsigned>{(unsigned)i});
        return { array_value, array_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_empty_optional_value(ll_type* value_type, llvm::IRBuilderBase& builder) {
        auto optional_type = get_optional_type(value_type);
        llvm::Value* value = llvm::UndefValue::get(optional_type->get_type());
        value = builder.CreateInsertValue(value, builder.getFalse(), vector<unsigned>{0});
        return { value, optional_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_filled_optional_value(typed_llvm_value<> value, llvm::IRBuilderBase& builder) {
        auto optional_type = get_optional_type(value.type);
        llvm::Value* optional_value = llvm::UndefValue::get(optional_type->get_type());
        optional_value = builder.CreateInsertValue(optional_value, builder.getTrue(), vector<unsigned>{0});
        optional_value = builder.CreateInsertValue(optional_value, value.value, vector<unsigned>{1});
        return { optional_value, optional_type };
    }

    code_generator::typed_llvm_value<llvm::Function> code_generator::declare_function(const prog::global_func& func) {
        // get function type
        vector<llvm::Type*> arg_types;
        for (auto& param : func.params)
            arg_types.push_back(get_type_from_prog(*param->tp->tp)->get_type());
        auto ret_type = get_type_from_prog(*func.return_tp);
        auto function_type = llvm::FunctionType::get(ret_type->get_type(), arg_types, false);

        // create function
        return { llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, func.name.value_or(""), mod), ret_type };
    }
    
    code_generator::typed_llvm_value<> code_generator::define_global_variable(const prog::global_var& var) {
        auto type = get_type_from_prog(*var.tp);
        auto value = llvm::UndefValue::get(type->get_type());
        return { new llvm::GlobalVariable(mod, type->get_type(), false, llvm::GlobalVariable::ExternalLinkage, value, var.name.value_or("")), type };
    }

    llvm::Function* code_generator::define_init_function() {
        auto init_var = new llvm::GlobalVariable(mod, llvm::Type::getInt1Ty(ctx), false, llvm::GlobalVariable::PrivateLinkage, llvm::ConstantInt::getFalse(ctx), internal_var_prefix + "after_init");
        auto type = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), { }, false);
        auto func = llvm::Function::Create(type, llvm::Function::PrivateLinkage, internal_func_prefix + "init", mod);
        auto entry_block = llvm::BasicBlock::Create(ctx, "entry", func);

        // init block
        auto init_block = llvm::BasicBlock::Create(ctx, "init", func);
        llvm::IRBuilder<> builder(init_block);
        for (size_t i = 0; i < prog.global_vars.size(); i++) {
            auto& global_var = *prog.global_vars[i];
            builder.CreateStore(make_constant(*global_var.value, *global_var.tp, builder).value, global_vars[i].value);
        }
        builder.CreateStore(llvm::ConstantInt::getTrue(ctx), init_var);
        builder.CreateRetVoid();

        // noinit block
        auto noinit_block = llvm::BasicBlock::Create(ctx, "noinit", func);
        builder.SetInsertPoint(noinit_block);
        builder.CreateRetVoid();

        // entry block
        builder.SetInsertPoint(entry_block);
        auto init_var_val = builder.CreateLoad(llvm::Type::getInt1Ty(ctx), init_var);
        builder.CreateCondBr(init_var_val, noinit_block, init_block);

        // verify corectness
        llvm_verify([&](llvm::raw_ostream* stream){ return llvm::verifyFunction(*func, stream); });

        return func;
    }


    void function_code_generator::generate(optional<llvm::Function*> init_func) {
        // create entry block
        auto entry_block = llvm::BasicBlock::Create(gen.ctx, "entry", llvm_function);
        llvm::IRBuilder<> builder(entry_block);

        // add call to init function
        if (init_func)
            builder.CreateCall(*init_func);

        // alloc variables
        for (size_t i = 0; i < func.vars.size(); i++) {
            auto type = gen.get_type_from_prog(*func.vars[i]->tp);
            vars[i] = { builder.CreateAlloca(type->get_type()), type };
        }
        for (size_t i = 0; i < func.params.size(); i++) {
            auto type = gen.get_type_from_prog(*func.params[i]->tp->tp);
            regs[i] = { llvm_function->getArg(i), type };
        }

        // prepare unreachable terminator block
        auto terminator_block = llvm::BasicBlock::Create(gen.ctx, "terminator", llvm_function);
        llvm::IRBuilder<>(terminator_block).CreateRet(llvm::UndefValue::get(return_type->get_type()));

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
                    regs[rv_instr.result] = { builder.CreateLoad(var.type->get_type(), var.value), var.type };
                } break;

                case prog::instr::READ_GLOBAL_VAR: {
                    auto& rgv_instr = *GET(*instr, READ_GLOBAL_VAR);
                    auto& gvar = gen.global_vars[rgv_instr.var];
                    regs[rgv_instr.result] = { builder.CreateLoad(gvar.type->get_type(), gvar.value), gvar.type };
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
                    regs[reg] = { builder.getFalse(), gen.get_unit_type() };
                } break;

                case prog::instr::MAKE_CONST: {
                    auto& mc_instr = *GET(*instr, MAKE_CONST);
                    auto constant = gen.make_constant(*mc_instr.value, *mc_instr.tp, builder);
                    regs[mc_instr.result] = { constant.value, constant.type };
                } break;

                case prog::instr::MAKE_TUPLE: {
                    auto& mt_instr = *GET(*instr, MAKE_TUPLE);
                    vector<code_generator::typed_llvm_value<>> fields;
                    for (auto reg : mt_instr.values)
                        fields.push_back(regs[reg]);
                    regs[mt_instr.result] = gen.make_tuple_value(fields, builder);

                } break;

                case prog::instr::MAKE_ARRAY: {
                    auto& ma_instr = *GET(*instr, MAKE_ARRAY);
                    vector<code_generator::typed_llvm_value<>> fields;
                    for (auto reg : ma_instr.values)
                        fields.push_back(regs[reg]);
                    regs[ma_instr.result] = gen.make_array_value(fields, builder);
                } break;

                case prog::instr::MAKE_OPTIONAL: {
                    auto& mo_instr = *GET(*instr, MAKE_OPTIONAL);
                    if (mo_instr.value)
                        regs[mo_instr.result] = gen.make_filled_optional_value(regs[*mo_instr.value], builder);
                    else
                        regs[mo_instr.result] = gen.make_empty_optional_value(gen.get_never_type(), builder);
                } break;

                case prog::instr::MAKE_STRUCT: {
                    auto& ms_instr = *GET(*instr, MAKE_STRUCT);
                    vector<code_generator::typed_llvm_value<>> fields;
                    for (auto reg : ms_instr.args)
                        fields.push_back(regs[reg]);
                    regs[ms_instr.result] = gen.make_struct_value(ms_instr.st, fields, builder);
                } break;

                case prog::instr::MAKE_ENUM_VARIANT: {
                    auto& mev_instr = *GET(*instr, MAKE_ENUM_VARIANT);
                    vector<code_generator::typed_llvm_value<>> fields;
                    for (auto reg : mev_instr.args)
                        fields.push_back(regs[reg]);
                    regs[mev_instr.result] = gen.make_enum_variant_value(mev_instr.en, mev_instr.variant, fields, builder);
                } break;

                case prog::instr::FROM_NEVER: {
                    auto& fn_instr = *GET(*instr, FROM_NEVER);
                    auto type = gen.get_type_from_prog(*fn_instr.tp);
                    regs[fn_instr.result] = { llvm::UndefValue::get(type->get_type()), type };
                } break;

                case prog::instr::TEST_OPTIONAL: {
                    auto& to_instr = *GET(*instr, TEST_OPTIONAL);
                    regs[to_instr.result] = { builder.CreateExtractValue(regs[to_instr.value].value, vector<unsigned>{0}), gen.get_number_type(prog::number_type::BOOL) };
                } break;

                case prog::instr::TEST_VARIANT: {
                    auto& tv_instr = *GET(*instr, TEST_VARIANT);
                    auto real_variant_field = builder.CreateExtractValue(regs[tv_instr.value].value, vector<unsigned>{0});
                    auto result = builder.CreateICmpEQ(real_variant_field, builder.getInt64(tv_instr.variant));
                    regs[tv_instr.result] = { result, gen.get_number_type(prog::number_type::BOOL) };
                } break;

                case prog::instr::EXTRACT_ITEM: {
                    auto& ei_instr = *GET(*instr, EXTRACT_ITEM);
                    auto& array_value = regs[ei_instr.value];
                    auto value_type = GET(*array_value.type, ARRAY).value;

                    auto buffer_ptr = builder.CreateAlloca(array_value.type->get_type());
                    builder.CreateStore(array_value.value, buffer_ptr);
                    auto item_adr = builder.CreateGEP(array_value.type->get_type(), buffer_ptr, vector<llvm::Value*>{regs[ei_instr.index].value});
                    regs[ei_instr.result] = { builder.CreateLoad(value_type->get_type(), item_adr), value_type };
                } break;

                case prog::instr::EXTRACT_FIELD: {
                    auto& ef_instr = *GET(*instr, EXTRACT_FIELD);
                    auto& value = regs[ef_instr.value];
                    ll_type* field_type;
                    if (INDEX_EQ(*value.type, STRUCT))
                        field_type = GET(*value.type, STRUCT).fields[ef_instr.field];
                    else if (INDEX_EQ(*value.type, TUPLE))
                        field_type = GET(*value.type, TUPLE).fields[ef_instr.field];
                    else
                        UNREACHABLE;
                    regs[ef_instr.result] = { builder.CreateExtractValue(value.value, vector<unsigned>{(unsigned)ef_instr.field}), field_type };
                } break;

                case prog::instr::EXTRACT_OPTIONAL_VALUE: {
                    auto& eov_instr = *GET(*instr, EXTRACT_OPTIONAL_VALUE);
                    auto& value = regs[eov_instr.value];
                    regs[eov_instr.result] = { builder.CreateExtractValue(value.value, vector<unsigned>{1}), value.type };
                } break;

                case prog::instr::EXTRACT_VARIANT_FIELD: {
                    auto& evf_instr = *GET(*instr, EXTRACT_VARIANT_FIELD);
                    auto value = regs[evf_instr.value];
                    auto enum_index = GET(*value.type, ENUM).index;
                    auto variant_type = GET(*gen.variant_types[enum_index][evf_instr.variant], ENUM_VARIANT);

                    auto buffer_ptr = builder.CreateAlloca(value.type->get_type());
                    builder.CreateStore(value.value, buffer_ptr);
                    auto buffer_ptr_casted = builder.CreateBitCast(buffer_ptr, llvm::PointerType::getUnqual(variant_type.tp));
                    auto value_casted = builder.CreateLoad(variant_type.tp, buffer_ptr_casted);
                    auto field_value = builder.CreateExtractValue(value_casted, vector<unsigned>{(unsigned)evf_instr.field});
                    regs[evf_instr.result] = { field_value, variant_type.fields[evf_instr.field] };
                } break;

                case prog::instr::BOOL_NOT: {
                    auto& uo_instr = *GET(*instr, BOOL_NOT);
                    regs[uo_instr.result] = { builder.CreateXor(regs[uo_instr.value].value, builder.getTrue()), regs[uo_instr.value].type };
                } break;

                case prog::instr::INT_NEG: {
                    auto& uo_instr = *GET(*instr, INT_NEG);
                    auto& val = regs[uo_instr.value];
                    regs[uo_instr.result] = { builder.CreateSub(llvm::ConstantInt::get(val.type->get_type(), 0), val.value), val.type };
                } break;

                case prog::instr::FLOAT_NEG: {
                    auto& uo_instr = *GET(*instr, FLOAT_NEG);
                    regs[uo_instr.result] = { builder.CreateFNeg(regs[uo_instr.value].value), regs[uo_instr.value].type };
                } break;

                case prog::instr::BIT_NEG: {
                    auto& uo_instr = *GET(*instr, BIT_NEG);
                    auto& reg = regs[uo_instr.value];
                    regs[uo_instr.result] = { builder.CreateXor(reg.value, llvm::ConstantInt::get(reg.type->get_type(), -1)), reg.type };
                } break;

                case prog::instr::INCR: {
                    auto& uo_instr = *GET(*instr, INCR);
                    auto& reg = regs[uo_instr.value];
                    regs[uo_instr.result] = { builder.CreateAdd(reg.value, llvm::ConstantInt::get(reg.type->get_type(), 1)), reg.type };
                } break;

                case prog::instr::DECR: {
                    auto& uo_instr = *GET(*instr, DECR);
                    auto& reg = regs[uo_instr.value];
                    regs[uo_instr.result] = { builder.CreateSub(reg.value, llvm::ConstantInt::get(reg.type->get_type(), 1)), reg.type };
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

                case prog::instr::TRUNC: {
                    auto& nc_instr = *GET(*instr, TRUNC);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateTrunc(regs[nc_instr.value].value, type->get_type()), type };
                } break;

                case prog::instr::ZERO_EXT: {
                    auto& nc_instr = *GET(*instr, ZERO_EXT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateZExt(regs[nc_instr.value].value, type->get_type()), type };
                } break;

                case prog::instr::SIGNED_EXT: {
                    auto& nc_instr = *GET(*instr, SIGNED_EXT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateSExt(regs[nc_instr.value].value, type->get_type()), type };
                } break;

                case prog::instr::UINT_TO_FLOAT: {
                    auto& nc_instr = *GET(*instr, UINT_TO_FLOAT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateUIToFP(regs[nc_instr.value].value, type->get_type()), type };
                } break;

                case prog::instr::SINT_TO_FLOAT: {
                    auto& nc_instr = *GET(*instr, SINT_TO_FLOAT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateSIToFP(regs[nc_instr.value].value, type->get_type()), type };
                } break;

                case prog::instr::FLOAT_TRUNC: {
                    auto& nc_instr = *GET(*instr, FLOAT_TRUNC);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateSIToFP(regs[nc_instr.value].value, type->get_type()), type };
                } break;

                case prog::instr::FLOAT_EXT: {
                    auto& nc_instr = *GET(*instr, FLOAT_EXT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateFPExt(regs[nc_instr.value].value, type->get_type()), type };
                } break;

                case prog::instr::FLOAT_TO_UINT: {
                    auto& nc_instr = *GET(*instr, FLOAT_TO_UINT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateFPToUI(regs[nc_instr.value].value, type->get_type()), type };
                } break;

                case prog::instr::FLOAT_TO_SINT: {
                    auto& nc_instr = *GET(*instr, FLOAT_TO_SINT);
                    auto type = gen.get_number_type(nc_instr.new_type->tp);
                    regs[nc_instr.result] = { builder.CreateFPToSI(regs[nc_instr.value].value, type->get_type()), type };
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
                        auto phi_node = llvm::PHINode::Create(type->get_type(), 2, "", continuation_block);
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
