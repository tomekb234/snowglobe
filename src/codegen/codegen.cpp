#include "codegen.hpp"
#include "utils.hpp"
#include "diags.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Bitcode/BitcodeWriterPass.h>
#include <llvm/Support/Host.h>

#include <fstream>
#include <cstdlib>

namespace sg {
    using namespace sg::utils;
    using std::ofstream;
    using std::ifstream;

    const string normal_func_prefix = "f.";
    const string internal_func_prefix = "if.";

    void code_generator::generate_code(ostream& stream) {
        generate();
        llvm::raw_os_ostream llvm_stream(stream);
        mod.print(llvm_stream, nullptr);
        llvm_stream.flush();
    }

    void code_generator::generate_bitcode(ostream& stream) {
        generate();
        llvm::raw_os_ostream llvm_stream(stream);
        llvm::legacy::PassManager pass_manager;
        pass_manager.add(llvm::createBitcodeWriterPass(llvm_stream));
        pass_manager.run(mod);
        llvm_stream.flush();
    }

    void code_generator::generate_executable(ostream& stream) {
        generate();

        // set terget triple
        mod.setTargetTriple(llvm::sys::getDefaultTargetTriple());

        // generate bitcode
        ofstream bc_file("/tmp/snowglobe_buffer.bc");
        llvm::raw_os_ostream llvm_stream(bc_file);
        llvm::legacy::PassManager pass_manager;
        pass_manager.add(llvm::createBitcodeWriterPass(llvm_stream));
        pass_manager.run(mod);
        llvm_stream.flush();
        bc_file.close();

        // run clang
        system("clang /tmp/snowglobe_buffer.bc -o /tmp/snowglobe_buffer.bin");

        // get the result
        ifstream bin_file("/tmp/snowglobe_buffer.bin");
        stream << bin_file.rdbuf();
    }

    void code_generator::generate() {
        // declare external functions
        declare_external_functions();

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

        // define global variables and init function
        global_vars.resize(prog.global_vars.size());
        for (size_t i = 0; i < prog.global_vars.size(); i++)
            global_vars[i] = define_global_variable(*prog.global_vars[i]);
        auto init_func = define_init_function();

        // generate function code
        for (size_t i = 0; i < prog.global_funcs.size(); i++) {
            auto& func_name = prog.global_funcs[i]->name;
            if (func_name && builtin_ctors.count(*func_name))
                (this->*builtin_ctors.at(*func_name))(functions[i].value);
            else
                function_code_generator(*this, *prog.global_funcs[i], functions[i]).generate();
        }
        define_internal_main_function(init_func, functions[prog.entry_func].value, functions[prog.cleanup_func].value);

        // verify module well-formedness
        llvm_verify([&](llvm::raw_ostream* stream){ return llvm::verifyModule(mod, stream); });
    }

    void code_generator::llvm_verify(function<bool(llvm::raw_ostream*)> func) {
        string error_msg;
        llvm::raw_string_ostream stream(error_msg);
        if (func(&stream))
            error(diags::code_generator_fail(error_msg, DUMMY_LOCATION));
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

            case prog::constant::GLOBAL_VAR_PTR: {
                auto& global_var = global_vars[GET(constant, GLOBAL_VAR_PTR)];
                auto pointer_type = get_pointer_type(global_var.type, false, false);
                return make_pointer_value(pointer_type, global_var.value, { }, { }, builder);
            }

            default:
                error(diags::not_implemented(DUMMY_LOCATION)); // TODO
        }

        UNREACHABLE;
    }

    code_generator::typed_llvm_value<> code_generator::make_struct_value(prog::global_index struct_index, vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder) {
        auto struct_type = struct_types[struct_index];
        llvm::Value* struct_value = llvm::UndefValue::get(struct_type->get_type());
        for (size_t i = 0; i < fields.size(); i++)
            struct_value = builder.CreateInsertValue(struct_value, fields[i].value, {(unsigned)i});
        return { struct_value, struct_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_enum_variant_value(prog::global_index enum_index, prog::variant_index variant_index, vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder) {
        // make variant
        auto variant_type = variant_types[enum_index][variant_index];
        llvm::Value* variant_value = llvm::UndefValue::get(variant_type->get_type());
        variant_value = builder.CreateInsertValue(variant_value, builder.getInt64(variant_index), {0});
        for (size_t i = 0; i < fields.size(); i++)
            variant_value = builder.CreateInsertValue(variant_value, fields[i].value, {(unsigned)i + 1});

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
            tuple_value = builder.CreateInsertValue(tuple_value, fields[i].value, {(unsigned)i});
        return { tuple_value, tuple_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_array_value(vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder) {
        auto value_type = fields.empty() ? get_never_type() : fields.front().type;
        auto array_type = get_array_type(value_type, fields.size());
        llvm::Value* array_value = llvm::UndefValue::get(array_type->get_type());
        for (size_t i = 0; i < fields.size(); i++)
            array_value = builder.CreateInsertValue(array_value, fields[i].value, {(unsigned)i});
        return { array_value, array_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_empty_optional_value(ll_type* value_type, llvm::IRBuilderBase& builder) {
        auto optional_type = get_optional_type(value_type);
        llvm::Value* value = llvm::UndefValue::get(optional_type->get_type());
        value = builder.CreateInsertValue(value, builder.getFalse(), {0});
        return { value, optional_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_filled_optional_value(typed_llvm_value<> value, llvm::IRBuilderBase& builder) {
        auto optional_type = get_optional_type(value.type);
        llvm::Value* optional_value = llvm::UndefValue::get(optional_type->get_type());
        optional_value = builder.CreateInsertValue(optional_value, builder.getTrue(), {0});
        optional_value = builder.CreateInsertValue(optional_value, value.value, {1});
        return { optional_value, optional_type };
    }

    code_generator::typed_llvm_value<> code_generator::make_pointer_value(ll_type* type, llvm::Value* data_ptr, optional<llvm::Value*> ref_cnts_ptr, optional<llvm::Value*> slice_size, llvm::IRBuilderBase& builder) {
        llvm::Value* value = llvm::UndefValue::get(type->get_type());
        unsigned index = 0;
        value = builder.CreateInsertValue(value, data_ptr, {index++});
        if (ref_cnts_ptr)
            value = builder.CreateInsertValue(value, *ref_cnts_ptr, {index++});
        if (slice_size)
            value = builder.CreateInsertValue(value, *slice_size, {index++});
        return { value, type };
    }

    llvm::Value* code_generator::extract_data_ptr_from_pointer(typed_llvm_value<> pointer, llvm::IRBuilderBase& builder) {
        return builder.CreateExtractValue(pointer.value, {0});
    }

    optional<llvm::Value*> code_generator::extract_ref_cnts_ptr_from_pointer(typed_llvm_value<> pointer, llvm::IRBuilderBase& builder) {
        if (!GET(*pointer.type, POINTER).ref_cnt)
            return { };
        return { builder.CreateExtractValue(pointer.value, {1}) };
    }

    optional<llvm::Value*> code_generator::extract_strong_ref_cnt_ptr_from_pointer(typed_llvm_value<> pointer, llvm::IRBuilderBase& builder) {
        if (!GET(*pointer.type, POINTER).ref_cnt)
            return { };
        auto ref_cnts_ptr = builder.CreateExtractValue(pointer.value, {1});
        return { builder.CreateGEP(llvm::ArrayType::get(builder.getInt64Ty(), 2), ref_cnts_ptr, {0, 0}) };
    }

    optional<llvm::Value*> code_generator::extract_weak_ref_cnt_ptr_from_pointer(typed_llvm_value<> pointer, llvm::IRBuilderBase& builder) {
        if (!GET(*pointer.type, POINTER).ref_cnt)
            return { };
        auto ref_cnts_ptr = builder.CreateExtractValue(pointer.value, {1});
        return { builder.CreateGEP(llvm::ArrayType::get(builder.getInt64Ty(), 2), ref_cnts_ptr, {0, 1}) };
    }

    optional<llvm::Value*> code_generator::extract_slice_len_from_pointer(typed_llvm_value<> pointer, llvm::IRBuilderBase& builder) {
        if (!GET(*pointer.type, POINTER).slice)
            return { };
        unsigned index = 1 + (GET(*pointer.type, POINTER).ref_cnt);
        return { builder.CreateExtractValue(pointer.value, {index}) };
    }

    code_generator::typed_llvm_value<llvm::Function> code_generator::declare_function(const prog::global_func& func) {
        // get function type
        vector<llvm::Type*> arg_types;
        for (auto& param : func.params)
            arg_types.push_back(get_type_from_prog(*param->tp->tp)->get_type());
        auto ret_type = get_type_from_prog(*func.return_tp);
        auto function_type = llvm::FunctionType::get(ret_type->get_type(), arg_types, false);

        // create function
        auto function_name = func.name ? (normal_func_prefix + *func.name) : "";
        return { llvm::Function::Create(function_type, llvm::Function::ExternalLinkage, function_name, mod), ret_type };
    }

    code_generator::typed_llvm_value<> code_generator::define_global_variable(const prog::global_var& var) {
        auto type = get_type_from_prog(*var.tp);
        auto value = llvm::UndefValue::get(type->get_type());
        return { new llvm::GlobalVariable(mod, type->get_type(), false, llvm::GlobalVariable::ExternalLinkage, value, var.name.value_or("")), type };
    }

    llvm::Function* code_generator::define_init_function() {
        auto type = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), { }, false);
        auto func = llvm::Function::Create(type, llvm::Function::PrivateLinkage, internal_func_prefix + "init", mod);
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        for (size_t i = 0; i < prog.global_vars.size(); i++) {
            auto& global_var = *prog.global_vars[i];
            builder.CreateStore(make_constant(*global_var.value, *global_var.tp, builder).value, global_vars[i].value);
        }
        builder.CreateRetVoid();

        llvm_verify([&](llvm::raw_ostream* stream){ return llvm::verifyFunction(*func, stream); });
        return func;
    }

    llvm::Function* code_generator::define_internal_main_function(llvm::Function* init_func, llvm::Function* main_func, llvm::Function* cleanup_func) {
        vector<llvm::Type*> arg_types{ llvm::Type::getInt32Ty(ctx), llvm::PointerType::getUnqual(llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(ctx))) };
        auto type = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), arg_types, false);
        auto func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, "main", mod);
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        builder.CreateCall(init_func->getFunctionType(), init_func);
        builder.CreateCall(main_func->getFunctionType(), main_func);
        builder.CreateCall(cleanup_func->getFunctionType(), cleanup_func);
        builder.CreateRet(builder.getInt32(0));

        llvm_verify([&](llvm::raw_ostream* stream){ return llvm::verifyFunction(*func, stream); });
        return func;
    }


    void function_code_generator::generate() {
        // create entry block
        auto entry_block = llvm::BasicBlock::Create(gen.ctx, "entry", llvm_function);
        llvm::IRBuilder<> builder(entry_block);

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
        process_instr_block(func.instrs, entry_block, terminator_block, nullptr, nullptr);

        // verify corectness
        gen.llvm_verify([&](llvm::raw_ostream* stream){ return llvm::verifyFunction(*llvm_function, stream); });
    }

    llvm::BasicBlock* function_code_generator::process_instr_block(const vector<prog::ptr<prog::instr>>& instrs, llvm::BasicBlock* init_block, llvm::BasicBlock* after_block, llvm::BasicBlock* loop_block, llvm::BasicBlock* after_loop_block) {
        llvm::IRBuilder<> builder(init_block);
        bool terminated = false;

        for (auto& instr : instrs) {
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
                    regs[to_instr.result] = { builder.CreateExtractValue(regs[to_instr.value].value, {0}), gen.get_bool_type() };
                } break;

                case prog::instr::TEST_VARIANT: {
                    auto& tv_instr = *GET(*instr, TEST_VARIANT);
                    auto real_variant_field = builder.CreateExtractValue(regs[tv_instr.value].value, {0});
                    auto result = builder.CreateICmpEQ(real_variant_field, builder.getInt64(tv_instr.variant));
                    regs[tv_instr.result] = { result, gen.get_bool_type() };
                } break;

                case prog::instr::EXTRACT_FIELD: {
                    auto& ef_instr = *GET(*instr, EXTRACT_FIELD);
                    auto& value = regs[ef_instr.value];
                    ll_type* field_type;
                    if (INDEX_EQ(*value.type, STRUCT))
                        field_type = GET(*value.type, STRUCT).fields[ef_instr.field];
                    else if (INDEX_EQ(*value.type, TUPLE))
                        field_type = GET(*value.type, TUPLE).fields[ef_instr.field];
                    else if (INDEX_EQ(*value.type, ARRAY))
                        field_type = GET(*value.type, ARRAY).value;
                    else
                        UNREACHABLE;
                    regs[ef_instr.result] = { builder.CreateExtractValue(value.value, {(unsigned)ef_instr.field}), field_type };
                } break;

                case prog::instr::EXTRACT_OPTIONAL_VALUE: {
                    auto& eov_instr = *GET(*instr, EXTRACT_OPTIONAL_VALUE);
                    auto& value = regs[eov_instr.value];
                    regs[eov_instr.result] = { builder.CreateExtractValue(value.value, {1}), value.type };
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
                    auto field_value = builder.CreateExtractValue(value_casted, {(unsigned)evf_instr.field});
                    regs[evf_instr.result] = { field_value, variant_type.fields[evf_instr.field] };
                } break;

                case prog::instr::CHECK_ARRAY_PTR_INDEX: {
                    auto& ci_instr = *GET(*instr, CHECK_ARRAY_PTR_INDEX);
                    auto& array_size = GET(*GET(*regs[ci_instr.value].type, POINTER).target, ARRAY).size;
                    regs[ci_instr.result] = { builder.CreateICmpULT(regs[ci_instr.index].value, builder.getInt64(array_size)), gen.get_bool_type() };
                } break;

                case prog::instr::CHECK_ARRAY_PTR_INDEX_RANGE: {
                    auto& cir_instr = *GET(*instr, CHECK_ARRAY_PTR_INDEX_RANGE);
                    auto& array_size = GET(*GET(*regs[cir_instr.value].type, POINTER).target, ARRAY).size;
                    auto check_begin = builder.CreateICmpULT(regs[cir_instr.begin].value, builder.getInt64(array_size));
                    auto check_end = builder.CreateICmpULE(regs[cir_instr.end].value, builder.getInt64(array_size));
                    auto check_begin_end = builder.CreateICmpULE(regs[cir_instr.begin].value, regs[cir_instr.end].value);
                    regs[cir_instr.result] = { builder.CreateAnd(check_begin, builder.CreateAnd(check_end, check_begin_end)), gen.get_bool_type() };
                } break;

                case prog::instr::CHECK_SLICE_INDEX: {
                    auto& ci_instr = *GET(*instr, CHECK_SLICE_INDEX);
                    auto slice_len = *gen.extract_slice_len_from_pointer(regs[ci_instr.value], builder);
                    regs[ci_instr.result] = { builder.CreateICmpULT(regs[ci_instr.index].value, slice_len), gen.get_bool_type() };
                } break;

                case prog::instr::CHECK_SLICE_INDEX_RANGE: {
                    auto& cir_instr = *GET(*instr, CHECK_SLICE_INDEX_RANGE);
                    auto slice_size = *gen.extract_slice_len_from_pointer(regs[cir_instr.value], builder);
                    auto check_begin = builder.CreateICmpULT(regs[cir_instr.begin].value, slice_size);
                    auto check_end = builder.CreateICmpULE(regs[cir_instr.end].value, slice_size);
                    auto check_begin_end = builder.CreateICmpULE(regs[cir_instr.begin].value, regs[cir_instr.end].value);
                    regs[cir_instr.result] = { builder.CreateAnd(check_begin, builder.CreateAnd(check_end, check_begin_end)), gen.get_bool_type() };
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
                    gen.error(diags::not_implemented(DUMMY_LOCATION)); // TODO

                case prog::instr::LS: {
                    auto& nbo_instr = *GET(*instr, LS);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpULT(lreg.value, rreg.value), gen.get_bool_type() };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpSLT(lreg.value, rreg.value), gen.get_bool_type() };
                    else
                        regs[nbo_instr.result] = { builder.CreateFCmpULT(lreg.value, rreg.value), gen.get_bool_type() };
                } break;

                case prog::instr::LSEQ: {
                    auto& nbo_instr = *GET(*instr, LSEQ);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpULE(lreg.value, rreg.value), gen.get_bool_type() };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpSLE(lreg.value, rreg.value), gen.get_bool_type() };
                    else
                        regs[nbo_instr.result] = { builder.CreateFCmpULE(lreg.value, rreg.value), gen.get_bool_type() };
                } break;

                case prog::instr::GT: {
                    auto& nbo_instr = *GET(*instr, GT);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpUGT(lreg.value, rreg.value), gen.get_bool_type() };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpSGT(lreg.value, rreg.value), gen.get_bool_type() };
                    else
                        regs[nbo_instr.result] = { builder.CreateFCmpUGT(lreg.value, rreg.value), gen.get_bool_type() };
                } break;

                case prog::instr::GTEQ: {
                    auto& nbo_instr = *GET(*instr, GTEQ);
                    auto& lreg = regs[nbo_instr.left];
                    auto& rreg = regs[nbo_instr.right];
                    if (nbo_instr.kind == prog::numeric_binary_operation_instr::UNSIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpUGE(lreg.value, rreg.value), gen.get_bool_type() };
                    else if (nbo_instr.kind == prog::numeric_binary_operation_instr::SIGNED)
                        regs[nbo_instr.result] = { builder.CreateICmpSGE(lreg.value, rreg.value), gen.get_bool_type() };
                    else
                        regs[nbo_instr.result] = { builder.CreateFCmpUGE(lreg.value, rreg.value), gen.get_bool_type() };
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

                case prog::instr::ALLOC: {
                    auto& a_instr = *GET(*instr, ALLOC);
                    auto& value = regs[a_instr.value];

                    auto heap_addr = gen.make_external_malloc_call(value.type->get_type(), builder.getInt64(1), builder);
                    builder.CreateStore(value.value, heap_addr);

                    auto ptr_type = gen.get_pointer_type(value.type, false, false);
                    regs[a_instr.result] = gen.make_pointer_value(ptr_type, heap_addr, { }, { }, builder);
                } break;

                case prog::instr::ALLOC_SLICE: {
                    auto& as_instr = *GET(*instr, ALLOC_SLICE);
                    auto& value = regs[as_instr.value];

                    auto heap_addr = gen.make_external_malloc_call(value.type->get_type(), regs[as_instr.size].value, builder);
                    auto copy_item_proc = [&](llvm::BasicBlock* insert_block, llvm::BasicBlock* loop_block, llvm::Value* iterator) {
                        llvm::IRBuilder<> inner_builder(insert_block);
                        auto item_ptr = inner_builder.CreateGEP(value.type->get_type(), heap_addr, vector<llvm::Value*>{iterator});
                        inner_builder.CreateStore(value.value, item_ptr);
                        inner_builder.CreateBr(loop_block);
                    };
                    auto continuation_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    make_repeat(regs[as_instr.size], { }, copy_item_proc, builder.GetInsertBlock(), continuation_block);
                    builder.SetInsertPoint(continuation_block);

                    auto ptr_type = gen.get_pointer_type(value.type, false, true);
                    regs[as_instr.result] = gen.make_pointer_value(ptr_type, heap_addr, { }, regs[as_instr.size].value, builder);
                } break;

                case prog::instr::ARRAY_PTR_INTO_SLICE: {
                    auto& pc_instr = *GET(*instr, ARRAY_PTR_INTO_SLICE);

                    auto& old_ptr = regs[pc_instr.value];
                    auto data_ptr = gen.extract_data_ptr_from_pointer(old_ptr, builder);
                    auto ref_cnts_ptr = gen.extract_ref_cnts_ptr_from_pointer(old_ptr, builder);

                    auto& old_ptr_type = GET(*old_ptr.type, POINTER);
                    auto& old_array_type = GET(*old_ptr_type.target, ARRAY);
                    auto data_ptr_casted = builder.CreateBitCast(data_ptr, llvm::PointerType::getUnqual(old_array_type.value->get_type()));
                    auto new_ptr_type = gen.get_pointer_type(old_array_type.value, old_ptr_type.ref_cnt, true);
                    regs[pc_instr.result] = gen.make_pointer_value(new_ptr_type, data_ptr_casted, { ref_cnts_ptr }, { builder.getInt64(old_array_type.size) }, builder);
                } break;

                case prog::instr::GET_SLICE_LENGTH: {
                    auto& gsl_instr = *GET(*instr, GET_SLICE_LENGTH);
                    regs[gsl_instr.result] = { *gen.extract_slice_len_from_pointer(regs[gsl_instr.ptr], builder), gen.get_number_type(prog::number_type::U64) };
                } break;

                case prog::instr::GET_FIELD_PTR: {
                    auto& gfp_instr = *GET(*instr, GET_FIELD_PTR);

                    auto& ptr_type = GET(*regs[gfp_instr.ptr].type, POINTER);
                    ll_type* target_type = INDEX_EQ(*ptr_type.target, STRUCT) ? GET(*ptr_type.target, STRUCT).fields[gfp_instr.field] : GET(*ptr_type.target, TUPLE).fields[gfp_instr.field];

                    auto data_ptr = gen.extract_data_ptr_from_pointer(regs[gfp_instr.ptr], builder);
                    auto field_ptr = builder.CreateGEP(ptr_type.target->get_type(), data_ptr, {builder.getInt64(0), builder.getInt64(gfp_instr.field)});
                    auto new_ptr_type = gen.get_pointer_type(target_type, false, false);
                    regs[gfp_instr.result] = gen.make_pointer_value(new_ptr_type, field_ptr, { }, { }, builder);
                } break;

                case prog::instr::GET_ITEM_PTR: {
                    auto& gip_instr = *GET(*instr, GET_ITEM_PTR);

                    auto& ptr_type = GET(*regs[gip_instr.ptr].type, POINTER);
                    auto data_ptr = gen.extract_data_ptr_from_pointer(regs[gip_instr.ptr], builder);
                    auto index_value = regs[gip_instr.index].value;
                    auto item_ptr = builder.CreateGEP(ptr_type.target->get_type(), data_ptr, ptr_type.slice ? vector<llvm::Value*>{index_value} : vector<llvm::Value*>{builder.getInt64(0), index_value});

                    auto target_type = ptr_type.slice ? ptr_type.target : GET(*ptr_type.target, ARRAY).value;
                    auto new_ptr_type = gen.get_pointer_type(target_type, false, false);
                    regs[gip_instr.result] = gen.make_pointer_value(new_ptr_type, item_ptr, { }, { }, builder);
                } break;

                case prog::instr::GET_ITEM_RANGE_SLICE: {
                    auto& girs_instr = *GET(*instr, GET_ITEM_RANGE_SLICE);

                    auto& ptr_type = GET(*regs[girs_instr.ptr].type, POINTER);
                    auto data_ptr = gen.extract_data_ptr_from_pointer(regs[girs_instr.ptr], builder);
                    auto begin = regs[girs_instr.begin].value;
                    auto end = regs[girs_instr.end].value;
                    auto data_ptr_begin = builder.CreateGEP(ptr_type.target->get_type(), data_ptr, ptr_type.slice ? vector<llvm::Value*>{begin} : vector<llvm::Value*>{builder.getInt64(0), begin});
                    auto slice_len = builder.CreateSub(end, begin);

                    auto target_type = ptr_type.slice ? ptr_type.target : GET(*ptr_type.target, ARRAY).value;
                    auto new_ptr_type = gen.get_pointer_type(target_type, false, true);
                    regs[girs_instr.result] = gen.make_pointer_value(new_ptr_type, data_ptr_begin, { }, { slice_len }, builder);
                } break;

                case prog::instr::ALLOC_REF_COUNTER: {
                    auto& pc_instr = *GET(*instr, ALLOC_REF_COUNTER);

                    auto array_type = llvm::ArrayType::get(builder.getInt64Ty(), 2);
                    auto ref_cnts_addr = gen.make_external_malloc_call(array_type, builder.getInt64(1), builder);
                    builder.CreateStore(llvm::ConstantArray::get(array_type, {builder.getInt64(1), builder.getInt64(0)}), ref_cnts_addr);

                    auto& old_ptr = regs[pc_instr.value];
                    auto data_ptr = gen.extract_data_ptr_from_pointer(old_ptr, builder);
                    auto slice_len = gen.extract_slice_len_from_pointer(old_ptr, builder);

                    auto& old_ptr_type = GET(*old_ptr.type, POINTER);
                    auto new_ptr_type = gen.get_pointer_type(old_ptr_type.target, true, old_ptr_type.slice);
                    regs[pc_instr.result] = gen.make_pointer_value(new_ptr_type, data_ptr, { ref_cnts_addr }, slice_len, builder);
                } break;

                case prog::instr::FORGET_REF_COUNTER: {
                    auto& pc_instr = *GET(*instr, FORGET_REF_COUNTER);

                    auto& old_ptr = regs[pc_instr.value];
                    auto data_ptr = gen.extract_data_ptr_from_pointer(old_ptr, builder);
                    auto slice_len = gen.extract_slice_len_from_pointer(old_ptr, builder);

                    auto& old_ptr_type = GET(*old_ptr.type, POINTER);
                    auto new_ptr_type = gen.get_pointer_type(old_ptr_type.target, false, old_ptr_type.slice);
                    regs[pc_instr.result] = gen.make_pointer_value(new_ptr_type, data_ptr, { }, slice_len, builder);
                } break;

                case prog::instr::PTR_READ: {
                    auto& pr_instr = *GET(*instr, PTR_READ);
                    auto data_ptr = gen.extract_data_ptr_from_pointer(regs[pr_instr.ptr], builder);
                    auto target_type = GET(*regs[pr_instr.ptr].type, POINTER).target;
                    regs[pr_instr.result] = { builder.CreateLoad(target_type->get_type(), data_ptr), target_type };
                } break;

                case prog::instr::PTR_WRITE: {
                    auto& pw_instr = *GET(*instr, PTR_WRITE);
                    auto data_ptr = gen.extract_data_ptr_from_pointer(regs[pw_instr.ptr], builder);
                    builder.CreateStore(regs[pw_instr.value].value, data_ptr);
                } break;

                case prog::instr::INCR_REF_COUNT: {
                    auto& reg = GET(*instr, INCR_REF_COUNT);
                    auto ref_cnt_ptr = *gen.extract_strong_ref_cnt_ptr_from_pointer(regs[reg], builder);
                    auto ref_cnt_old = builder.CreateLoad(builder.getInt64Ty(), ref_cnt_ptr);
                    auto ref_cnt_new = builder.CreateAdd(ref_cnt_old, builder.getInt64(1));
                    builder.CreateStore(ref_cnt_new, ref_cnt_ptr);
                } break;

                case prog::instr::INCR_WEAK_REF_COUNT: {
                    auto& reg = GET(*instr, INCR_WEAK_REF_COUNT);
                    auto ref_cnt_ptr = *gen.extract_weak_ref_cnt_ptr_from_pointer(regs[reg], builder);
                    auto ref_cnt_old = builder.CreateLoad(builder.getInt64Ty(), ref_cnt_ptr);
                    auto ref_cnt_new = builder.CreateAdd(ref_cnt_old, builder.getInt64(1));
                    builder.CreateStore(ref_cnt_new, ref_cnt_ptr);
                } break;

                case prog::instr::DECR_REF_COUNT: {
                    auto& reg = GET(*instr, DECR_REF_COUNT);
                    auto ref_cnt_ptr = *gen.extract_strong_ref_cnt_ptr_from_pointer(regs[reg], builder);
                    auto ref_cnt_old = builder.CreateLoad(builder.getInt64Ty(), ref_cnt_ptr);
                    auto ref_cnt_new = builder.CreateSub(ref_cnt_old, builder.getInt64(1));
                    builder.CreateStore(ref_cnt_new, ref_cnt_ptr);
                } break;

                case prog::instr::DECR_WEAK_REF_COUNT: {
                    auto& reg = GET(*instr, DECR_WEAK_REF_COUNT);
                    auto ref_cnt_ptr = *gen.extract_weak_ref_cnt_ptr_from_pointer(regs[reg], builder);
                    auto ref_cnt_old = builder.CreateLoad(builder.getInt64Ty(), ref_cnt_ptr);
                    auto ref_cnt_new = builder.CreateSub(ref_cnt_old, builder.getInt64(1));
                    builder.CreateStore(ref_cnt_new, ref_cnt_ptr);
                } break;

                case prog::instr::TEST_REF_COUNT: {
                    auto& trc_instr = *GET(*instr, TEST_REF_COUNT);
                    auto ref_cnt_ptr = *gen.extract_strong_ref_cnt_ptr_from_pointer(regs[trc_instr.ptr], builder);
                    auto ref_cnt = builder.CreateLoad(builder.getInt64Ty(), ref_cnt_ptr);
                    regs[trc_instr.result] = { builder.CreateICmpNE(ref_cnt, builder.getInt64(0)), gen.get_bool_type() };
                } break;

                case prog::instr::TEST_ANY_REF_COUNT: {
                    auto& trc_instr = *GET(*instr, TEST_ANY_REF_COUNT);
                    auto strong_ref_cnt_ptr = *gen.extract_strong_ref_cnt_ptr_from_pointer(regs[trc_instr.ptr], builder);
                    auto weak_ref_cnt_ptr = *gen.extract_weak_ref_cnt_ptr_from_pointer(regs[trc_instr.ptr], builder);
                    auto strong_ref_cnt = builder.CreateLoad(builder.getInt64Ty(), strong_ref_cnt_ptr);
                    auto weak_ref_cnt = builder.CreateLoad(builder.getInt64Ty(), weak_ref_cnt_ptr);
                    auto zero = builder.getInt64(0);
                    auto result = builder.CreateAnd(builder.CreateICmpNE(strong_ref_cnt, zero), builder.CreateICmpNE(weak_ref_cnt, zero));
                    regs[trc_instr.result] = { result, gen.get_bool_type() };
                } break;

                case prog::instr::DELETE: {
                    auto reg = GET(*instr, DELETE);
                    auto data_ptr = gen.extract_data_ptr_from_pointer(regs[reg], builder);
                    gen.make_external_free_call(data_ptr, builder);
                } break;

                case prog::instr::DELETE_REF_COUNTER: {
                    auto reg = GET(*instr, DELETE);
                    auto ref_cnt_ptr = *gen.extract_ref_cnts_ptr_from_pointer(regs[reg], builder);
                    gen.make_external_free_call(ref_cnt_ptr, builder);
                } break;

                case prog::instr::BRANCH:
                case prog::instr::VALUE_BRANCH: {
                    auto& b_instr = INDEX_EQ(*instr, BRANCH) ? *GET(*instr, BRANCH) : *GET(*instr, VALUE_BRANCH);
                    auto continuation_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);

                    auto true_init_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    auto true_return_block = process_instr_block(b_instr.true_block->instrs, true_init_block, continuation_block, loop_block, after_loop_block);
                    auto false_init_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    auto false_return_block = process_instr_block(b_instr.false_block->instrs, false_init_block, continuation_block, loop_block, after_loop_block);

                    builder.CreateCondBr(regs[b_instr.cond].value, true_init_block, false_init_block);
                    builder.SetInsertPoint(continuation_block);

                    if (INDEX_EQ(*instr, VALUE_BRANCH)) {
                        auto& vb_instr = *GET(*instr, VALUE_BRANCH);
                        auto& type = regs[vb_instr.true_value].type ? regs[vb_instr.true_value].type : regs[vb_instr.false_value].type;
                        if (type) {
                            auto phi_node = llvm::PHINode::Create(type->get_type(), 2, "", continuation_block);
                            phi_node->addIncoming(regs[vb_instr.true_value].value ? regs[vb_instr.true_value].value : llvm::UndefValue::get(type->get_type()), true_return_block);
                            phi_node->addIncoming(regs[vb_instr.false_value].value ? regs[vb_instr.false_value].value : llvm::UndefValue::get(type->get_type()), false_return_block);
                            regs[vb_instr.result] = { phi_node, type };
                        }
                        else
                            terminated = true;
                    }
                } break;

                case prog::instr::LOOP: {
                    auto& l_instr = *GET(*instr, LOOP);
                    auto loop_body_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    auto continuation_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    process_instr_block(l_instr.instrs, loop_body_block, loop_body_block, loop_body_block, continuation_block);

                    builder.CreateBr(loop_body_block);
                    builder.SetInsertPoint(continuation_block);
                } break;

                case prog::instr::REPEAT: {
                    auto& r_instr = *GET(*instr, REPEAT);
                    auto continuation_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
                    auto body_generator = [&](llvm::BasicBlock* insert_block, llvm::BasicBlock* loop_block, llvm::Value*) {
                        process_instr_block(r_instr.block->instrs, insert_block, loop_block, loop_block, continuation_block);
                    };
                    make_repeat(regs[r_instr.count], r_instr.index, body_generator, builder.GetInsertBlock(), continuation_block);
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

                case prog::instr::ABORT: {
                    gen.make_external_printf_call("ERROR: Runtime error\n", { }, builder);
                    gen.make_external_exit_call(builder.getInt32(1), builder);
                } break;

                default:
                    gen.error(diags::not_implemented(DUMMY_LOCATION)); // TODO
            }
            if (terminated)
                break;
        }

        if (builder.GetInsertBlock()->getTerminator()) 
            builder.SetInsertPoint(llvm::BasicBlock::Create(gen.ctx, "", llvm_function));
        builder.CreateBr(after_block);
        return builder.GetInsertBlock();
    }

    void function_code_generator::make_repeat(code_generator::typed_llvm_value<> count, optional<prog::reg_index> index, function<void(llvm::BasicBlock*,llvm::BasicBlock*,llvm::Value*)> loop_body, llvm::BasicBlock* init_block, llvm::BasicBlock* after_block) {
        llvm::IRBuilder<> builder(init_block);
        auto counter_var = builder.CreateAlloca(count.type->get_type());
        builder.CreateStore(llvm::ConstantInt::get(count.type->get_type(), 0), counter_var);

        auto outer_loop_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
        auto inner_loop_block = llvm::BasicBlock::Create(gen.ctx, "", llvm_function);
        builder.CreateBr(outer_loop_block);
        builder.SetInsertPoint(outer_loop_block);

        auto old_cnt_val = builder.CreateLoad(count.type->get_type(), counter_var);
        if (index)
            regs[*index] = { old_cnt_val, count.type };
        auto break_cond = builder.CreateICmpEQ(old_cnt_val, count.value);
        builder.CreateCondBr(break_cond, after_block, inner_loop_block);

        builder.SetInsertPoint(inner_loop_block);
        auto new_cnt_val = builder.CreateAdd(old_cnt_val, llvm::ConstantInt::get(count.type->get_type(), 1));
        builder.CreateStore(new_cnt_val, counter_var);
        loop_body(inner_loop_block, outer_loop_block, old_cnt_val);
    }
}
