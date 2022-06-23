#include "codegen.hpp"

namespace sg {
    enum {
        EXIT,
        MALLOC,
        FREE,
        PRINTF,
        NUMBER_OF_FUNCS // this must be the last one
    };

    void code_generator::declare_external_functions() {
        external_functions.resize(NUMBER_OF_FUNCS);

        // void exit(int status)
        {
            auto type = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), {llvm::Type::getInt32Ty(ctx)}, false);
            auto func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, "exit", mod);
            external_functions[EXIT] = func;
        }

        // void* malloc(size_t size)
        {
            auto type = llvm::FunctionType::get(llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(ctx)), {llvm::Type::getInt64Ty(ctx)}, false);
            auto func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, "malloc", mod);
            external_functions[MALLOC] = func;
        }

        // void free(void* ptr)
        {
            auto type = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), {llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(ctx))}, false);
            auto func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, "free", mod);
            external_functions[FREE] = func;
        }

        // int printf(char* format, ...)
        {
            auto type = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), {llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(ctx))}, true);
            auto func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, "printf", mod);
            external_functions[PRINTF] = func;
        }
    }

    code_generator::typed_llvm_value<> code_generator::make_external_exit_call(llvm::Value* status, llvm::IRBuilderBase& builder) {
        builder.CreateCall(external_functions[EXIT]->getFunctionType(), external_functions[EXIT], {status});
        return { builder.getInt1(false), get_never_type() };
    }

    llvm::Value* code_generator::make_external_malloc_call(llvm::Type* target_type, llvm::Value* size, llvm::IRBuilderBase& builder) {
        auto size_i64 = builder.CreateZExtOrTrunc(size, builder.getInt64Ty());
        auto bytes = builder.CreateMul(size_i64, llvm::ConstantExpr::getSizeOf(target_type));
        auto heap_ptr = builder.CreateCall(external_functions[MALLOC]->getFunctionType(), external_functions[MALLOC], {bytes});
        return builder.CreateBitCast(heap_ptr, llvm::PointerType::getUnqual(target_type));
    }

    void code_generator::make_external_free_call(llvm::Value* heap_ptr, llvm::IRBuilderBase& builder) {
        auto heap_ptr_casted = builder.CreateBitCast(heap_ptr, llvm::PointerType::getUnqual(builder.getInt8Ty()));
        builder.CreateCall(external_functions[FREE]->getFunctionType(), external_functions[FREE], {heap_ptr_casted});
    }

    code_generator::typed_llvm_value<> code_generator::make_external_printf_call(string format, vector<llvm::Value*> args, llvm::IRBuilderBase& builder) {
        // prepare character array
        vector<llvm::Constant*> array(format.length() + 1);
        for (size_t i = 0; i < format.length(); i++)
            array[i] = builder.getInt8(format[i]);
        array[format.length()] = builder.getInt8(0);
        auto array_type = llvm::ArrayType::get(builder.getInt8Ty(), array.size());
        auto llvm_array = llvm::ConstantArray::get(array_type, array);

        // copy array to the stack
        auto llvm_array_on_stack_ptr = builder.CreateAlloca(array_type);
        builder.CreateStore(llvm_array, llvm_array_on_stack_ptr);
        auto llvm_array_on_stack_ptr_casted = builder.CreateBitCast(llvm_array_on_stack_ptr, llvm::PointerType::getUnqual(builder.getInt8Ty()));

        // call printf
        args.insert(args.begin(), llvm_array_on_stack_ptr_casted);
        return { builder.CreateCall(external_functions[PRINTF]->getFunctionType(), external_functions[PRINTF], args), get_number_type(prog::number_type::I32) };

    }
}
