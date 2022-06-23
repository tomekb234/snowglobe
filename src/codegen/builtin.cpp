#include "codegen.hpp"

namespace sg {
    void code_generator::define_builtin_read_int_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        auto buffer = builder.CreateAlloca(builder.getInt64Ty());
        make_external_scanf_call("%lld", { buffer }, builder);
        builder.CreateRet(builder.CreateLoad(builder.getInt64Ty(), buffer));
    }
    
    void code_generator::define_builtin_read_uint_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        auto buffer = builder.CreateAlloca(builder.getInt64Ty());
        make_external_scanf_call("%llu", { buffer }, builder);
        builder.CreateRet(builder.CreateLoad(builder.getInt64Ty(), buffer));
    }

    void code_generator::define_builtin_read_float_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        auto buffer = builder.CreateAlloca(builder.getDoubleTy());
        make_external_scanf_call("%lf", { buffer }, builder);
        builder.CreateRet(builder.CreateLoad(builder.getDoubleTy(), buffer));
    }

    void code_generator::define_builtin_print_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        auto arg = typed_llvm_value<> { func->getArg(0), get_pointer_type(get_number_type(prog::number_type::U8), false, { }, true) };
        auto string_ptr = extract_data_ptr_from_pointer(arg, builder);
        auto string_len = extract_slice_len_from_pointer(arg, builder);
        auto string_len_trunc = builder.CreateTrunc(*string_len, builder.getInt32Ty());
        make_external_printf_call("%.*s", { string_len_trunc, string_ptr }, builder);
        builder.CreateRet(builder.getFalse());
    }

    void code_generator::define_builtin_print_word_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        auto arg = typed_llvm_value<> { func->getArg(0), get_pointer_type(get_number_type(prog::number_type::U8), false, { }, true) };
        auto string_ptr = extract_data_ptr_from_pointer(arg, builder);
        auto string_len = extract_slice_len_from_pointer(arg, builder);
        auto string_len_trunc = builder.CreateTrunc(*string_len, builder.getInt32Ty());
        make_external_printf_call("%.*s ", { string_len_trunc, string_ptr }, builder);
        builder.CreateRet(builder.getFalse());
    }

    void code_generator::define_builtin_print_line_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        auto arg = typed_llvm_value<> { func->getArg(0), get_pointer_type(get_number_type(prog::number_type::U8), false, { }, true) };
        auto string_ptr = extract_data_ptr_from_pointer(arg, builder);
        auto string_len = extract_slice_len_from_pointer(arg, builder);
        auto string_len_trunc = builder.CreateTrunc(*string_len, builder.getInt32Ty());
        make_external_printf_call("%.*s\n", { string_len_trunc, string_ptr }, builder);
        builder.CreateRet(builder.getFalse());
    }

    void code_generator::define_builtin_print_int_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        make_external_printf_call("%lld", { func->getArg(0) }, builder);
        builder.CreateRet(builder.getFalse());
    }

    void code_generator::define_builtin_print_uint_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        make_external_printf_call("%llu", { func->getArg(0) }, builder);
        builder.CreateRet(builder.getFalse());
    }

    void code_generator::define_builtin_print_float_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        make_external_printf_call("%lf", { func->getArg(0) }, builder);
        builder.CreateRet(builder.getFalse());
    }

    void code_generator::define_builtin_exit_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        make_external_exit_call(func->getArg(0), builder);
        builder.CreateRet(builder.getFalse());
    }

    void code_generator::define_builtin_error_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        auto arg = typed_llvm_value<> { func->getArg(0), get_pointer_type(get_number_type(prog::number_type::U8), false, { }, true) };
        auto string_ptr = extract_data_ptr_from_pointer(arg, builder);
        auto string_len = extract_slice_len_from_pointer(arg, builder);
        auto string_len_trunc = builder.CreateTrunc(*string_len, builder.getInt32Ty());
        make_external_printf_call("ERROR: %.*s\n", { string_len_trunc, string_ptr }, builder);

        make_external_exit_call(builder.getInt32(1), builder);
        builder.CreateRet(builder.getFalse());
    }

    void code_generator::define_builtin_unreachable_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        make_external_printf_call("ERROR: The program has reached an unreachable state\n", { }, builder);
        make_external_exit_call(builder.getInt32(1), builder);
        builder.CreateRet(builder.getFalse());
    }

    const unordered_map<string, void(code_generator::*)(llvm::Function*)> code_generator::builtin_ctors = {
        { "read_int", &code_generator::define_builtin_read_int_func },
        { "read_uint", &code_generator::define_builtin_read_uint_func },
        { "read_float", &code_generator::define_builtin_read_float_func },

        { "print", &code_generator::define_builtin_print_func },
        { "print_word", &code_generator::define_builtin_print_word_func },
        { "print_line", &code_generator::define_builtin_print_line_func },
        { "print_int", &code_generator::define_builtin_print_int_func },
        { "print_uint", &code_generator::define_builtin_print_uint_func },
        { "print_float", &code_generator::define_builtin_print_float_func },

        { "exit", &code_generator::define_builtin_exit_func },
        { "error", &code_generator::define_builtin_error_func },
        { "unreachable", &code_generator::define_builtin_unreachable_func }
    };
}
