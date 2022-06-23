#include "codegen.hpp"

namespace sg {
    void code_generator::define_builtin_exit_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        make_external_exit_call(func->getArg(0), builder);
        builder.CreateRet(builder.getFalse());
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
        make_external_printf_call("%f", { func->getArg(0) }, builder);
        builder.CreateRet(builder.getFalse());
    }

    const unordered_map<string, void(code_generator::*)(llvm::Function*)> code_generator::builtin_ctors = {
        {"print", &code_generator::define_builtin_print_func },
        {"print_word", &code_generator::define_builtin_print_word_func },
        {"print_line", &code_generator::define_builtin_print_line_func },
        {"print_int", &code_generator::define_builtin_print_int_func },
        {"print_uint", &code_generator::define_builtin_print_uint_func },
        {"print_float", &code_generator::define_builtin_print_float_func },
        {"exit", &code_generator::define_builtin_exit_func }
    };
}
