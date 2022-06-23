#include "codegen.hpp"

namespace sg {
    void code_generator::define_builtin_exit_func(llvm::Function* func) {
        auto block = llvm::BasicBlock::Create(ctx, "entry", func);
        llvm::IRBuilder<> builder(block);

        make_external_exit_call(func->getArg(0), builder);
        builder.CreateRet(builder.getFalse());
    }

    const unordered_map<string, void(code_generator::*)(llvm::Function*)> code_generator::builtin_ctors = {
        {"exit", &code_generator::define_builtin_exit_func }
    };
}
