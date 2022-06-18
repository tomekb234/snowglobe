#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "program.hpp"
#include "diagcol.hpp"

#include <llvm/IR/Module.h>

namespace sg {
    using std::ostream;
    using std::string;
    using std::unordered_map;
    using std::make_unique;
    using std::function;
    using std::vector;

    class code_generator {
        const prog::program& prog;
        diagnostic_collector& diags;
        llvm::LLVMContext ctx;
        llvm::Module mod;

        vector<llvm::Function*> functions;

        friend class function_code_generator;

        public:

        code_generator(const prog::program& prog, diagnostic_collector& diags, string name) : prog(prog), diags(diags), ctx(), mod(name, ctx) {}

        bool generate();
        string get_code();

        private:

        // diagnostic helpers
        struct generator_error { };

        template<typename T>
        [[noreturn]] void error(T&& diag) {
            diag.loc = { };
            diags.add(make_unique<T>(move(diag)));
            throw generator_error();
        }

        void llvm_verify(function<bool(llvm::raw_ostream*)> func);

        // LLVM types handling
        llvm::Type* get_llvm_type(const prog::type& tp);
        llvm::Type* get_llvm_number_type(const prog::number_type& ntp);

        // constants
        llvm::Value* make_constant(const prog::constant& constant);

        // top-level declarations
        llvm::Function* declare_function(const prog::global_func& func);
    };

    class function_code_generator {
        code_generator& gen;
        const prog::global_func& func;
        llvm::Function* llvm_function;

        unordered_map<prog::reg_index, llvm::Value*> regs;
        unordered_map<prog::var_index, llvm::Value*> vars;

        public:

        function_code_generator(code_generator& gen, const prog::global_func& func, llvm::Function* llvm_function) : gen(gen), func(func), llvm_function(llvm_function) {}

        void generate();

        private:

        void process_instr_block(const prog::instr_block& block, llvm::BasicBlock* init_basic_block, llvm::BasicBlock* next_basic_block);
    };
}

#endif
