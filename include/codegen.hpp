#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "program.hpp"
#include "diagcol.hpp"

#include <llvm/IR/Module.h>
#include <llvm/IR/Instructions.h>

namespace sg {
    using std::ostream;
    using std::string;
    using std::unordered_map;
    using std::make_unique;
    using std::function;
    using std::vector;

    class code_generator {
        friend class function_code_generator;

        // helper structs
        template<typename T = llvm::Type>
        struct named_llvm_type {
            string name;
            T* type;
            named_llvm_type<llvm::Type> to_type() const {
                return named_llvm_type<llvm::Type>{ name, type };
            }
        };
        template<typename VT = llvm::Value>
        struct typed_llvm_value {
            VT* value;
            named_llvm_type<llvm::Type> type;
        };
        struct generator_error { };

        // internal variables
        const prog::program& prog;
        diagnostic_collector& diags;
        llvm::LLVMContext ctx;
        llvm::Module mod;

        vector<typed_llvm_value<llvm::Function>> functions; // "type" of function is its return type
        vector<typed_llvm_value<>> global_vars;
        vector<llvm::StructType*> struct_types;
        unordered_map<string, llvm::Type*> types;

        public:

        code_generator(const prog::program& prog, diagnostic_collector& diags, string name) : prog(prog), diags(diags), ctx(), mod(name, ctx) {}
        bool generate();
        string get_code();

        private:

        // diagnostic helpers
        void llvm_verify(function<bool(llvm::raw_ostream*)> func);
        template<typename T>
        [[noreturn]] void error(T&& diag) {
            diag.loc = { };
            diags.add(make_unique<T>(move(diag)));
            throw generator_error();
        }

        // LLVM types handling
        named_llvm_type<> get_type_from_prog(const prog::type& type);
        named_llvm_type<> get_never_type();
        named_llvm_type<> get_unit_type();
        named_llvm_type<> get_number_type(prog::number_type::type_t number_type);
        named_llvm_type<llvm::StructType> get_struct_type(prog::global_index struct_index);
        named_llvm_type<llvm::StructType> get_tuple_type(const vector<named_llvm_type<llvm::Type>>& field_types);
        named_llvm_type<llvm::ArrayType> get_array_type(const named_llvm_type<llvm::Type>& value_type, size_t size);

        // constants
        typed_llvm_value<llvm::Constant> make_constant(const prog::constant& constant);

        // top-level declarations
        typed_llvm_value<llvm::Function> declare_function(const prog::global_func& func);
        typed_llvm_value<> define_global_variable(const prog::global_var& var);
        llvm::StructType* declare_struct_type(const prog::struct_type& struct_type);
        void define_struct_type(const prog::struct_type& struct_type, llvm::StructType* llvm_struct_type);
    };

    class function_code_generator {
        code_generator& gen;
        const prog::global_func& func;
        llvm::Function* llvm_function;
        code_generator::named_llvm_type<> return_type;

        unordered_map<prog::reg_index, code_generator::typed_llvm_value<>> regs;
        unordered_map<prog::var_index, code_generator::typed_llvm_value<>> vars;

        public:

        function_code_generator(code_generator& gen, const prog::global_func& func, code_generator::typed_llvm_value<llvm::Function> llvm_function) : gen(gen), func(func), llvm_function(llvm_function.value), return_type(llvm_function.type) {}

        void generate();

        private:

        llvm::BasicBlock* process_instr_block(const prog::instr_block& block, llvm::BasicBlock* init_block, llvm::BasicBlock* after_block, llvm::BasicBlock* loop_block, llvm::BasicBlock* after_loop_block);
    };
}

#endif
