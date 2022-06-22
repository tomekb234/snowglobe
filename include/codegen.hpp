#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "program.hpp"
#include "diagcol.hpp"

#include <variant>
#include <vector>
#include <set>

#include <llvm/IR/Module.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>

namespace sg {
    using std::ostream;
    using std::string;
    using std::unordered_map;
    using std::make_unique;
    using std::function;
    using std::vector;
    using std::variant;
    using std::set;

    struct ll_number_type;
    struct ll_struct_type;
    struct ll_enum_type;
    struct ll_enum_variant_type;
    struct ll_tuple_type;
    struct ll_array_type;
    struct ll_optional_type;
    struct ll_type;

    struct ll_number_type {
        enum kind_t {
            I1,
            I8,
            I16,
            I32,
            I64,
            FLOAT,
            DOUBLE
        } kind;
        llvm::Type* tp;
    };

    struct ll_struct_type {
        prog::global_index index;
        vector<ll_type*> fields;
        llvm::StructType* tp;
    };

    struct ll_enum_type {
        prog::global_index index;
        ll_array_type* placeholder;
        llvm::StructType* tp;
    };

    struct ll_enum_variant_type {
        prog::global_index enum_index;
        prog::variant_index variant_index;
        vector<ll_type*> fields;
        ll_array_type* placeholder;
        llvm::StructType* tp;
    };

    struct ll_tuple_type {
        vector<ll_type*> fields;
        llvm::StructType* tp;
    };

    struct ll_array_type {
        ll_type* value;
        size_t size;
        llvm::ArrayType* tp;
    };

    struct ll_optional_type {
        ll_type* value;
        llvm::StructType* tp;
    };

    struct ll_type {
        enum {
            NUMBER,
            STRUCT,
            ENUM,
            ENUM_VARIANT,
            TUPLE,
            ARRAY,
            OPTIONAL
        };

        variant<
            ll_number_type, 
            ll_struct_type,
            ll_enum_type,
            ll_enum_variant_type,
            ll_tuple_type,
            ll_array_type,
            ll_optional_type
        > value;

        llvm::Type* get_type() const;
        string get_name() const;
        int compare(const ll_type& other) const;
        bool operator<(const ll_type& other) const;
    };

    struct ll_type_wrapper {
        unique_ptr<ll_type> type;
        bool operator<(const ll_type_wrapper& other) const {
            return *type < *other.type;
        }
    };



    class code_generator {
        friend class function_code_generator;

        // helper structs
        template<typename T = llvm::Value>
        struct typed_llvm_value {
            T* value;
            ll_type* type;
        };
        struct generator_error { };

        // internal variables
        const prog::program& prog;
        diagnostic_collector& diags;
        llvm::LLVMContext ctx;
        llvm::Module mod;

        // global objects
        vector<typed_llvm_value<llvm::Function>> functions; // "type" of function is its return type
        vector<typed_llvm_value<>> global_vars;
        vector<ll_type*> struct_types;
        vector<ll_type*> enum_types;
        vector<vector<ll_type*>> variant_types;
        set<ll_type_wrapper> type_set;

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
        ll_type* get_never_type();
        ll_type* get_unit_type();
        ll_type* get_byte_type();
        ll_type* get_number_type(prog::number_type::type_t number_type);
        ll_type* get_tuple_type(const vector<ll_type*>& field_types);
        ll_type* get_array_type(ll_type* value_type, size_t size);
        ll_type* get_optional_type(ll_type* value_type);

        ll_type* get_type_from_prog(const prog::type& type);

        ll_type* declare_struct_type(prog::global_index index);
        ll_type* declare_enum_type(prog::global_index index);
        ll_type* declare_enum_variant_type(prog::global_index enum_index, prog::variant_index variant_index);
        void define_struct_type(const prog::struct_type& prog_struct_type, ll_struct_type* llvm_struct_type);
        void define_enum_type_with_variants(const prog::enum_type& prog_enum_type, ll_enum_type* llvm_enum_type, vector<ll_enum_variant_type*> llvm_variant_types);

        // values
        typed_llvm_value<> make_constant(const prog::constant& constant, const prog::type& type, llvm::IRBuilderBase& builder);
        typed_llvm_value<> make_struct_value(prog::global_index struct_index, vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder);
        typed_llvm_value<> make_enum_variant_value(prog::global_index enum_index, prog::variant_index variant_index, vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder);
        typed_llvm_value<> make_tuple_value(vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder);
        typed_llvm_value<> make_array_value(vector<typed_llvm_value<>> fields, llvm::IRBuilderBase& builder);
        typed_llvm_value<> make_empty_optional_value(ll_type* value_type, llvm::IRBuilderBase& builder);
        typed_llvm_value<> make_filled_optional_value(typed_llvm_value<> value, llvm::IRBuilderBase& builder);

        // top-level declarations
        typed_llvm_value<llvm::Function> declare_function(const prog::global_func& func);
        typed_llvm_value<> define_global_variable(const prog::global_var& var);
        llvm::Function* define_init_function();
    };

    class function_code_generator {
        code_generator& gen;
        const prog::global_func& func;
        llvm::Function* llvm_function;
        ll_type* return_type;

        unordered_map<prog::reg_index, code_generator::typed_llvm_value<>> regs;
        unordered_map<prog::var_index, code_generator::typed_llvm_value<>> vars;

        public:

        function_code_generator(code_generator& gen, const prog::global_func& func, code_generator::typed_llvm_value<llvm::Function> llvm_function) : gen(gen), func(func), llvm_function(llvm_function.value), return_type(llvm_function.type) {}

        void generate(optional<llvm::Function*> init_func);

        private:

        llvm::BasicBlock* process_instr_block(const prog::instr_block& block, llvm::BasicBlock* init_block, llvm::BasicBlock* after_block, llvm::BasicBlock* loop_block, llvm::BasicBlock* after_loop_block);
    };
}

#endif
