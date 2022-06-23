#ifndef FUNCTIONS_HPP
#define FUNCTIONS_HPP

#include "compiler/compiler.hpp"
#include <optional>
#include <functional>
#include <variant>

namespace sg {
    using std::optional;
    using std::function;
    using std::monostate;
    using std::variant;
    using std::pair;

    struct lvalue {
        enum {
            IGNORED,
            VAR,
            GLOBAL_VAR,
            TUPLE,
            ARRAY,
            STRUCT,
            DEREFERENCE
        };

        variant<
            monostate, // IGNORE
            prog::var_index, // VAR
            prog::global_index, // GLOBAL_VAR
            vector<ptr<lvalue>>, // TUPLE
            vector<ptr<lvalue>>, // ARRAY
            pair<prog::global_index, vector<ptr<lvalue>>>, // STRUCT
            pair<prog::reg_index, prog::type> // DEREFERENCE
        > value;
    };

    class function_compiler : compiler_base {
        typedef unsigned char var_state;
        typedef size_t frame_index;

        struct variable {
            optional<string> name;
            prog::type_local type;
            var_state state;
            size_t outside_loop;
            size_t outside_confinement;
        };

        struct frame {
            vector<prog::instr> instrs;
            vector<prog::var_index> vars;
            vector<string> var_names;
            bool loop;
            vector<function<void()>> cleanup_actions;
        };

        static constexpr var_state VAR_UNINITIALIZED = 1 << 0;
        static constexpr var_state VAR_INITIALIZED = 1 << 1;
        static constexpr var_state VAR_MOVED_OUT = 1 << 2;

        compiler& clr;
        prog::global_func& func;
        prog::reg_index reg_counter = 0;
        vector<variable> vars;
        vector<frame> frames;
        unordered_map<string, vector<prog::var_index>> var_names;
        bool returned = false;

        friend class conversion_compiler;
        friend class copy_compiler;
        friend class deletion_compiler;
        friend class assignment_compiler;
        friend class statement_compiler;
        friend class expression_compiler;

        public:

        function_compiler(compiler& clr, prog::global_func& func) : compiler_base(clr), clr(clr), func(func) { }

        void init();
        void commit();

        void compile(const ast::func_def& ast);

        void make_func_wrapper(prog::global_index func_index);
        void make_struct_destructor(prog::global_index struct_index);
        void make_enum_destructor(prog::global_index enum_index);
        void make_cleanup_func();

        private:

        prog::reg_index new_reg();
        prog::reg_index new_unit_reg();
        void add_instr(prog::instr&& instr);
        void add_instrs(prog::instr_block&& block);

        void push_frame();
        void push_loop_frame();
        void push_confining_frame();
        prog::instr_block pop_frame();
        prog::instr_block pop_loop_frame();
        prog::instr_block pop_confining_frame();
        void defer_cleanup_action(function<void()> cleanup_action);
        void add_frame_cleanup(frame_index rev_index, location loc);

        prog::var_index add_var(prog::type_local&& type);
        prog::var_index add_var(string name, prog::type_local&& type);
        optional<prog::var_index> try_get_var(string name);
        prog::var_index get_var(string name, location loc);
        void move_out_var(prog::var_index index, location loc);
        vector<var_state> backup_var_states();
        void restore_var_states(const vector<var_state>& states);
        void merge_var_states(const vector<var_state>& states);

        pair<prog::reg_index, prog::type_local> add_var_read(prog::var_index var_index, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> add_var_confinement(prog::var_index var_index, location loc);
        void add_var_deletion(prog::var_index index, location loc);

        pair<prog::reg_index, prog::ptr_type> add_ptr_extraction(prog::reg_index value, prog::type&& type, location loc);

        void add_return(prog::reg_index value, location loc);
        void add_break(location loc);
        void add_continue(location loc);
        prog::branch_instr make_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch);
        void add_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch);
        void add_loop(function<prog::reg_index()> head, function<void()> true_branch, function<void()> false_branch, function<void()> end);
    };
}

#endif
