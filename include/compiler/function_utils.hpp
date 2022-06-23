#ifndef FUNCTION_UTILS_HPP
#define FUNCTION_UTILS_HPP

#include "compiler/functions.hpp"

namespace sg {
    using std::tuple;

    class function_utils : compiler_base {
        function_compiler& fclr;
        compiler& clr;

        public:

        function_utils(function_compiler& fclr) : compiler_base(fclr), fclr(fclr), clr(fclr.clr) { }

        void add_frame_cleanup(function_compiler::frame_index rev_index, location loc);

        void add_return(prog::reg_index value, location loc);
        void add_break(location loc);
        void add_continue(location loc);

        prog::branch_instr make_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch);
        void add_branch(prog::reg_index cond, function<void()> true_branch, function<void()> false_branch);
        void add_loop(function<prog::reg_index()> head, function<void()> true_branch, function<void()> false_branch, function<void()> end);

        pair<prog::reg_index, prog::type_local> add_var_read(prog::var_index var_index, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> add_var_confinement(prog::var_index var_index, location loc);
        void add_var_deletion(prog::var_index index, location loc);

        pair<prog::reg_index, prog::ptr_type> add_ptr_extraction(prog::reg_index value, const prog::type& type, location loc);
        tuple<prog::reg_index, prog::ptr_type::kind_t, prog::type_pointed> add_ptr_owner_extraction(prog::reg_index value, const prog::type& type);

        pair<prog::reg_index, prog::type_local> add_read_for_swap(const lvalue& lval, location loc);
    };
}

#endif
