#ifndef EXPRESSIONS_HPP
#define EXPRESSIONS_HPP

#include "compiler/functions.hpp"

namespace sg {
    using std::tuple;

    class expression_compiler : compiler_base {
        function_compiler& fclr;
        compiler& clr;

        public:

        expression_compiler(function_compiler& fclr) : compiler_base(fclr), fclr(fclr), clr(fclr.clr) { }

        pair<prog::reg_index, prog::type_local> compile(const ast::expr& ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_return(optional<cref<ast::expr>> ast, location loc);
        pair<prog::reg_index, prog::type_local> compile_binary_operation(const ast::binary_operation_expr& ast);

        lvalue compile_left(const ast::expr& ast, optional<cref<prog::type_local>> implicit_type);

        private:

        pair<prog::reg_index, prog::type_local> compile_global_name(string name, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_variant_name(string name, string variant_name, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_tuple(vector<cref<ast::expr_marked>> asts, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_array(vector<cref<ast::expr_marked>> asts, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, bool confined, location loc);
        pair<prog::reg_index, prog::type_local> compile_unary_operation(const ast::unary_operation_expr& ast);
        pair<prog::reg_index, prog::type_local> compile_numeric_cast(const ast::numeric_cast_expr& ast);
        pair<prog::reg_index, prog::type_local> compile_conditional(const ast::conditional_expr& ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_heap_alloc(const ast::expr& ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_dereference(const ast::expr& ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_weak_ptr_test(const ast::expr& ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_heap_slice_alloc(const ast::heap_slice_alloc_expr& ast, bool confined);
        pair<prog::reg_index, prog::type_local> compile_length(const ast::expr& ast);
        pair<prog::reg_index, prog::type_local> compile_extraction(const ast::expr& expr_ast, const ast::extraction_expr& extr_ast, bool confined);

        lvalue compile_left_tuple(vector<cref<ast::expr_marked>> asts, optional<cref<prog::type_local>> implicit_type, location loc);
        lvalue compile_left_array(vector<cref<ast::expr_marked>> asts, optional<cref<prog::type_local>> implicit_type, location loc);
        lvalue compile_left_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, optional<cref<prog::type_local>> implicit_type, location loc);
        lvalue compile_left_extraction(const ast::expr& expr_ast, const ast::extraction_expr& extr_ast);

        tuple<vector<cref<ast::expr>>, vector<prog::reg_index>, vector<prog::type>, bool> compile_args(
                vector<cref<ast::expr_marked>> asts,
                optional<function<size_t(string, location)>> arg_with_name,
                optional<size_t> expected_count,
                bool confined,
                location loc);

        vector<prog::reg_index> compile_call_args(
                vector<cref<ast::expr_marked>> asts,
                const prog::func_type& ftype,
                optional<cref<prog::global_func>> func,
                location loc);
    };
}

#endif
