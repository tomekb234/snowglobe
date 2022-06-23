#include "compiler/lvalues.hpp"
#include "compiler/expressions.hpp"
#include "compiler/conversions.hpp"
#include "compiler/types.hpp"
#include "compiler/compiler_utils.hpp"
#include "compiler/function_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    lvalue lvalue_compiler::compile(const ast::expr& ast) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE: {
                auto tuple_asts = as_cref_vector(GET(ast, TUPLE));
                return compile_tuple(tuple_asts, ast.loc);
            }

            case ast::expr::ARRAY: {
                auto array_asts = as_cref_vector(GET(ast, ARRAY));
                return compile_array(array_asts, ast.loc);
            }

            case ast::expr::APPLICATION: {
                auto& [receiver_ast_ptr, arg_ast_ptrs] = GET(ast, APPLICATION);
                return compile_application(*receiver_ast_ptr, as_cref_vector(arg_ast_ptrs), ast.loc);
            }

            case ast::expr::NAME: {
                auto name = GET(ast, NAME);
                return compile_name(name, ast.loc);
            }

            case ast::expr::VAR_DECL: {
                auto& var_decl_ast = *GET(ast, VAR_DECL);
                return compile_var_decl(var_decl_ast);
            }

            case ast::expr::DEREFERENCE: {
                auto& expr_ast = *GET(ast, DEREFERENCE);
                return compile_dereference(expr_ast);
            }

            case ast::expr::EXTRACTION: {
                auto& [expr_ast_ptr, extr_ast_ptr] = GET(ast, EXTRACTION);
                return compile_extraction(*expr_ast_ptr, *extr_ast_ptr);
            }

            default:
                error(diags::expression_not_assignable(ast.loc));
        }
    }

    lvalue lvalue_compiler::compile_tuple(vector<cref<ast::expr_marked>> asts, location loc) {
        auto value_asts = compiler_utils(clr).order_args(asts, { }, { }, loc);
        auto count = value_asts.size();

        if (count == 0)
            error(diags::expression_not_assignable(loc));

        if (count == 1)
            return lvalue_compiler(fclr, implicit_type).compile(value_asts[0]);

        vector<prog::type_local> implicit_types;
        if (implicit_type && INDEX_EQ(*implicit_type->get().tp, TUPLE)) {
            auto confined = implicit_type->get().confined;
            for (const prog::type& type : as_cref_vector(GET(*implicit_type->get().tp, TUPLE)))
                implicit_types.push_back({ make_ptr(copy_type(type)), confined });
        }

        vector<lvalue> lvals;
        for (size_t index = 0; index < count; index++) {
            auto& value_ast = value_asts[index];
            if (index < implicit_types.size())
                lvals.push_back(lvalue_compiler(fclr, { implicit_types[index] }).compile(value_ast));
            else
                lvals.push_back(lvalue_compiler(fclr, { }).compile(value_ast));
        }

        return VARIANT(lvalue, TUPLE, into_ptr_vector(lvals));
    }

    lvalue lvalue_compiler::compile_array(vector<cref<ast::expr_marked>> asts, location loc) {
        auto value_asts = compiler_utils(clr).order_args(asts, { }, { }, loc);

        vector<lvalue> lvals;
        for (auto& value_ast : value_asts) {
            if (implicit_type && INDEX_EQ(*implicit_type->get().tp, ARRAY)) {
                auto& type = *GET(*implicit_type->get().tp, ARRAY)->tp;
                auto confined = implicit_type->get().confined;
                auto type_local = prog::type_local { make_ptr(copy_type(type)), confined };
                lvals.push_back(lvalue_compiler(fclr, { type_local }).compile(value_ast));
            } else
                lvals.push_back(lvalue_compiler(fclr, { }).compile(value_ast));
        }

        return VARIANT(lvalue, ARRAY, into_ptr_vector(lvals));
    }

    lvalue lvalue_compiler::compile_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, location loc) {
        if (!INDEX_EQ(receiver_ast, NAME))
            error(diags::expression_not_assignable(receiver_ast.loc));

        auto name = GET(receiver_ast, NAME);
        auto struct_index = clr.get_global_name(name, { global_name_kind::STRUCT }, receiver_ast.loc).index;
        auto& st = *prog.struct_types[struct_index];
        auto count = st.fields.size();

        auto arg_with_name = [&] (string name, location loc) -> size_t {
            auto iter = st.field_names.find(name);
            if (iter == st.field_names.end())
                error(diags::unknown_struct_field(st, name, loc));
            return iter->second;
        };

        auto value_asts = compiler_utils(clr).order_args(arg_asts, arg_with_name, { count }, loc);
        vector<lvalue> lvals;

        for (size_t index = 0; index < count; index++) {
            auto& value_ast = value_asts[index].get();
            if (implicit_type && INDEX_EQ(*implicit_type->get().tp, STRUCT) && GET(*implicit_type->get().tp, STRUCT) == struct_index) {
                auto& type = *st.fields[index]->tp;
                auto confined = implicit_type->get().confined;
                auto type_local = prog::type_local { make_ptr(copy_type(type)), confined };
                lvals.push_back(lvalue_compiler(fclr, { type_local }).compile(value_ast));
            } else
                lvals.push_back(lvalue_compiler(fclr, { }).compile(value_ast));
        }

        return VARIANT(lvalue, STRUCT, make_pair(struct_index, into_ptr_vector(lvals)));
    }

    lvalue lvalue_compiler::compile_name(string name, location loc) {
        if (name == ast::IGNORED_PLACEHOLDER)
            return VARIANT(lvalue, IGNORED, monostate());

        auto var = fclr.try_get_var(name);
        if (var)
            return VARIANT(lvalue, VAR, *var);

        auto index = clr.get_global_name(name, { global_name_kind::VAR }, loc).index;
        return VARIANT(lvalue, GLOBAL_VAR, index);
    }

    lvalue lvalue_compiler::compile_var_decl(const ast::var_decl_expr& ast) {
        if (!ast.tp && !implicit_type)
            error(diags::variable_without_type(ast.loc));

        auto name = ast.name;
        if (name == ast::IGNORED_PLACEHOLDER)
            error(diags::invalid_variable_name(name, ast.loc));

        auto type = ast.tp ? type_compiler(clr, false).compile_local(**ast.tp) : copy_type_local(*implicit_type);

        auto var = fclr.add_var(name, move(type));
        return VARIANT(lvalue, VAR, var);
    }

    lvalue lvalue_compiler::compile_dereference(const ast::expr& ast) {
        auto [value, type] = expression_compiler(fclr, true).compile(ast);
        auto [ptr_value, ptr_type] = function_utils(fclr).add_ptr_extraction(value, *type.tp, ast.loc);
        auto& type_pointed = *ptr_type.target_tp;
        auto& target_type = *type_pointed.tp;

        if (ptr_type.kind == prog::ptr_type::WEAK)
            error(diags::weak_pointer_dereference(ast.loc));
        if (type_pointed.slice)
            error(diags::slice_dereference(ast.loc));

        return VARIANT(lvalue, DEREFERENCE, make_pair(ptr_value, move(target_type)));
    }

    lvalue lvalue_compiler::compile_extraction(const ast::expr& expr_ast, const ast::extraction_expr& extr_ast) {
        auto [left_value, left_type_local] = expression_compiler(fclr, true).compile(expr_ast);
        auto& left_type = *left_type_local.tp;

        auto [ptr_value, ptr_type] = function_utils(fclr).add_ptr_extraction(left_value, left_type, expr_ast.loc);
        auto slice = ptr_type.target_tp->slice;
        auto& type = *ptr_type.target_tp->tp;

        if (ptr_type.kind == prog::ptr_type::WEAK)
            error(diags::weak_pointer_dereference(expr_ast.loc));

        prog::reg_index target_ptr_value;
        prog::type_pointed target_type;

        switch (INDEX(extr_ast)) {
            case ast::extraction_expr::FIELD: {
                auto name = GET(extr_ast, FIELD);

                if (slice || !INDEX_EQ(type, STRUCT))
                    error(diags::invalid_type(prog, move(type), diags::type_kind::STRUCT, expr_ast.loc));

                auto struct_index = GET(type, STRUCT);
                auto& st = *prog.struct_types[struct_index];

                auto iter = st.field_names.find(name);
                if (iter == st.field_names.end())
                    error(diags::unknown_struct_field(st, name, extr_ast.loc));

                auto field = iter->second;

                target_ptr_value = fclr.new_reg();
                target_type = prog::type_pointed { make_ptr(copy_type(*st.fields[field]->tp)), false };

                auto instr = prog::get_field_ptr_instr { ptr_value, field, target_ptr_value };
                fclr.add_instr(VARIANT(prog::instr, GET_FIELD_PTR, into_ptr(instr)));
            } break;

            case ast::extraction_expr::INDEX: {
                auto field = GET(extr_ast, INDEX);

                if (slice || !INDEX_EQ(type, TUPLE))
                    error(diags::invalid_type(prog, move(type), diags::type_kind::TUPLE, expr_ast.loc));

                auto types = as_cref_vector(GET(type, TUPLE));

                if (field >= types.size())
                    error(diags::invalid_tuple_index(field, types.size(), extr_ast.loc));

                target_ptr_value = fclr.new_reg();
                target_type = prog::type_pointed { make_ptr(copy_type(types[field])), false };

                auto instr = prog::get_field_ptr_instr { ptr_value, field, target_ptr_value };
                fclr.add_instr(VARIANT(prog::instr, GET_FIELD_PTR, into_ptr(instr)));
            } break;

            case ast::extraction_expr::ITEM: {
                auto& index_expr = *GET(extr_ast, ITEM);

                if (!slice && !INDEX_EQ(type, ARRAY))
                    error(diags::invalid_type(prog, move(type), diags::type_kind::SLICE_OR_ARRAY_POINTER, expr_ast.loc));

                auto [index_value, index_type] = expression_compiler(fclr, true).compile(index_expr);
                index_value = conversion_generator(fclr, index_value, prog::SIZE_TYPE).convert_from(index_type, index_expr.loc);

                auto check_result = fclr.new_reg();
                auto check_instr = prog::check_index_instr { ptr_value, index_value, check_result };

                if (slice)
                    fclr.add_instr(VARIANT(prog::instr, CHECK_SLICE_INDEX, into_ptr(check_instr)));
                else
                    fclr.add_instr(VARIANT(prog::instr, CHECK_ARRAY_PTR_INDEX, into_ptr(check_instr)));

                auto false_branch = [&] () {
                    fclr.add_instr(VARIANT(prog::instr, ABORT, monostate()));
                    fclr.returned = true;
                };

                function_utils(fclr).add_branch(check_result, [] { }, false_branch);

                target_ptr_value = fclr.new_reg();

                if (slice)
                    target_type = prog::type_pointed { into_ptr(type), false };
                else
                    target_type = prog::type_pointed { into_ptr(*GET(type, ARRAY)->tp), false };

                auto instr = prog::get_item_ptr_instr { ptr_value, index_value, target_ptr_value };
                fclr.add_instr(VARIANT(prog::instr, GET_ITEM_PTR, into_ptr(instr)));
            } break;

            default:
                error(diags::expression_not_assignable(extr_ast.loc));
        }

        return VARIANT(lvalue, DEREFERENCE, make_pair(target_ptr_value, move(*target_type.tp)));
    }
}
