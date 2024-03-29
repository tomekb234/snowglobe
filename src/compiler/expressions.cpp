#include "compiler/expressions.hpp"
#include "compiler/types.hpp"
#include "compiler/constants.hpp"
#include "compiler/conversions.hpp"
#include "compiler/copying.hpp"
#include "compiler/deletion.hpp"
#include "compiler/compiler_utils.hpp"
#include "compiler/function_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    pair<prog::reg_index, prog::type_local> expression_compiler::compile(const ast::expr& ast) {
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

            case ast::expr::VARIANT_NAME: {
                auto& [name, variant_name] = GET(ast, VARIANT_NAME);
                return compile_variant_name(name, variant_name, ast.loc);
            }

            case ast::expr::LITERAL: {
                auto& literal_ast = *GET(ast, LITERAL);
                return compile_literal(literal_ast);
            }

            case ast::expr::UNARY_OPERATION: {
                auto& unop_ast = *GET(ast, UNARY_OPERATION);
                return compile_unary_operation(unop_ast);
            }

            case ast::expr::BINARY_OPERATION: {
                auto& binop_ast = *GET(ast, BINARY_OPERATION);
                return compile_binary_operation(binop_ast);
            }

            case ast::expr::NUMERIC_CAST: {
                auto& cast_ast = *GET(ast, NUMERIC_CAST);
                return compile_numeric_cast(cast_ast);
            }

            case ast::expr::NONE:
                return compile_none();

            case ast::expr::SOME: {
                auto& expr_ast = *GET(ast, SOME);
                return compile_some(expr_ast);
            }

            case ast::expr::RETURN: {
                auto expr_ast = as_optional_cref(GET(ast, RETURN));
                return compile_return(expr_ast, ast.loc);
            }

            case ast::expr::BREAK:
                return compile_break(ast.loc);

            case ast::expr::CONTINUE:
                return compile_continue(ast.loc);

            case ast::expr::CONDITIONAL: {
                auto& conditional_ast = *GET(ast, CONDITIONAL);
                return compile_conditional(conditional_ast);
            }

            case ast::expr::GLOBAL_VAR_REF: {
                auto name = GET(ast, GLOBAL_VAR_REF);
                return compile_global_var_ref(name, ast.loc);
            }

            case ast::expr::HEAP_ALLOC: {
                auto& expr_ast = *GET(ast, HEAP_ALLOC);
                return compile_heap_alloc(expr_ast);
            }

            case ast::expr::DEREFERENCE: {
                auto& expr_ast = *GET(ast, DEREFERENCE);
                return compile_dereference(expr_ast);
            }

            case ast::expr::WEAK_PTR_TEST: {
                auto& expr_ast = *GET(ast, WEAK_PTR_TEST);
                return compile_weak_ptr_test(expr_ast);
            }

            case ast::expr::HEAP_SLICE_ALLOC: {
                auto& alloc_ast = *GET(ast, HEAP_SLICE_ALLOC);
                return compile_heap_slice_alloc(alloc_ast);
            }

            case ast::expr::LENGTH: {
                auto& expr_ast = *GET(ast, LENGTH);
                return compile_length(expr_ast);
            }

            case ast::expr::EXTRACTION: {
                auto& [expr_ast_ptr, extr_ast_ptr] = GET(ast, EXTRACTION);
                return compile_extraction(*expr_ast_ptr, *extr_ast_ptr);
            }

            case ast::expr::LAMBDA:
                error(diags::not_implemented(ast.loc)); // TODO

            default:
                error(diags::invalid_expression(ast.loc));
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_name(string name, location loc) {
        auto var_index = fclr.try_get_var(name);

        if (var_index)
            return function_utils(fclr).add_var_read(*var_index, confined, loc);

        auto& gname = clr.get_global_name(name, loc);

        switch (gname.kind) {
            case global_name_kind::VAR: {
                auto& var = *prog.global_vars[gname.index];

                auto result = fclr.new_reg();
                auto instr = prog::read_global_var_instr { gname.index, result };
                fclr.add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(instr)));

                if (!confined) {
                    if (type_copyable(prog, *var.tp))
                        copy_generator(fclr, result).add(*var.tp);
                    else
                        error(diags::global_variable_moved_out(loc));
                }

                auto type = prog::type_local { make_ptr(copy_type(*var.tp)), confined };

                return { result, move(type) };
            }

            case global_name_kind::CONST: {
                auto& var = clr.consts[gname.index];
                auto& type = *var.tp;
                auto& value = *var.value;

                auto result = fclr.new_reg();
                auto instr = prog::make_const_instr { make_ptr(copy_const(value)), make_ptr(copy_type(type)), result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(instr)));

                auto result_type = prog::type_local { make_ptr(copy_type(type)), confined };

                return { result, move(result_type) };
            }

            case global_name_kind::FUNC: {
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, KNOWN_FUNC, gname.index)), false };
                return { fclr.new_unit_reg(), move(type) };
            }

            case global_name_kind::STRUCT: {
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, STRUCT_CTOR, gname.index)), false };
                return { fclr.new_unit_reg(), move(type) };
            }

            case global_name_kind::ENUM:
                error(diags::invalid_kind(name, global_name_kind::ENUM, { }, loc));
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_variant_name(string name, string variant_name, location loc) {
        if (fclr.try_get_var(name))
            error(diags::expected_enum_name(loc));

        auto enum_index = clr.get_global_name(name, { global_name_kind::ENUM }, loc).index;
        auto& en = *prog.enum_types[enum_index];

        auto iter = en.variant_names.find(variant_name);
        if (iter == en.variant_names.end())
            error(diags::unknown_enum_variant(en, name, loc));
        auto variant_index = iter->second;
        auto& variant = *en.variants[variant_index];

        if (variant.tps.empty()) {
            auto result = fclr.new_reg();
            auto instr = prog::make_enum_variant_instr { enum_index, variant_index, { }, result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_ENUM_VARIANT, into_ptr(instr)));
            auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM, enum_index)), confined };
            return { result, move(type) };
        }

        auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM_CTOR, make_pair(enum_index, variant_index))), false };
        return { fclr.new_unit_reg(), move(type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_literal(const ast::literal_expr& ast) {
        auto [value, value_type] = constant_compiler(clr).compile_literal(ast);

        auto result = fclr.new_reg();
        auto instr = prog::make_const_instr { into_ptr(value), make_ptr(copy_type(value_type)), result };
        fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(instr)));

        auto type = prog::type_local { into_ptr(value_type), false };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_return(optional<cref<ast::expr>> ast, location loc) {
        prog::reg_index result;
        prog::type_local type;

        if (ast)
            tie(result, type) = expression_compiler(fclr, false).compile(*ast);
        else {
            result = fclr.new_unit_reg();
            type = copy_type_local(prog::UNIT_TYPE_LOCAL);
        }

        result = conversion_generator(fclr, result, *fclr.func.return_tp).convert_from(type, loc);

        function_utils(fclr).add_return(result, loc);

        return { result, copy_type_local(prog::NEVER_TYPE_LOCAL) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_tuple(vector<cref<ast::expr_marked>> asts, location loc) {
        auto [value_asts, values, types, all_confined] = compile_args(asts, { }, { }, loc);
        auto count = values.size();

        if (count == 0)
            return { fclr.new_unit_reg(), copy_type_local(prog::UNIT_TYPE_LOCAL) };

        if (count == 1) {
            auto result = values[0];
            auto type = prog::type_local { into_ptr(types[0]), all_confined };
            return { result, move(type) };
        }

        auto result = fclr.new_reg();
        auto instr = prog::make_tuple_instr { values, result };
        fclr.add_instr(VARIANT(prog::instr, MAKE_TUPLE, into_ptr(instr)));
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, TUPLE, into_ptr_vector(types))), all_confined };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_array(vector<cref<ast::expr_marked>> asts, location loc) {
        auto [value_asts, values, types, all_confined] = compile_args(asts, { }, { }, loc);
        auto count = values.size();

        auto common_type = VARIANT(prog::type, NEVER, monostate());

        for (auto& type : types)
            common_type = compiler_utils(clr).common_supertype(common_type, type, loc);

        for (size_t index = 0; index < count; index++)
            values[index] = conversion_generator(fclr, values[index], common_type, all_confined).convert_from(types[index], value_asts[index].get().loc);

        auto result = fclr.new_reg();
        auto instr = prog::make_array_instr { values, result };
        fclr.add_instr(VARIANT(prog::instr, MAKE_ARRAY, into_ptr(instr)));
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(common_type), count }))), all_confined };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_application(
            const ast::expr& receiver_ast,
            vector<cref<ast::expr_marked>> arg_asts,
            location loc) {
        auto[receiver, receiver_type_local] = expression_compiler(fclr, true).compile(receiver_ast);
        auto& receiver_type = *receiver_type_local.tp;

        switch (INDEX(receiver_type)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(receiver_type, STRUCT_CTOR);
                auto& st = *prog.struct_types[struct_index];
                auto size = st.fields.size();

                auto arg_with_name = [&] (string name, location loc) -> size_t {
                    auto iter = st.field_names.find(name);
                    if (iter == st.field_names.end())
                        error(diags::unknown_struct_field(st, name, loc));
                    return iter->second;
                };

                auto [value_asts, values, types, all_confined] = compile_args(arg_asts, { arg_with_name }, { size }, loc);

                for (size_t index = 0; index < size; index++) {
                    auto& type = types[index];
                    auto& field_type = *st.fields[index]->tp;
                    values[index] = conversion_generator(fclr, values[index], field_type, all_confined).convert_from(type, value_asts[index].get().loc);
                }

                auto result = fclr.new_reg();
                auto instr = prog::make_struct_instr { struct_index, values, result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_STRUCT, into_ptr(instr)));
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, STRUCT, struct_index)), all_confined };
                return { result, move(type) };
            }

            case prog::type::ENUM_CTOR: {
                auto [enum_index, variant_index] = GET(receiver_type, ENUM_CTOR);
                auto& en = *prog.enum_types[enum_index];
                auto& variant = *en.variants[variant_index];
                auto size = variant.tps.size();

                auto [value_asts, values, types, all_confined] = compile_args(arg_asts, { }, { size }, loc);

                for (size_t index = 0; index < size; index++) {
                    auto& type = types[index];
                    auto& field_type = *variant.tps[index];
                    values[index] = conversion_generator(fclr, values[index], field_type, all_confined).convert_from(type, value_asts[index].get().loc);
                }

                auto result = fclr.new_reg();
                auto instr = prog::make_enum_variant_instr { enum_index, variant_index, values, result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_ENUM_VARIANT, into_ptr(instr)));
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM, enum_index)), all_confined };
                return { result, move(type) };
            }

            case prog::type::FUNC:
            case prog::type::FUNC_WITH_PTR: {
                auto& ftype = INDEX_EQ(receiver_type, FUNC) ? *GET(receiver_type, FUNC) : *GET(receiver_type, FUNC_WITH_PTR);

                if (confined && !type_trivial(prog, *ftype.return_tp))
                    error(diags::not_allowed_in_confined_context(diags::value_kind::FUNCTION_RESULT, loc));

                auto args = compile_call_args(arg_asts, ftype, { }, loc);

                auto ptr = fclr.new_reg();
                auto extract_ptr_instr = prog::ptr_conversion_instr { receiver, ptr };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(extract_ptr_instr)));
                args.insert(args.begin(), ptr);

                auto func = fclr.new_reg();
                auto extract_func_instr = prog::ptr_conversion_instr { receiver, func };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_FUNC_PTR, into_ptr(extract_func_instr)));

                auto result = fclr.new_reg();
                auto instr = prog::func_ptr_call_instr { func, args, result };
                fclr.add_instr(VARIANT(prog::instr, FUNC_PTR_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*ftype.return_tp)), false };
                return { result, move(type) };
            }

            case prog::type::GLOBAL_FUNC: {
                auto& ftype = *GET(receiver_type, GLOBAL_FUNC);

                if (confined && !type_trivial(prog, *ftype.return_tp))
                    error(diags::not_allowed_in_confined_context(diags::value_kind::FUNCTION_RESULT, loc));

                auto args = compile_call_args(arg_asts, ftype, { }, loc);

                auto result = fclr.new_reg();
                auto instr = prog::func_ptr_call_instr { receiver, args, result };
                fclr.add_instr(VARIANT(prog::instr, FUNC_PTR_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*ftype.return_tp)), false };
                return { result, move(type) };
            }

            case prog::type::KNOWN_FUNC: {
                auto func_index = GET(receiver_type, KNOWN_FUNC);
                auto& func = *prog.global_funcs[func_index];
                auto ftype = get_func_type(func);

                if (confined && !type_trivial(prog, *ftype.return_tp))
                    error(diags::not_allowed_in_confined_context(diags::value_kind::FUNCTION_RESULT, loc));

                auto args = compile_call_args(arg_asts, ftype, { func }, loc);

                auto result = fclr.new_reg();
                auto instr = prog::func_call_instr { func_index, args, result };
                fclr.add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*func.return_tp)), false };
                return { result, move(type) };
            }

            default:
                error(diags::invalid_expression(loc));
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_unary_operation(const ast::unary_operation_expr& ast) {
        auto [value, type] = expression_compiler(fclr, true).compile(*ast.value);
        auto result = fclr.new_reg();

        using num = prog::number_type;
        using unop = ast::unary_operation_expr;

        switch (ast.operation) {
            case unop::NOT: {
                value = conversion_generator(fclr, value, prog::BOOL_TYPE).convert_from(type, ast.value->loc);
                auto instr = prog::unary_operation_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, BOOL_NOT, into_ptr(instr)));
                return { result, copy_type_local(prog::BOOL_TYPE_LOCAL) };
            }

            case unop::MINUS: {
                if (!INDEX_EQ(*type.tp, NUMBER))
                    error(diags::invalid_unary_operation(prog, ast.operation, move(*type.tp), ast.loc));

                switch (GET(*type.tp, NUMBER)->tp) {
                    case num::I8: case num::I16: case num::I32: case num::I64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        fclr.add_instr(VARIANT(prog::instr, INT_NEG, into_ptr(instr)));
                    } break;

                    case num::F32: case num::F64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        fclr.add_instr(VARIANT(prog::instr, FLOAT_NEG, into_ptr(instr)));
                    } break;

                    default:
                        error(diags::invalid_unary_operation(prog, ast.operation, move(*type.tp), ast.loc));
                }

                return { result, move(type) };
            }

            case unop::BIT_NEG: {
                if (!INDEX_EQ(*type.tp, NUMBER))
                    error(diags::invalid_unary_operation(prog, ast.operation, move(*type.tp), ast.loc));

                switch (GET(*type.tp, NUMBER)->tp) {
                    case num::I8: case num::I16: case num::I32: case num::I64:
                    case num::U8: case num::U16: case num::U32: case num::U64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        fclr.add_instr(VARIANT(prog::instr, BIT_NEG, into_ptr(instr)));
                        return { result, move(type) };
                    }

                    default:
                        error(diags::invalid_unary_operation(prog, ast.operation, move(*type.tp), ast.loc));
                }
            }
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_binary_operation(const ast::binary_operation_expr& ast) {
        #define INVALID_BINARY_OP { \
            error(diags::invalid_binary_operation(prog, ast.operation, \
                copy_type(*left_type.tp), copy_type(*right_type.tp), ast.loc)); \
        }

        using num = prog::number_type;

        auto is_uint = [&] (const prog::number_type& tp) -> bool {
            switch (tp.tp) {
                case num::U8: case num::U16: case num::U32: case num::U64:
                    return true;
                default:
                    return false;
            }
        };

        auto is_sint = [&] (const prog::number_type& tp) -> bool {
            switch (tp.tp) {
                case num::I8: case num::I16: case num::I32: case num::I64:
                    return true;
                default:
                    return false;
            }
        };

        auto is_float = [&] (const prog::number_type& tp) -> bool {
            switch (tp.tp) {
                case num::F32: case num::F64:
                    return true;
                default:
                    return false;
            }
        };

        using binop = ast::binary_operation_expr;

        switch (ast.operation) {
            case binop::AND:
            case binop::OR: {
                auto[left_value, left_type] = expression_compiler(fclr, true).compile(*ast.left);
                left_value = conversion_generator(fclr, left_value, prog::BOOL_TYPE).convert_from(left_type, ast.left->loc);
                prog::reg_index right_value;
                auto result = fclr.new_reg();

                auto short_branch = [&](){};

                auto long_branch = [&](){
                    auto[right_raw_value, right_type] = expression_compiler(fclr, true).compile(*ast.right);
                    right_value = conversion_generator(fclr, right_raw_value, prog::BOOL_TYPE).convert_from(right_type, ast.right->loc);
                };

                if (ast.operation == binop::AND) {
                    auto branch_instr = function_utils(fclr).make_branch(left_value, long_branch, short_branch);
                    auto value_branch_instr = prog::value_branch_instr{ move(branch_instr), right_value, left_value, result };
                    fclr.add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(value_branch_instr)));
                } else {
                    auto branch_instr = function_utils(fclr).make_branch(left_value, short_branch, long_branch);
                    auto value_branch_instr = prog::value_branch_instr{ move(branch_instr), left_value, right_value, result };
                    fclr.add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(value_branch_instr)));
                }

                return { result, copy_type_local(prog::BOOL_TYPE_LOCAL) };
            } break;

            case binop::EQ:
            case binop::NEQ:
            case binop::LS:
            case binop::LSEQ:
            case binop::GT:
            case binop::GTEQ:
            case binop::ADD:
            case binop::SUB:
            case binop::MUL:
            case binop::DIV:
            case binop::MOD:
            case binop::BIT_AND:
            case binop::BIT_OR:
            case binop::BIT_XOR: {
                auto[left_value, left_type] = expression_compiler(fclr, true).compile(*ast.left);
                auto[right_value, right_type] = expression_compiler(fclr, true).compile(*ast.right);
                auto common_type = compiler_utils(clr).common_supertype(*left_type.tp, *right_type.tp, ast.loc);

                if (!INDEX_EQ(common_type, NUMBER))
                    INVALID_BINARY_OP;

                left_value = conversion_generator(fclr, left_value, common_type).convert_from(*left_type.tp, ast.left->loc);
                right_value = conversion_generator(fclr, right_value, common_type).convert_from(*right_type.tp, ast.right->loc);
                auto result = fclr.new_reg();
                prog::binary_operation_instr::kind_t kind;
                auto& ntype = *GET(common_type, NUMBER);

                if (is_uint(ntype))
                    kind = prog::binary_operation_instr::UNSIGNED;
                else if (is_sint(ntype))
                    kind = prog::binary_operation_instr::SIGNED;
                else if (is_float(ntype))
                    kind = prog::binary_operation_instr::FLOAT;
                else
                    INVALID_BINARY_OP;

                auto op_instr = make_ptr(prog::binary_operation_instr{ left_value, right_value, result, kind });
                bool bool_result_type = false;

                switch (ast.operation) {
                    case binop::EQ: {
                        fclr.add_instr(VARIANT(prog::instr, EQ, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::NEQ: {
                        fclr.add_instr(VARIANT(prog::instr, NEQ, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::LSEQ: {
                        fclr.add_instr(VARIANT(prog::instr, LSEQ, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::GT: {
                        fclr.add_instr(VARIANT(prog::instr, GT, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::GTEQ: {
                        fclr.add_instr(VARIANT(prog::instr, GTEQ, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::ADD:
                        fclr.add_instr(VARIANT(prog::instr, ADD, move(op_instr)));
                        break;

                    case binop::SUB:
                        fclr.add_instr(VARIANT(prog::instr, SUB, move(op_instr)));
                        break;

                    case binop::MUL:
                        fclr.add_instr(VARIANT(prog::instr, MUL, move(op_instr)));
                        break;

                    case binop::DIV:
                        fclr.add_instr(VARIANT(prog::instr, DIV, move(op_instr)));
                        break;

                    case binop::MOD:
                        fclr.add_instr(VARIANT(prog::instr, MOD, move(op_instr)));
                        break;

                    case binop::BIT_AND: {
                        if (is_float(ntype))
                            INVALID_BINARY_OP;
                        fclr.add_instr(VARIANT(prog::instr, BIT_AND, move(op_instr)));
                    } break;

                    case binop::BIT_OR: {
                        if (is_float(ntype))
                            INVALID_BINARY_OP;
                        fclr.add_instr(VARIANT(prog::instr, BIT_OR, move(op_instr)));
                    } break;

                    case binop::BIT_XOR: {
                        if (is_float(ntype))
                            INVALID_BINARY_OP;
                        fclr.add_instr(VARIANT(prog::instr, BIT_XOR, move(op_instr)));
                    } break;

                    default:
                        UNREACHABLE;
                }

                if (bool_result_type)
                    return { result, copy_type_local(prog::BOOL_TYPE_LOCAL) };
                else
                    return { result, prog::type_local{ into_ptr(common_type), false } };
            }

            case binop::BIT_LSH:
            case binop::BIT_RSH: {
                auto[left_value, left_type] = expression_compiler(fclr, true).compile(*ast.left);
                auto[right_value, right_type] = expression_compiler(fclr, true).compile(*ast.right);

                if (!INDEX_EQ(*left_type.tp, NUMBER) || !INDEX_EQ(*right_type.tp, NUMBER) || !is_uint(*GET(*right_type.tp, NUMBER)))
                    INVALID_BINARY_OP;

                auto result = fclr.new_reg();
                prog::binary_operation_instr::kind_t kind;
                auto& left_ntype = *GET(*left_type.tp, NUMBER);

                if (is_uint(left_ntype))
                    kind = prog::binary_operation_instr::UNSIGNED;
                else if (is_sint(left_ntype))
                    kind = prog::binary_operation_instr::SIGNED;
                else
                    INVALID_BINARY_OP;

                auto op_instr = make_ptr(prog::binary_operation_instr{ left_value, right_value, result, kind });

                if (ast.operation == binop::BIT_LSH)
                    fclr.add_instr(VARIANT(prog::instr, BIT_LSH, move(op_instr)));
                else
                    fclr.add_instr(VARIANT(prog::instr, BIT_RSH, move(op_instr)));

                return { result, move(left_type) };
            }
        }

        UNREACHABLE;

        #undef INVALID_BINARY_OP
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_numeric_cast(const ast::numeric_cast_expr& ast) {
        auto [value, type] = expression_compiler(fclr, true).compile(*ast.value);
        auto new_type = type_compiler(clr, false).compile_local(*ast.tp);

        if (!INDEX_EQ(*type.tp, NUMBER))
            error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::NUMBER, ast.value->loc));
        if (!INDEX_EQ(*new_type.tp, NUMBER))
            error(diags::invalid_type(prog, move(*new_type.tp), diags::type_kind::NUMBER, ast.loc));

        auto& ntype = *GET(*type.tp, NUMBER);
        auto& new_ntype = *GET(*new_type.tp, NUMBER);

        #define PASS { \
            return { value, move(new_type) }; \
        }

        #define CAST(instr_name) { \
            auto result = fclr.new_reg(); \
            auto instr = prog::numeric_conversion_instr { value, into_ptr(new_ntype), result }; \
            fclr.add_instr(VARIANT(prog::instr, instr_name, into_ptr(instr))); \
            return { result, move(new_type) }; \
        }

        using num = prog::number_type;

        switch (ntype.tp) {
            case num::BOOL: {
                switch (new_ntype.tp) {
                    case num::BOOL:
                        PASS;
                    case num::I8: case num::I16: case num::I32: case num::I64:
                    case num::U8: case num::U16: case num::U32: case num::U64:
                        CAST(ZERO_EXT);
                    case num::F32: case num::F64:
                        CAST(UINT_TO_FLOAT);
                }
            } break;

            case num::I8: {
                switch (new_ntype.tp) {
                    case num::I8: case num::U8:
                        PASS;
                    case num::BOOL:
                        CAST(TRUNC);
                    case num::U16:
                    case num::U32: case num::U64:
                        CAST(ZERO_EXT);
                    case num::I16: case num::I32: case num::I64:
                        CAST(SIGNED_EXT);
                    case num::F32: case num::F64:
                        CAST(SINT_TO_FLOAT);
                }
            } break;

            case num::I16: {
                switch (new_ntype.tp) {
                    case num::I16: case num::U16:
                        PASS;
                    case num::BOOL: case num::I8: case num::U8:
                        CAST(TRUNC);
                    case num::U32: case num::U64:
                        CAST(ZERO_EXT);
                    case num::I32: case num::I64:
                        CAST(SIGNED_EXT);
                    case num::F32: case num::F64:
                        CAST(SINT_TO_FLOAT);
                }
            } break;

            case num::I32: {
                switch (new_ntype.tp) {
                    case num::I32: case num::U32:
                        PASS;
                    case num::BOOL: case num::I8: case num::I16:
                    case num::U8: case num::U16:
                        CAST(TRUNC);
                    case num::U64:
                        CAST(ZERO_EXT);
                    case num::I64:
                        CAST(SIGNED_EXT);
                    case num::F32: case num::F64:
                        CAST(SINT_TO_FLOAT);
                }
            } break;

            case num::I64: {
                switch (new_ntype.tp) {
                    case num::I64: case num::U64:
                        PASS;
                    case num::BOOL:
                    case num::I8: case num::I16: case num::I32:
                    case num::U8: case num::U16: case num::U32:
                        CAST(TRUNC);
                    case num::F32: case num::F64:
                        CAST(SINT_TO_FLOAT);
                }
            } break;

            case num::U8: {
                switch (new_ntype.tp) {
                    case num::I8: case num::U8:
                        PASS;
                    case num::BOOL:
                        CAST(TRUNC);
                    case num::I16: case num::I32: case num::I64:
                    case num::U16: case num::U32: case num::U64:
                        CAST(ZERO_EXT);
                    case num::F32: case num::F64:
                        CAST(UINT_TO_FLOAT);
                }
            } break;

            case num::U16: {
                switch (new_ntype.tp) {
                    case num::I16: case num::U16:
                        PASS;
                    case num::BOOL: case num::I8: case num::U8:
                        CAST(TRUNC);
                    case num::I32: case num::I64:
                    case num::U32: case num::U64:
                        CAST(ZERO_EXT);
                    case num::F32: case num::F64:
                        CAST(UINT_TO_FLOAT);
                }
            } break;

            case num::U32: {
                switch (new_ntype.tp) {
                    case num::I32: case num::U32:
                        PASS;
                    case num::BOOL:
                    case num::I8: case num::I16:
                    case num::U8: case num::U16:
                        CAST(TRUNC);
                    case num::I64: case num::U64:
                        CAST(ZERO_EXT);
                    case num::F32: case num::F64:
                        CAST(UINT_TO_FLOAT);
                }
            } break;

            case num::U64: {
                switch (new_ntype.tp) {
                    case num::I64: case num::U64:
                        PASS;
                    case num::BOOL:
                    case num::I8: case num::I16: case num::I32:
                    case num::U8: case num::U16: case num::U32:
                        CAST(TRUNC);
                    case num::F32: case num::F64:
                        CAST(UINT_TO_FLOAT);
                }
            } break;

            case num::F32: {
                switch (new_ntype.tp) {
                    case num::F32:
                        PASS;
                    case num::F64:
                        CAST(FLOAT_EXT);
                    case num::BOOL:
                    case num::U8: case num::U16:
                    case num::U32: case num::U64:
                        CAST(FLOAT_TO_UINT);
                    case num::I8: case num::I16: case num::I32: case num::I64:
                        CAST(FLOAT_TO_SINT);
                }
            } break;

            case num::F64: {
                switch (new_ntype.tp) {
                    case num::F64:
                        PASS;
                    case num::F32:
                        CAST(FLOAT_TRUNC);
                    case num::BOOL:
                    case num::U8: case num::U16: case num::U32: case num::U64:
                        CAST(FLOAT_TO_UINT);
                    case num::I8: case num::I16: case num::I32: case num::I64:
                        CAST(FLOAT_TO_SINT);
                }
            } break;
        }

        UNREACHABLE;

        #undef PASS
        #undef CAST
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_none() {
        auto result = fclr.new_reg();
        auto instr = prog::make_optional_instr { make_ptr(copy_type(prog::NEVER_TYPE)), { }, result };
        fclr.add_instr(VARIANT(prog::instr, MAKE_OPTIONAL, into_ptr(instr)));
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, NEVER, monostate())))), false };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_some(const ast::expr& ast) {
        auto [value, value_type] = expression_compiler(fclr, confined).compile(ast);
        auto result = fclr.new_reg();
        auto instr = prog::make_optional_instr { make_ptr(copy_type(*value_type.tp)), { value }, result };
        fclr.add_instr(VARIANT(prog::instr, MAKE_OPTIONAL, into_ptr(instr)));
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, OPTIONAL, move(value_type.tp))), value_type.confined };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_break(location loc) {
        function_utils(fclr).add_break(loc);
        return { fclr.new_unit_reg(), copy_type_local(prog::NEVER_TYPE_LOCAL) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_continue(location loc) {
        function_utils(fclr).add_continue(loc);
        return { fclr.new_unit_reg(), copy_type_local(prog::NEVER_TYPE_LOCAL) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_conditional(const ast::conditional_expr& ast) {
        auto [value, type] = expression_compiler(fclr, true).compile(*ast.value);
        auto cond = conversion_generator(fclr, value, prog::BOOL_TYPE).convert_from(type, ast.value->loc);

        prog::reg_index true_value, false_value;
        prog::type_local true_type, false_type;

        auto true_branch = [&] () {
            tie(true_value, true_type) = expression_compiler(fclr, confined).compile(*ast.true_result);
        };

        auto false_branch = [&] () {
            tie(false_value, false_type) = expression_compiler(fclr, confined).compile(*ast.false_result);
        };

        auto result = fclr.new_reg();
        auto branch_instr = function_utils(fclr).make_branch(cond, true_branch, false_branch);
        auto value_branch_instr = prog::value_branch_instr { move(branch_instr), true_value, false_value, result };
        fclr.add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(value_branch_instr)));

        optional<bool> both_confined;

        if (!type_trivial(prog, *true_type.tp))
            both_confined = { true_type.confined };

        if (!type_trivial(prog, *false_type.tp)) {
            if (!both_confined)
                both_confined = { false_type.confined };
            else if (false_type.confined != *both_confined)
                error(diags::confinement_ambiguous(ast.loc));
        }

        auto common_type = compiler_utils(clr).common_supertype(*true_type.tp, *false_type.tp, ast.loc);
        auto common_type_local = prog::type_local { into_ptr(common_type), both_confined ? *both_confined : false };

        prog::reg_index true_conv_value, false_conv_value;

        auto true_conv_branch = [&] () {
            true_conv_value = conversion_generator(fclr, result, common_type_local).convert_from(true_type, ast.true_result->loc);
        };

        auto false_conv_branch = [&] () {
            false_conv_value = conversion_generator(fclr, result, common_type_local).convert_from(false_type, ast.false_result->loc);
        };

        auto conv_result = fclr.new_reg();
        auto conv_branch_instr = function_utils(fclr).make_branch(cond, true_conv_branch, false_conv_branch);
        auto conv_value_branch_instr = prog::value_branch_instr { move(conv_branch_instr), true_conv_value, false_conv_value, conv_result };
        fclr.add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(conv_value_branch_instr)));

        return { conv_result, move(common_type_local) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_global_var_ref(string name, location loc) {
        auto var_index = clr.get_global_name(name, { global_name_kind::VAR }, loc).index;

        auto& var_type = *prog.global_vars[var_index]->tp;
        auto type = prog::type_local { make_ptr(prog::make_ptr_type(copy_type(var_type), prog::ptr_type::GLOBAL, false)), false };

        auto result = fclr.new_reg();
        auto constant = VARIANT(prog::constant, GLOBAL_VAR_PTR, var_index);
        auto ptr_type = prog::make_ptr_type(copy_type(var_type), prog::ptr_type::GLOBAL, false);
        auto instr = prog::make_const_instr { into_ptr(constant), into_ptr(ptr_type), result };
        fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(instr)));

        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_heap_alloc(const ast::expr& ast) {
        if (confined)
            error(diags::not_allowed_in_confined_context(diags::value_kind::ALLOCATION, ast.loc));

        auto [value, value_type] = expression_compiler(fclr, false).compile(ast);

        if (value_type.confined && !type_trivial(prog, *value_type.tp))
            error(diags::confinement_mismatch(value_type.confined, ast.loc));

        auto result = fclr.new_reg();
        auto instr = prog::alloc_instr { value, result };
        fclr.add_instr(VARIANT(prog::instr, ALLOC, into_ptr(instr)));

        auto result_type = prog::type_local { make_ptr(prog::make_ptr_type(move(*value_type.tp), prog::ptr_type::UNIQUE, false)), false };

        return { result, move(result_type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_dereference(const ast::expr& ast) {
        prog::reg_index value;
        prog::type_local type;
        optional<prog::var_index> var_index;

        if (INDEX_EQ(ast, NAME) && (var_index = fclr.try_get_var(GET(ast, NAME))))
            tie(value, type) = function_utils(fclr).add_var_read(*var_index, true, ast.loc);
        else
            tie(value, type) = expression_compiler(fclr, true).compile(ast);

        auto [ptr_value, ptr_type] = function_utils(fclr).add_ptr_extraction(value, *type.tp, ast.loc);
        auto& type_pointed = *ptr_type.target_tp;
        auto& target_type = *type_pointed.tp;

        if (ptr_type.kind == prog::ptr_type::WEAK)
            error(diags::weak_pointer_dereference(ast.loc));
        if (type_pointed.slice)
            error(diags::slice_dereference(ast.loc));

        auto result = fclr.new_reg();
        auto read_instr = prog::ptr_read_instr { ptr_value, result };
        fclr.add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(read_instr)));

        if (!type_trivial(prog, target_type)) {
            if (confined)
                error(diags::not_allowed_in_confined_context(diags::value_kind::DEREFERENCE, ast.loc));

            if (type_copyable(prog, target_type))
                copy_generator(fclr, result).add(target_type);
            else if (ptr_type.kind == prog::ptr_type::UNIQUE && !INDEX_EQ(*type.tp, INNER_PTR) && var_index && !fclr.vars[*var_index].type.confined) {
                fclr.add_instr(VARIANT(prog::instr, DELETE, value));
                fclr.move_out_var(*var_index, ast.loc);
            } else
                error(diags::type_not_copyable(prog, move(target_type), ast.loc));
        }

        auto target_type_local = prog::type_local { into_ptr(target_type), false };

        return { result, move(target_type_local) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_weak_ptr_test(const ast::expr& ast) {
        prog::reg_index value;
        prog::type_local type_local;
        tie(value, type_local) = expression_compiler(fclr, true).compile(ast);
        auto& type = *type_local.tp;

        auto [ptr_value, ptr_type] = function_utils(fclr).add_ptr_extraction(value, type, ast.loc);

        if (ptr_type.kind != prog::ptr_type::WEAK)
            error(diags::invalid_type(prog, move(type), diags::type_kind::WEAK_POINTER, ast.loc));

        auto test_result = fclr.new_reg();
        auto test_instr = prog::test_ref_count_instr { ptr_value, test_result };
        fclr.add_instr(VARIANT(prog::instr, TEST_REF_COUNT, into_ptr(test_instr)));

        auto true_result = fclr.new_reg();
        auto false_result = fclr.new_reg();

        switch (INDEX(type)) {
            case prog::type::PTR:
                GET(type, PTR)->kind = prog::ptr_type::SHARED;
                break;
            case prog::type::INNER_PTR:
                GET(type, INNER_PTR)->kind = prog::ptr_type::SHARED;
                break;
            case prog::type::FUNC_WITH_PTR:
                GET(type, FUNC_WITH_PTR)->kind = prog::ptr_type::SHARED;
                break;
        }

        auto true_branch = [&] () {
            auto make_instr = prog::make_optional_instr { make_ptr(copy_type(type)), { value }, true_result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_OPTIONAL, into_ptr(make_instr)));
            if (!confined)
                fclr.add_instr(VARIANT(prog::instr, INCR_REF_COUNT, value));
        };

        auto false_branch = [&] () {
            auto make_instr = prog::make_optional_instr { make_ptr(copy_type(type)), { }, false_result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_OPTIONAL, into_ptr(make_instr)));
        };

        auto result = fclr.new_reg();
        auto branch_instr = function_utils(fclr).make_branch(test_result, true_branch, false_branch);
        auto value_branch_instr = prog::value_branch_instr { move(branch_instr), true_result, false_result, result };
        fclr.add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(value_branch_instr)));

        auto result_type = prog::type_local { make_ptr(VARIANT(prog::type, OPTIONAL, into_ptr(type))), confined };

        return { result, move(result_type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_heap_slice_alloc(const ast::heap_slice_alloc_expr& ast) {
        if (confined)
            error(diags::not_allowed_in_confined_context(diags::value_kind::ALLOCATION, ast.loc));

        auto& value_ast = *ast.value;
        auto& size_ast = *ast.size;

        auto [value, type_local] = expression_compiler(fclr, false).compile(value_ast);
        auto& type = *type_local.tp;

        if (type_local.confined && !type_trivial(prog, *type_local.tp))
            error(diags::confinement_mismatch(type_local.confined, ast.loc));

        auto [size_value, size_type] = expression_compiler(fclr, true).compile(size_ast);
        size_value = conversion_generator(fclr, size_value, prog::SIZE_TYPE).convert_from(size_type, size_ast.loc);

        if (!type_copyable(prog, type))
            error(diags::type_not_copyable(prog, move(type), value_ast.loc));

        fclr.push_frame();
        copy_generator(fclr, value).add(type);
        auto block = fclr.pop_frame();

        auto repeat_instr = prog::repeat_instr { size_value, fclr.new_reg(), into_ptr(block) };
        fclr.add_instr(VARIANT(prog::instr, REPEAT, into_ptr(repeat_instr)));

        deletion_generator(fclr, value).add(type);

        auto result = fclr.new_reg();
        auto instr = prog::alloc_slice_instr { value, size_value, result };
        fclr.add_instr(VARIANT(prog::instr, ALLOC_SLICE, into_ptr(instr)));

        auto result_type = prog::type_local { make_ptr(prog::make_ptr_type(move(type), prog::ptr_type::UNIQUE, true)), false };

        return { result, move(result_type) };
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_length(const ast::expr& ast) {
        auto [value, type_local] = expression_compiler(fclr, true).compile(ast);
        auto& type = *type_local.tp;

        if (INDEX_EQ(type, ARRAY)) {
            auto value = VARIANT(prog::constant, NUMBER, GET(type, ARRAY)->size);
            auto result = fclr.new_reg();
            auto make_instr = prog::make_const_instr { into_ptr(value), make_ptr(copy_type(prog::SIZE_TYPE)), result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(make_instr)));
            return { result, copy_type_local(prog::SIZE_TYPE_LOCAL) };
        }

        auto [ptr_value, ptr_type] = function_utils(fclr).add_ptr_extraction(value, type, ast.loc);
        auto& type_pointed = *ptr_type.target_tp;

        if (!type_pointed.slice && !INDEX_EQ(*type_pointed.tp, ARRAY))
            error(diags::invalid_type(prog, move(type), diags::type_kind::SLICE_OR_ARRAY_POINTER, ast.loc));

        auto result = fclr.new_reg();

        if (type_pointed.slice) {
            auto get_instr = prog::get_slice_length_instr { ptr_value, result };
            fclr.add_instr(VARIANT(prog::instr, GET_SLICE_LENGTH, into_ptr(get_instr)));
            return { result, copy_type_local(prog::SIZE_TYPE_LOCAL) };
        } else {
            auto value = VARIANT(prog::constant, NUMBER, GET(*type_pointed.tp, ARRAY)->size);
            auto result = fclr.new_reg();
            auto make_instr = prog::make_const_instr { into_ptr(value), make_ptr(copy_type(prog::SIZE_TYPE)), result };
            fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(make_instr)));
            return { result, copy_type_local(prog::SIZE_TYPE_LOCAL) };
        }
    }

    pair<prog::reg_index, prog::type_local> expression_compiler::compile_extraction(const ast::expr& expr_ast, const ast::extraction_expr& extr_ast) {
        switch (INDEX(extr_ast)) {
            case ast::extraction_expr::FIELD:
            case ast::extraction_expr::INDEX:
            case ast::extraction_expr::ITEM: {
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
                        UNREACHABLE;
                }

                auto result = fclr.new_reg();
                auto read_instr = prog::ptr_read_instr { target_ptr_value, result };
                fclr.add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(read_instr)));

                auto& value_type = *target_type.tp;

                if (!type_trivial(prog, value_type)) {
                    if (confined)
                        error(diags::not_allowed_in_confined_context(diags::value_kind::DEREFERENCE, extr_ast.loc));

                    if (type_copyable(prog, value_type))
                        copy_generator(fclr, result).add(value_type);
                    else
                        error(diags::type_not_copyable(prog, move(value_type), extr_ast.loc));
                }

                auto result_type = prog::type_local { into_ptr(value_type), false };

                return { result, move(result_type) };
            }

            case ast::extraction_expr::FIELD_REF:
            case ast::extraction_expr::INDEX_REF:
            case ast::extraction_expr::ITEM_REF:
            case ast::extraction_expr::ITEM_RANGE_REF: {
                auto [left_value, left_type_local] = expression_compiler(fclr, confined).compile(expr_ast);
                auto& left_type = *left_type_local.tp;

                auto [ptr_value, ptr_type] = function_utils(fclr).add_ptr_extraction(left_value, left_type, expr_ast.loc);
                auto slice = ptr_type.target_tp->slice;
                auto& type = *ptr_type.target_tp->tp;

                prog::reg_index target_ptr_value;
                prog::type_pointed target_type;

                switch (INDEX(extr_ast)) {
                    case ast::extraction_expr::FIELD_REF: {
                        auto name = GET(extr_ast, FIELD_REF);

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

                    case ast::extraction_expr::INDEX_REF: {
                        auto field = GET(extr_ast, INDEX_REF);

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

                    case ast::extraction_expr::ITEM_REF: {
                        auto& index_expr = *GET(extr_ast, ITEM_REF);

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

                    case ast::extraction_expr::ITEM_RANGE_REF: {
                        auto begin_expr = as_optional_cref(GET(extr_ast, ITEM_RANGE_REF).first);
                        auto end_expr = as_optional_cref(GET(extr_ast, ITEM_RANGE_REF).second);

                        if (!slice && !INDEX_EQ(type, ARRAY))
                            error(diags::invalid_type(prog, move(type), diags::type_kind::SLICE_OR_ARRAY_POINTER, expr_ast.loc));

                        prog::reg_index begin_value;
                        prog::reg_index end_value;

                        if (begin_expr) {
                            prog::type_local begin_type;
                            tie(begin_value, begin_type) = expression_compiler(fclr, true).compile(*begin_expr);
                            begin_value = conversion_generator(fclr, begin_value, prog::SIZE_TYPE).convert_from(begin_type, begin_expr->get().loc);
                        } else {
                            begin_value = fclr.new_reg();
                            auto zero = VARIANT(prog::constant, NUMBER, 0);
                            auto instr = prog::make_const_instr { into_ptr(zero), make_ptr(copy_type(prog::SIZE_TYPE)), begin_value };
                            fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(instr)));
                        }

                        if (end_expr) {
                            prog::type_local end_type;
                            tie(end_value, end_type) = expression_compiler(fclr, true).compile(*end_expr);
                            end_value = conversion_generator(fclr, end_value, prog::SIZE_TYPE).convert_from(end_type, end_expr->get().loc);
                        } else {
                            end_value = fclr.new_reg();
                            if (slice) {
                                auto instr = prog::get_slice_length_instr { ptr_value, end_value };
                                fclr.add_instr(VARIANT(prog::instr, GET_SLICE_LENGTH, into_ptr(instr)));
                            } else {
                                auto size = VARIANT(prog::constant, NUMBER, GET(type, ARRAY)->size);
                                auto instr = prog::make_const_instr { into_ptr(size), make_ptr(copy_type(prog::SIZE_TYPE)), end_value };
                                fclr.add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(instr)));
                            }
                        }

                        auto check_result = fclr.new_reg();
                        auto check_instr = prog::check_index_range_instr { ptr_value, begin_value, end_value, check_result };

                        if (slice)
                            fclr.add_instr(VARIANT(prog::instr, CHECK_SLICE_INDEX_RANGE, into_ptr(check_instr)));
                        else
                            fclr.add_instr(VARIANT(prog::instr, CHECK_ARRAY_PTR_INDEX_RANGE, into_ptr(check_instr)));

                        auto false_branch = [&] () {
                            fclr.add_instr(VARIANT(prog::instr, ABORT, monostate()));
                            fclr.returned = true;
                        };

                        function_utils(fclr).add_branch(check_result, [] { }, false_branch);

                        target_ptr_value = fclr.new_reg();

                        if (slice)
                            target_type = prog::type_pointed { into_ptr(type), true };
                        else
                            target_type = prog::type_pointed { into_ptr(*GET(type, ARRAY)->tp), true };

                        auto instr = prog::get_item_range_slice_instr { ptr_value, begin_value, end_value, target_ptr_value };
                        fclr.add_instr(VARIANT(prog::instr, GET_ITEM_RANGE_SLICE, into_ptr(instr)));
                    } break;

                    default:
                        UNREACHABLE;
                }

                prog::reg_index owner_ptr_value;
                prog::type_pointed owner_type;

                if (INDEX_EQ(left_type, PTR)) {
                    owner_ptr_value = left_value;
                    owner_type = move(*GET(left_type, PTR)->target_tp);
                } else if (INDEX_EQ(left_type, FUNC_WITH_PTR)) {
                    owner_ptr_value = fclr.new_reg();
                    auto instr = prog::ptr_conversion_instr { left_value, owner_ptr_value };
                    fclr.add_instr(VARIANT(prog::instr, EXTRACT_VALUE_PTR, into_ptr(instr)));
                    owner_type = move(*GET(left_type, FUNC_WITH_PTR)->target_tp);
                } else if (INDEX_EQ(left_type, INNER_PTR)) {
                    owner_ptr_value = fclr.new_reg();
                    auto instr = prog::ptr_conversion_instr { left_value, owner_ptr_value };
                    fclr.add_instr(VARIANT(prog::instr, EXTRACT_OUTER_PTR, into_ptr(instr)));
                    owner_type = move(*GET(left_type, INNER_PTR)->owner_tp);
                } else
                    UNREACHABLE;

                auto result = fclr.new_reg();
                auto make_instr = prog::make_joint_inner_ptr_instr { target_ptr_value, owner_ptr_value, result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_JOINT_INNER_PTR, into_ptr(make_instr)));

                auto inner = prog::inner_ptr_type { { ptr_type.kind, into_ptr(target_type) }, into_ptr(owner_type) };
                auto result_type = VARIANT(prog::type, INNER_PTR, into_ptr(inner));
                auto result_type_local = prog::type_local { into_ptr(result_type), left_type_local.confined };

                return { result, move(result_type_local) };
            }

            case ast::extraction_expr::OWNER_REF: {
                auto [value, type_local] = expression_compiler(fclr, confined).compile(expr_ast);
                auto& type = *type_local.tp;

                if (!INDEX_EQ(type, INNER_PTR))
                    error(diags::invalid_type(prog, move(type), diags::type_kind::INNER_POINTER, expr_ast.loc));

                auto result = fclr.new_reg();
                auto instr = prog::ptr_conversion_instr { value, result };
                fclr.add_instr(VARIANT(prog::instr, EXTRACT_OUTER_PTR, into_ptr(instr)));

                auto& inner = *GET(type, INNER_PTR);
                auto result_type = VARIANT(prog::type, PTR, make_ptr(prog::ptr_type { inner.kind, move(inner.owner_tp) }));
                auto result_type_local = prog::type_local { into_ptr(result_type), type_local.confined };

                return { value, move(result_type_local) };
            }
        }

        UNREACHABLE;
    }

    tuple<vector<cref<ast::expr>>, vector<prog::reg_index>, vector<prog::type>, bool> expression_compiler::compile_args(
            vector<cref<ast::expr_marked>> asts,
            optional<function<size_t(string, location)>> arg_with_name,
            optional<size_t> expected_count,
            location loc) {
        auto value_asts = compiler_utils(clr).order_args(asts, arg_with_name, expected_count, loc);

        vector<prog::reg_index> values;
        vector<prog::type> types;
        optional<bool> all_confined;

        for (auto& value_ast : value_asts) {
            auto [value, type] = expression_compiler(fclr, confined).compile(value_ast);

            if (!type_trivial(prog, *type.tp)) {
                if (!all_confined)
                    all_confined = { type.confined };
                else if (type.confined != *all_confined)
                    error(diags::confinement_ambiguous(value_ast.get().loc));
            }

            values.push_back(value);
            types.push_back(move(*type.tp));
        }

        if (!all_confined)
            all_confined = { confined };

        return { value_asts, values, move(types), *all_confined };
    }

    vector<prog::reg_index> expression_compiler::compile_call_args(
            vector<cref<ast::expr_marked>> asts,
            const prog::func_type& ftype,
            optional<cref<prog::global_func>> func,
            location loc) {
        auto size = ftype.param_tps.size();

        auto arg_with_name = [&] (string name, location loc) -> size_t {
            auto iter = func->get().param_names.find(name);
            if (iter == func->get().param_names.end())
                error(diags::unknown_function_parameter(*func, name, loc));
            return iter->second;
        };

        auto value_asts = compiler_utils(clr).order_args(asts, func ? make_optional(arg_with_name) : optional<decltype(arg_with_name)>(), { size }, loc);

        vector<prog::reg_index> values;
        vector<prog::type_local> types;

        for (size_t index = 0; index < size; index++) {
            auto& value_ast = value_asts[index];
            auto confined = ftype.param_tps[index]->confined;
            auto [value, type] = expression_compiler(fclr, confined).compile(value_ast);
            values.push_back(value);
            types.push_back(move(type));
        }

        for (size_t index = 0; index < size; index++) {
            auto& type = types[index];
            auto& param_type = *ftype.param_tps[index];
            values[index] = conversion_generator(fclr, values[index], param_type).convert_from(type, value_asts[index].get().loc);
        }

        return values;
    }
}
