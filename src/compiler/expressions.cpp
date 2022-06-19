#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    pair<prog::reg_index, prog::type_local> function_compiler::compile_expr(const ast::expr& ast, bool confined) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
                return compile_tuple(as_cref_vector(GET(ast, TUPLE)), confined, ast.loc);

            case ast::expr::ARRAY:
                return compile_array(as_cref_vector(GET(ast, ARRAY)), confined, ast.loc);

            case ast::expr::APPLICATION: {
                auto& [receiver_ast_ptr, arg_ast_ptrs] = GET(ast, APPLICATION);
                return compile_application(*receiver_ast_ptr, as_cref_vector(arg_ast_ptrs), confined, ast.loc);
            }

            case ast::expr::NAME: {
                auto& name = GET(ast, NAME);

                auto var_index = try_get_var(name);
                if (var_index)
                    return add_var_read(*var_index, confined, ast.loc);

                return compile_global_name(name, confined, ast.loc);
            }

            case ast::expr::VARIANT_NAME: {
                auto& [name, variant_name] = GET(ast, VARIANT_NAME);
                return compile_variant_name(name, variant_name, confined, ast.loc);
            }

            case ast::expr::LITERAL: {
                auto& literal_ast = *GET(ast, LITERAL);
                auto [value, value_type] = clr.compile_const_literal(literal_ast);

                auto result = new_reg();
                auto instr = prog::make_const_instr { into_ptr(value), result };
                add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(instr)));

                auto type = prog::type_local { into_ptr(value_type), false };
                return { result, move(type) };
            }

            case ast::expr::UNARY_OPERATION:
                return compile_unary_operation(*GET(ast, UNARY_OPERATION));

            case ast::expr::BINARY_OPERATION:
                return compile_binary_operation(*GET(ast, BINARY_OPERATION));

            case ast::expr::NUMERIC_CAST:
                return compile_numeric_cast(*GET(ast, NUMERIC_CAST));

            case ast::expr::NONE: {
                auto result = new_reg();
                auto instr = prog::make_optional_instr { { }, result };
                add_instr(VARIANT(prog::instr, MAKE_OPTIONAL, into_ptr(instr)));
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, NEVER, monostate())))), false };
                return { result, move(type) };
            }

            case ast::expr::SOME: {
                auto& expr_ast = *GET(ast, SOME);
                auto [value, value_type] = compile_expr(expr_ast, confined);
                auto result = new_reg();
                auto instr = prog::make_optional_instr { { value }, result };
                add_instr(VARIANT(prog::instr, MAKE_OPTIONAL, into_ptr(instr)));
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, OPTIONAL, move(value_type.tp))), value_type.confined };
                return { result, move(type) };
            }

            case ast::expr::RETURN: {
                auto return_expr = as_optional_cref(GET(ast, RETURN));
                return compile_return(return_expr, return_expr ? (*return_expr).get().loc : ast.loc);
            }

            case ast::expr::BREAK: {
                add_break(ast.loc);
                return { new_unit_reg(), copy_type_local(prog::NEVER_TYPE) };
            }

            case ast::expr::CONTINUE: {
                add_continue(ast.loc);
                return { new_unit_reg(), copy_type_local(prog::NEVER_TYPE) };
            }

            case ast::expr::CONDITIONAL: {
                auto& conditional_expr = *GET(ast, CONDITIONAL);
                return compile_conditional(conditional_expr, confined);
            }

            case ast::expr::GLOBAL_REF: {
                auto name = GET(ast, GLOBAL_REF);
                auto var_index = clr.get_global_name(name, { global_name_kind::VAR }, ast.loc).index;

                auto& var_type = *clr.prog.global_vars[var_index]->tp;
                auto type = prog::type_local { make_ptr(prog::make_ptr_type(copy_type(var_type), prog::ptr_type::GLOBAL, false)), false };

                auto result = new_reg();
                auto instr = prog::get_global_ptr_instr { var_index, result };
                add_instr(VARIANT(prog::instr, GET_GLOBAL_VAR_PTR, into_ptr(instr)));

                return { result, move(type) };
            }

            case ast::expr::HEAP_ALLOC: {
                if (confined)
                    error(diags::allocation_in_confined_context(), ast.loc);

                auto [value, value_type] = compile_expr(*GET(ast, HEAP_ALLOC), false);
                auto type = prog::type_local { make_ptr(prog::make_ptr_type(move(*value_type.tp), prog::ptr_type::UNIQUE, false)), false };

                auto result = new_reg();
                auto instr = prog::alloc_instr { value, result };
                add_instr(VARIANT(prog::instr, ALLOC, into_ptr(instr)));

                return { result, move(type) };
            }

            case ast::expr::DEREFERENCE: {
                auto& expr = *GET(ast, DEREFERENCE);
                return compile_dereference(expr, confined);
            }

            case ast::expr::TEST:
            case ast::expr::SIZED_ARRAY:
            case ast::expr::HEAP_SLICE_ALLOC:
            case ast::expr::LENGTH:
            case ast::expr::EXTRACT:
            case ast::expr::PTR_EXTRACT:
            case ast::expr::LAMBDA:
                error(diags::not_implemented(), ast.loc); // TODO

            default:
                error(diags::invalid_expression(), ast.loc);
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_global_name(string name, bool confined, location loc) {
        auto& gname = clr.get_global_name(name, loc);

        switch (gname.kind) {
            case global_name_kind::VAR: {
                auto& var = *clr.prog.global_vars[gname.index];

                auto result = new_reg();
                auto instr = prog::read_global_var_instr { gname.index, result };
                add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(instr)));

                if (!confined) {
                    if (clr.type_copyable(*var.tp))
                        add_copy(result, *var.tp);
                    else
                        error(diags::global_variable_moved(name), loc);
                }

                auto type = prog::type_local { make_ptr(copy_type(*var.tp)), confined };
                return { result, move(type) };
            }

            case global_name_kind::CONST: {
                auto& value = clr.consts[gname.index];

                auto result = new_reg();
                auto instr = prog::make_const_instr { make_ptr(copy_const(*value.value)), result };
                add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(instr)));

                auto type = prog::type_local { make_ptr(copy_type(*value.tp)), confined };
                return { result, move(type) };
            }

            case global_name_kind::FUNC: {
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, KNOWN_FUNC, gname.index)), false };
                return { new_unit_reg(), move(type) };
            }

            case global_name_kind::STRUCT: {
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, STRUCT_CTOR, gname.index)), false };
                return { new_unit_reg(), move(type) };
            }

            case global_name_kind::ENUM:
                error(diags::invalid_kind(name, global_name_kind::ENUM, { }), loc);
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_variant_name(string name, string variant_name, bool confined, location loc) {
        if (try_get_var(name))
            error(diags::expected_enum_name(), loc);

        auto enum_index = clr.get_global_name(name, { global_name_kind::ENUM }, loc).index;
        auto& en = *clr.prog.enum_types[enum_index];

        auto iter = en.variant_names.find(variant_name);
        if (iter == en.variant_names.end())
            error(diags::unknown_enum_variant(en, name), loc);
        auto variant_index = iter->second;
        auto& variant = *en.variants[variant_index];

        if (variant.tps.empty()) {
            auto result = new_reg();
            auto instr = prog::make_enum_variant_instr { enum_index, variant_index, { }, result };
            add_instr(VARIANT(prog::instr, MAKE_ENUM_VARIANT, into_ptr(instr)));
            auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM, enum_index)), confined };
            return { result, move(type) };
        }

        auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM_CTOR, make_pair(enum_index, variant_index))), false };
        return { new_unit_reg(), move(type) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_return(optional<cref<ast::expr>> ast, location loc) {
        prog::reg_index result;
        prog::type_local type;

        if (ast)
            tie(result, type) = compile_expr(*ast, false);
        else {
            result = new_unit_reg();
            type = copy_type_local(prog::UNIT_TYPE);
        }

        result = conv_clr.convert(result, type, *func.return_tp, loc);

        add_return(result, loc);

        return { result, copy_type_local(prog::NEVER_TYPE) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_tuple(vector<cref<ast::expr_marked>> asts, bool confined, location loc) {
        auto [value_asts, values, types, all_confined] = compile_args(asts, { }, { }, confined, loc);
        auto count = values.size();

        if (count == 0)
            return { new_unit_reg(), copy_type_local(prog::UNIT_TYPE) };

        if (count == 1) {
            auto result = values[0];
            auto type = prog::type_local { into_ptr(types[0]), all_confined };
            return { result, move(type) };
        }

        auto result = new_reg();
        auto instr = prog::make_tuple_instr { values, result };
        add_instr(VARIANT(prog::instr, MAKE_TUPLE, into_ptr(instr)));
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, TUPLE, into_ptr_vector(types))), all_confined };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_array(vector<cref<ast::expr_marked>> asts, bool confined, location loc) {
        auto [value_asts, values, types, all_confined] = compile_args(asts, { }, { }, confined, loc);
        auto count = values.size();

        auto common_type = VARIANT(prog::type, NEVER, monostate());

        for (auto& type : types)
            common_type = clr.common_supertype(common_type, type, loc);

        for (size_t index = 0; index < count; index++)
            values[index] = conv_clr.convert(values[index], types[index], common_type, all_confined, value_asts[index].get().loc);

        auto result = new_reg();
        auto instr = prog::make_array_instr { values, result };
        add_instr(VARIANT(prog::instr, MAKE_ARRAY, into_ptr(instr)));
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(common_type), count }))), all_confined };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, bool confined, location loc) {
        auto[receiver, receiver_type_local] = compile_expr(receiver_ast, true);
        auto& receiver_type = *receiver_type_local.tp;

        switch (INDEX(receiver_type)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(receiver_type, STRUCT_CTOR);
                auto& st = *clr.prog.struct_types[struct_index];
                auto size = st.fields.size();

                auto arg_with_name = [&] (string name, location loc) -> size_t {
                    auto iter = st.field_names.find(name);
                    if (iter == st.field_names.end())
                        error(diags::unknown_struct_field(st, name), loc);
                    return iter->second;
                };

                auto [value_asts, values, types, all_confined] = compile_args(arg_asts, { arg_with_name }, { size }, confined, loc);

                for (size_t index = 0; index < size; index++) {
                    auto& type = types[index];
                    auto& field_type = *st.fields[index]->tp;
                    values[index] = conv_clr.convert(values[index], type, field_type, all_confined, value_asts[index].get().loc);
                }

                auto result = new_reg();
                auto instr = prog::make_struct_instr { struct_index, values, result };
                add_instr(VARIANT(prog::instr, MAKE_STRUCT, into_ptr(instr)));
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, STRUCT, struct_index)), all_confined };
                return { result, move(type) };
            }

            case prog::type::ENUM_CTOR: {
                auto [enum_index, variant_index] = GET(receiver_type, ENUM_CTOR);
                auto& en = *clr.prog.enum_types[enum_index];
                auto& variant = *en.variants[variant_index];
                auto size = variant.tps.size();

                auto [value_asts, values, types, all_confined] = compile_args(arg_asts, { }, { size }, confined, loc);

                for (size_t index = 0; index < size; index++) {
                    auto& type = types[index];
                    auto& field_type = *variant.tps[index];
                    values[index] = conv_clr.convert(values[index], type, field_type, all_confined, value_asts[index].get().loc);
                }

                auto result = new_reg();
                auto instr = prog::make_enum_variant_instr { enum_index, variant_index, values, result };
                add_instr(VARIANT(prog::instr, MAKE_ENUM_VARIANT, into_ptr(instr)));
                auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM, enum_index)), all_confined };
                return { result, move(type) };
            }

            case prog::type::FUNC:
            case prog::type::FUNC_WITH_PTR: {
                auto& ftype = INDEX_EQ(receiver_type, FUNC) ? *GET(receiver_type, FUNC) : *GET(receiver_type, FUNC_WITH_PTR);

                if (confined && !clr.type_trivial(*ftype.return_tp))
                    error(diags::function_call_in_confined_context(), loc);

                auto args = compile_call_args(arg_asts, ftype, { }, loc);

                auto ptr = new_reg();
                auto extract_ptr_instr = prog::ptr_conversion_instr { receiver, ptr };
                add_instr(VARIANT(prog::instr, EXTRACT_PTR, into_ptr(extract_ptr_instr)));
                args.insert(args.begin(), ptr);

                auto func = new_reg();
                auto extract_func_instr = prog::ptr_conversion_instr { receiver, func };
                add_instr(VARIANT(prog::instr, EXTRACT_FUNC_PTR, into_ptr(extract_func_instr)));

                auto result = new_reg();
                auto instr = prog::func_ptr_call_instr { func, args, result };
                add_instr(VARIANT(prog::instr, FUNC_PTR_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*ftype.return_tp)), false };
                return { result, move(type) };
            }

            case prog::type::GLOBAL_FUNC: {
                auto& ftype = *GET(receiver_type, GLOBAL_FUNC);

                if (confined && !clr.type_trivial(*ftype.return_tp))
                    error(diags::function_call_in_confined_context(), loc);

                auto args = compile_call_args(arg_asts, ftype, { }, loc);

                auto result = new_reg();
                auto instr = prog::func_ptr_call_instr { receiver, args, result };
                add_instr(VARIANT(prog::instr, FUNC_PTR_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*ftype.return_tp)), false };
                return { result, move(type) };
            }

            case prog::type::KNOWN_FUNC: {
                auto func_index = GET(receiver_type, KNOWN_FUNC);
                auto& func = *clr.prog.global_funcs[func_index];
                auto ftype = prog::get_func_type(func);

                if (confined && !clr.type_trivial(*ftype.return_tp))
                    error(diags::function_call_in_confined_context(), loc);

                auto args = compile_call_args(arg_asts, ftype, { func }, loc);

                auto result = new_reg();
                auto instr = prog::func_call_instr { func_index, args, result };
                add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*func.return_tp)), false };
                return { result, move(type) };
            }

            default:
                error(diags::invalid_expression(), loc);
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_unary_operation(const ast::unary_operation_expr& ast) {
        auto [value, type] = compile_expr(*ast.value, true);
        auto result = new_reg();

        using num = prog::number_type;
        using unop = ast::unary_operation_expr;

        switch (ast.operation) {
            case unop::NOT: {
                value = conv_clr.convert(value, type, prog::BOOL_TYPE, ast.value->loc);
                auto instr = prog::unary_operation_instr { value, result };
                add_instr(VARIANT(prog::instr, BOOL_NOT, into_ptr(instr)));
                return { result, copy_type_local(prog::BOOL_TYPE) };
            }

            case unop::MINUS: {
                if (!INDEX_EQ(*type.tp, NUMBER))
                    error(diags::invalid_unary_operation(clr.prog, ast.operation, move(*type.tp)), ast.loc);
                switch (GET(*type.tp, NUMBER)->tp) {
                    case num::I8:
                    case num::I16:
                    case num::I32:
                    case num::I64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, INT_NEG, into_ptr(instr)));
                    } break;

                    case num::F32:
                    case num::F64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, FLOAT_NEG, into_ptr(instr)));
                    } break;

                    default:
                        error(diags::invalid_unary_operation(clr.prog, ast.operation, move(*type.tp)), ast.loc);
                }
                return { result, move(type) };
            }

            case unop::BIT_NEG: {
                if (!INDEX_EQ(*type.tp, NUMBER))
                    error(diags::invalid_unary_operation(clr.prog, ast.operation, move(*type.tp)), ast.loc);
                switch (GET(*type.tp, NUMBER)->tp) {
                    case num::I8:
                    case num::I16:
                    case num::I32:
                    case num::I64:
                    case num::U8:
                    case num::U16:
                    case num::U32:
                    case num::U64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, BIT_NEG, into_ptr(instr)));
                        return { result, move(type) };
                    }

                    default:
                        error(diags::invalid_unary_operation(clr.prog, ast.operation, move(*type.tp)), ast.loc);
                }
            }
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_binary_operation(const ast::binary_operation_expr& ast) {
        #define INVALID_BINARY_OP { \
            error(diags::invalid_binary_operation(clr.prog, ast.operation, \
                copy_type(*left_type.tp), copy_type(*right_type.tp)), ast.loc); \
        }

        using num = prog::number_type;

        auto is_uint = [&] (const prog::number_type& tp) -> bool {
            switch (tp.tp) {
                case num::U8:
                case num::U16:
                case num::U32:
                case num::U64:
                    return true;
                default:
                    return false;
            }
        };

        auto is_sint = [&] (const prog::number_type& tp) -> bool {
            switch (tp.tp) {
                case num::I8:
                case num::I16:
                case num::I32:
                case num::I64:
                    return true;
                default:
                    return false;
            }
        };

        auto is_float = [&] (const prog::number_type& tp) -> bool {
            switch (tp.tp) {
                case num::F32:
                case num::F64:
                    return true;
                default:
                    return false;
            }
        };

        using binop = ast::binary_operation_expr;

        switch (ast.operation) {
            case binop::AND:
            case binop::OR: {
                auto[left_value, left_type] = compile_expr(*ast.left, true);
                left_value = conv_clr.convert(left_value, left_type, prog::BOOL_TYPE, ast.left->loc);
                prog::reg_index right_value;
                auto result = new_reg();

                auto short_branch = [&](){};

                auto long_branch = [&](){
                    auto[right_raw_value, right_type] = compile_expr(*ast.right, true);
                    right_value = conv_clr.convert(right_raw_value, right_type, prog::BOOL_TYPE, ast.right->loc);
                };

                if (ast.operation == binop::AND) {
                    auto branch_instr = make_branch(left_value, long_branch, short_branch);
                    auto value_branch_instr = prog::value_branch_instr{ move(branch_instr), right_value, left_value, result };
                    add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(value_branch_instr)));
                } else {
                    auto branch_instr = make_branch(left_value, short_branch, long_branch);
                    auto value_branch_instr = prog::value_branch_instr{ move(branch_instr), left_value, right_value, result };
                    add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(value_branch_instr)));
                }

                return { result, copy_type_local(prog::BOOL_TYPE) };
            } break;

            case binop::EQ:
            case binop::NEQ: {
                auto [left_value, left_type] = compile_expr(*ast.left, true);
                auto [right_value, right_type] = compile_expr(*ast.right, true);

                auto common_type = clr.common_supertype(*left_type.tp, *right_type.tp, ast.loc);

                left_value = conv_clr.convert(left_value, *left_type.tp, common_type, ast.left->loc);
                right_value = conv_clr.convert(right_value, *right_type.tp, common_type, ast.right->loc);

                auto result = new_reg();
                auto instr = prog::binary_operation_instr { left_value, right_value, result };

                if (ast.operation == binop::EQ)
                    add_instr(VARIANT(prog::instr, EQ, into_ptr(instr)));
                else if (ast.operation == binop::NEQ)
                    add_instr(VARIANT(prog::instr, NEQ, into_ptr(instr)));

                return { result, copy_type_local(prog::BOOL_TYPE) };
            } break;

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
                auto[left_value, left_type] = compile_expr(*ast.left, true);
                auto[right_value, right_type] = compile_expr(*ast.right, true);
                auto common_type = clr.common_supertype(*left_type.tp, *right_type.tp, ast.loc);

                if (!INDEX_EQ(common_type, NUMBER))
                    INVALID_BINARY_OP;

                left_value = conv_clr.convert(left_value, *left_type.tp, common_type, ast.left->loc);
                right_value = conv_clr.convert(right_value, *right_type.tp, common_type, ast.right->loc);
                auto result = new_reg();
                prog::numeric_binary_operation_instr::kind_t kind;
                auto& ntype = *GET(common_type, NUMBER);

                if (is_uint(ntype))
                    kind = prog::numeric_binary_operation_instr::UNSIGNED;
                else if (is_sint(ntype))
                    kind = prog::numeric_binary_operation_instr::SIGNED;
                else if (is_float(ntype))
                    kind = prog::numeric_binary_operation_instr::FLOAT;
                else
                    INVALID_BINARY_OP;

                auto op_instr = make_ptr(prog::numeric_binary_operation_instr{ { left_value, right_value, result }, kind });
                bool bool_result_type = false;

                switch (ast.operation) {
                    case binop::LS: {
                        add_instr(VARIANT(prog::instr, LS, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::LSEQ: {
                        add_instr(VARIANT(prog::instr, LSEQ, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::GT: {
                        add_instr(VARIANT(prog::instr, GT, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::GTEQ: {
                        add_instr(VARIANT(prog::instr, GTEQ, move(op_instr)));
                        bool_result_type = true;
                    } break;

                    case binop::ADD:
                        add_instr(VARIANT(prog::instr, ADD, move(op_instr)));
                        break;

                    case binop::SUB:
                        add_instr(VARIANT(prog::instr, SUB, move(op_instr)));
                        break;

                    case binop::MUL:
                        add_instr(VARIANT(prog::instr, MUL, move(op_instr)));
                        break;

                    case binop::DIV:
                        add_instr(VARIANT(prog::instr, DIV, move(op_instr)));
                        break;

                    case binop::MOD:
                        add_instr(VARIANT(prog::instr, MOD, move(op_instr)));
                        break;

                    case binop::BIT_AND: {
                        if (is_float(ntype))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_AND, move(op_instr)));
                    } break;

                    case binop::BIT_OR: {
                        if (is_float(ntype))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_OR, move(op_instr)));
                    } break;

                    case binop::BIT_XOR: {
                        if (is_float(ntype))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_XOR, move(op_instr)));
                    } break;

                    default:
                        UNREACHABLE;
                }

                if (bool_result_type)
                    return { result, copy_type_local(prog::BOOL_TYPE) };
                else
                    return { result, prog::type_local{ into_ptr(common_type), false } };
            }

            case binop::BIT_LSH:
            case binop::BIT_RSH: {
                auto[left_value, left_type] = compile_expr(*ast.left, true);
                auto[right_value, right_type] = compile_expr(*ast.right, true);

                if (!INDEX_EQ(*left_type.tp, NUMBER) || !INDEX_EQ(*right_type.tp, NUMBER) || !is_uint(*GET(*right_type.tp, NUMBER)))
                    INVALID_BINARY_OP;

                auto result = new_reg();
                prog::numeric_binary_operation_instr::kind_t kind;
                auto& left_ntype = *GET(*left_type.tp, NUMBER);

                if (is_uint(left_ntype))
                    kind = prog::numeric_binary_operation_instr::UNSIGNED;
                else if (is_sint(left_ntype))
                    kind = prog::numeric_binary_operation_instr::SIGNED;
                else
                    INVALID_BINARY_OP;

                auto op_instr = make_ptr(prog::numeric_binary_operation_instr{ {left_value, right_value, result }, kind });

                if (ast.operation == binop::BIT_LSH)
                    add_instr(VARIANT(prog::instr, BIT_LSH, move(op_instr)));
                else
                    add_instr(VARIANT(prog::instr, BIT_RSH, move(op_instr)));

                return { result, move(left_type) };
            }
        }

        UNREACHABLE;

        #undef INVALID_BINARY_OP
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_numeric_cast(const ast::numeric_cast_expr& ast) {
        auto [value, type] = compile_expr(*ast.value, true);
        auto new_type = clr.compile_type_local(*ast.tp, false);

        if (!INDEX_EQ(*type.tp, NUMBER))
            error(diags::expected_number_type(clr.prog, move(*type.tp)), ast.value->loc);
        if (!INDEX_EQ(*new_type.tp, NUMBER))
            error(diags::expected_number_type(clr.prog, move(*new_type.tp)), ast.loc);

        auto& ntype = *GET(*type.tp, NUMBER);
        auto& new_ntype = *GET(*type.tp, NUMBER);

        #define PASS { \
            return { value, move(type) }; \
        }

        #define CAST(instr_name) { \
            auto result = new_reg(); \
            auto instr = prog::numeric_conversion_instr { value, into_ptr(new_ntype), result }; \
            add_instr(VARIANT(prog::instr, instr_name, into_ptr(instr))); \
            return { result, move(new_type) }; \
        }

        using num = prog::number_type;

        switch (ntype.tp) {
            case num::BOOL: {
                switch (new_ntype.tp) {
                    case num::BOOL:
                        PASS;
                    case num::I8: case num::I16: case num::I32: case num::I64: case num::U8: case num::U16: case num::U32: case num::U64:
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
                    case num::U16: case num::U32: case num::U64:
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
                    case num::BOOL: case num::I8: case num::I16: case num::U8: case num::U16:
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
                    case num::BOOL: case num::I8: case num::I16: case num::I32: case num::U8: case num::U16: case num::U32:
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
                    case num::I16: case num::I32: case num::I64: case num::U16: case num::U32: case num::U64:
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
                    case num::I32: case num::I64: case num::U32: case num::U64:
                        CAST(ZERO_EXT);
                    case num::F32: case num::F64:
                        CAST(UINT_TO_FLOAT);
                }
            } break;

            case num::U32: {
                switch (new_ntype.tp) {
                    case num::I32: case num::U32:
                        PASS;
                    case num::BOOL: case num::I8: case num::I16: case num::U8: case num::U16:
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
                    case num::BOOL: case num::I8: case num::I16: case num::I32: case num::U8: case num::U16: case num::U32:
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
                    case num::BOOL: case num::U8: case num::U16: case num::U32: case num::U64:
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
                    case num::BOOL: case num::U8: case num::U16: case num::U32: case num::U64:
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

    pair<prog::reg_index, prog::type_local> function_compiler::compile_conditional(const ast::conditional_expr& ast, bool confined) {
        auto [value, type] = compile_expr(*ast.value, true);
        auto cond = conv_clr.convert(value, type, prog::BOOL_TYPE, ast.value->loc);

        prog::reg_index true_value, false_value;
        prog::type_local true_type, false_type;

        auto true_branch = [&] () {
            tie(true_value, true_type) = compile_expr(*ast.true_result, confined);
        };

        auto false_branch = [&] () {
            tie(false_value, false_type) = compile_expr(*ast.false_result, confined);
        };

        auto result = new_reg();
        auto branch_instr = make_branch(cond, true_branch, false_branch);
        auto value_branch_instr = prog::value_branch_instr { move(branch_instr), true_value, false_value, result };
        add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(value_branch_instr)));

        optional<bool> both_confined;

        if (!clr.type_trivial(*true_type.tp))
            both_confined = { true_type.confined };

        if (!clr.type_trivial(*false_type.tp)) {
            if (!both_confined)
                both_confined = { false_type.confined };
            else if (false_type.confined != *both_confined)
                error(diags::confinement_ambiguous(), ast.loc);
        }

        auto common_type = clr.common_supertype(*true_type.tp, *false_type.tp, ast.loc);
        auto common_type_local = prog::type_local { into_ptr(common_type), both_confined ? *both_confined : false };

        prog::reg_index true_conv_value, false_conv_value;

        auto true_conv_branch = [&] () {
            true_conv_value = conv_clr.convert(result, true_type, common_type_local, ast.true_result->loc);
        };

        auto false_conv_branch = [&] () {
            false_conv_value = conv_clr.convert(result, false_type, common_type_local, ast.false_result->loc);
        };

        auto conv_result = new_reg();
        auto conv_branch_instr = make_branch(cond, true_conv_branch, false_conv_branch);
        auto conv_value_branch_instr = prog::value_branch_instr { move(conv_branch_instr), true_conv_value, false_conv_value, conv_result };
        add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(conv_value_branch_instr)));

        return { conv_result, move(common_type_local) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_dereference(const ast::expr& ast, bool confined) {
        prog::reg_index value;
        prog::type_local type_local;
        optional<prog::var_index> var_index;

        if (INDEX_EQ(ast, NAME) && (var_index = try_get_var(GET(ast, NAME))))
            tie(value, type_local) = add_var_read(*var_index, true, ast.loc);
        else
            tie(value, type_local) = compile_expr(ast, true);

        auto& type = *type_local.tp;

        switch (INDEX(type)) {
            case prog::type::OPTIONAL: {
                auto inner_type = prog::type_local { make_ptr(copy_type(*GET(type, OPTIONAL))), type_local.confined };

                auto test_result = new_reg();
                auto test_instr = prog::test_optional_instr { value, test_result };
                add_instr(VARIANT(prog::instr, TEST_OPTIONAL, into_ptr(test_instr)));

                auto true_result = new_reg();
                auto false_result = new_unit_reg();

                auto true_branch = [&] () {
                    auto extract_instr = prog::extract_optional_value_instr { value, true_result };
                    add_instr(VARIANT(prog::instr, EXTRACT_OPTIONAL_VALUE, into_ptr(extract_instr)));
                };

                auto false_branch = [&] () {
                    add_instr(VARIANT(prog::instr, ABORT, monostate()));
                };

                auto result = new_reg();
                auto branch_instr = make_branch(test_result, true_branch, false_branch);
                auto value_branch_instr = prog::value_branch_instr { move(branch_instr), true_result, false_result, result };
                add_instr(VARIANT(prog::instr, VALUE_BRANCH, into_ptr(value_branch_instr)));

                return { result, move(inner_type) };
            }

            case prog::type::PTR:
            case prog::type::INNER_PTR:
            case prog::type::FUNC_WITH_PTR: {
                auto ptr_value = value;
                prog::ptr_type* ptr_type_ptr;

                if (INDEX_EQ(type, PTR))
                    ptr_type_ptr = GET(type, PTR).get();
                if (INDEX_EQ(type, INNER_PTR)) {
                    ptr_type_ptr = GET(type, INNER_PTR).get();
                    ptr_value = new_reg();
                    auto extract_instr = prog::ptr_conversion_instr { value, ptr_value };
                    add_instr(VARIANT(prog::instr, EXTRACT_INNER_PTR, into_ptr(extract_instr)));
                } else if (INDEX_EQ(type, FUNC_WITH_PTR)) {
                    ptr_type_ptr = GET(type, FUNC_WITH_PTR).get();
                    ptr_value = new_reg();
                    auto extract_instr = prog::ptr_conversion_instr { value, ptr_value };
                    add_instr(VARIANT(prog::instr, EXTRACT_PTR, into_ptr(extract_instr)));
                }

                auto& ptr_type = *ptr_type_ptr;
                auto& type_pointed = *ptr_type.target_tp;
                auto& target_type = *type_pointed.tp;

                if (type_pointed.slice)
                    error(diags::slice_dereference(), ast.loc);

                auto result = new_reg();
                auto deref_instr = prog::deref_instr { ptr_value, result };
                add_instr(VARIANT(prog::instr, DEREF, into_ptr(deref_instr)));

                if (!confined && !clr.type_trivial(target_type)) {
                    if (clr.type_copyable(target_type))
                        add_copy(result, target_type);
                    else if (ptr_type.kind == prog::ptr_type::UNIQUE && var_index && !vars[*var_index].type.confined) {
                        add_deletion(value, type);
                        move_out_var(*var_index, ast.loc);
                    } else
                        error(diags::type_not_copyable(clr.prog, move(target_type)), ast.loc);
                }

                auto target_type_local = prog::type_local { into_ptr(target_type), confined };

                return { result, move(target_type_local) };
            }

            default:
                error(diags::invalid_dereference_type(clr.prog, move(type)), ast.loc);
        }
    }

    function_compiler::lvalue function_compiler::compile_left_expr(const ast::expr& ast, optional<cref<prog::type_local>> implicit_type) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
                return compile_left_tuple(as_cref_vector(GET(ast, TUPLE)), implicit_type, ast.loc);

            case ast::expr::ARRAY:
                return compile_left_array(as_cref_vector(GET(ast, ARRAY)), implicit_type, ast.loc);

            case ast::expr::APPLICATION: {
                auto& [receiver_ast_ptr, arg_ast_ptrs] = GET(ast, APPLICATION);
                return compile_left_application(*receiver_ast_ptr, as_cref_vector(arg_ast_ptrs), implicit_type, ast.loc);
            }

            case ast::expr::NAME: {
                auto name = GET(ast, NAME);

                if (name == ast::IGNORED_PLACEHOLDER)
                    return VARIANT(lvalue, IGNORED, monostate());

                auto var = try_get_var(name);
                if (var)
                    return VARIANT(lvalue, VAR, *var);

                auto index = clr.get_global_name(name, { global_name_kind::VAR }, ast.loc).index;
                return VARIANT(lvalue, GLOBAL_VAR, index);
            }

            case ast::expr::VARIANT_NAME: {
                auto [name, variant_name] = GET(ast, VARIANT_NAME);
                auto enum_index = clr.get_global_name(name, { global_name_kind::ENUM }, ast.loc).index;
                auto& en = *clr.prog.enum_types[enum_index];

                auto iter = en.variant_names.find(variant_name);
                if (iter == en.variant_names.end())
                    error(diags::unknown_enum_variant(en, variant_name), ast.loc);

                auto variant_index = iter->second;

                if (!en.variants[variant_index]->tps.empty())
                    error(diags::invalid_expression(), ast.loc);

                return VARIANT(lvalue, ENUM_VARIANT, make_tuple(enum_index, variant_index, vector<ptr<lvalue>>()));
            }

            case ast::expr::VAR_DECL: {
                auto& var_decl_ast = *GET(ast, VAR_DECL);
                if (!var_decl_ast.tp && !implicit_type)
                    error(diags::variable_without_type(), var_decl_ast.loc);

                auto name = var_decl_ast.name;
                if (name == ast::IGNORED_PLACEHOLDER)
                    error(diags::invalid_variable_name(name), var_decl_ast.loc);

                auto type = var_decl_ast.tp ? clr.compile_type_local(**var_decl_ast.tp, false) : copy_type_local(*implicit_type);

                auto var = add_var(name, move(type));
                return VARIANT(lvalue, VAR, var);
            }

            case ast::expr::DEREFERENCE:
            case ast::expr::EXTRACT:
                error(diags::not_implemented(), ast.loc); // TODO

            default:
                error(diags::expression_not_assignable(), ast.loc);
        }
    }

    function_compiler::lvalue function_compiler::compile_left_tuple(vector<cref<ast::expr_marked>> asts, optional<cref<prog::type_local>> implicit_type, location loc) {
        auto value_asts = clr.order_args(asts, { }, { }, loc);
        auto count = value_asts.size();

        if (count == 0)
            error(diags::expression_not_assignable(), loc);

        if (count == 1)
            return compile_left_expr(value_asts[0], implicit_type);

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
                lvals.push_back(compile_left_expr(value_ast, { implicit_types[index] }));
            else
                lvals.push_back(compile_left_expr(value_ast, { }));
        }

        return VARIANT(lvalue, TUPLE, into_ptr_vector(lvals));
    }

    function_compiler::lvalue function_compiler::compile_left_array(vector<cref<ast::expr_marked>> asts, optional<cref<prog::type_local>> implicit_type, location loc) {
        auto value_asts = clr.order_args(asts, { }, { }, loc);

        vector<lvalue> lvals;
        for (auto& value_ast : value_asts) {
            if (implicit_type && INDEX_EQ(*implicit_type->get().tp, ARRAY)) {
                auto& type = *GET(*implicit_type->get().tp, ARRAY)->tp;
                auto confined = implicit_type->get().confined;
                auto type_local = prog::type_local { make_ptr(copy_type(type)), confined };
                lvals.push_back(compile_left_expr(value_ast, { type_local }));
            } else
                lvals.push_back(compile_left_expr(value_ast, { }));
        }

        return VARIANT(lvalue, ARRAY, into_ptr_vector(lvals));
    }

    function_compiler::lvalue function_compiler::compile_left_application(const ast::expr& receiver_ast, vector<cref<ast::expr_marked>> arg_asts, optional<cref<prog::type_local>> implicit_type, location loc) {
        switch (INDEX(receiver_ast)) {
            case ast::expr::NAME: {
                auto name = GET(receiver_ast, NAME);
                auto struct_index = clr.get_global_name(name, { global_name_kind::STRUCT }, receiver_ast.loc).index;
                auto& st = *clr.prog.struct_types[struct_index];
                auto count = st.fields.size();

                auto arg_with_name = [&] (string name, location loc) -> size_t {
                    auto iter = st.field_names.find(name);
                    if (iter == st.field_names.end())
                        error(diags::unknown_struct_field(st, name), loc);
                    return iter->second;
                };

                auto value_asts = clr.order_args(arg_asts, arg_with_name, { count }, loc);
                vector<lvalue> lvals;

                for (size_t index = 0; index < count; index++) {
                    auto& value_ast = value_asts[index].get();
                    if (implicit_type && INDEX_EQ(*implicit_type->get().tp, STRUCT) && GET(*implicit_type->get().tp, STRUCT) == struct_index) {
                        auto& type = *st.fields[index]->tp;
                        auto confined = implicit_type->get().confined;
                        auto type_local = prog::type_local { make_ptr(copy_type(type)), confined };
                        lvals.push_back(compile_left_expr(value_ast, { type_local }));
                    } else
                        lvals.push_back(compile_left_expr(value_ast, { }));
                }

                return VARIANT(lvalue, STRUCT, make_pair(struct_index, into_ptr_vector(lvals)));
            }

            case ast::expr::VARIANT_NAME: {
                auto [name, variant_name] = GET(receiver_ast, VARIANT_NAME);
                auto enum_index = clr.get_global_name(name, { global_name_kind::ENUM }, receiver_ast.loc).index;
                auto& en = *clr.prog.enum_types[enum_index];

                auto iter = en.variant_names.find(variant_name);
                if (iter == en.variant_names.end())
                    error(diags::unknown_enum_variant(en, variant_name), loc);

                auto variant_index = iter->second;
                auto& variant = *en.variants[variant_index];
                auto count = variant.tps.size();

                if (count == 0)
                    error(diags::invalid_expression(), loc);

                auto value_asts = clr.order_args(arg_asts, { }, { count }, loc);
                vector<lvalue> lvals;

                for (size_t index = 0; index < count; index++) {
                    auto& value_ast = value_asts[index].get();
                    if (implicit_type && INDEX_EQ(*implicit_type->get().tp, ENUM) && GET(*implicit_type->get().tp, ENUM) == enum_index) {
                        auto& type = *variant.tps[index];
                        auto confined = implicit_type->get().confined;
                        auto type_local = prog::type_local { make_ptr(copy_type(type)), confined };
                        lvals.push_back(compile_left_expr(value_ast, { type_local }));
                    } else
                        lvals.push_back(compile_left_expr(value_ast, { }));
                }

                return VARIANT(lvalue, ENUM_VARIANT, make_tuple(enum_index, variant_index, into_ptr_vector(lvals)));
            }

            default:
                error(diags::expression_not_assignable(), receiver_ast.loc);
        }
    }

    tuple<vector<cref<ast::expr>>, vector<prog::reg_index>, vector<prog::type>, bool> function_compiler::compile_args(
            vector<cref<ast::expr_marked>> asts,
            optional<function<size_t(string, location)>> arg_with_name,
            optional<size_t> expected_count,
            bool confined,
            location loc) {
        auto value_asts = clr.order_args(asts, arg_with_name, expected_count, loc);

        vector<prog::reg_index> values;
        vector<prog::type> types;
        optional<bool> all_confined;

        for (auto& value_ast : value_asts) {
            auto [value, type] = compile_expr(value_ast, confined);

            if (!clr.type_trivial(*type.tp)) {
                if (!all_confined)
                    all_confined = { type.confined };
                else if (type.confined != *all_confined)
                    error(diags::confinement_ambiguous(), value_ast.get().loc);
            }

            values.push_back(value);
            types.push_back(move(*type.tp));
        }

        if (!all_confined)
            all_confined = { confined };

        return { value_asts, values, move(types), *all_confined };
    }

    vector<prog::reg_index> function_compiler::compile_call_args(
            vector<cref<ast::expr_marked>> asts,
            const prog::func_type& ftype,
            optional<cref<prog::global_func>> func,
            location loc) {
        auto size = ftype.param_tps.size();

        auto arg_with_name = [&] (string name, location loc) -> size_t {
            auto iter = func->get().param_names.find(name);
            if (iter == func->get().param_names.end())
                error(diags::unknown_function_parameter(*func, name), loc);
            return iter->second;
        };

        auto value_asts = clr.order_args(asts, func ? make_optional(arg_with_name) : optional<decltype(arg_with_name)>(), { size }, loc);

        vector<prog::reg_index> values;
        vector<prog::type_local> types;

        for (size_t index = 0; index < size; index++) {
            auto& value_ast = value_asts[index];
            auto confined = ftype.param_tps[index]->confined;
            auto [value, type] = compile_expr(value_ast, confined);
            values.push_back(value);
            types.push_back(move(type));
        }

        for (size_t index = 0; index < size; index++) {
            auto& type = types[index];
            auto& param_type = *ftype.param_tps[index];
            values[index] = conv_clr.convert(values[index], type, param_type, value_asts[index].get().loc);
        }

        return values;
    }
}
