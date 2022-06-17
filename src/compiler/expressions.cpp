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
            } break;

            case ast::expr::NAME: {
                auto& name = GET(ast, NAME);

                auto var = get_var(name);

                if (var) {
                    auto& state = var_states[*var];
                    auto& loop_level = var_loop_levels[*var];

                    if (state != VAR_INITIALIZED)
                        clr.error(diags::variable_not_usable(name, state & VAR_INITIALIZED, state & VAR_UNINITIALIZED, state & VAR_MOVED_OUT), ast.loc);

                    auto result = new_reg();
                    auto instr = prog::read_var_instr { *var, result };
                    add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(instr)));

                    auto& var_type = var_types[*var];

                    if (!confined && !var_type.confined) {
                        if (clr.type_copyable(*var_type.tp))
                            add_copy_instrs(result, *var_type.tp);
                        else if (loop_level == 0)
                            state = VAR_MOVED_OUT;
                        else
                            clr.error(diags::variable_moved_inside_loop(name), ast.loc);
                    }

                    auto type = copy_type_local(var_type);

                    if (confined)
                        type.confined = true;

                    return { result, move(type) };
                }

                auto& gname = clr.get_global_name(name, ast.loc);

                switch (gname.kind) {
                    case global_name_kind::VAR: {
                        auto& var = *clr.prog.global_vars[gname.index];

                        auto result = new_reg();
                        auto instr = prog::read_global_var_instr { gname.index, result };
                        add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(instr)));

                        if (!confined) {
                            if (clr.type_copyable(*var.tp))
                                add_copy_instrs(result, *var.tp);
                            else
                                clr.error(diags::global_variable_moved(name), ast.loc);
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
                        return { unit_reg(), move(type) };
                    }

                    case global_name_kind::STRUCT: {
                        auto type = prog::type_local { make_ptr(VARIANT(prog::type, STRUCT_CTOR, gname.index)), false };
                        return { unit_reg(), move(type) };
                    }

                    case global_name_kind::ENUM:
                        clr.error(diags::invalid_kind(name, global_name_kind::ENUM, { }), ast.loc);
                }
            } break;

            case ast::expr::VARIANT_NAME: {
                auto& [name, variant_name] = GET(ast, VARIANT_NAME);

                if (get_var(name))
                    clr.error(diags::expected_enum_name(), ast.loc);

                auto enum_index = clr.get_global_name(name, { global_name_kind::ENUM }, ast.loc).index;
                auto& en = *clr.prog.enum_types[enum_index];

                auto iter = en.variant_names.find(variant_name);
                if (iter == en.variant_names.end())
                    clr.error(diags::unknown_enum_variant(en, name), ast.loc);
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
                return { unit_reg(), move(type) };
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
                clr.error(diags::not_implemented(), ast.loc); // TODO

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
                add_break_instr(ast.loc);
                return { unit_reg(), copy_type_local(prog::NEVER_TYPE) };
            }

            case ast::expr::CONTINUE: {
                add_continue_instr(ast.loc);
                return { unit_reg(), copy_type_local(prog::NEVER_TYPE) };
            }

            case ast::expr::REFERENCE:
            case ast::expr::HEAP_ALLOC:
            case ast::expr::DEREFERENCE:
            case ast::expr::TEST:
            case ast::expr::SIZED_ARRAY:
            case ast::expr::HEAP_SLICE_ALLOC:
            case ast::expr::LENGTH:
            case ast::expr::EXTRACT:
            case ast::expr::PTR_EXTRACT:
            case ast::expr::LAMBDA:
                clr.error(diags::not_implemented(), ast.loc); // TODO

            default:
                clr.error(diags::invalid_expression(), ast.loc);
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_return(optional<cref<ast::expr>> ast, location loc) {
        prog::reg_index result;
        prog::type_local type;

        if (ast)
            tie(result, type) = compile_expr(*ast, false);
        else {
            result = unit_reg();
            type = copy_type_local(prog::UNIT_TYPE);
        }

        result = conv_clr.convert(result, type, *func.return_tp, loc);

        add_return_instr(result, loc);

        return { result, copy_type_local(prog::NEVER_TYPE) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_tuple(vector<cref<ast::expr_marked>> asts, bool confined, location loc) {
        auto [value_asts, values, types, all_confined] = compile_args(asts, { }, { }, confined, loc);
        auto count = values.size();

        if (count == 0)
            return { unit_reg(), copy_type_local(prog::UNIT_TYPE) };

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
                        clr.error(diags::unknown_struct_field(st, name), loc);
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

                if (confined && !clr.type_trivially_copyable(*ftype.return_tp))
                    clr.error(diags::function_call_in_confined_context(), loc);

                auto args = compile_call_args(arg_asts, ftype, { }, loc);

                auto ptr = new_reg();
                auto instr1 = prog::ptr_conversion_instr { receiver, ptr };
                add_instr(VARIANT(prog::instr, EXTRACT_PTR, into_ptr(instr1)));
                args.insert(args.begin(), ptr);

                auto func = new_reg();
                auto instr2 = prog::ptr_conversion_instr { receiver, func };
                add_instr(VARIANT(prog::instr, EXTRACT_FUNC, into_ptr(instr2)));

                auto result = new_reg();
                auto instr = prog::func_ptr_call_instr { func, args, result };
                add_instr(VARIANT(prog::instr, FUNC_PTR_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*ftype.return_tp)), false };
                return { result, move(type) };
            }

            case prog::type::GLOBAL_FUNC: {
                auto& ftype = *GET(receiver_type, GLOBAL_FUNC);

                if (confined && !clr.type_trivially_copyable(*ftype.return_tp))
                    clr.error(diags::function_call_in_confined_context(), loc);

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

                if (confined && !clr.type_trivially_copyable(*ftype.return_tp))
                    clr.error(diags::function_call_in_confined_context(), loc);

                auto args = compile_call_args(arg_asts, ftype, { func }, loc);

                auto result = new_reg();
                auto instr = prog::func_call_instr { func_index, args, result };
                add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*func.return_tp)), false };
                return { result, move(type) };
            }

            default:
                clr.error(diags::invalid_expression(), loc);
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_unary_operation(const ast::unary_operation_expr& ast) {
        auto [value, type] = compile_expr(*ast.value, true);
        auto result = new_reg();

        switch (ast.operation) {
            case ast::unary_operation_expr::NOT: {
                value = conv_clr.convert(value, type, prog::BOOL_TYPE, ast.value->loc);
                auto instr = prog::unary_operation_instr { value, result };
                add_instr(VARIANT(prog::instr, BOOL_NOT, into_ptr(instr)));
                return { result, copy_type_local(prog::BOOL_TYPE) };
            }

            case ast::unary_operation_expr::MINUS: {
                if (!INDEX_EQ(*type.tp, NUMBER))
                    clr.error(diags::invalid_unary_operation(clr.prog, ast.operation, copy_type(*type.tp)), ast.loc);
                switch (GET(*type.tp, NUMBER)->tp) {
                    case prog::number_type::I8:
                    case prog::number_type::I16:
                    case prog::number_type::I32:
                    case prog::number_type::I64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, INT_NEG, into_ptr(instr)));
                    } break;

                    case prog::number_type::F32:
                    case prog::number_type::F64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, FLOAT_NEG, into_ptr(instr)));
                    } break;

                    default:
                        clr.error(diags::invalid_unary_operation(clr.prog, ast.operation, copy_type(*type.tp)), ast.loc);
                }
                return { result, move(type) };
            }

            case ast::unary_operation_expr::BIT_NEG: {
                if (!INDEX_EQ(*type.tp, NUMBER))
                    clr.error(diags::invalid_unary_operation(clr.prog, ast.operation, copy_type(*type.tp)), ast.loc);
                switch (GET(*type.tp, NUMBER)->tp) {
                    case prog::number_type::I8:
                    case prog::number_type::I16:
                    case prog::number_type::I32:
                    case prog::number_type::I64:
                    case prog::number_type::U8:
                    case prog::number_type::U16:
                    case prog::number_type::U32:
                    case prog::number_type::U64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, BIT_NEG, into_ptr(instr)));
                        return { result, move(type) };
                    }

                    default:
                        clr.error(diags::invalid_unary_operation(clr.prog, ast.operation, copy_type(*type.tp)), ast.loc);
                }
            }
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_binary_operation(const ast::binary_operation_expr& ast) {
        #define INVALID_BINARY_OP { clr.error(diags::invalid_binary_operation(clr.prog, ast.operation, copy_type(*left_type.tp), copy_type(*right_type.tp)), ast.loc); }
        #define UINT(tp) ((tp) == prog::number_type::U8 || (tp) == prog::number_type::U16 || (tp) == prog::number_type::U32 || (tp) == prog::number_type::U64)
        #define SINT(tp) ((tp) == prog::number_type::I8 || (tp) == prog::number_type::I16 || (tp) == prog::number_type::I32 || (tp) == prog::number_type::I64)
        #define FLOAT(tp) ((tp) == prog::number_type::F32 || (tp) == prog::number_type::F64)

        switch (ast.operation) {
            case ast::binary_operation_expr::AND:
            case ast::binary_operation_expr::OR: {
                auto[left_value, left_type] = compile_expr(*ast.left, true);
                auto left_value_converted = conv_clr.convert(left_value, left_type, prog::BOOL_TYPE, ast.left->loc);
                auto result = new_reg();

                auto return_left_branch = [&](){
                    add_instr(VARIANT(prog::instr, COPY_REG, make_ptr(prog::copy_reg_instr{ left_value_converted, result })));
                };

                auto eval_right_branch = [&](){
                    auto[right_value, right_type] = compile_expr(*ast.right, true);
                    right_value = conv_clr.convert(right_value, right_type, prog::BOOL_TYPE, ast.right->loc);
                    add_instr(VARIANT(prog::instr, COPY_REG, make_ptr(prog::copy_reg_instr{ right_value, result })));
                };

                if (ast.operation == ast::binary_operation_expr::AND)
                    add_branch_instr(left_value, eval_right_branch, return_left_branch);
                else
                    add_branch_instr(left_value, return_left_branch, eval_right_branch);
                return { result, copy_type_local(prog::BOOL_TYPE) };
            } break;

            case ast::binary_operation_expr::EQ:
            case ast::binary_operation_expr::NEQ:
                clr.error(diags::not_implemented(), ast.loc); // TODO

            case ast::binary_operation_expr::LS:
            case ast::binary_operation_expr::LSEQ:
            case ast::binary_operation_expr::GT:
            case ast::binary_operation_expr::GTEQ:
            case ast::binary_operation_expr::ADD:
            case ast::binary_operation_expr::SUB:
            case ast::binary_operation_expr::MUL:
            case ast::binary_operation_expr::DIV:
            case ast::binary_operation_expr::MOD:
            case ast::binary_operation_expr::BIT_AND:
            case ast::binary_operation_expr::BIT_OR:
            case ast::binary_operation_expr::BIT_XOR: {
                auto[left_value, left_type] = compile_expr(*ast.left, true);
                auto[right_value, right_type] = compile_expr(*ast.right, true);
                auto common_type = clr.common_supertype(*left_type.tp, *right_type.tp, ast.loc);
                if (!INDEX_EQ(common_type, NUMBER))
                    INVALID_BINARY_OP;
                left_value = conv_clr.convert(left_value, *left_type.tp, common_type, ast.left->loc);
                right_value = conv_clr.convert(right_value, *right_type.tp, common_type, ast.right->loc);
                auto result = new_reg();

                prog::numeric_binary_operation_instr::kind_t kind;
                auto& ntype = GET(common_type, NUMBER)->tp;
                if (UINT(ntype))
                    kind = prog::numeric_binary_operation_instr::UNSIGNED;
                else if (SINT(ntype))
                    kind = prog::numeric_binary_operation_instr::SIGNED;
                else if (FLOAT(ntype))
                    kind = prog::numeric_binary_operation_instr::FLOAT;
                else
                    INVALID_BINARY_OP;
                auto numeric_binary_operation = make_ptr(prog::numeric_binary_operation_instr{ { left_value, right_value, result }, kind });

                bool bool_result_type = false;
                switch (ast.operation) {
                    case ast::binary_operation_expr::LS: {
                        add_instr(VARIANT(prog::instr, LS, move(numeric_binary_operation)));
                        bool_result_type = true;
                    } break;

                    case ast::binary_operation_expr::LSEQ: {
                        add_instr(VARIANT(prog::instr, LSEQ, move(numeric_binary_operation)));
                        bool_result_type = true;
                    } break;

                    case ast::binary_operation_expr::GT: {
                        add_instr(VARIANT(prog::instr, GT, move(numeric_binary_operation)));
                        bool_result_type = true;
                    } break;

                    case ast::binary_operation_expr::GTEQ: {
                        add_instr(VARIANT(prog::instr, GTEQ, move(numeric_binary_operation)));
                        bool_result_type = true;
                    } break;

                    case ast::binary_operation_expr::ADD: {
                        add_instr(VARIANT(prog::instr, ADD, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::SUB: {
                        add_instr(VARIANT(prog::instr, SUB, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::MUL: {
                        add_instr(VARIANT(prog::instr, MUL, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::DIV: {
                        add_instr(VARIANT(prog::instr, DIV, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::MOD: {
                        add_instr(VARIANT(prog::instr, MOD, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::BIT_AND: {
                        if (FLOAT(ntype))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_AND, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::BIT_OR: {
                        if (FLOAT(ntype))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_OR, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::BIT_XOR: {
                        if (FLOAT(ntype))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_XOR, move(numeric_binary_operation)));
                    } break;

                    default:
                        UNREACHABLE;
                }

                if (bool_result_type)
                    return { result, copy_type_local(prog::BOOL_TYPE) };
                else
                    return { result, prog::type_local{ into_ptr(common_type), false } };
            }

            case ast::binary_operation_expr::BIT_LSH:
            case ast::binary_operation_expr::BIT_RSH: {
                auto[left_value, left_type] = compile_expr(*ast.left, true);
                auto[right_value, right_type] = compile_expr(*ast.right, true);
                if (!INDEX_EQ(*left_type.tp, NUMBER) || !INDEX_EQ(*right_type.tp, NUMBER) || !UINT(GET(*right_type.tp, NUMBER)->tp))
                    INVALID_BINARY_OP;
                auto result = new_reg();

                prog::numeric_binary_operation_instr::kind_t kind;
                auto& left_ntype = GET(*left_type.tp, NUMBER)->tp;
                if (UINT(left_ntype))
                    kind = prog::numeric_binary_operation_instr::UNSIGNED;
                else if (SINT(left_ntype))
                    kind = prog::numeric_binary_operation_instr::SIGNED;
                else
                    INVALID_BINARY_OP;
                auto numeric_binary_operation = make_ptr(prog::numeric_binary_operation_instr{ {left_value, right_value, result }, kind });

                if (ast.operation == ast::binary_operation_expr::BIT_LSH)
                    add_instr(VARIANT(prog::instr, BIT_LSH, move(numeric_binary_operation)));
                else
                    add_instr(VARIANT(prog::instr, BIT_RSH, move(numeric_binary_operation)));

                return { result, move(left_type) };
            }
        }

        UNREACHABLE;

        #undef FLOAT
        #undef SINT
        #undef UINT
        #undef INVALID_BINARY_OP
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

                auto var = get_var(name);
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
                    clr.error(diags::unknown_enum_variant(en, variant_name), ast.loc);

                auto variant_index = iter->second;

                if (!en.variants[variant_index]->tps.empty())
                    clr.error(diags::invalid_expression(), ast.loc);

                return VARIANT(lvalue, ENUM_VARIANT, make_tuple(enum_index, variant_index, vector<ptr<lvalue>>()));
            }

            case ast::expr::VAR_DECL: {
                auto& var_decl_ast = *GET(ast, VAR_DECL);
                if (!var_decl_ast.tp && !implicit_type)
                    clr.error(diags::variable_without_type(), var_decl_ast.loc);

                auto name = var_decl_ast.name;
                if (name == ast::IGNORED_PLACEHOLDER)
                    clr.error(diags::invalid_variable_name(name), var_decl_ast.loc);

                auto type = var_decl_ast.tp ? clr.compile_type_local(**var_decl_ast.tp, false) : copy_type_local(*implicit_type);

                auto var = add_var(name, move(type));
                return VARIANT(lvalue, VAR, var);
            }

            case ast::expr::DEREFERENCE:
            case ast::expr::EXTRACT:
                clr.error(diags::not_implemented(), ast.loc); // TODO

            default:
                clr.error(diags::expression_not_assignable(), ast.loc);
        }
    }

    function_compiler::lvalue function_compiler::compile_left_tuple(vector<cref<ast::expr_marked>> asts, optional<cref<prog::type_local>> implicit_type, location loc) {
        auto value_asts = clr.order_args(asts, { }, { }, loc);
        auto count = value_asts.size();

        if (count == 0)
            clr.error(diags::expression_not_assignable(), loc);

        if (count == 1)
            return compile_left_expr(value_asts[0], implicit_type);

        vector<prog::type_local> implicit_types;
        if (implicit_type && INDEX_EQ(*implicit_type->get().tp, TUPLE)) {
            auto confined = implicit_type->get().confined;
            for (auto& type_ptr : GET(*implicit_type->get().tp, TUPLE))
                implicit_types.push_back({ make_ptr(copy_type(*type_ptr)), confined });
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
                        clr.error(diags::unknown_struct_field(st, name), loc);
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
                    clr.error(diags::unknown_enum_variant(en, variant_name), loc);

                auto variant_index = iter->second;
                auto& variant = *en.variants[variant_index];
                auto count = variant.tps.size();

                if (count == 0)
                    clr.error(diags::invalid_expression(), loc);

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
                clr.error(diags::expression_not_assignable(), receiver_ast.loc);
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

            if (!clr.type_trivially_copyable(*type.tp)) {
                if (!all_confined)
                    all_confined = { type.confined };
                else if (type.confined != *all_confined)
                    clr.error(diags::confinement_ambiguous(), value_ast.get().loc);
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
                clr.error(diags::unknown_function_parameter(*func, name), loc);
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