#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    using std::monostate;
    using std::tie;

    function_compiler::lvalue function_compiler::compile_left_expr(const ast::expr& ast, optional<ref<const prog::type_local>> implicit_type) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
            case ast::expr::ARRAY:
            case ast::expr::APPLICATION:
                clr.error(diags::not_implemented(), ast); // TODO

            case ast::expr::NAME: {
                auto name = GET(ast, NAME);

                auto var = get_var(name);
                if (var)
                    return VARIANT(lvalue, VAR, *var);

                auto& global_name = clr.get_global_name(ast, name, global_name_kind::VAR);
                return VARIANT(lvalue, GLOBAL_VAR, global_name.index);
            }

            case ast::expr::VAR_DECL: {
                auto& var_decl_ast = *GET(ast, VAR_DECL);
                if (!var_decl_ast.tp && !implicit_type)
                    clr.error(diags::variable_without_type(), var_decl_ast);
                auto type = var_decl_ast.tp ? clr.compile_type_local(**var_decl_ast.tp) : copy_type_local(*implicit_type);
                auto var = add_var(var_decl_ast.name, into_ptr(type));
                return VARIANT(lvalue, VAR, var);
            }

            case ast::expr::DEREFERENCE:
            case ast::expr::EXTRACT:
                clr.error(diags::not_implemented(), ast); // TODO

            default:
                clr.error(diags::expression_not_assignable(), ast);
        }
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_expr(const ast::expr& ast, bool confined) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
                return compile_tuple(ast, GET(ast, TUPLE), confined);

            case ast::expr::ARRAY:
                return compile_array(ast, GET(ast, ARRAY), confined);

            case ast::expr::APPLICATION: {
                auto& [receiver_ast, args_ast] = GET(ast, APPLICATION);
                return compile_application(ast, *receiver_ast, args_ast, confined);
            } break;

            case ast::expr::NAME: {
                auto& name = GET(ast, NAME);

                auto local_var = get_var(name);

                if (local_var) {
                    auto& state = var_states[*local_var];

                    if (!(state.initialized && !state.uninitialized && !state.moved_out))
                        clr.error(diags::variable_not_usable(name, state.initialized, state.uninitialized, state.moved_out), ast);

                    auto result = new_reg();
                    auto instr = prog::read_var_instr { *local_var, result };
                    add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(instr)));

                    auto& var_type = *var_types[*local_var];

                    if (!confined && !var_type.confined) {
                        if (clr.type_copyable(*var_type.tp))
                            result = add_copy_instrs(result, *var_type.tp);
                        else if (state.loop_level == 0)
                            move_out_var(*local_var);
                        else
                            clr.error(diags::variable_moved_inside_loop(name), ast);
                    }

                    auto type = copy_type_local(var_type);

                    if (confined)
                        type.confined = true;

                    return { result, move(type) };
                }

                auto& global_name = clr.get_global_name(ast, name);

                switch (global_name.kind) {
                    case global_name_kind::VAR: {
                        auto& var = *clr.prog.global_vars[global_name.index];

                        auto result = new_reg();
                        auto instr = prog::read_global_var_instr { global_name.index, result };
                        add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(instr)));

                        if (!confined) {
                            if (clr.type_copyable(*var.tp))
                                result = add_copy_instrs(result, *var.tp);
                            else
                                clr.error(diags::global_variable_moved(name), ast);
                        }

                        auto type = prog::type_local { make_ptr(copy_type(*var.tp)), confined };
                        return { result, move(type) };
                    }

                    case global_name_kind::CONST: {
                        auto& value = clr.consts[global_name.index];

                        auto result = new_reg();
                        auto instr = prog::make_const_instr { make_ptr(copy_const(*value.value)), result };
                        add_instr(VARIANT(prog::instr, MAKE_CONST, into_ptr(instr)));

                        auto type = prog::type_local { make_ptr(copy_type(*value.tp)), confined };
                        return { result, move(type) };
                    }

                    case global_name_kind::FUNC: {
                        auto type = prog::type_local { make_ptr(VARIANT(prog::type, KNOWN_FUNC, global_name.index)), false };
                        return { unit_reg(), move(type) };
                    }

                    case global_name_kind::STRUCT: {
                        auto type = prog::type_local { make_ptr(VARIANT(prog::type, STRUCT_CTOR, global_name.index)), false };
                        return { unit_reg(), move(type) };
                    }

                    case global_name_kind::ENUM:
                        clr.error(diags::invalid_kind(name, global_name_kind::ENUM, { }), ast);
                }
            } break;

            case ast::expr::VARIANT_NAME: {
                auto& [name, variant_name] = GET(ast, VARIANT_NAME);

                if (get_var(name))
                    clr.error(diags::expected_variant_name(), ast);

                auto& global_name = clr.get_global_name(ast, name, global_name_kind::ENUM);
                auto& en = *clr.prog.enum_types[global_name.index];
                if (!en.variant_names.count(variant_name))
                    clr.error(diags::invalid_enum_variant(en, name), ast);

                prog::variant_index variant_index = en.variant_names[variant_name];
                auto& variant = *en.variants[variant_index];

                if (variant.tps.empty()) {
                    auto result = new_reg();
                    auto instr = prog::make_enum_variant_instr { global_name.index, variant_index, { }, result };
                    add_instr(VARIANT(prog::instr, MAKE_ENUM_VARIANT, into_ptr(instr)));
                    auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM, global_name.index)), confined };
                    return { result, move(type) };
                }

                auto type = prog::type_local { make_ptr(VARIANT(prog::type, ENUM_CTOR, make_pair(global_name.index, variant_index))), false };
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
                clr.error(diags::not_implemented(), ast); // TODO

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
                auto& return_expr = GET(ast, RETURN);
                return compile_return(return_expr ? **return_expr : ast, return_expr);
            }

            case ast::expr::BREAK: {
                add_instr(VARIANT(prog::instr, BREAK_LOOP, monostate()));
                return { unit_reg(), copy_type_local(NEVER_TYPE) };
            }

            case ast::expr::CONTINUE: {
                add_instr(VARIANT(prog::instr, CONTINUE_LOOP, monostate()));
                return { unit_reg(), copy_type_local(NEVER_TYPE) };
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
                clr.error(diags::not_implemented(), ast); // TODO

            default:
                clr.error(diags::invalid_expression(), ast);
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_return(const ast::node& ast, const optional<ast::ptr<ast::expr>>& expr_ast) {
        prog::reg_index result;
        prog::type_local type;

        if (expr_ast)
            tie(result, type) = compile_expr(**expr_ast, false);
        else {
            result = unit_reg();
            type = copy_type_local(UNIT_TYPE);
        }

        result = conv_clr.convert(ast, result, type, *func.return_tp);

        add_return_instr(result);

        return { result, copy_type_local(NEVER_TYPE) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_tuple(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& args_ast, bool confined) {
        auto [values_ast, values, types, all_confined] = compile_args(ast, args_ast, { }, { }, confined);
        auto size = values.size();

        if (size == 0)
            return { unit_reg(), copy_type_local(UNIT_TYPE) };

        if (size == 1) {
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

    pair<prog::reg_index, prog::type_local> function_compiler::compile_array(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& args_ast, bool confined) {
        auto [values_ast, values, types, all_confined] = compile_args(ast, args_ast, { }, { }, confined);
        auto size = values.size();

        auto common_type = VARIANT(prog::type, NEVER, monostate());

        for (auto& type : types)
            common_type = clr.common_supertype(ast, common_type, type);

        for (size_t index = 0; index < size; index++)
            values[index] = conv_clr.convert(values_ast[index], values[index], types[index], common_type, all_confined);

        auto result = new_reg();
        auto instr = prog::make_array_instr { values, result };
        add_instr(VARIANT(prog::instr, MAKE_ARRAY, into_ptr(instr)));
        auto type = prog::type_local { make_ptr(VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(common_type), size }))), all_confined };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_application(const ast::node& ast, const ast::expr& receiver_ast, const vector<ast::ptr<ast::expr_marked>>& args_ast, bool confined) {
        auto[receiver, receiver_type_local] = compile_expr(receiver_ast, true);
        auto& receiver_type = *receiver_type_local.tp;

        switch (INDEX(receiver_type)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(receiver_type, STRUCT_CTOR);
                auto& st = *clr.prog.struct_types[struct_index];
                auto size = st.fields.size();

                auto arg_with_name = [&] (const ast::node& ast, string name) -> size_t {
                    if (!st.field_names.count(name))
                        clr.error(diags::invalid_struct_field(st, name), ast);
                    return st.field_names[name];
                };

                auto [values_ast, values, types, all_confined] = compile_args(ast, args_ast, { arg_with_name }, { size }, confined);

                for (size_t index = 0; index < size; index++) {
                    auto& type = types[index];
                    auto& field_type = *st.fields[index]->tp;
                    values[index] = conv_clr.convert(values_ast[index], values[index], type, field_type, all_confined);
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

                auto [values_ast, values, types, all_confined] = compile_args(ast, args_ast, { }, { size }, confined);

                for (size_t index = 0; index < size; index++) {
                    auto& type = types[index];
                    auto& field_type = *variant.tps[index];
                    values[index] = conv_clr.convert(values_ast[index], values[index], type, field_type, all_confined);
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
                    clr.error(diags::function_call_in_confined_context(), ast);

                auto args = compile_call_args(ast, args_ast, ftype, { });

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
                    clr.error(diags::function_call_in_confined_context(), ast);

                auto args = compile_call_args(ast, args_ast, ftype, { });

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
                    clr.error(diags::function_call_in_confined_context(), ast);

                auto args = compile_call_args(ast, args_ast, ftype, { func });

                auto result = new_reg();
                auto instr = prog::func_call_instr { func_index, args, result };
                add_instr(VARIANT(prog::instr, FUNC_CALL, into_ptr(instr)));
                auto type = prog::type_local{ make_ptr(copy_type(*func.return_tp)), false };
                return { result, move(type) };
            }

            default:
                clr.error(diags::invalid_expression(), ast);
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_unary_operation(const ast::unary_operation_expr& ast) {
        auto [value, type] = compile_expr(*ast.value, true);
        auto result = new_reg();

        switch (ast.operation) {
            case ast::unary_operation_expr::NOT: {
                value = conv_clr.convert(*ast.value, value, type, BOOL_TYPE);
                auto instr = prog::unary_operation_instr { value, result };
                add_instr(VARIANT(prog::instr, BOOL_NOT, into_ptr(instr)));
                return { result, copy_type_local(BOOL_TYPE) };
            }

            case ast::unary_operation_expr::MINUS: {
                if (!INDEX_EQ(*type.tp, PRIMITIVE))
                    clr.error(diags::invalid_unary_operation(clr.prog, ast.operation, copy_type(*type.tp)), ast);
                switch (GET(*type.tp, PRIMITIVE)->tp) {
                    case prog::primitive_type::I8:
                    case prog::primitive_type::I16:
                    case prog::primitive_type::I32:
                    case prog::primitive_type::I64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, INT_NEG, into_ptr(instr)));
                    } break;

                    case prog::primitive_type::F32:
                    case prog::primitive_type::F64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, FLOAT_NEG, into_ptr(instr)));
                    } break;

                    default:
                        clr.error(diags::invalid_unary_operation(clr.prog, ast.operation, copy_type(*type.tp)), ast);
                }
                return { result, move(type) };
            }

            case ast::unary_operation_expr::BIT_NEG: {
                if (!INDEX_EQ(*type.tp, PRIMITIVE))
                    clr.error(diags::invalid_unary_operation(clr.prog, ast.operation, copy_type(*type.tp)), ast);
                switch (GET(*type.tp, PRIMITIVE)->tp) {
                    case prog::primitive_type::I8:
                    case prog::primitive_type::I16:
                    case prog::primitive_type::I32:
                    case prog::primitive_type::I64:
                    case prog::primitive_type::U8:
                    case prog::primitive_type::U16:
                    case prog::primitive_type::U32:
                    case prog::primitive_type::U64: {
                        auto instr = prog::unary_operation_instr{ value, result };
                        add_instr(VARIANT(prog::instr, BIT_NEG, into_ptr(instr)));
                        return { result, move(type) };
                    }

                    default:
                        clr.error(diags::invalid_unary_operation(clr.prog, ast.operation, copy_type(*type.tp)), ast);
                }
            }
        }

        UNREACHABLE;
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_binary_operation(const ast::binary_operation_expr& ast) {
        #define INVALID_BINARY_OP { clr.error(diags::invalid_binary_operation(clr.prog, ast.operation, copy_type(*left_type.tp), copy_type(*right_type.tp)), ast); }
        #define UINT(tp) ((tp) == prog::primitive_type::U8 || (tp) == prog::primitive_type::U16 || (tp) == prog::primitive_type::U32 || (tp) == prog::primitive_type::U64)
        #define SINT(tp) ((tp) == prog::primitive_type::I8 || (tp) == prog::primitive_type::I16 || (tp) == prog::primitive_type::I32 || (tp) == prog::primitive_type::I64)
        #define FLOAT(tp) ((tp) == prog::primitive_type::F32 || (tp) == prog::primitive_type::F64)

        switch (ast.operation) {
            case ast::binary_operation_expr::AND:
            case ast::binary_operation_expr::OR: {
                auto[left_value, left_type] = compile_expr(*ast.left, true);
                auto left_value_converted = conv_clr.convert(*ast.left, left_value, left_type, BOOL_TYPE);
                auto result = new_reg();

                auto return_left_branch = [&](){
                    add_instr(VARIANT(prog::instr, REG_COPY, make_ptr(prog::reg_copy_instr{ left_value_converted, result })));
                };

                auto eval_right_branch = [&](){
                    auto[right_value, right_type] = compile_expr(*ast.right, true);
                    right_value = conv_clr.convert(*ast.right, right_value, right_type, BOOL_TYPE);
                    add_instr(VARIANT(prog::instr, REG_COPY, make_ptr(prog::reg_copy_instr{ right_value, result })));
                };

                if (ast.operation == ast::binary_operation_expr::AND)
                    add_branch_instr(left_value, eval_right_branch, return_left_branch);
                else
                    add_branch_instr(left_value, return_left_branch, eval_right_branch);
                return { result, copy_type_local(BOOL_TYPE) };
            } break;

            case ast::binary_operation_expr::EQ:
            case ast::binary_operation_expr::NEQ: 
                clr.error(diags::not_implemented(), ast); // TODO

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
                auto common_type = clr.common_supertype(ast, *left_type.tp, *right_type.tp);
                if (!INDEX_EQ(common_type, PRIMITIVE))
                    INVALID_BINARY_OP
                left_value = conv_clr.convert(*ast.left, left_value, *left_type.tp, common_type);
                right_value = conv_clr.convert(*ast.right, right_value, *right_type.tp, common_type);
                auto result = new_reg();

                prog::numeric_binary_operation_instr::kind_t kind;
                auto& primitive_type = GET(common_type, PRIMITIVE)->tp;
                if (UINT(primitive_type))
                    kind = prog::numeric_binary_operation_instr::UNSIGNED;
                else if (SINT(primitive_type))
                    kind = prog::numeric_binary_operation_instr::SIGNED;
                else if (FLOAT(primitive_type))
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
                        if (FLOAT(primitive_type))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_AND, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::BIT_OR: {
                        if (FLOAT(primitive_type))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_OR, move(numeric_binary_operation)));
                    } break;

                    case ast::binary_operation_expr::BIT_XOR: {
                        if (FLOAT(primitive_type))
                            INVALID_BINARY_OP;
                        add_instr(VARIANT(prog::instr, BIT_XOR, move(numeric_binary_operation)));
                    } break;

                    default:
                        UNREACHABLE;
                }

                if (bool_result_type)
                    return { result, copy_type_local(BOOL_TYPE) };
                else
                    return { result, prog::type_local{ into_ptr(common_type), false } };
            }

            case ast::binary_operation_expr::BIT_LSH:
            case ast::binary_operation_expr::BIT_RSH: {
                auto[left_value, left_type] = compile_expr(*ast.left, true);
                auto[right_value, right_type] = compile_expr(*ast.right, true);
                if (!INDEX_EQ(*left_type.tp, PRIMITIVE) || !INDEX_EQ(*right_type.tp, PRIMITIVE) || !UINT(GET(*right_type.tp, PRIMITIVE)->tp))
                    INVALID_BINARY_OP;
                auto result = new_reg();

                prog::numeric_binary_operation_instr::kind_t kind;
                auto& left_primitive_type = GET(*left_type.tp, PRIMITIVE)->tp;
                if (UINT(left_primitive_type))
                    kind = prog::numeric_binary_operation_instr::UNSIGNED;
                else if (SINT(left_primitive_type))
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

    tuple<vector<ref<const ast::expr>>, vector<prog::reg_index>, vector<prog::type>, bool> function_compiler::compile_args(
            const ast::node& ast,
            const args_ast_vector& args_ast,
            optional<arg_with_name_function> arg_with_name,
            optional<size_t> expected_number,
            bool confined) {
        auto values_ast = clr.order_args(ast, args_ast, arg_with_name, expected_number);

        vector<prog::reg_index> values;
        vector<prog::type> types;
        optional<bool> all_confined;

        for (auto& value_ast : values_ast) {
            auto [value, type] = compile_expr(value_ast, confined);

            if (!clr.type_trivially_copyable(*type.tp)) {
                if (!all_confined)
                    all_confined = { type.confined };
                else if (type.confined != *all_confined)
                    clr.error(diags::confinement_ambiguous(), value_ast);
            }

            values.push_back(value);
            types.push_back(move(*type.tp));
        }

        if (!all_confined)
            all_confined = { confined };

        return { values_ast, values, move(types), *all_confined };
    }

    vector<prog::reg_index> function_compiler::compile_call_args(
            const ast::node& ast,
            const args_ast_vector& args_ast,
            const prog::func_type& ftype,
            optional<ref<const prog::global_func>> func) {
        auto size = ftype.param_tps.size();

        auto arg_with_name = [&] (const ast::node& ast, string name) -> size_t {
            if (func->get().param_names.count(name))
                clr.error(diags::invalid_function_parameter(*func, name), ast);
            return func->get().param_names.at(name);
        };

        auto values_ast = clr.order_args(ast, args_ast, func ? make_optional(arg_with_name) : optional<decltype(arg_with_name)>(), { size });

        vector<prog::reg_index> values;
        vector<prog::type_local> types;

        for (size_t index = 0; index < size; index++) {
            auto& value_ast = values_ast[index];
            auto confined = ftype.param_tps[index]->confined;
            auto [value, type] = compile_expr(value_ast, confined);
            values.push_back(value);
            types.push_back(move(type));
        }

        for (size_t index = 0; index < size; index++) {
            auto& type = types[index];
            auto& param_type = *ftype.param_tps[index];
            values[index] = conv_clr.convert(values_ast[index], values[index], type, param_type);
        }

        return values;
    }
}
