#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    using std::monostate;
    using std::tie;

    prog::global_func compiler::declare_global_func(const ast::func_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used(name), ast.name_loc);

        if (ast.copying)
            error(diags::global_func_copyable(), ast.name_loc);

        vector<prog::ptr<prog::func_param>> params;
        unordered_map<string, size_t> param_names;

        for (auto& param_ast : ast.params) {
            param_names[param_ast->name] = params.size();
            params.push_back(make_ptr(prog::func_param { param_ast->name, make_ptr(compile_type_local(*param_ast->tp, true)) }));
        }

        prog::ptr<prog::type> return_tp;
        if (ast.return_tp)
            return_tp = make_ptr(compile_type(**ast.return_tp, true));
        else
            return_tp = make_ptr(VARIANT(prog::type, UNIT, monostate()));

        return { name, move(params), move(param_names), move(return_tp), { }, { } };
    }

    void compiler::compile_global_func(const ast::func_def& ast, prog::global_func& global_func) {
        function_compiler(*this, global_func).compile(ast);
    }

    void function_compiler::compile(const ast::func_def& ast) {
        reg_counter = 0;

        push_frame();

        for (auto& param : func.params) {
            auto var = add_var(param->name, make_ptr(copy_type_local(*param->tp)));
            add_instr(VARIANT(prog::instr, WRITE_VAR, make_ptr(prog::write_var_instr { var, reg_counter++ })));
        }

        compile_stmt_block(*ast.body->block, false);

        if (!frames.back().always_returns || ast.body->return_value) {
            if (!ast.body->return_value && !INDEX_EQ(*func.return_tp, UNIT))
                clr.error(diags::missing_return(), ast.end_loc);
            if (frames.back().always_returns)
                clr.warning(diags::dead_code(), **ast.body->return_value);
            add_return_instr(ast.body->return_value);
        }

        func.instrs = make_ptr(prog::instr_block { into_ptr_vector(frames.back().instrs) });

        for (auto& var : vars)
            func.vars.push_back(move(var.tp));

        pop_frame();
    }

    void function_compiler::push_frame() {
        frames.push_back({ { }, { }, false });
    }

    void function_compiler::pop_frame() {
        auto& cur_frame_vars = frames.back().vars;

        for (auto& name : cur_frame_vars) {
            var_names[name].pop_back();
            if (var_names[name].empty())
                var_names.erase(name);
        }

        frames.pop_back();
    }

    prog::reg_index function_compiler::new_reg() {
        return ++reg_counter;
    }

    void function_compiler::add_instr(prog::instr&& instr) {
        frames.back().instrs.push_back(move(instr));
    }

    prog::var_index function_compiler::add_var(string name, prog::ptr<prog::type_local> type) {
        auto index = vars.size();

        vars.push_back({ move(type) });
        frames.back().vars.push_back(name);
        var_names[name].push_back(index);

        return index;
    }

    optional<prog::var_index> function_compiler::get_var(string name) {
        if (var_names.count(name) == 0)
            return { };
        return { var_names[name].back() };
    }

    void function_compiler::add_cleanup_instrs(bool) {
        // TODO
    }

    void function_compiler::add_return_instr(const optional<ast::ptr<ast::expr>>& ast) {
        prog::reg_index result;
        prog::type_local type;

        if (ast)
            tie(result, type) = compile_expr(**ast);
        else {
            result = new_reg();
            type = prog::type_local{ make_ptr(VARIANT(prog::type, UNIT, monostate())), false };
            add_instr(VARIANT(prog::instr, MAKE_UNIT, result));
        }

        result = conv_clr.convert(**ast, type, *func.return_tp, result);
        add_cleanup_instrs(true);
        add_instr(VARIANT(prog::instr, RETURN, make_ptr(prog::return_instr{ result })));
    }

    void function_compiler::compile_stmt_block(const ast::stmt_block& ast, bool cleanup) {
        for (auto& stmt_ast : ast.stmts) {
            if (frames.back().always_returns)
                clr.warning(diags::dead_code(), *stmt_ast);

            switch (INDEX(*stmt_ast)) {
                case ast::stmt::EXPR_EVAL: {
                    auto& expr_ast = *GET(*stmt_ast, EXPR_EVAL);
                    if (INDEX_EQ(expr_ast, VAR_DECL))
                        compile_left_expr(expr_ast, { });
                    else
                        compile_expr(expr_ast);
                } break;

                case ast::stmt::ASSIGNMENT: {
                    auto& assignment_ast = *GET(*stmt_ast, ASSIGNMENT);
                    auto[result, type] = compile_expr(*assignment_ast.value);
                    auto lval = compile_left_expr(*assignment_ast.lvalue, type);

                    switch (INDEX(lval)) { // TODO add more assignment options, move to separate method
                        case lvalue::LOCAL_VAR: {
                            auto index = GET(lval, LOCAL_VAR);
                            auto& var = vars[index];
                            result = conv_clr.convert(assignment_ast, type, *var.tp, result);
                            add_instr(VARIANT(prog::instr, WRITE_VAR, make_ptr(prog::write_var_instr{ index, result })));
                        } break;

                        case lvalue::GLOBAL_VAR: {
                            auto index = GET(lval, GLOBAL_VAR);
                            auto& var = *clr.program.global_vars[index];
                            result = conv_clr.convert(assignment_ast, type, *var.tp, result);
                            add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, make_ptr(prog::write_global_var_instr{ index, result })));
                        } break;

                        default:
                            clr.error(diags::not_implemented(), assignment_ast);
                    }
                } break;

                case ast::stmt::COMPOUND_ASSIGNMENT:
                case ast::stmt::LOCALLY_BLOCK:
                case ast::stmt::SWAP:
                case ast::stmt::SWAP_BLOCK:
                case ast::stmt::IF:
                case ast::stmt::MATCH:
                case ast::stmt::WHILE:
                case ast::stmt::FOR:
                case ast::stmt::FUNC_DEF:
                    clr.error(diags::not_implemented(), *stmt_ast); // TODO
            }
        }

        if (cleanup)
            add_cleanup_instrs();
    }

    function_compiler::lvalue function_compiler::compile_left_expr(const ast::expr& ast, optional<reference_wrapper<const prog::type_local>> implicit_type) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
            case ast::expr::ARRAY:
            case ast::expr::APPLICATION:
                clr.error(diags::not_implemented(), ast); // TODO

            case ast::expr::NAME: {
                auto name = GET(ast, NAME);

                auto local_var = get_var(name);
                if (local_var)
                    return VARIANT(lvalue, LOCAL_VAR, *local_var);

                auto& global_name = clr.get_global_name(ast, name, global_name_kind::VARIABLE);
                return VARIANT(lvalue, GLOBAL_VAR, global_name.index);
            }

            case ast::expr::VAR_DECL: {
                auto& var_decl_ast = *GET(ast, VAR_DECL);
                if (!var_decl_ast.tp && !implicit_type)
                    clr.error(diags::variable_without_type(), var_decl_ast);
                auto type = var_decl_ast.tp ? clr.compile_type_local(**var_decl_ast.tp) : copy_type_local(*implicit_type);
                auto var = add_var(var_decl_ast.name, into_ptr(type));
                return VARIANT(lvalue, LOCAL_VAR, var);
            }

            case ast::expr::DEREFERENCE:
            case ast::expr::EXTRACT:
                clr.error(diags::not_implemented(), ast); // TODO

            default:
                clr.error(diags::expression_not_assignable(), ast);
        }
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_expr(const ast::expr& ast) {
        switch (INDEX(ast)) {
            case ast::expr::TUPLE:
                return compile_tuple(ast, GET(ast, TUPLE));

            case ast::expr::ARRAY:
                return compile_array(ast, GET(ast, ARRAY));

            case ast::expr::APPLICATION: {
                auto& [receiver_ast, args_ast] = GET(ast, APPLICATION);
                return compile_application(ast, *receiver_ast, args_ast);
            } break;

            case ast::expr::NAME: {
                auto& name = GET(ast, NAME);

                auto local_var = get_var(name);
                if (local_var) {
                    // TODO move out if not copyable
                    auto result = new_reg();
                    add_instr(VARIANT(prog::instr, READ_VAR, make_ptr(prog::read_var_instr{ *local_var, result })));
                    return { result, copy_type_local(*vars[*local_var].tp) };
                }

                auto& global_name = clr.get_global_name(ast, name);
                switch (global_name.kind) {
                    case global_name_kind::VARIABLE: {
                        auto& global_var = clr.program.global_vars[global_name.index];
                        auto result = new_reg();
                        add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, make_ptr(prog::read_global_var_instr{ global_name.index, result })));
                        return { result, prog::type_local { make_ptr(copy_type(*global_var->tp)), false } };
                    }

                    case global_name_kind::CONSTANT: {
                        auto& constant = clr.constants[global_name.index];
                        auto result = new_reg();
                        add_instr(VARIANT(prog::instr, MAKE_CONST, make_ptr(prog::make_const_instr{ make_ptr(copy_constant(*constant.value)), result })));
                        return { result, prog::type_local{ make_ptr(copy_type(*constant.tp)), false } };
                    }

                    case global_name_kind::FUNCTION: {
                        auto result = new_reg();
                        add_instr(VARIANT(prog::instr, MAKE_UNIT, result));
                        return { result, prog::type_local { make_ptr(VARIANT(prog::type, KNOWN_FUNC, global_name.index)), false } };
                    }

                    case global_name_kind::STRUCT: {
                        auto result = new_reg();
                        add_instr(VARIANT(prog::instr, MAKE_UNIT, result));
                        return { result, prog::type_local { make_ptr(VARIANT(prog::type, STRUCT_CTOR, global_name.index)), false } };
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
                auto& enum_type = *clr.program.enum_types[global_name.index];
                if (!enum_type.variant_names.count(variant_name))
                    clr.error(diags::invalid_enum_variant(enum_type, name), ast);

                prog::variant_index variant_index = enum_type.variant_names[variant_name];
                auto& enum_variant = *enum_type.variants[variant_index];

                if (enum_variant.tps.empty()) {
                    auto result = new_reg();
                    add_instr(VARIANT(prog::instr, MAKE_ENUM_VARIANT, make_ptr(prog::make_enum_variant_instr { global_name.index, variant_index, { }, result })));
                    return { result, prog::type_local{ make_ptr(VARIANT(prog::type, ENUM, global_name.index)), false } };
                }

                auto result = new_reg();
                add_instr(VARIANT(prog::instr, MAKE_UNIT, result));
                return { result, prog::type_local { make_ptr(VARIANT(prog::type, ENUM_CTOR, make_pair(global_name.index, variant_index))), false } };
            }

            case ast::expr::LITERAL: {
                auto& literal_ast = *GET(ast, LITERAL);
                auto[constant, type] = clr.compile_constant_literal(literal_ast);
                auto result = new_reg();
                add_instr(VARIANT(prog::instr, MAKE_CONST, make_ptr(prog::make_const_instr{ into_ptr(constant), result })));
                return { result, prog::type_local{ into_ptr(type), false } };
            }

            case ast::expr::UNARY_OPERATION:
            case ast::expr::BINARY_OPERATION:
            case ast::expr::NUMERIC_CAST:
                clr.error(diags::not_implemented(), ast); // TODO

            case ast::expr::NONE: {
                auto result = new_reg();
                add_instr(VARIANT(prog::instr, MAKE_OPTIONAL, make_ptr(prog::make_optional_instr { { }, result })));
                auto type = make_ptr(VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, NEVER, monostate()))));
                return { result, prog::type_local { move(type), false } };
            }

            case ast::expr::SOME: {
                auto& expr_ast = *GET(ast, SOME);
                auto[value, type] = compile_expr(expr_ast);
                auto result = new_reg();
                add_instr(VARIANT(prog::instr, MAKE_OPTIONAL, make_ptr(prog::make_optional_instr{ { value }, result })));
                return { result, prog::type_local { make_ptr(VARIANT(prog::type, OPTIONAL, move(type.tp))), type.confined } };
            }

            case ast::expr::RETURN: {
                auto& return_expr = GET(ast, RETURN);
                add_return_instr(return_expr);
                frames.back().always_returns = true;
                auto result = new_reg();
                add_instr(VARIANT(prog::instr, MAKE_UNIT, result));
                return { result, prog::type_local{ make_ptr(VARIANT(prog::type, NEVER, monostate())), false } };
            }

            case ast::expr::BREAK:
            case ast::expr::CONTINUE:
            case ast::expr::REFERENCE:
            case ast::expr::HEAP_ALLOC:
            case ast::expr::DEREFERENCE:
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

    pair<prog::reg_index, prog::type_local> function_compiler::compile_tuple(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items_ast) {
        auto size = items_ast.size();
        vector<bool> used_items(size, false);
        vector<prog::reg_index> values(size);
        vector<prog::type> types(size);
        optional<bool> confined;

        for (auto& item_ast : items_ast) { // TODO refactor
            size_t index = 0;
            ast::expr* value_ast;

            switch (INDEX(*item_ast)) {
                case ast::expr_marked::EXPR: {
                    while (index < size && used_items[index])
                        index++;
                    value_ast = GET(*item_ast, EXPR).get();
                } break;

                case ast::expr_marked::EXPR_WITH_NAME:
                    clr.error(diags::invalid_expression_marker(), *item_ast);

                case ast::expr_marked::EXPR_WITH_COORD: {
                    index = GET(*item_ast, EXPR_WITH_COORD).first;
                    value_ast = GET(*item_ast, EXPR_WITH_COORD).second.get();
                } break;
            }

            if (index >= size)
                clr.error(diags::invalid_argument(index, size), *item_ast);
            if (used_items[index])
                clr.error(diags::reused_argument(index), *item_ast);

            auto[value, type] = compile_expr(*value_ast);

            if (!clr.type_trivially_copyable(*type.tp)) {
                if (!confined)
                    confined = { type.confined };
                else if (type.confined != *confined)
                    clr.error(diags::type_confinement_mismatch(type.confined, *confined, false), *value_ast);
            }
            
            values[index] = value;
            types[index] = move(*type.tp);
            used_items[index] = true;
        }

        if (!confined)
            confined = { false };

        for (size_t index = 0; index < size; index++) {
            if (!used_items[index])
                clr.error(diags::missing_argument(index), ast);
        }
        
        if (size == 0) {
            auto result = new_reg();
            add_instr(VARIANT(prog::instr, MAKE_UNIT, result));
            auto type = prog::type_local { make_ptr(VARIANT(prog::type, UNIT, prog::monostate())), false };
            return { result, move(type) };
        }

        if (size == 1)
            return { values[0], prog::type_local { into_ptr(types[0]), *confined } };
        
        auto result = new_reg();
        add_instr(VARIANT(prog::instr, MAKE_TUPLE, make_ptr(prog::make_tuple_instr{ values, result })));
        auto type = prog::type_local{ make_ptr(VARIANT(prog::type, TUPLE, into_ptr_vector(types))), *confined };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_array(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& items_ast) {
        auto size = items_ast.size();
        vector<bool> used_items(size, false);
        vector<prog::reg_index> values(size);
        vector<prog::type> types(size);
        auto common_type = VARIANT(prog::type, NEVER, monostate());
        optional<bool> confined;

        for (auto& item_ast : items_ast) {
            size_t index = 0;
            ast::expr* value_ast;

            switch (INDEX(*item_ast)) {
                case ast::expr_marked::EXPR: {
                    while (index < size && used_items[index])
                        index++;
                    value_ast = GET(*item_ast, EXPR).get();
                } break;

                case ast::expr_marked::EXPR_WITH_NAME:
                    clr.error(diags::invalid_expression_marker(), *item_ast);

                case ast::expr_marked::EXPR_WITH_COORD: {
                    index = GET(*item_ast, EXPR_WITH_COORD).first;
                    value_ast = GET(*item_ast, EXPR_WITH_COORD).second.get();
                } break;
            }

            if (index >= size)
                clr.error(diags::invalid_argument(index, size), *item_ast);
            if (used_items[index])
                clr.error(diags::reused_argument(index), *item_ast);

            auto[value, type] = compile_expr(*value_ast);

            if (!clr.type_trivially_copyable(*type.tp)) {
                if (!confined)
                    confined = { type.confined };
                else if (type.confined != *confined)
                    clr.error(diags::type_confinement_mismatch(type.confined, *confined, false), *value_ast);
            }

            common_type = clr.common_supertype(ast, common_type, *type.tp);
            values[index] = value;
            types[index] = move(*type.tp);
            used_items[index] = true;
        }

        if (!confined)
            confined = { false };

        for (size_t index = 0; index < size; index++) {
            if (!used_items[index])
                clr.error(diags::missing_argument(index), ast);
            values[index] = conv_clr.convert(ast, types[index], common_type, *confined, values[index]);
        }

        auto result = new_reg();
        add_instr(VARIANT(prog::instr, MAKE_ARRAY, make_ptr(prog::make_array_instr{ values, result })));
        auto type = prog::type_local{ make_ptr(VARIANT(prog::type, ARRAY, make_ptr(prog::array_type { into_ptr(common_type), size }))), *confined };
        return { result, move(type) };
    }

    pair<prog::reg_index, prog::type_local> function_compiler::compile_application(const ast::node& ast, const ast::expr& receiver_ast, const vector<ast::ptr<ast::expr_marked>>& args_ast) {
        auto[receiver, receiver_type] = compile_expr(receiver_ast);

        switch (INDEX(*receiver_type.tp)) {
            case prog::type::STRUCT_CTOR: {
                auto struct_index = GET(*receiver_type.tp, STRUCT_CTOR);
                auto& struct_type = *clr.program.struct_types[struct_index];

                size_t num_args = struct_type.fields.size();
                vector<bool> used_args(num_args, false);
                vector<prog::reg_index> args(num_args);
                vector<prog::type> types(num_args);
                optional<bool> confined;

                for (auto& arg_ast : args_ast) {
                    size_t index = 0;
                    ast::expr* value_ast;

                    switch (INDEX(*arg_ast)) {
                        case ast::expr_marked::EXPR: {
                            while (index < num_args && used_args[index])
                                index++;
                            value_ast = GET(*arg_ast, EXPR).get();
                        } break;

                        case ast::expr_marked::EXPR_WITH_NAME: {
                            auto& name = GET(*arg_ast, EXPR_WITH_NAME).first;
                            if (!struct_type.field_names.count(name))
                                clr.error(diags::invalid_struct_field(struct_type, name), *arg_ast);
                            index = struct_type.field_names[name];
                            value_ast = GET(*arg_ast, EXPR_WITH_NAME).second.get();
                        } break;

                        case ast::expr_marked::EXPR_WITH_COORD: {
                            index = GET(*arg_ast, EXPR_WITH_COORD).first;
                            value_ast = GET(*arg_ast, EXPR_WITH_COORD).second.get();
                        } break;
                    }

                    if (index >= num_args)
                        clr.error(diags::invalid_argument(index, num_args), *arg_ast);
                    if (used_args[index])
                        clr.error(diags::reused_argument(index), *arg_ast);

                    auto[value, type] = compile_expr(*value_ast);

                    if (!clr.type_trivially_copyable(*type.tp)) {
                        if (!confined)
                            confined = { type.confined };
                        else if (type.confined != *confined)
                            clr.error(diags::type_confinement_mismatch(type.confined, *confined, false), *value_ast);
                    }

                    args[index] = value;
                    types[index] = move(*type.tp);
                    used_args[index] = true;
                }

                if (!confined)
                    confined = { false };

                for (size_t index = 0; index < num_args; index++) {
                    if (!used_args[index])
                        clr.error(diags::missing_argument(index), ast);
                    auto& type = types[index];
                    auto& declared_type = *struct_type.fields[index]->tp;
                    args[index] = conv_clr.convert(ast, type, declared_type, *confined, args[index]);
                }

                auto result = new_reg();
                add_instr(VARIANT(prog::instr, MAKE_STRUCT, make_ptr(prog::make_struct_instr{ struct_index, args, result })));
                auto type = prog::type_local{ make_ptr(VARIANT(prog::type, STRUCT, struct_index)), *confined };
                return { result, move(type) };
            }

            case prog::type::ENUM_CTOR: {
                auto [enum_index, variant_index] = GET(*receiver_type.tp, ENUM_CTOR);
                auto& enum_type = *clr.program.enum_types[enum_index];
                auto& variant = *enum_type.variants[variant_index];

                auto num_args = variant.tps.size();
                vector<bool> used_args(num_args, false);
                vector<prog::reg_index> args(num_args);
                vector<prog::type> types(num_args);
                optional<bool> confined;

                for (auto& arg_ast : args_ast) {
                    size_t index = 0;
                    ast::expr* value_ast;

                    switch (INDEX(*arg_ast)) {
                        case ast::expr_marked::EXPR: {
                            while (index < num_args && used_args[index])
                                index++;
                            value_ast = GET(*arg_ast, EXPR).get();
                        } break;

                        case ast::expr_marked::EXPR_WITH_NAME:
                            clr.error(diags::invalid_expression_marker(), *arg_ast);

                        case ast::expr_marked::EXPR_WITH_COORD: {
                            index = GET(*arg_ast, EXPR_WITH_COORD).first;
                            value_ast = GET(*arg_ast, EXPR_WITH_COORD).second.get();
                        } break;
                    }

                    if (index >= num_args)
                        clr.error(diags::invalid_argument(index, num_args), *arg_ast);
                    if (used_args[index])
                        clr.error(diags::reused_argument(index), *arg_ast);

                    auto[value, type] = compile_expr(*value_ast);

                    if (!clr.type_trivially_copyable(*type.tp)) {
                        if (!confined)
                            confined = { type.confined };
                        else if (type.confined != *confined)
                            clr.error(diags::type_confinement_mismatch(type.confined, *confined, false), *value_ast);
                    }

                    args[index] = value;
                    types[index] = move(*type.tp);
                    used_args[index] = true;
                }

                if (!confined)
                    confined = { false };

                for (size_t index = 0; index < num_args; index++) {
                    if (!used_args[index])
                        clr.error(diags::missing_argument(index), ast);
                    auto& type = types[index];
                    auto& declared_type = *variant.tps[index];
                    args[index] = conv_clr.convert(ast, type, declared_type, *confined, args[index]);
                }

                auto result = new_reg();
                add_instr(VARIANT(prog::instr, MAKE_ENUM_VARIANT, make_ptr(prog::make_enum_variant_instr{ enum_index, variant_index, args, result })));
                auto type = prog::type_local{ make_ptr(VARIANT(prog::type, ENUM, enum_index)), *confined };
                return { result, move(type) };
            }

            case prog::type::FUNC:
            case prog::type::FUNC_WITH_PTR: {
                auto& ftype = INDEX_EQ(*receiver_type.tp, FUNC) ? *GET(*receiver_type.tp, FUNC) : *GET(*receiver_type.tp, FUNC_WITH_PTR);

                auto args = compile_call_arguments(ast, args_ast, ftype, { });

                auto ptr = new_reg();
                add_instr(VARIANT(prog::instr, EXTRACT_PTR, make_ptr(prog::ptr_conversion_instr { receiver, ptr })));
                args.insert(args.begin(), ptr);

                auto func = new_reg();
                add_instr(VARIANT(prog::instr, EXTRACT_FUNC, make_ptr(prog::ptr_conversion_instr { receiver, func })));

                auto result = new_reg();
                add_instr(VARIANT(prog::instr, FUNC_PTR_CALL, make_ptr(prog::func_ptr_call_instr { func, args, result })));
                auto type = prog::type_local{ make_ptr(copy_type(*ftype.return_tp)), false };
                return { result, move(type) };
            } break;

            case prog::type::GLOBAL_FUNC: {
                auto& ftype = *GET(*receiver_type.tp, GLOBAL_FUNC);

                auto args = compile_call_arguments(ast, args_ast, ftype, { });

                auto result = new_reg();
                add_instr(VARIANT(prog::instr, FUNC_PTR_CALL, make_ptr(prog::func_ptr_call_instr { receiver, args, result })));
                auto type = prog::type_local{ make_ptr(copy_type(*ftype.return_tp)), false };
                return { result, move(type) };
            } break;

            case prog::type::KNOWN_FUNC: {
                auto func_index = GET(*receiver_type.tp, KNOWN_FUNC);
                auto& func = *clr.program.global_funcs[func_index];
                auto ftype = prog::get_func_type(func);

                auto args = compile_call_arguments(ast, args_ast, ftype, { func.param_names });

                auto result = new_reg();
                add_instr(VARIANT(prog::instr, FUNC_CALL, make_ptr(prog::func_call_instr { func_index, args, result })));
                auto type = prog::type_local{ make_ptr(copy_type(*func.return_tp)), false };
                return { result, move(type) };
            } break;

            default:
                clr.error(diags::invalid_expression(), ast);
        }
    }

    vector<prog::reg_index> function_compiler::compile_call_arguments(const ast::node& ast, const vector<ast::ptr<ast::expr_marked>>& args_ast, const prog::func_type& ftype, optional<reference_wrapper<const unordered_map<string, size_t>>> param_names) {
        auto num_args = ftype.param_tps.size();
        vector<bool> used_args(num_args, false);
        vector<prog::reg_index> args(num_args);
        vector<prog::type_local> types(num_args);

        for (auto& arg_ast : args_ast) {
            size_t index = 0;
            ast::expr* value_ast;

            switch (INDEX(*arg_ast)) {
                case ast::expr_marked::EXPR: {
                    while (index < num_args && used_args[index])
                        index++;
                    value_ast = GET(*arg_ast, EXPR).get();
                } break;

                case ast::expr_marked::EXPR_WITH_NAME: {
                    if (!param_names)
                        clr.error(diags::invalid_expression_marker(), *arg_ast);
                    auto& name = GET(*arg_ast, EXPR_WITH_NAME).first;
                    if (!param_names->get().count(name))
                        clr.error(diags::invalid_argument_name(func, name), *arg_ast);
                    index = param_names->get().at(name);
                    value_ast = GET(*arg_ast, EXPR_WITH_NAME).second.get();
                } break;

                case ast::expr_marked::EXPR_WITH_COORD: {
                    index = GET(*arg_ast, EXPR_WITH_COORD).first;
                    value_ast = GET(*arg_ast, EXPR_WITH_COORD).second.get();
                } break;
            }

            if (index >= num_args)
                clr.error(diags::invalid_argument(index, num_args), *arg_ast);
            if (used_args[index])
                clr.error(diags::reused_argument(index), *arg_ast);

            auto[value, type] = compile_expr(*value_ast);
            args[index] = value;
            types[index] = move(type);
            used_args[index] = true;
        }

        for (size_t index = 0; index < num_args; index++) {
            if (!used_args[index])
                clr.error(diags::missing_argument(index), ast);
            auto& type = types[index];
            auto& declared_type = *ftype.param_tps[index];
            args[index] = conv_clr.convert_call(ast, type, declared_type, args[index]);
        }

        return args;
    }
}
