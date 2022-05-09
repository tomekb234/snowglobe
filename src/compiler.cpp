#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "utils.hpp"
#include <utility>
#include <variant>
#include <limits>

namespace sg {
    using namespace sg::utils;

    using std::move;
    using std::get;
    using std::numeric_limits;
    using std::is_signed;
    using std::is_unsigned;
    using std::to_string;
    using std::bind;
    using std::placeholders::_1;

    optional<prog::program> compiler::compile(const ast::program& ast) {
        try {
            program = { };
            compile_program(ast);
            return { move(program) };
        } catch (...) {
            return { };
        }
    }

    void compiler::compile_program(const ast::program& ast) {
        // Phase 1: Prepare struct and enum declarations

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *get<ast::global_def::STRUCT_DEF>(global_def->value);
                    auto struct_type = declare_struct_type(struct_type_ast);
                    auto name = struct_type.name;
                    auto index = program.struct_types.size();
                    program.struct_types.push_back(into_ptr(struct_type));
                    global_names[name] = { global_name::STRUCT, index, false };
                    break;
                }

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *get<ast::global_def::ENUM_DEF>(global_def->value);
                    auto enum_type = declare_enum_type(enum_type_ast);
                    auto name = enum_type.name;
                    auto index = program.enum_types.size();
                    program.enum_types.push_back(into_ptr(enum_type));
                    global_names[name] = { global_name::ENUM, index, false };
                    break;
                }

            }
        }

        // Phase 2: Compile struct, enum definitions and constants

        size_t struct_type_index = 0;
        size_t enum_type_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *get<ast::global_def::STRUCT_DEF>(global_def->value);
                    auto& struct_type = *program.struct_types[struct_type_index++];
                    compile_struct_type(struct_type_ast, struct_type);
                    global_names[struct_type.name].compiled = true;
                    break;
                }

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *get<ast::global_def::ENUM_DEF>(global_def->value);
                    auto& enum_type = *program.enum_types[enum_type_index++];
                    compile_enum_type(enum_type_ast, enum_type);
                    global_names[enum_type.name].compiled = true;
                    break;
                }

                case ast::global_def::CONST_DEF: {
                    auto& global_const_ast = *get<ast::global_def::CONST_DEF>(global_def->value);
                    auto global_const = compile_global_var(global_const_ast);
                    auto name = global_const.name;
                    auto index = constants.size();
                    constants.push_back(move(global_const));
                    global_names[name] = { global_name::CONSTANT, index, true };
                    break;
                }
            }
        }

        // Phase 3: Prepare global function declarations

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *get<ast::global_def::FUNC_DEF>(global_def->value);
                    auto global_func = declare_global_func(global_func_ast);
                    auto name = global_func.name;
                    auto index = program.global_funcs.size();
                    program.global_funcs.push_back(into_ptr(global_func));
                    global_names[name] = { global_name::FUNCTION, index, false };
                    break;
                }
            }
        }


        // Phase 4: Compile global variable definitions

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::VAR_DEF: {
                    auto& global_var_ast = *get<ast::global_def::VAR_DEF>(global_def->value);
                    auto global_var = compile_global_var(global_var_ast);
                    auto name = global_var.name;
                    auto index = program.global_vars.size();
                    program.global_vars.push_back(into_ptr(global_var));
                    global_names[name] = { global_name::VARIABLE, index, true };
                    break;
                }
            }
        }

        // Phase 5: Compile global function definitions

        size_t global_func_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *get<ast::global_def::FUNC_DEF>(global_def->value);
                    auto& global_func = *program.global_funcs[global_func_index++];
                    compile_global_func(global_func_ast, global_func);
                    global_names[global_func.name].compiled = true;
                    break;
                }
            }
        }
    }

    prog::global_var compiler::compile_global_var(const ast::var_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used_error(name), ast);

        auto[value, value_tp] = compile_constant_expr(*ast.value);

        prog::type tp;

        if (ast.tp) {
            tp = compile_type(**ast.tp);
            value = convert_constant(ast, value, value_tp, tp);
        } else
            tp = move(value_tp);

        return { name, into_ptr(tp), into_ptr(value) };
    }

    prog::global_func compiler::declare_global_func(const ast::func_def& ast) {
        error(diags::not_implemented_error(), ast); // TODO
    }

    prog::struct_type compiler::declare_struct_type(const ast::struct_def& ast) {
        error(diags::not_implemented_error(), ast); // TODO
    }

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        error(diags::not_implemented_error(), ast); // TODO
    }

    void compiler::compile_global_func(const ast::func_def& ast, prog::global_func& global_func) {
        error(diags::not_implemented_error(), ast); // TODO
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type) {
        error(diags::not_implemented_error(), ast); // TODO
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type) {
        error(diags::not_implemented_error(), ast); // TODO
    }

    template<typename T>
    static optional<T> try_make_number(unsigned long long abs_value, bool negative) {
        if (abs_value == 0)
            return 0;
        if (!negative)
            return abs_value <= numeric_limits<T>::max() ? static_cast<T>(abs_value) : optional<T>();
        if (is_unsigned<T>())
            return { };
        return abs_value - 1 <= numeric_limits<T>::max() ? -static_cast<T>(abs_value - 1) - 1 : optional<T>();
    }

    template<typename T>
    static unsigned long long encode_number(T number) {
        unsigned long long result;
        reinterpret_cast<T&>(result) = number;
        return result;
    }

    template<typename T>
    static T decode_number(unsigned long long number) {
        return reinterpret_cast<T&>(number);
    }

    pair<prog::constant, prog::type> compiler::compile_constant_expr(const ast::expr& ast) {
        switch (ast.value.index()) { // TODO more constant expressions
            case ast::expr::TUPLE: {
                return compile_constant_tuple(get<ast::expr::TUPLE>(ast.value), ast);
            }

            case ast::expr::ARRAY: {
                return compile_constant_array(get<ast::expr::ARRAY>(ast.value), ast);
            }

            case ast::expr::NAME: {
                return compile_constant_name(get<ast::expr::NAME>(ast.value), ast);
            }

            case ast::expr::CONST: {
                return compile_constant_literal(*get<ast::expr::CONST>(ast.value));
            }

            case ast::expr::SOME: {
                auto[inner_value, inner_type] = compile_constant_expr(*get<ast::expr::SOME>(ast.value));
                auto value = VARIANT(prog::constant, SOME, into_ptr(inner_value));
                auto type = VARIANT(prog::type, OPTIONAL, into_ptr(inner_type));
                return { move(value), move(type) };
            }

            case ast::expr::NONE: {
                auto value = VARIANT(prog::constant, NONE, std::monostate{ });
                auto type = VARIANT(prog::type, OPTIONAL, make_ptr(VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{ prog::primitive_type::NEVER }))));
                return { move(value), move(type) };
            }

            default:
                error(diags::expression_not_constant(), ast);
        }
    }
    
    pair<prog::constant, prog::type> compiler::compile_constant_tuple(const vector<ast::ptr<ast::expr_marked>>& asts, const ast::node& ast) {
        vector<prog::ptr<prog::constant>> values;
        vector<prog::ptr<prog::type>> types;

        for (auto& item_ptr : asts) {
            auto& expr_marked = *item_ptr;
            switch (expr_marked.value.index()) {
                case ast::expr_marked::EXPR: {
                    auto[value, type] = compile_constant_expr(*get<ast::expr_marked::EXPR>(expr_marked.value));
                    values.push_back(into_ptr(value));
                    types.push_back(into_ptr(type));
                    break;
                }

                default:
                    error(diags::expression_not_constant(), ast);
            }
        }

        return { VARIANT(prog::constant, TUPLE, move(values)), VARIANT(prog::type, TUPLE, move(types)) };
    }

    pair<prog::constant, prog::type> compiler::compile_constant_array(const vector<ast::ptr<ast::expr_marked>>& asts, const ast::node& ast) {
        vector<prog::ptr<prog::constant>> values;
        prog::type type;

        for (auto& item_ptr : asts) {
            auto& expr_marked = *item_ptr;
            switch (expr_marked.value.index()) {
                case ast::expr_marked::EXPR: {
                    auto[item_value, item_type] = compile_constant_expr(*get<ast::expr_marked::EXPR>(expr_marked.value));
                    if (!values.empty() && !types_equal(expr_marked, type, item_type))
                        error(diags::different_types_in_array(), ast);
                    if (values.empty())
                        type = move(item_type);
                    values.push_back(into_ptr(item_value));
                    break;
                }

                default:
                    error(diags::expression_not_constant(), expr_marked);
            }
        }

        if (values.empty())
            type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{ prog::primitive_type::NEVER }));
        return { VARIANT(prog::constant, ARRAY, move(values)), VARIANT(prog::type, ARRAY, make_ptr(prog::array_type{ into_ptr(type), values.size() })) };
    }

    pair<prog::constant, prog::type> compiler::compile_constant_name(const string& name, const ast::node& ast) {
        if (!global_names.count(name))
            error(diags::name_not_declared_error(name), ast);
        auto& global_name = global_names[name];

        switch (global_name.kind) {
            case global_name::VARIABLE:
                error(diags::expression_not_constant(), ast);

            case global_name::CONSTANT: {
                auto& global_var = constants[global_name.index];
                return { copy_constant(*global_var.value), copy_type(*global_var.tp) };
            }

            default:
                error(diags::not_implemented_error(), ast);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_constant_literal(const ast::const_expr& ast) {
        switch (ast.value.index()) {
            case ast::const_expr::BOOL: {
                auto value = VARIANT(prog::constant, BOOL, get<ast::const_expr::BOOL>(ast.value));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::BOOL}));
                return {move(value), move(type)};
            }

            case ast::const_expr::CHAR: {
                auto value = VARIANT(prog::constant, INT, encode_number(static_cast<uint8_t>(get<ast::const_expr::CHAR>(ast.value))));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::U8}));
                return {move(value), move(type)};
            }

            case ast::const_expr::STRING: {
                error(diags::not_implemented_error(), ast); // TODO
            }

            case ast::const_expr::INT: {
                auto[value, type] = compile_int_token(*get<ast::const_expr::INT>(ast.value));
                return {move(value), VARIANT(prog::type, PRIMITIVE, into_ptr(type))};
            }

            case ast::const_expr::FLOAT: {
                auto[value, type] = compile_float_token(*get<ast::const_expr::FLOAT>(ast.value));
                return {move(value), VARIANT(prog::type, PRIMITIVE, into_ptr(type))};
            }

            default:
                error(diags::not_implemented_error(), ast);
        }
    }

    pair<prog::constant, prog::primitive_type> compiler::compile_int_token(const ast::int_token& ast) {
#define RETURN_IF_OK(type, type_marker) {auto opt_val = try_make_number<type>(ast.value, ast.negative); if(opt_val) return { VARIANT(prog::constant, INT, encode_number(*opt_val)), { prog::primitive_type::type_marker } }; }
#define RETURN_OR_ERROR(type, type_marker) {RETURN_IF_OK(type, type_marker) error(diags::integer_overflow_error((ast.negative ? "-" : "") + to_string(ast.value), is_signed<type>(), 8 * sizeof(type)), ast); throw 0;}

        switch (ast.marker) {
            case ast::int_token::NONE:
            case ast::int_token::I:{
                RETURN_IF_OK(int8_t, I8)
                RETURN_IF_OK(int16_t, I16)
                RETURN_IF_OK(int32_t, I32)
                RETURN_OR_ERROR(int64_t, I64)
            }

            case ast::int_token::I8:
                RETURN_OR_ERROR(int8_t, I8)

            case ast::int_token::I16:
                RETURN_OR_ERROR(int16_t, I16)

            case ast::int_token::I32:
                RETURN_OR_ERROR(int32_t, I32)

            case ast::int_token::I64:
                RETURN_OR_ERROR(int64_t, I64)

            case ast::int_token::U:{
                RETURN_IF_OK(uint8_t, U8)
                RETURN_IF_OK(uint16_t, U16)
                RETURN_IF_OK(uint32_t, U32)
                RETURN_OR_ERROR(uint64_t, U64)
            }

            case ast::int_token::U8:
                RETURN_OR_ERROR(uint8_t, U8)

            case ast::int_token::U16:
                RETURN_OR_ERROR(uint16_t, U16)

            case ast::int_token::U32:
                RETURN_OR_ERROR(uint32_t, U32)

            case ast::int_token::U64:
                RETURN_OR_ERROR(uint64_t, U64)

            default:
                throw 0; // unreachable state
        }

#undef RETURN_OR_ERROR
#undef RETURN_IF_OK
    }

    pair<prog::constant, prog::primitive_type> compiler::compile_float_token(const ast::float_token& ast) {
        double value = ast.negative ? -ast.value : ast.value;
        switch (ast.marker) {
            case ast::float_token::NONE:
            case ast::float_token::F:
            case ast::float_token::F64: {
                return { VARIANT(prog::constant, FLOAT, value), {prog::primitive_type::F64} };
            }

            case ast::float_token::F32: {
                return { VARIANT(prog::constant, FLOAT, value), {prog::primitive_type::F32} };
            }

            default:
                throw 0; // unreachable state
        }
    }

    prog::type compiler::compile_type(const ast::type& ast) {
        switch (ast.value.index()) {
            case ast::type::PRIMITIVE: {
                auto tp = compile_primitive_type(*get<ast::type::PRIMITIVE>(ast.value));
                return VARIANT(prog::type, PRIMITIVE, into_ptr(tp));
            }

            case ast::type::USER_TYPE: {
                error(diags::not_implemented_error(), ast); // TODO
            }
            
            case ast::type::TUPLE: {
                auto types = compile_tuple_type(get<ast::type::TUPLE>(ast.value));
                return VARIANT(prog::type, TUPLE, move(types));
            }

            case ast::type::ARRAY: {
                auto tp = compile_array_type(*get<ast::type::ARRAY>(ast.value));
                return VARIANT(prog::type, ARRAY, into_ptr(tp));
            }

            case ast::type::OPTIONAL: {
                auto tp = compile_type(*get<ast::type::OPTIONAL>(ast.value));
                return VARIANT(prog::type, OPTIONAL, into_ptr(tp));
            }

            case ast::type::PTR: {
                auto tp = compile_ptr_type(*get<ast::type::PTR>(ast.value));
                return VARIANT(prog::type, PTR, into_ptr(tp));
            }

            case ast::type::INNER_PTR: {
                auto tp = compile_inner_ptr_type(*get<ast::type::INNER_PTR>(ast.value));
                return VARIANT(prog::type, INNER_PTR, into_ptr(tp));
            }

            case ast::type::FUNC: {
                auto tp = compile_func_type(*get<ast::type::FUNC>(ast.value));
                return VARIANT(prog::type, FUNC, into_ptr(tp));
            }

            case ast::type::GLOBAL_FUNC: {
                auto tp = compile_func_type(*get<ast::type::GLOBAL_FUNC>(ast.value));
                return VARIANT(prog::type, GLOBAL_FUNC, into_ptr(tp));
            }

            case ast::type::FUNC_WITH_PTR: {
                auto tp = compile_func_with_ptr_type(*get<ast::type::FUNC_WITH_PTR>(ast.value));
                return VARIANT(prog::type, FUNC_WITH_PTR, into_ptr(tp));
            }

            default:
                throw 0; // unreachable state
        }
    }
    
    prog::func_with_ptr_type compiler::compile_func_with_ptr_type(const ast::func_with_ptr_type& ast) {
        auto base = compile_func_type(ast);

        decltype(prog::func_with_ptr_type::kind) kind;

        switch(ast.kind) {
            case ast::func_with_ptr_type::BASIC: {
                kind = prog::func_with_ptr_type::BASIC;
                break;
            }

            case ast::func_with_ptr_type::SHARED: {
                kind = prog::func_with_ptr_type::SHARED;
                break;
            }

            case ast::func_with_ptr_type::WEAK: {
                kind = prog::func_with_ptr_type::WEAK;
                break;
            }

            case ast::func_with_ptr_type::UNIQUE: {
                kind = prog::func_with_ptr_type::UNIQUE;
                break;
            }

            default:
                throw 0; // unreachable state
        }

        auto target_tp = compile_type_pointed(*ast.target_tp);

        return { move(base.param_tps), move(base.return_tp), kind, into_ptr(target_tp) };
    }

    prog::type_local compiler::compile_type_local(const ast::type_local& ast) {
        auto tp = compile_type(*ast.tp);
        return { into_ptr(tp), ast.confined };
    }

    prog::func_type compiler::compile_func_type(const ast::func_type& ast) {
        vector<prog::ptr<prog::type_local>> param_tps;

        for(const auto& param_tp : ast.param_tps) {
            auto&& tp = compile_type_local(*param_tp);
            param_tps.push_back(into_ptr(tp));
        }

        auto return_tp = compile_type(*ast.return_tp);

        return { move(param_tps), into_ptr(return_tp) };
    }

    prog::inner_ptr_type compiler::compile_inner_ptr_type(const ast::inner_ptr_type& ast) {
        auto base = compile_ptr_type(ast);
        auto owner_tp = compile_type_pointed(*ast.owner_tp);

        return { base.kind, move(base.target_tp), into_ptr(owner_tp) };
    }

    prog::type_pointed compiler::compile_type_pointed(const ast::type_pointed& ast) {
        prog::type&& tp = compile_type(*ast.tp);
        return { into_ptr(tp), ast.slice };
    }

    prog::ptr_type compiler::compile_ptr_type(const ast::ptr_type& ast) {
        decltype(prog::ptr_type::kind) kind;

        switch(ast.kind) {
            case ast::ptr_type::GLOBAL: {
                kind = prog::ptr_type::GLOBAL;
                break;
            }

            case ast::ptr_type::BASIC: {
                kind = prog::ptr_type::BASIC;
                break;
            }

            case ast::ptr_type::SHARED: {
                kind = prog::ptr_type::SHARED;
                break;
            }

            case ast::ptr_type::WEAK: {
                kind = prog::ptr_type::WEAK;
                break;
            }

            case ast::ptr_type::UNIQUE: {
                kind = prog::ptr_type::UNIQUE;
                break;
            }

            default:
                throw 0; // unreachable state
        }

        auto target_tp = compile_type_pointed(*ast.target_tp);

        return { kind, into_ptr(target_tp) };
    }

    prog::array_type compiler::compile_array_type(const ast::array_type& ast) {
        prog::type&& tp = compile_type(*ast.tp);
        return { into_ptr(tp), get<ast::const_integer::INTEGER>(ast.size->value) };
    }

    vector<prog::ptr<prog::type>> compiler::compile_tuple_type(const vector<ast::ptr<ast::type>>& ast) {
        vector<prog::ptr<prog::type>> result;

        for(const auto& ast_type : ast) {
            prog::type&& type = compile_type(*ast_type); 
            result.push_back(into_ptr(type));
        }

        return result;
    }

    prog::primitive_type compiler::compile_primitive_type(const ast::primitive_type& ast) {
        switch(ast.tp) {
            case ast::primitive_type::BOOL: {
                return { prog::primitive_type::BOOL };
            }

            case ast::primitive_type::I8: {
                return { prog::primitive_type::I8 };
            }

            case ast::primitive_type::I16: {
                return { prog::primitive_type::I16 };
            }

            case ast::primitive_type::I32: {
                return { prog::primitive_type::I32 };
            }

            case ast::primitive_type::I64: {
                return { prog::primitive_type::I64 };
            }
            case ast::primitive_type::U8: {
                return { prog::primitive_type::U8 };
            }

            case ast::primitive_type::U16: {
                return { prog::primitive_type::U16 };
            }

            case ast::primitive_type::U32: {
                return { prog::primitive_type::U32 };
            }

            case ast::primitive_type::U64: {
                return { prog::primitive_type::U64 };
            }

            case ast::primitive_type::F32: {
                return { prog::primitive_type::F32 };
            }

            case ast::primitive_type::F64: {
                return { prog::primitive_type::F64 };
            }

            case ast::primitive_type::NEVER: {
                return { prog::primitive_type::NEVER };
            }
            
            default:
                throw 0; // unreachable state
        }
    }

    prog::constant compiler::convert_constant(const ast::node& ast, const prog::constant& constant, const prog::type& from_tp, const prog::type& to_tp) {
        error(diags::not_implemented_error(), ast); // TODO
    }

    bool compiler::types_equal(const ast::node& ast, const prog::type& type1, const prog::type& type2) {
        error(diags::not_implemented_error(), ast); // TODO
    }

    prog::constant compiler::copy_constant(const prog::constant& source) {
        switch (source.value.index()) {
            case prog::constant::BOOL:
                return VARIANT(prog::constant, BOOL, get<prog::constant::BOOL>(source.value));

            case prog::constant::INT:
                return VARIANT(prog::constant, INT, get<prog::constant::INT>(source.value));

            case prog::constant::FLOAT:
                return VARIANT(prog::constant, FLOAT, get<prog::constant::FLOAT>(source.value));

            case prog::constant::STRUCT: {
                auto vec = copy_ptr_vector<prog::constant>(get<prog::constant::STRUCT>(source.value), bind(&compiler::copy_constant, this, _1));
                return VARIANT(prog::constant, STRUCT, move(vec));
            }

            case prog::constant::ENUM: {
                auto& p = get<prog::constant::ENUM>(source.value);
                auto vec = copy_ptr_vector<prog::constant>(p.second, bind(&compiler::copy_constant, this, _1));
                return VARIANT(prog::constant, ENUM, make_pair( p.first, move(vec) ));
            }

            case prog::constant::TUPLE: {
                auto vec = copy_ptr_vector<prog::constant>(get<prog::constant::TUPLE>(source.value), bind(&compiler::copy_constant, this, _1));
                return VARIANT(prog::constant, TUPLE, move(vec));
            }

            case prog::constant::ARRAY: {
                auto vec = copy_ptr_vector<prog::constant>(get<prog::constant::ARRAY>(source.value), bind(&compiler::copy_constant, this, _1));
                return VARIANT(prog::constant, ARRAY, move(vec));
            }

            case prog::constant::SIZED_ARRAY: {
                auto& p = get<prog::constant::SIZED_ARRAY>(source.value);
                return VARIANT(prog::constant, SIZED_ARRAY, make_pair( make_ptr(copy_constant(*p.first)), p.second ));
            }

            case prog::constant::SOME:
                return VARIANT(prog::constant, SOME, make_ptr(copy_constant(*get<prog::constant::SOME>(source.value))));

            case prog::constant::NONE:
                return VARIANT(prog::constant, NONE, ast::monostate());

            case prog::constant::GLOBAL_PTR:
                return VARIANT(prog::constant, GLOBAL_PTR, get<prog::constant::GLOBAL_PTR>(source.value));

            case prog::constant::GLOBAL_INNER_PTR: {
                auto p = get<prog::constant::GLOBAL_INNER_PTR>(source.value);
                return VARIANT(prog::constant, GLOBAL_INNER_PTR, move(p));
            }

            case prog::constant::GLOBAL_FUNC_PTR:
                return VARIANT(prog::constant, GLOBAL_FUNC_PTR, get<prog::constant::GLOBAL_FUNC_PTR>(source.value));

            default:
                throw 0; // unreachable state
        }
    }

    prog::type compiler::copy_type(const prog::type& source) {
        switch (source.value.index()) {
            case prog::type::PRIMITIVE:
                return VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{ get<prog::type::PRIMITIVE>(source.value)->tp }));

            case prog::type::STRUCT:
                return VARIANT(prog::type, STRUCT, get<prog::type::STRUCT>(source.value));

            case prog::type::ENUM:
                return VARIANT(prog::type, ENUM, get<prog::type::ENUM>(source.value));

            case prog::type::TUPLE: {
                auto vec = copy_ptr_vector<prog::type>(get<prog::type::TUPLE>(source.value), bind(&compiler::copy_type, this, _1));
                return VARIANT(prog::type, TUPLE, move(vec));
            }

            case prog::type::ARRAY:
                return VARIANT(prog::type, ARRAY, make_ptr(copy_array_type(*get<prog::type::ARRAY>(source.value))));

            case prog::type::OPTIONAL:
                return VARIANT(prog::type, OPTIONAL, make_ptr(copy_type(*get<prog::type::OPTIONAL>(source.value))));

            case prog::type::PTR:
                return VARIANT(prog::type, PTR, make_ptr(copy_ptr_type(*get<prog::type::PTR>(source.value))));

            case prog::type::INNER_PTR:
                return VARIANT(prog::type, INNER_PTR, make_ptr(copy_inner_ptr_type(*get<prog::type::INNER_PTR>(source.value))));

            case prog::type::FUNC:
                return VARIANT(prog::type, FUNC, make_ptr(copy_func_type(*get<prog::type::FUNC>(source.value))));

            case prog::type::GLOBAL_FUNC:
                return VARIANT(prog::type, GLOBAL_FUNC, make_ptr(copy_func_type(*get<prog::type::GLOBAL_FUNC>(source.value))));

            case prog::type::FUNC_WITH_PTR:
                return VARIANT(prog::type, FUNC_WITH_PTR, make_ptr(copy_func_with_ptr_type(*get<prog::type::FUNC_WITH_PTR>(source.value))));

            default:
                throw 0; // unreachable state
        }
    }

    prog::array_type compiler::copy_array_type(const prog::array_type& source) {
        return { make_ptr(copy_type(*source.tp)), source.size };
    }

    prog::ptr_type compiler::copy_ptr_type(const prog::ptr_type& source) {
        return { source.kind, make_ptr(copy_type_pointed(*source.target_tp)) };
    }

    prog::inner_ptr_type compiler::copy_inner_ptr_type(const prog::inner_ptr_type& source) {
        return { /*base*/copy_ptr_type(source), make_ptr(copy_type_pointed(*source.owner_tp)) };
    }

    prog::func_type compiler::copy_func_type(const prog::func_type& source) {
        auto vec = copy_ptr_vector<prog::type_local>(source.param_tps, bind(&compiler::copy_type_local, this, _1));
        return { move(vec), make_ptr(copy_type(*source.return_tp)) };
    }

    prog::func_with_ptr_type compiler::copy_func_with_ptr_type(const prog::func_with_ptr_type& source) {
        return { /*base*/copy_func_type(source), source.kind, make_ptr(copy_type_pointed(*source.target_tp)) };
    }

    prog::type_pointed compiler::copy_type_pointed(const prog::type_pointed& source) {
        return { make_ptr(copy_type(*source.tp)), source.slice };
    }

    prog::type_local compiler::copy_type_local(const prog::type_local& source) {
        return { make_ptr(copy_type(*source.tp)), source.confined };
    }
}
