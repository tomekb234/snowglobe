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
            switch (INDEX(*global_def)) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *GET(*global_def, STRUCT_DEF);
                    auto struct_type = declare_struct_type(struct_type_ast);
                    auto name = struct_type.name;
                    auto index = program.struct_types.size();
                    program.struct_types.push_back(into_ptr(struct_type));
                    global_names[name] = { global_name::STRUCT, index, false };
                } break;

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                    auto enum_type = declare_enum_type(enum_type_ast);
                    auto name = enum_type.name;
                    auto index = program.enum_types.size();
                    program.enum_types.push_back(into_ptr(enum_type));
                    global_names[name] = { global_name::ENUM, index, false };
                } break;
            }
        }

        // Phase 2: Compile struct, enum definitions and constants, prepare global function declarations

        size_t struct_type_index = 0;
        size_t enum_type_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (INDEX(*global_def)) {
                case ast::global_def::STRUCT_DEF: {
                    auto& struct_type_ast = *GET(*global_def, STRUCT_DEF);
                    auto& struct_type = *program.struct_types[struct_type_index++];
                    compile_struct_type(struct_type_ast, struct_type);
                    global_names[struct_type.name].compiled = true;
                } break;

                case ast::global_def::ENUM_DEF: {
                    auto& enum_type_ast = *GET(*global_def, ENUM_DEF);
                    auto& enum_type = *program.enum_types[enum_type_index++];
                    compile_enum_type(enum_type_ast, enum_type);
                    global_names[enum_type.name].compiled = true;
                } break;

                case ast::global_def::CONST_DEF: {
                    auto& global_const_ast = *GET(*global_def, CONST_DEF);
                    auto global_const = compile_global_var(global_const_ast);
                    auto name = global_const.name;
                    auto index = constants.size();
                    constants.push_back(move(global_const));
                    global_names[name] = { global_name::CONSTANT, index, true };
                } break;

                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                    auto global_func = declare_global_func(global_func_ast);
                    auto name = global_func.name;
                    auto index = program.global_funcs.size();
                    program.global_funcs.push_back(into_ptr(global_func));
                    global_names[name] = { global_name::FUNCTION, index, false };
                } break;
            }
        }

        // Phase 3: Compile global variable definitions

        for (auto& global_def : ast.global_defs) {
            switch (INDEX(*global_def)) {
                case ast::global_def::VAR_DEF: {
                    auto& global_var_ast = *GET(*global_def, VAR_DEF);
                    auto global_var = compile_global_var(global_var_ast);
                    auto name = global_var.name;
                    auto index = program.global_vars.size();
                    program.global_vars.push_back(into_ptr(global_var));
                    global_names[name] = { global_name::VARIABLE, index, true };
                } break;
            }
        }

        // Phase 4: Compile global function definitions

        size_t global_func_index = 0;

        for (auto& global_def : ast.global_defs) {
            switch (INDEX(*global_def)) {
                case ast::global_def::FUNC_DEF: {
                    auto& global_func_ast = *GET(*global_def, FUNC_DEF);
                    auto& global_func = *program.global_funcs[global_func_index++];
                    compile_global_func(global_func_ast, global_func);
                    global_names[global_func.name].compiled = true;
                } break;
            }
        }
    }

    prog::global_var compiler::compile_global_var(const ast::var_def& ast) {
        auto name = ast.name;

        if (global_names.count(name))
            error(diags::global_name_used(name), ast);

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
        error(diags::not_implemented(), ast); // TODO
    }

    prog::struct_type compiler::declare_struct_type(const ast::struct_def& ast) {
        error(diags::not_implemented(), ast); // TODO
    }

    prog::enum_type compiler::declare_enum_type(const ast::enum_def& ast) {
        error(diags::not_implemented(), ast); // TODO
    }

    void compiler::compile_global_func(const ast::func_def& ast, prog::global_func& global_func) {
        error(diags::not_implemented(), ast); // TODO
    }

    void compiler::compile_struct_type(const ast::struct_def& ast, prog::struct_type& struct_type) {
        error(diags::not_implemented(), ast); // TODO
    }

    void compiler::compile_enum_type(const ast::enum_def& ast, prog::enum_type& enum_type) {
        error(diags::not_implemented(), ast); // TODO
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
        switch (INDEX(ast)) { // TODO more constant expressions
            case ast::expr::TUPLE:
                return compile_constant_tuple(GET(ast, TUPLE), ast);

            case ast::expr::ARRAY:
                return compile_constant_array(GET(ast, ARRAY), ast);

            case ast::expr::NAME:
                return compile_constant_name(GET(ast, NAME), ast);

            case ast::expr::CONST:
                return compile_constant_literal(*GET(ast, CONST));

            case ast::expr::SOME: {
                auto[inner_value, inner_type] = compile_constant_expr(*GET(ast, SOME));
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
        vector<prog::constant> values;
        vector<prog::type> types;

        for (auto& item_ptr : asts) {
            auto& expr_marked = *item_ptr;
            switch (INDEX(expr_marked)) {
                case ast::expr_marked::EXPR: {
                    auto[value, type] = compile_constant_expr(*GET(expr_marked, EXPR));
                    values.push_back(move(value));
                    types.push_back(move(type));
                } break;

                default:
                    error(diags::expression_not_constant(), ast);
            }
        }

        if (values.empty()) {
            auto value = VARIANT(prog::constant, UNIT, prog::monostate{ });
            auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{ prog::primitive_type::UNIT }));
            return { move(value), move(type) };
        }
        if (values.size() == 1)
            return { move(values[0]), move(types[0]) };

        return { VARIANT(prog::constant, TUPLE, into_ptr_vector(values)), VARIANT(prog::type, TUPLE, into_ptr_vector(types)) };
    }

    pair<prog::constant, prog::type> compiler::compile_constant_array(const vector<ast::ptr<ast::expr_marked>>& asts, const ast::node& ast) {
        vector<prog::ptr<prog::constant>> values;
        prog::type type;

        for (auto& item_ptr : asts) {
            auto& expr_marked = *item_ptr;
            switch (INDEX(expr_marked)) {
                case ast::expr_marked::EXPR: {
                    auto[item_value, item_type] = compile_constant_expr(*GET(expr_marked, EXPR));
                    if (values.empty())
                        type = move(item_type);
                    else
                        type = common_supertype(ast, type, item_type);
                    values.push_back(into_ptr(item_value));
                } break;

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
            error(diags::name_not_declared(name), ast);
        auto& global_name = global_names[name];

        switch (global_name.kind) {
            case global_name::VARIABLE:
                error(diags::expression_not_constant(), ast);

            case global_name::CONSTANT: {
                auto& global_var = constants[global_name.index];
                return { copy_constant(*global_var.value), copy_type(*global_var.tp) };
            }

            default:
                error(diags::not_implemented(), ast);
        }
    }

    pair<prog::constant, prog::type> compiler::compile_constant_literal(const ast::const_expr& ast) {
        switch (INDEX(ast)) {
            case ast::const_expr::BOOL: {
                auto value = VARIANT(prog::constant, BOOL, GET(ast, BOOL));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::BOOL}));
                return {move(value), move(type)};
            }

            case ast::const_expr::CHAR: {
                auto value = VARIANT(prog::constant, INT, encode_number(static_cast<uint8_t>(GET(ast, CHAR))));
                auto type = VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{prog::primitive_type::U8}));
                return {move(value), move(type)};
            }

            case ast::const_expr::STRING: {
                error(diags::not_implemented(), ast); // TODO
            }

            case ast::const_expr::INT: {
                auto[value, type] = compile_int_token(*GET(ast, INT));
                return {move(value), VARIANT(prog::type, PRIMITIVE, into_ptr(type))};
            }

            case ast::const_expr::FLOAT: {
                auto[value, type] = compile_float_token(*GET(ast, FLOAT));
                return {move(value), VARIANT(prog::type, PRIMITIVE, into_ptr(type))};
            }

            default:
                error(diags::not_implemented(), ast);
        }
    }

    pair<prog::constant, prog::primitive_type> compiler::compile_int_token(const ast::int_token& ast) {
        #define RETURN_IF_OK(type, type_marker) { \
            auto opt_val = try_make_number<type>(ast.value, ast.negative); \
            if (opt_val) return { VARIANT(prog::constant, INT, encode_number(*opt_val)), { prog::primitive_type::type_marker } }; \
        }

        #define RETURN_OR_ERROR(type, type_marker) { \
            RETURN_IF_OK(type, type_marker) \
            error(diags::integer_overflow((ast.negative ? "-" : "") + to_string(ast.value), is_signed<type>(), 8 * sizeof(type)), ast); \
        }

        switch (ast.marker) {
            case ast::int_token::NONE:
            case ast::int_token::I: {
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

            case ast::int_token::U: {
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
        }

        UNREACHABLE;

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
        }

        UNREACHABLE;
    }

    prog::type compiler::compile_type(const ast::type& ast) {
        switch (INDEX(ast)) {
            case ast::type::PRIMITIVE: {
                auto tp = compile_primitive_type(*GET(ast, PRIMITIVE));
                return VARIANT(prog::type, PRIMITIVE, into_ptr(tp));
            }

            case ast::type::USER_TYPE: {
                error(diags::not_implemented(), ast); // TODO
            }

            case ast::type::TUPLE: {
                auto types = compile_tuple_type(GET(ast, TUPLE));
                return VARIANT(prog::type, TUPLE, move(types));
            }

            case ast::type::ARRAY: {
                auto tp = compile_array_type(*GET(ast, ARRAY));
                return VARIANT(prog::type, ARRAY, into_ptr(tp));
            }

            case ast::type::OPTIONAL: {
                auto tp = compile_type(*GET(ast, OPTIONAL));
                return VARIANT(prog::type, OPTIONAL, into_ptr(tp));
            }

            case ast::type::PTR: {
                auto tp = compile_ptr_type(*GET(ast, PTR));
                return VARIANT(prog::type, PTR, into_ptr(tp));
            }

            case ast::type::INNER_PTR: {
                auto tp = compile_inner_ptr_type(*GET(ast, INNER_PTR));
                return VARIANT(prog::type, INNER_PTR, into_ptr(tp));
            }

            case ast::type::FUNC: {
                auto tp = compile_func_type(*GET(ast, FUNC));
                return VARIANT(prog::type, FUNC, into_ptr(tp));
            }

            case ast::type::GLOBAL_FUNC: {
                auto tp = compile_func_type(*GET(ast, GLOBAL_FUNC));
                return VARIANT(prog::type, GLOBAL_FUNC, into_ptr(tp));
            }

            case ast::type::FUNC_WITH_PTR: {
                auto tp = compile_func_with_ptr_type(*GET(ast, FUNC_WITH_PTR));
                return VARIANT(prog::type, FUNC_WITH_PTR, into_ptr(tp));
            }
        }

        UNREACHABLE;
    }

    prog::func_with_ptr_type compiler::compile_func_with_ptr_type(const ast::func_with_ptr_type& ast) {
        auto base = compile_func_type(ast);

        decltype(prog::func_with_ptr_type::kind) kind;

        switch(ast.kind) {
            case ast::func_with_ptr_type::BASIC:
                kind = prog::func_with_ptr_type::BASIC;
                break;

            case ast::func_with_ptr_type::SHARED:
                kind = prog::func_with_ptr_type::SHARED;
                break;

            case ast::func_with_ptr_type::WEAK:
                kind = prog::func_with_ptr_type::WEAK;
                break;

            case ast::func_with_ptr_type::UNIQUE:
                kind = prog::func_with_ptr_type::UNIQUE;
                break;
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
            case ast::ptr_type::GLOBAL:
                kind = prog::ptr_type::GLOBAL;

            case ast::ptr_type::BASIC:
                kind = prog::ptr_type::BASIC;
                break;

            case ast::ptr_type::SHARED:
                kind = prog::ptr_type::SHARED;
                break;

            case ast::ptr_type::WEAK:
                kind = prog::ptr_type::WEAK;
                break;

            case ast::ptr_type::UNIQUE:
                kind = prog::ptr_type::UNIQUE;
                break;
        }

        auto target_tp = compile_type_pointed(*ast.target_tp);

        return { kind, into_ptr(target_tp) };
    }

    prog::array_type compiler::compile_array_type(const ast::array_type& ast) {
        prog::type&& tp = compile_type(*ast.tp);
        return { into_ptr(tp), GET(*ast.size, INTEGER) }; // TODO named constants
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
            case ast::primitive_type::NEVER:
                return { prog::primitive_type::NEVER };

            case ast::primitive_type::BOOL:
                return { prog::primitive_type::BOOL };

            case ast::primitive_type::I8:
                return { prog::primitive_type::I8 };

            case ast::primitive_type::I16:
                return { prog::primitive_type::I16 };

            case ast::primitive_type::I32:
                return { prog::primitive_type::I32 };

            case ast::primitive_type::I64:
                return { prog::primitive_type::I64 };

            case ast::primitive_type::U8:
                return { prog::primitive_type::U8 };

            case ast::primitive_type::U16:
                return { prog::primitive_type::U16 };

            case ast::primitive_type::U32:
                return { prog::primitive_type::U32 };

            case ast::primitive_type::U64:
                return { prog::primitive_type::U64 };

            case ast::primitive_type::F32:
                return { prog::primitive_type::F32 };

            case ast::primitive_type::F64:
                return { prog::primitive_type::F64 };
        }

        UNREACHABLE;
    }

    prog::constant compiler::convert_constant(const ast::node& ast, const prog::constant& constant, const prog::type& from_tp, const prog::type& to_tp) {
        error(diags::not_implemented(), ast);
    }

    bool compiler::subtype(const prog::type& type1, const prog::type& type2, bool confined) {
        switch (INDEX(type1)) {
            case prog::type::PRIMITIVE: {
                auto& ptype1 = *GET(type1, PRIMITIVE);

                if (ptype1.tp == prog::primitive_type::NEVER)
                    return true;

                if (!INDEX_EQ(type2, PRIMITIVE))
                    break;

                auto& ptype2 = *GET(type2, PRIMITIVE);

                switch (ptype1.tp) {
                    case prog::primitive_type::UNIT: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::UNIT:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::BOOL: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::BOOL:
                            case prog::primitive_type::I8:
                            case prog::primitive_type::I16:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64:
                            case prog::primitive_type::U8:
                            case prog::primitive_type::U16:
                            case prog::primitive_type::U32:
                            case prog::primitive_type::U64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::I8: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::I8:
                            case prog::primitive_type::I16:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::I16: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::I16:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::I32: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::I64: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::I64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::U8: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::U8:
                            case prog::primitive_type::U16:
                            case prog::primitive_type::U32:
                            case prog::primitive_type::U64:
                            case prog::primitive_type::I16:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::U16: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::U16:
                            case prog::primitive_type::U32:
                            case prog::primitive_type::U64:
                            case prog::primitive_type::I32:
                            case prog::primitive_type::I64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::U32: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::U32:
                            case prog::primitive_type::U64:
                            case prog::primitive_type::I64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::U64: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::U64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::F32: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::F32:
                            case prog::primitive_type::F64:
                                return true;
                        }
                    } break;

                    case prog::primitive_type::F64: {
                        switch (ptype2.tp) {
                            case prog::primitive_type::F64:
                                return true;
                        }
                    } break;
                }
            } break;

            case prog::type::STRUCT: {
                if (!INDEX_EQ(type2, STRUCT))
                    break;
                if (GET(type1, STRUCT) == GET(type2, STRUCT))
                    return true;
            } break;

            case prog::type::ENUM: {
                if (!INDEX_EQ(type2, ENUM))
                    break;
                if (GET(type1, ENUM) == GET(type2, ENUM))
                    return true;
            } break;

            case prog::type::TUPLE: {
                if(!INDEX_EQ(type2, TUPLE))
                    break;

                auto& tuple1 = GET(type1, TUPLE);
                auto& tuple2 = GET(type2, TUPLE);

                if(tuple1.size() != tuple2.size()) {
                    break;
                }

                bool ok = true;

                for(size_t i = 0; i < tuple1.size(); i++) {
                    if(!subtype(*tuple1[i], *tuple2[i], confined)) {
                        ok = false;
                        break;
                    }
                }

                if(ok)
                    return true;
            } break;

            case prog::type::ARRAY: {
                if (!INDEX_EQ(type2, ARRAY))
                    break;
                auto& array1 = *GET(type1, ARRAY);
                auto& array2 = *GET(type2, ARRAY);
                if (array1.size == array2.size && subtype(*array1.tp, *array2.tp, confined))
                    return true;
            } break;

            case prog::type::OPTIONAL: {
                if (INDEX_EQ(type2, PRIMITIVE) && GET(type2, PRIMITIVE)->tp == prog::primitive_type::BOOL)
                    return true;
                if (!INDEX_EQ(type2, OPTIONAL))
                    break;
                auto& tp1 = *GET(type1, OPTIONAL);
                auto& tp2 = *GET(type2, OPTIONAL);
                if (subtype(tp1, tp2, confined))
                    return true;
            } break;

            case prog::type::PTR: {
                if (!INDEX_EQ(type2, PTR))
                    break;

                auto& ptr1 = *GET(type1, PTR);
                auto& ptr2 = *GET(type2, PTR);

                if(ptr_subkind(ptr1.kind, ptr2.kind, confined)) {
                    if(ptr_target_subtype(*ptr1.target_tp, *ptr2.target_tp)) {
                        return true;
                    }
                }

            } break;

            case prog::type::INNER_PTR: {
                auto& iptr1 = *GET(type1, INNER_PTR);
                auto& tar1 = *iptr1.target_tp;
                auto& own1 = *iptr1.owner_tp;

                if (INDEX_EQ(type2, PTR)) {
                    auto& ptr2 = *GET(type2, PTR);
                    auto& tar2 = *ptr2.target_tp;

                    if (ptr_target_subtype(tar1, tar2) && ptr_kind_trivial(iptr1.kind, confined) && ptr_subkind(iptr1.kind, ptr2.kind, confined))
                        return true;
                }
                else if (INDEX_EQ(type2, INNER_PTR)) {
                    auto& iptr2 = *GET(type2, INNER_PTR);
                    if (ptr_subkind(iptr1.kind, iptr2.kind, confined)) {
                        auto& tar2 = *iptr2.target_tp;
                        auto& own2 = *iptr2.owner_tp;

                        if (ptr_target_subtype(tar1, tar2) && ptr_target_subtype(own1, own2))
                            return true;
                    }
                }
            } break;

            case prog::type::FUNC: {
                if(!INDEX_EQ(type2, FUNC))
                    break;
                auto& func1 = *GET(type1, FUNC);
                auto& func2 = *GET(type2, FUNC);
                if (func_subtype(func1, func2))
                    return true;
            } break;

            case prog::type::GLOBAL_FUNC: {
                if(INDEX_EQ(type2, GLOBAL_FUNC)) {
                    auto& func1 = *GET(type1, GLOBAL_FUNC);
                    auto& func2 = *GET(type2, GLOBAL_FUNC);
                    if (func_subtype(func1, func2))
                        return true;
                }

                else if(INDEX_EQ(type2, FUNC)) {
                    auto& func1 = *GET(type1, GLOBAL_FUNC);
                    auto& func2 = *GET(type2, FUNC);
                    if (func_subtype(func1, func2))
                        return true;
                }
            } break;

            case prog::type::FUNC_WITH_PTR: {
                auto& fwp1 = *GET(type1, FUNC_WITH_PTR);

                if(INDEX_EQ(type2, FUNC_WITH_PTR)) {
                    auto& fwp2 = *GET(type2, FUNC_WITH_PTR);
                    if (ptr_subkind(fwp1.kind, fwp2.kind, confined) && func_subtype(fwp1, fwp2))
                        return true;
                }

                else if(INDEX_EQ(type2, FUNC) && ptr_kind_trivial(fwp1.kind, confined)) {
                    auto& func2 = *GET(type2, FUNC);
                    if (func_subtype(fwp1, func2))
                        return true;
                }
            } break;
        }

        return false;
    }

    bool compiler::ptr_target_subtype(const prog::type_pointed& type1, const prog::type_pointed& type2) {
        if (type1.slice == type2.slice && subtype(*type1.tp, *type2.tp) && subtype(*type1.tp, *type2.tp))
            return true;

        if (!type1.slice && type2.slice && INDEX_EQ(*type1.tp, ARRAY)) {
            auto& arr1 = *GET(*type1.tp, ARRAY);
            if (subtype(*arr1.tp, *type2.tp) && subtype(*type2.tp, *arr1.tp))
                return true;
            if (INDEX_EQ(*arr1.tp, PRIMITIVE)) {
                auto& ptype1 = *GET(*arr1.tp, PRIMITIVE);
                if (ptype1.tp == prog::primitive_type::NEVER)
                    return true;
            }
        }

        if (type1.slice && type2.slice && INDEX_EQ(*type1.tp, PRIMITIVE)) {
            auto& ptype1 = *GET(*type1.tp, PRIMITIVE);
            if (ptype1.tp == prog::primitive_type::NEVER)
                return true;
        }

        return false;
    }

    bool compiler::func_subtype(const prog::func_type& func1, const prog::func_type& func2) {
        if (subtype(*func1.return_tp, *func2.return_tp)) {
            auto& args1 = func1.param_tps;
            auto& args2 = func2.param_tps;

            if(args1.size() != args2.size()) {
                return false;
            }

            bool ok = true;

            for (size_t i = 0; i < args1.size(); i++) {
                if (args2[i]->confined != args1[i]->confined || !subtype(*args2[i]->tp, *args1[i]->tp, args1[i]->confined)) {
                    ok = false;
                    break;
                }
            }

            if(ok)
                return true;
        }

        return false;
    }

    bool compiler::ptr_kind_trivial(prog::ptr_type::kind_t kind, bool confined) {
        switch (kind) {
            case prog::ptr_type::GLOBAL:
            case prog::ptr_type::BASIC:
                return true;

            case prog::ptr_type::SHARED:
            case prog::ptr_type::UNIQUE:
                if (confined)
                    return true;
        }

        return false;
    }

    bool compiler::ptr_subkind(prog::ptr_type::kind_t kind1, prog::ptr_type::kind_t kind2, bool confined) {
        switch (kind1) {
            case prog::ptr_type::GLOBAL: {
                switch (kind2) {
                    case prog::ptr_type::GLOBAL:
                    case prog::ptr_type::BASIC:
                        return true;
                }
            } break;

            case prog::ptr_type::BASIC: {
                switch (kind2) {
                    case prog::ptr_type::BASIC:
                        return true;
                    case prog::ptr_type::SHARED:
                    case prog::ptr_type::WEAK:
                    case prog::ptr_type::UNIQUE:
                        if (confined)
                            return true;
                }
            } break;

            case prog::ptr_type::SHARED: {
                switch (kind2) {
                    case prog::ptr_type::SHARED:
                    case prog::ptr_type::WEAK:
                        return true;
                    case prog::ptr_type::BASIC:
                    case prog::ptr_type::UNIQUE:
                        if (confined)
                            return true;
                }
            } break;

            case prog::ptr_type::WEAK: {
                switch (kind2) {
                    case prog::ptr_type::WEAK:
                        return true;
                }
            } break;

            case prog::ptr_type::UNIQUE: {
                switch (kind2) {
                    case prog::ptr_type::UNIQUE:
                    case prog::ptr_type::SHARED:
                    case prog::ptr_type::WEAK:
                        return true;
                    case prog::ptr_type::BASIC:
                        if (confined)
                            return true;
                }
            } break;
        }

        return false;
    }

    prog::type compiler::common_supertype(const ast::node& ast, const prog::type& type1, const prog::type& type2) {
        if (subtype(type1, type2))
            return copy_type(type2);

        if (subtype(type2, type1))
            return copy_type(type1);

        error(diags::no_common_supertype(), ast);
    }

    prog::constant compiler::copy_constant(const prog::constant& source) {
        switch (INDEX(source)) {
            case prog::constant::UNIT:
                return VARIANT(prog::constant, UNIT, prog::monostate{ });

            case prog::constant::BOOL:
                return VARIANT(prog::constant, BOOL, GET(source, BOOL));

            case prog::constant::INT:
                return VARIANT(prog::constant, INT, GET(source, INT));

            case prog::constant::FLOAT:
                return VARIANT(prog::constant, FLOAT, GET(source, FLOAT));

            case prog::constant::STRUCT: {
                auto vec = copy_ptr_vector<prog::constant>(GET(source, STRUCT), bind(&compiler::copy_constant, this, _1));
                return VARIANT(prog::constant, STRUCT, move(vec));
            }

            case prog::constant::ENUM: {
                auto& p = GET(source, ENUM);
                auto vec = copy_ptr_vector<prog::constant>(p.second, bind(&compiler::copy_constant, this, _1));
                return VARIANT(prog::constant, ENUM, make_pair( p.first, move(vec) ));
            }

            case prog::constant::TUPLE: {
                auto vec = copy_ptr_vector<prog::constant>(GET(source, TUPLE), bind(&compiler::copy_constant, this, _1));
                return VARIANT(prog::constant, TUPLE, move(vec));
            }

            case prog::constant::ARRAY: {
                auto vec = copy_ptr_vector<prog::constant>(GET(source, ARRAY), bind(&compiler::copy_constant, this, _1));
                return VARIANT(prog::constant, ARRAY, move(vec));
            }

            case prog::constant::SIZED_ARRAY: {
                auto& p = GET(source, SIZED_ARRAY);
                return VARIANT(prog::constant, SIZED_ARRAY, make_pair( make_ptr(copy_constant(*p.first)), p.second ));
            }

            case prog::constant::SOME:
                return VARIANT(prog::constant, SOME, make_ptr(copy_constant(*GET(source, SOME))));

            case prog::constant::NONE:
                return VARIANT(prog::constant, NONE, ast::monostate());

            case prog::constant::GLOBAL_PTR:
                return VARIANT(prog::constant, GLOBAL_PTR, GET(source, GLOBAL_PTR));

            case prog::constant::GLOBAL_INNER_PTR: {
                auto p = GET(source, GLOBAL_INNER_PTR);
                return VARIANT(prog::constant, GLOBAL_INNER_PTR, move(p));
            }

            case prog::constant::GLOBAL_FUNC_PTR:
                return VARIANT(prog::constant, GLOBAL_FUNC_PTR, GET(source, GLOBAL_FUNC_PTR));
        }

        UNREACHABLE;
    }

    prog::type compiler::copy_type(const prog::type& source) {
        switch (INDEX(source)) {
            case prog::type::PRIMITIVE:
                return VARIANT(prog::type, PRIMITIVE, make_ptr(prog::primitive_type{ GET(source, PRIMITIVE)->tp }));

            case prog::type::STRUCT:
                return VARIANT(prog::type, STRUCT, GET(source, STRUCT));

            case prog::type::ENUM:
                return VARIANT(prog::type, ENUM, GET(source, ENUM));

            case prog::type::TUPLE: {
                auto vec = copy_ptr_vector<prog::type>(GET(source, TUPLE), bind(&compiler::copy_type, this, _1));
                return VARIANT(prog::type, TUPLE, move(vec));
            }

            case prog::type::ARRAY:
                return VARIANT(prog::type, ARRAY, make_ptr(copy_array_type(*GET(source, ARRAY))));

            case prog::type::OPTIONAL:
                return VARIANT(prog::type, OPTIONAL, make_ptr(copy_type(*GET(source, OPTIONAL))));

            case prog::type::PTR:
                return VARIANT(prog::type, PTR, make_ptr(copy_ptr_type(*GET(source, PTR))));

            case prog::type::INNER_PTR:
                return VARIANT(prog::type, INNER_PTR, make_ptr(copy_inner_ptr_type(*GET(source, INNER_PTR))));

            case prog::type::FUNC:
                return VARIANT(prog::type, FUNC, make_ptr(copy_func_type(*GET(source, FUNC))));

            case prog::type::GLOBAL_FUNC:
                return VARIANT(prog::type, GLOBAL_FUNC, make_ptr(copy_func_type(*GET(source, GLOBAL_FUNC))));

            case prog::type::FUNC_WITH_PTR:
                return VARIANT(prog::type, FUNC_WITH_PTR, make_ptr(copy_func_with_ptr_type(*GET(source, FUNC_WITH_PTR))));
        }

        UNREACHABLE;
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
        return { /*base 1*/copy_func_type(source), /*base 2*/copy_ptr_type(source) }; // maybe FIXME
    }

    prog::type_pointed compiler::copy_type_pointed(const prog::type_pointed& source) {
        return { make_ptr(copy_type(*source.tp)), source.slice };
    }

    prog::type_local compiler::copy_type_local(const prog::type_local& source) {
        return { make_ptr(copy_type(*source.tp)), source.confined };
    }
}
