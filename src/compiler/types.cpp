#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"
#include <variant>

namespace sg {
    using namespace sg::utils;

    using std::monostate;

    prog::type compiler::compile_type(const ast::type& ast, bool allow_uncompiled) {
        switch (INDEX(ast)) {
            case ast::type::NEVER:
                return VARIANT(prog::type, NEVER, monostate());

            case ast::type::PRIMITIVE: {
                auto type = compile_primitive_type(*GET(ast, PRIMITIVE));
                return VARIANT(prog::type, PRIMITIVE, into_ptr(type));
            }

            case ast::type::USER_TYPE:
                return compile_user_type(ast, allow_uncompiled);

            case ast::type::TUPLE: {
                auto types = compile_tuple_type(GET(ast, TUPLE), allow_uncompiled);
                return VARIANT(prog::type, TUPLE, move(types));
            }

            case ast::type::ARRAY: {
                auto type = compile_array_type(*GET(ast, ARRAY), allow_uncompiled);
                return VARIANT(prog::type, ARRAY, into_ptr(type));
            }

            case ast::type::OPTIONAL: {
                auto type = compile_type(*GET(ast, OPTIONAL), allow_uncompiled);
                return VARIANT(prog::type, OPTIONAL, into_ptr(type));
            }

            case ast::type::PTR: {
                auto type = compile_ptr_type(*GET(ast, PTR));
                return VARIANT(prog::type, PTR, into_ptr(type));
            }

            case ast::type::INNER_PTR: {
                auto type = compile_inner_ptr_type(*GET(ast, INNER_PTR));
                return VARIANT(prog::type, INNER_PTR, into_ptr(type));
            }

            case ast::type::FUNC: {
                auto type = compile_func_type(*GET(ast, FUNC), allow_uncompiled);
                return VARIANT(prog::type, FUNC, into_ptr(type));
            }

            case ast::type::GLOBAL_FUNC: {
                auto type = compile_func_type(*GET(ast, GLOBAL_FUNC), allow_uncompiled);
                return VARIANT(prog::type, GLOBAL_FUNC, into_ptr(type));
            }

            case ast::type::FUNC_WITH_PTR: {
                auto type = compile_func_with_ptr_type(*GET(ast, FUNC_WITH_PTR), allow_uncompiled);
                return VARIANT(prog::type, FUNC_WITH_PTR, into_ptr(type));
            }
        }

        UNREACHABLE;
    }

    prog::type compiler::compile_user_type(const ast::type& ast, bool allow_uncompiled) {
        auto name = GET(ast, USER_TYPE);
        auto& global_name = get_global_name(ast, name, allow_uncompiled);

        switch (global_name.kind) {
            case global_name::ENUM:
                return VARIANT(prog::type, ENUM, global_name.index);

            case global_name::STRUCT:
                return VARIANT(prog::type, STRUCT, global_name.index);

            default:
                error(diags::invalid_kind(), ast);
        }
    }

    prog::type_local compiler::compile_type_local(const ast::type_local& ast, bool allow_uncompiled) {
        auto type = compile_type(*ast.tp, allow_uncompiled);
        return { into_ptr(type), ast.confined };
    }

    prog::primitive_type compiler::compile_primitive_type(const ast::primitive_type& ast) {
        switch (ast.tp) {
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

    vector<prog::ptr<prog::type>> compiler::compile_tuple_type(const vector<ast::ptr<ast::type>>& ast, bool allow_uncompiled) {
        vector<prog::ptr<prog::type>> types;
        for (auto& type_ast : ast)
            types.push_back(make_ptr(compile_type(*type_ast, allow_uncompiled)));
        return types;
    }

    prog::array_type compiler::compile_array_type(const ast::array_type& ast, bool allow_uncompiled) {
        auto type = compile_type(*ast.tp, allow_uncompiled);
        auto size = compile_constant_size(*ast.size);
        return { into_ptr(type), size };
    }

    prog::ptr_type compiler::compile_ptr_type(const ast::ptr_type& ast) {
        decltype(prog::ptr_type::kind) kind;

        switch(ast.kind) {
            case ast::ptr_type::GLOBAL:
                kind = prog::ptr_type::GLOBAL;
                break;

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

        auto type = compile_type_pointed(*ast.target_tp);
        return { kind, into_ptr(type) };
    }

    prog::type_pointed compiler::compile_type_pointed(const ast::type_pointed& ast) {
        auto type = compile_type(*ast.tp, true);
        return { into_ptr(type), ast.slice };
    }

    prog::inner_ptr_type compiler::compile_inner_ptr_type(const ast::inner_ptr_type& ast) {
        auto base = compile_ptr_type(ast);
        auto owner_type = compile_type_pointed(*ast.owner_tp);
        return { base.kind, move(base.target_tp), into_ptr(owner_type) };
    }

    prog::func_type compiler::compile_func_type(const ast::func_type& ast, bool allow_uncompiled) {
        vector<prog::ptr<prog::type_local>> param_types;
        for (auto& type_ast : ast.param_tps)
            param_types.push_back(make_ptr(compile_type_local(*type_ast, allow_uncompiled)));

        auto return_type = compile_type(*ast.return_tp, allow_uncompiled);
        return { move(param_types), into_ptr(return_type) };
    }

    prog::func_with_ptr_type compiler::compile_func_with_ptr_type(const ast::func_with_ptr_type& ast, bool allow_uncompiled) {
        auto base = compile_func_type(ast, allow_uncompiled);

        decltype(prog::func_with_ptr_type::kind) kind;

        switch(ast.kind) {
            case ast::ptr_type::GLOBAL:
                kind = prog::ptr_type::GLOBAL;
                break;

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

        auto type = compile_type_pointed(*ast.target_tp);
        return { move(base.param_tps), move(base.return_tp), kind, into_ptr(type) };
    }
}
