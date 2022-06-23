#include "compiler/types.hpp"
#include "compiler/constants.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    prog::type type_compiler::compile(const ast::type& ast) {
        switch (INDEX(ast)) {
            case ast::type::NEVER:
                return VARIANT(prog::type, NEVER, monostate());

            case ast::type::NUMBER: {
                auto& number_ast = *GET(ast, NUMBER);
                auto type = compile_number_type(number_ast);
                return VARIANT(prog::type, NUMBER, into_ptr(type));
            }

            case ast::type::USER_TYPE: {
                auto name = GET(ast, USER_TYPE);
                return compile_user_type(name, ast.loc);
            }

            case ast::type::TUPLE: {
                auto tuple_asts = as_cref_vector(GET(ast, TUPLE));
                auto types = compile_tuple_type(tuple_asts);
                return VARIANT(prog::type, TUPLE, into_ptr_vector(types));
            }

            case ast::type::ARRAY: {
                auto& array_ast = *GET(ast, ARRAY);
                auto type = compile_array_type(array_ast);
                return VARIANT(prog::type, ARRAY, into_ptr(type));
            }

            case ast::type::OPTIONAL: {
                auto& inner_ast = *GET(ast, OPTIONAL);
                auto type = type_compiler(clr, allow_uncompiled).compile(inner_ast);
                return VARIANT(prog::type, OPTIONAL, into_ptr(type));
            }

            case ast::type::PTR: {
                auto& ptr_ast = *GET(ast, PTR);
                auto type = compile_ptr_type(ptr_ast);
                return VARIANT(prog::type, PTR, into_ptr(type));
            }

            case ast::type::INNER_PTR: {
                auto& inptr_ast = *GET(ast, INNER_PTR);
                auto type = compile_inner_ptr_type(inptr_ast);
                return VARIANT(prog::type, INNER_PTR, into_ptr(type));
            }

            case ast::type::FUNC: {
                auto& func_ast = *GET(ast, FUNC);
                auto type = compile_func_type(func_ast);
                return VARIANT(prog::type, FUNC, into_ptr(type));
            }

            case ast::type::GLOBAL_FUNC: {
                auto& func_ast = *GET(ast, GLOBAL_FUNC);
                auto type = compile_func_type(func_ast);
                return VARIANT(prog::type, GLOBAL_FUNC, into_ptr(type));
            }

            case ast::type::FUNC_WITH_PTR: {
                auto& fptr_ast = *GET(ast, FUNC_WITH_PTR);
                auto type = compile_func_with_ptr_type(fptr_ast);
                return VARIANT(prog::type, FUNC_WITH_PTR, into_ptr(type));
            }
        }

        UNREACHABLE;
    }

    prog::type_local type_compiler::compile_local(const ast::type_local& ast) {
        auto type = compile(*ast.tp);
        auto confined = ast.confined;

        if (type_trivial(prog, type) && ast.marked)
            warning(diags::trivial_type_with_confinement_marker(ast.loc));

        if ((INDEX_EQ(type, PTR) || INDEX_EQ(type, INNER_PTR) || INDEX_EQ(type, FUNC_WITH_PTR)) && confined) {
            auto kind =
                INDEX_EQ(type, PTR) ? GET(type, PTR)->kind
                : INDEX_EQ(type, INNER_PTR) ? GET(type, INNER_PTR)->kind
                : GET(type, FUNC_WITH_PTR)->kind;

            if (kind == prog::ptr_type::SHARED || kind == prog::ptr_type::WEAK || kind == prog::ptr_type::UNIQUE)
                warning(diags::restrictive_pointer_type(ast.loc));
        }

        return { into_ptr(type), confined };
    }

    prog::type type_compiler::compile_user_type(string name, location loc) {
        auto& gname = clr.get_global_type(name, allow_uncompiled, loc);

        switch (gname.kind) {
            case global_name_kind::STRUCT:
                return VARIANT(prog::type, STRUCT, gname.index);

            case global_name_kind::ENUM:
                return VARIANT(prog::type, ENUM, gname.index);

            default:
                UNREACHABLE;
        }
    }

    prog::number_type type_compiler::compile_number_type(const ast::number_type& ast) {
        switch (ast.tp) {
            case ast::number_type::BOOL:
                return { prog::number_type::BOOL };

            case ast::number_type::I8:
                return { prog::number_type::I8 };

            case ast::number_type::I16:
                return { prog::number_type::I16 };

            case ast::number_type::I32:
                return { prog::number_type::I32 };

            case ast::number_type::I64:
                return { prog::number_type::I64 };

            case ast::number_type::U8:
                return { prog::number_type::U8 };

            case ast::number_type::U16:
                return { prog::number_type::U16 };

            case ast::number_type::U32:
                return { prog::number_type::U32 };

            case ast::number_type::U64:
                return { prog::number_type::U64 };

            case ast::number_type::F32:
                return { prog::number_type::F32 };

            case ast::number_type::F64:
                return { prog::number_type::F64 };
        }

        UNREACHABLE;
    }

    vector<prog::type> type_compiler::compile_tuple_type(vector<cref<ast::type>> asts) {
        vector<prog::type> types;
        for (const ast::type& type_ast : asts)
            types.push_back(type_compiler(clr, allow_uncompiled).compile(type_ast));
        return types;
    }

    prog::array_type type_compiler::compile_array_type(const ast::array_type& ast) {
        auto type = type_compiler(clr, allow_uncompiled).compile(*ast.tp);
        auto size = constant_compiler(clr).compile_size(*ast.size);
        return { into_ptr(type), size };
    }

    prog::ptr_type type_compiler::compile_ptr_type(const ast::ptr_type& ast) {
        prog::ptr_type::kind_t kind;

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

            default:
                UNREACHABLE;
        }

        auto type = compile_type_pointed(*ast.target_tp);

        return { kind, into_ptr(type) };
    }

    prog::type_pointed type_compiler::compile_type_pointed(const ast::type_pointed& ast) {
        auto type = type_compiler(clr, true).compile(*ast.tp);

        return { into_ptr(type), ast.slice };
    }

    prog::inner_ptr_type type_compiler::compile_inner_ptr_type(const ast::inner_ptr_type& ast) {
        auto ptr_type = compile_ptr_type(ast);
        auto owner_type = compile_type_pointed(*ast.owner_tp);

        return { { ptr_type.kind, move(ptr_type.target_tp) }, into_ptr(owner_type) };
    }

    prog::func_type type_compiler::compile_func_type(const ast::func_type& ast) {
        vector<prog::type_local> param_types;
        for (const ast::type_local& type_ast : as_cref_vector(ast.param_tps))
            param_types.push_back(type_compiler(clr, allow_uncompiled).compile_local(type_ast));

        auto return_type = type_compiler(clr, allow_uncompiled).compile(*ast.return_tp);

        return { into_ptr_vector(param_types), into_ptr(return_type) };
    }

    prog::func_with_ptr_type type_compiler::compile_func_with_ptr_type(const ast::func_with_ptr_type& ast) {
        auto func_type = compile_func_type(ast);

        prog::func_with_ptr_type::kind_t kind;

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

            default:
                UNREACHABLE;
        }

        auto type = compile_type_pointed(*ast.target_tp);

        return { { move(func_type.param_tps), move(func_type.return_tp) }, { kind, into_ptr(type) } };
    }
}
