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
                auto tp = compile_primitive_type(*GET(ast, PRIMITIVE));
                return VARIANT(prog::type, PRIMITIVE, into_ptr(tp));
            }

            case ast::type::USER_TYPE: {
                return compile_user_type(ast, allow_uncompiled);
            }

            case ast::type::TUPLE: {
                auto types = compile_tuple_type(GET(ast, TUPLE), allow_uncompiled);
                return VARIANT(prog::type, TUPLE, move(types));
            }

            case ast::type::ARRAY: {
                auto tp = compile_array_type(*GET(ast, ARRAY), allow_uncompiled);
                return VARIANT(prog::type, ARRAY, into_ptr(tp));
            }

            case ast::type::OPTIONAL: {
                auto tp = compile_type(*GET(ast, OPTIONAL), allow_uncompiled);
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
                auto tp = compile_func_type(*GET(ast, FUNC), allow_uncompiled);
                return VARIANT(prog::type, FUNC, into_ptr(tp));
            }

            case ast::type::GLOBAL_FUNC: {
                auto tp = compile_func_type(*GET(ast, GLOBAL_FUNC), allow_uncompiled);
                return VARIANT(prog::type, GLOBAL_FUNC, into_ptr(tp));
            }

            case ast::type::FUNC_WITH_PTR: {
                auto tp = compile_func_with_ptr_type(*GET(ast, FUNC_WITH_PTR), allow_uncompiled);
                return VARIANT(prog::type, FUNC_WITH_PTR, into_ptr(tp));
            }
        }

        UNREACHABLE;
    }

    prog::type compiler::compile_user_type(const ast::type& ast, bool allow_uncompiled) {
        string tp = GET(ast, USER_TYPE);
        
        auto it = global_names.find(tp);
        if (it == global_names.end()) {
            error(diags::name_not_declared(tp), ast);
        }

        auto& type_obj = it->second;

        if(!allow_uncompiled) {
            if (!type_obj.compiled) {
                error(diags::name_not_compiled(tp), ast);
            }
        }

        switch (type_obj.kind) {
            case sg::compiler::global_name::ENUM: {
                return VARIANT(prog::type, ENUM, type_obj.index);
            }

            case sg::compiler::global_name::STRUCT: {
                return VARIANT(prog::type, STRUCT, type_obj.index);
            }

        }

        UNREACHABLE;
    }

    prog::type_local compiler::compile_type_local(const ast::type_local& ast, bool allow_uncompiled) {
        auto tp = compile_type(*ast.tp, allow_uncompiled);
        return { into_ptr(tp), ast.confined };
    }

    prog::primitive_type compiler::compile_primitive_type(const ast::primitive_type& ast) {
        switch(ast.tp) {
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
        vector<prog::ptr<prog::type>> result;

        for(const auto& ast_type : ast) {
            prog::type&& type = compile_type(*ast_type, allow_uncompiled); 
            result.push_back(into_ptr(type));
        }

        return result;
    }

    prog::array_type compiler::compile_array_type(const ast::array_type& ast, bool allow_uncompiled) {
        prog::type&& tp = compile_type(*ast.tp, allow_uncompiled);
        return { into_ptr(tp), GET(*ast.size, INTEGER) }; // TODO named constants
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

    prog::type_pointed compiler::compile_type_pointed(const ast::type_pointed& ast) {
        prog::type&& tp = compile_type(*ast.tp, true);
        return { into_ptr(tp), ast.slice };
    }

    prog::inner_ptr_type compiler::compile_inner_ptr_type(const ast::inner_ptr_type& ast) {
        auto base = compile_ptr_type(ast);
        auto owner_tp = compile_type_pointed(*ast.owner_tp);

        return { base.kind, move(base.target_tp), into_ptr(owner_tp) };
    }

    prog::func_type compiler::compile_func_type(const ast::func_type& ast, bool allow_uncompiled) {
        vector<prog::ptr<prog::type_local>> param_tps;

        for(const auto& param_tp : ast.param_tps) {
            auto&& tp = compile_type_local(*param_tp, allow_uncompiled);
            param_tps.push_back(into_ptr(tp));
        }

        auto return_tp = compile_type(*ast.return_tp, allow_uncompiled);

        return { move(param_tps), into_ptr(return_tp) };
    }

    prog::func_with_ptr_type compiler::compile_func_with_ptr_type(const ast::func_with_ptr_type& ast, bool allow_uncompiled) {
        auto base = compile_func_type(ast, allow_uncompiled);

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
}
