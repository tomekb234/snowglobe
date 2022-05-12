#include "compiler.hpp"
#include "ast.hpp"
#include "program.hpp"
#include "diagnostics.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

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

    prog::type compiler::common_supertype(const ast::node& ast, const prog::type& type1, const prog::type& type2, bool confined) {
        if (subtype(type1, type2, confined))
            return prog::copy_type(type2);

        if (subtype(type2, type1, confined))
            return prog::copy_type(type1);

        error(diags::no_common_supertype(), ast);
    }
}

