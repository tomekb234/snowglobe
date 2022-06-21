#include "codegen.hpp"
#include "utils.hpp"
#include "diags.hpp"

namespace sg {
    using namespace sg::utils;
    using std::string;
    using std::to_string;
    using std::max;

    const string type_prefix = "tp.";

    llvm::Type* ll_type::get_type() const {
        switch (INDEX(*this)) {
            case ll_type::NUMBER: return GET(*this, NUMBER).tp;
            case ll_type::STRUCT: return GET(*this, STRUCT).tp;
            case ll_type::ENUM: return GET(*this, ENUM).tp;
            case ll_type::ENUM_VARIANT: return GET(*this, ENUM_VARIANT).tp;
            case ll_type::TUPLE: return GET(*this, TUPLE).tp;
            case ll_type::ARRAY: return GET(*this, ARRAY).tp;
            case ll_type::OPTIONAL: return GET(*this, OPTIONAL).tp;
        }

        UNREACHABLE;
    }

    string ll_type::get_name() const {
        switch (INDEX(*this)) {
            case ll_type::NUMBER: {
                switch (GET(*this, NUMBER).kind) {
                    case ll_number_type::I1: return "i1";
                    case ll_number_type::I8: return "i8";
                    case ll_number_type::I16: return "i16";
                    case ll_number_type::I32: return "i32";
                    case ll_number_type::I64: return "i64";
                    case ll_number_type::FLOAT: return "float";
                    case ll_number_type::DOUBLE: return "double";
                }
            }

            case ll_type::STRUCT:
                return "S" + to_string(GET(*this, STRUCT).index);

            case ll_type::ENUM:
                return "E" + to_string(GET(*this, ENUM).index);

            case ll_type::ENUM_VARIANT: {
                auto& variant_type = GET(*this, ENUM_VARIANT);
                return "V" + to_string(variant_type.enum_index) + "." + to_string(variant_type.variant_index);
            }

            case ll_type::TUPLE: {
                string str = "T";
                for (auto field : GET(*this, TUPLE).fields)
                    str += "." + field->get_name();
                return str + "..";
            }

            case ll_type::ARRAY: {
                auto& array_type = GET(*this, ARRAY);
                return "A" + to_string(array_type.size) + "." + array_type.value->get_name();
            }

            case ll_type::OPTIONAL: {
                return "O." + GET(*this, OPTIONAL).value->get_name();
            }
        }

        UNREACHABLE;
    }

    int ll_type::compare(const ll_type& other) const {
        #define INT_COMPARE(x, y) (((x)<(y)) ? -1 : ((x)>(y)))
        #define MAYBE_RETURN(x) { decltype(x) tmp = x; if (tmp) return tmp; }

        MAYBE_RETURN(INT_COMPARE(INDEX(*this), INDEX(other)))
        switch (INDEX(*this)) {
            case ll_type::NUMBER:
                return INT_COMPARE(GET(*this, NUMBER).kind, GET(other, NUMBER).kind);

            case ll_type::STRUCT:
                return INT_COMPARE(GET(*this, STRUCT).index, GET(other, STRUCT).index);

            case ll_type::ENUM:
                return INT_COMPARE(GET(*this, ENUM).index, GET(other, ENUM).index);

            case ll_type::ENUM_VARIANT: {
                auto& left = GET(*this, ENUM_VARIANT);
                auto& right = GET(other, ENUM_VARIANT);
                MAYBE_RETURN(INT_COMPARE(left.enum_index, right.enum_index));
                return INT_COMPARE(left.variant_index, right.variant_index);
            }

            case ll_type::TUPLE: {
                auto& left = GET(*this, TUPLE);
                auto& right = GET(other, TUPLE);
                MAYBE_RETURN(INT_COMPARE(left.fields.size(), right.fields.size()));
                for (size_t i = 0; i < left.fields.size(); i++)
                    MAYBE_RETURN(left.fields[i]->compare(*right.fields[i]));
                return 0;
            }

            case ll_type::ARRAY: {
                auto& left = GET(*this, ARRAY);
                auto& right = GET(other, ARRAY);
                MAYBE_RETURN(INT_COMPARE(left.size, right.size));
                return left.value->compare(*right.value);
            }

            case ll_type::OPTIONAL:
                return GET(*this, OPTIONAL).value->compare(*GET(other, OPTIONAL).value);
        }

        UNREACHABLE;
        #undef MAYBE_RETURN
        #undef INT_COMPARE
    }
    bool ll_type::operator<(const ll_type& other) const {
        return compare(other) < 0;
    }


    
    #define RETURN_TYPE(kind, kind_struct, make_instr) { \
        auto type_ptr = make_ptr(VARIANT(ll_type, kind, kind_struct)); \
        auto [it, inserted] = type_set.insert(ll_type_wrapper{ move(type_ptr) }); \
        if (inserted) {\
            auto& gen_ins = *it->type; \
            auto& ins = GET(gen_ins, kind); \
            make_instr; \
        } \
        return it->type.get(); \
    }




    ll_type* code_generator::get_never_type() {
        return get_number_type(prog::number_type::BOOL);
    }

    ll_type* code_generator::get_unit_type() {
        return get_number_type(prog::number_type::BOOL);
    }

    ll_type* code_generator::get_byte_type() {
        return get_number_type(prog::number_type::U8);
    }


    ll_type* code_generator::get_number_type(prog::number_type::type_t type) {
        switch (type) {
            case prog::number_type::BOOL: {
                auto ntp = ll_number_type{ ll_number_type::I1, nullptr };
                RETURN_TYPE(NUMBER, ntp, ins.tp = llvm::Type::getInt1Ty(ctx));
            }

            case prog::number_type::I8:
            case prog::number_type::U8: {
                auto ntp = ll_number_type{ ll_number_type::I8, nullptr };
                RETURN_TYPE(NUMBER, ntp, ins.tp = llvm::Type::getInt8Ty(ctx));
            }

            case prog::number_type::I16:
            case prog::number_type::U16: {
                auto ntp = ll_number_type{ ll_number_type::I16, nullptr };
                auto tp = make_ptr(VARIANT(ll_type, NUMBER, ntp));
                RETURN_TYPE(NUMBER, ntp, ins.tp = llvm::Type::getInt16Ty(ctx));
            }

            case prog::number_type::I32:
            case prog::number_type::U32: {
                auto ntp = ll_number_type{ ll_number_type::I32, nullptr };
                RETURN_TYPE(NUMBER, ntp, ins.tp = llvm::Type::getInt32Ty(ctx));
            }

            case prog::number_type::I64:
            case prog::number_type::U64: {
                auto ntp = ll_number_type{ ll_number_type::I64, nullptr };
                RETURN_TYPE(NUMBER, ntp, ins.tp = llvm::Type::getInt64Ty(ctx));
            }

            case prog::number_type::F32: {
                auto ntp = ll_number_type{ ll_number_type::FLOAT, nullptr };
                RETURN_TYPE(NUMBER, ntp, ins.tp = llvm::Type::getFloatTy(ctx));
            }

            case prog::number_type::F64: {
                auto ntp = ll_number_type{ ll_number_type::DOUBLE, nullptr };
                RETURN_TYPE(NUMBER, ntp, ins.tp = llvm::Type::getDoubleTy(ctx));
            }
        }

        UNREACHABLE;
    }

    ll_type* code_generator::get_tuple_type(const vector<ll_type*>& fields) {
        auto ttp = ll_tuple_type{ fields, nullptr };
        RETURN_TYPE(TUPLE, ttp, {
            vector<llvm::Type*> llvm_types;
            for (auto field : fields)
                llvm_types.push_back(field->get_type());
            ins.tp = llvm::StructType::create(llvm_types, type_prefix + gen_ins.get_name());
        });
    }

    ll_type* code_generator::get_array_type(ll_type* value_type, size_t size) {
        auto atp = ll_array_type{ value_type, size, nullptr };
        RETURN_TYPE(ARRAY, atp, ins.tp = llvm::ArrayType::get(value_type->get_type(), size));
    }

    ll_type* code_generator::get_optional_type(ll_type* value_type) {
        auto otp = ll_optional_type{ value_type, nullptr };
        RETURN_TYPE(OPTIONAL, otp, {
            vector<llvm::Type*> fields;
            fields.push_back(llvm::Type::getInt1Ty(ctx));
            fields.push_back(value_type->get_type());
            ins.tp = llvm::StructType::create(fields, type_prefix + gen_ins.get_name());
        });
    }

    ll_type* code_generator::get_type_from_prog(const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::NEVER:
                return get_never_type();

            case prog::type::UNIT:
                return get_unit_type();

            case prog::type::NUMBER:
                return get_number_type(GET(type, NUMBER)->tp);

            case prog::type::STRUCT:
                return struct_types[GET(type, STRUCT)];

            case prog::type::ENUM:
                return enum_types[GET(type, ENUM)];

            case prog::type::TUPLE: {
                auto& tuple_type = GET(type, TUPLE);
                vector<ll_type*> field_types;
                for (auto& field : tuple_type)
                    field_types.push_back(get_type_from_prog(*field));
                return get_tuple_type(field_types);
            }

            case prog::type::ARRAY: {
                auto& array_type = *GET(type, ARRAY);
                auto value_type = get_type_from_prog(*array_type.tp);
                return get_array_type(value_type, array_type.size);
            }

            case prog::type::OPTIONAL:
                return get_optional_type(get_type_from_prog(*GET(type, OPTIONAL)));

            default:
                error(diags::not_implemented()); // TODO
        }

        UNREACHABLE;
    }

    ll_type* code_generator::declare_struct_type(prog::global_index index) {
        auto stp = ll_struct_type{ index, { }, nullptr };
        RETURN_TYPE(STRUCT, stp, ins.tp = llvm::StructType::create(ctx, type_prefix + gen_ins.get_name()));
    }

    ll_type* code_generator::declare_enum_type(prog::global_index index) {
        auto etp = ll_enum_type{ index, nullptr, nullptr };
        RETURN_TYPE(ENUM, etp, ins.tp = llvm::StructType::create(ctx, type_prefix + gen_ins.get_name()));
    }

    ll_type* code_generator::declare_enum_variant_type(prog::global_index enum_index, prog::variant_index variant_index) {
        auto evtp = ll_enum_variant_type{ enum_index, variant_index, { }, nullptr, nullptr };
        RETURN_TYPE(ENUM_VARIANT, evtp, ins.tp = llvm::StructType::create(ctx, type_prefix + gen_ins.get_name()));
    }

    void code_generator::define_struct_type(const prog::struct_type& prog_struct_type, ll_struct_type* llvm_struct_type) {
        vector<llvm::Type*> raw_field_types;
        for (auto& field : prog_struct_type.fields) {
            llvm_struct_type->fields.push_back(get_type_from_prog(*field->tp));
            raw_field_types.push_back(llvm_struct_type->fields.back()->get_type());
        }
        llvm_struct_type->tp->setBody(raw_field_types);
    }

    void code_generator::define_enum_type_with_variants(const prog::enum_type& prog_enum_type, ll_enum_type* llvm_enum_type, vector<ll_enum_variant_type*> llvm_variant_types) {
        // first pass - field types processing
        vector<vector<llvm::Type*>> llvm_type_lists(prog_enum_type.variants.size());
        vector<size_t> variant_sizes;
        size_t max_variant_size = 8;
        for (size_t i = 0; i < prog_enum_type.variants.size(); i++) {
            llvm_type_lists[i].push_back(llvm::Type::getInt64Ty(ctx));
            for (auto& field : prog_enum_type.variants[i]->tps) {
                llvm_variant_types[i]->fields.push_back(get_type_from_prog(*field));
                llvm_type_lists[i].push_back(llvm_variant_types[i]->fields.back()->get_type());
            }
            auto raw_llvm_variant = llvm::StructType::create(ctx, llvm_type_lists[i]);
            variant_sizes.push_back(mod.getDataLayout().getTypeAllocSize(raw_llvm_variant));
            max_variant_size = max(max_variant_size, variant_sizes.back());
        }

        // second pass - defining equal-size variant types
        for (size_t i = 0; i < prog_enum_type.variants.size(); i++) {
            llvm_variant_types[i]->placeholder = &GET(*get_array_type(get_byte_type(), max_variant_size - variant_sizes[i]), ARRAY);
            llvm_type_lists[i].push_back(llvm_variant_types[i]->placeholder->tp);
            llvm_variant_types[i]->tp->setBody(llvm_type_lists[i]);
        }

        // defining enum type
        llvm_enum_type->placeholder = &GET(*get_array_type(get_byte_type(), max_variant_size - 8), ARRAY);
        vector<llvm::Type*> enum_fields {llvm::Type::getInt64Ty(ctx), llvm_enum_type->placeholder->tp};
        llvm_enum_type->tp->setBody(enum_fields);
    }
}
