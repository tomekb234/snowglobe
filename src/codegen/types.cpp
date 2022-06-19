#include "codegen.hpp"
#include "utils.hpp"
#include "diags.hpp"

namespace sg {
    using namespace sg::utils;
    using std::string;
    using std::to_string;

    #define RETURN_CACHED(str, tp) { \
        if (types.count(str)) \
            return named_llvm_type<tp>{ (str), static_cast<tp*>(types[str]) }; \
    }
    #define RETURN_NEW(str, make_expr, tp) { \
        auto new_type = (make_expr); \
        types[str] = new_type; \
        return named_llvm_type<tp>{ (str), new_type }; \
    }
    #define RETURN_NAMED_TYPE(str, make_expr, tp) RETURN_CACHED(str, tp) RETURN_NEW(str, make_expr, tp)

    code_generator::named_llvm_type<> code_generator::get_type_from_prog(const prog::type& type) {
        switch (INDEX(type)) {
            case prog::type::NEVER:
            case prog::type::UNIT:
                return get_number_type(prog::number_type::I8);

            case prog::type::NUMBER:
                return get_number_type(GET(type, NUMBER)->tp);

            case prog::type::STRUCT:
                return get_struct_type(GET(type, STRUCT)).to_type();

            case prog::type::TUPLE: {
                auto& tuple_type = GET(type, TUPLE);
                vector<named_llvm_type<llvm::Type>> field_types;
                for (auto& field : tuple_type)
                    field_types.push_back(get_type_from_prog(*field));
                return get_tuple_type(field_types).to_type();
            }

            case prog::type::ARRAY: {
                auto& array_type = *GET(type, ARRAY);
                auto value_type = get_type_from_prog(*array_type.tp);
                return get_array_type(value_type, array_type.size).to_type();
            }

            default:
                error(diags::not_implemented()); // TODO
        }

        UNREACHABLE;
    }

    code_generator::named_llvm_type<> code_generator::get_never_type() {
        return get_number_type(prog::number_type::I8);
    }

    code_generator::named_llvm_type<> code_generator::get_unit_type() {
        return get_number_type(prog::number_type::I8);
    }

    code_generator::named_llvm_type<> code_generator::get_number_type(prog::number_type::type_t type) {
        switch (type) {
            case prog::number_type::BOOL:
                RETURN_NAMED_TYPE("i1", llvm::Type::getInt1Ty(ctx), llvm::Type);

            case prog::number_type::I8:
            case prog::number_type::U8:
                RETURN_NAMED_TYPE("i8", llvm::Type::getInt8Ty(ctx), llvm::Type);

            case prog::number_type::I16:
            case prog::number_type::U16:
                RETURN_NAMED_TYPE("i16", llvm::Type::getInt16Ty(ctx), llvm::Type);

            case prog::number_type::I32:
            case prog::number_type::U32:
                RETURN_NAMED_TYPE("i32", llvm::Type::getInt32Ty(ctx), llvm::Type);

            case prog::number_type::I64:
            case prog::number_type::U64:
                RETURN_NAMED_TYPE("i64", llvm::Type::getInt64Ty(ctx), llvm::Type);

            case prog::number_type::F32:
                RETURN_NAMED_TYPE("f32", llvm::Type::getFloatTy(ctx), llvm::Type);

            case prog::number_type::F64:
                RETURN_NAMED_TYPE("f64", llvm::Type::getDoubleTy(ctx), llvm::Type);
        }

        UNREACHABLE;
    }

    code_generator::named_llvm_type<llvm::StructType> code_generator::get_struct_type(prog::global_index struct_index) {
        string struct_name = "S" + prog.struct_types[struct_index]->name;
        RETURN_NAMED_TYPE(struct_name, struct_types[struct_index], llvm::StructType);
    }

    code_generator::named_llvm_type<llvm::StructType> code_generator::get_tuple_type(const vector<named_llvm_type<llvm::Type>>& fields) {
        vector<llvm::Type*> field_types;
        string tuple_name = "T";
        for (auto& field : fields) {
            field_types.push_back(field.type);
            tuple_name.append('.' + field.name);
        }
        tuple_name.append("..");
        RETURN_NAMED_TYPE(tuple_name, llvm::StructType::create(field_types, tuple_name), llvm::StructType);
    }

    code_generator::named_llvm_type<llvm::ArrayType> code_generator::get_array_type(const named_llvm_type<llvm::Type>& value_type, size_t size) {
        string name = "A." + value_type.name + "." + to_string(size);
        RETURN_NAMED_TYPE(name, llvm::ArrayType::get(value_type.type, size), llvm::ArrayType);
    }
}
