#include "compiler/assignment.hpp"
#include "compiler/conversions.hpp"
#include "compiler/deletion.hpp"
#include "compiler/compiler_utils.hpp"
#include "compiler/function_utils.hpp"
#include "diags.hpp"
#include "utils.hpp"

namespace sg {
    using namespace sg::utils;

    void assignment_generator::add(const lvalue& lval, prog::reg_index value, const prog::type_local& type, location loc) {
        switch (INDEX(lval)) {
            case lvalue::IGNORED:
                if (!type.confined)
                    deletion_generator(fclr).add(value, *type.tp);
                break;

            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                auto& var = fclr.vars[var_index];

                if (type.confined && !type_trivial(prog, *type.tp) && var.outside_confinement > 0)
                    error(diags::variable_outside_confinement(var.name, loc));

                function_utils(fclr).add_var_deletion(var_index, loc);

                value = conversion_generator(fclr).convert(value, type, var.type, loc);

                auto write_instr = prog::write_var_instr { var_index, value };
                fclr.add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

                var.state = VAR_INITIALIZED;
                var.outside_loop = 0;
            } break;

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                auto& var_type = *prog.global_vars[var_index]->tp;

                auto old_value = fclr.new_reg();
                auto read_instr = prog::read_global_var_instr { var_index, old_value };
                fclr.add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(read_instr)));
                deletion_generator(fclr).add(old_value, var_type);

                value = conversion_generator(fclr).convert(value, type, var_type, loc);

                auto instr = prog::write_global_var_instr { var_index, value };
                fclr.add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, into_ptr(instr)));
            } break;

            case lvalue::TUPLE: {
                auto lvals = as_cref_vector(GET(lval, TUPLE));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, TUPLE))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::TUPLE, loc));

                auto field_types = as_cref_vector(GET(*type.tp, TUPLE));
                auto confined = type.confined;

                if (field_types.size() != count)
                    error(diags::invalid_tuple_size(field_types.size(), count, loc));

                for (size_t index = 0; index < count; index++) {
                    auto extracted = fclr.new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(field_types[index])), confined };
                    add(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::ARRAY: {
                auto lvals = as_cref_vector(GET(lval, ARRAY));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, ARRAY))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::ARRAY, loc));

                auto& array_type = *GET(*type.tp, ARRAY);
                auto item_type = prog::type_local { make_ptr(copy_type(*array_type.tp)), type.confined };

                if (array_type.size != count)
                    error(diags::invalid_array_size(array_type.size, count, loc));

                for (size_t index = 0; index < count; index++) {
                    auto extracted = fclr.new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    add(lvals[index], extracted, item_type, loc);
                }
            } break;

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                auto lvals = as_cref_vector(lval_ptrs);

                if (!INDEX_EQ(*type.tp, STRUCT))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::STRUCT, loc));
                if (GET(*type.tp, STRUCT) != struct_index)
                    error(diags::invalid_struct(*prog.struct_types[GET(*type.tp, STRUCT)], *prog.struct_types[struct_index], loc));

                auto& st = *prog.struct_types[struct_index];
                auto count = st.fields.size();
                auto confined = type.confined;

                for (size_t index = 0; index < count; index++) {
                    auto extracted = fclr.new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(*st.fields[index]->tp)), confined };
                    add(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, target_type] = GET(lval, DEREFERENCE);

                auto old_value = fclr.new_reg();
                auto read_instr = prog::ptr_read_instr { ptr_value, old_value };
                fclr.add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(read_instr)));
                deletion_generator(fclr).add(old_value, target_type);

                value = conversion_generator(fclr).convert(value, type, target_type, loc);

                auto write_instr = prog::ptr_write_instr { ptr_value, value };
                fclr.add_instr(VARIANT(prog::instr, PTR_WRITE, into_ptr(write_instr)));
            } break;
        }
    }

    pair<prog::reg_index, prog::type_local> assignment_generator::add_read_for_swap(const lvalue& lval, location loc) {
        auto fields = [&] (vector<cref<lvalue>> lvals) -> tuple<vector<prog::reg_index>, vector<prog::type>, bool> {
            vector<prog::reg_index> values;
            vector<prog::type> types;
            optional<bool> all_confined;

            for (const lvalue& lval : lvals) {
                auto [value, type] = add_read_for_swap(lval, loc);

                if (!type_trivial(prog, *type.tp)) {
                    if (!all_confined)
                        all_confined = { type.confined };
                    else if (type.confined != *all_confined)
                        error(diags::confinement_ambiguous(loc));
                }

                values.push_back(value);
                types.push_back(move(*type.tp));
            }

            if (!all_confined)
                all_confined = { false };

            return { move(values), move(types), *all_confined };
        };

        switch (INDEX(lval)) {
            case lvalue::IGNORED:
                error(diags::expression_not_swappable(loc));

            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                auto& var = fclr.vars[var_index];
                auto& state = var.state;

                if (state != VAR_INITIALIZED)
                    error(diags::variable_not_usable(var.name, state, loc));

                auto result = fclr.new_reg();
                auto instr = prog::read_var_instr { var_index, result };
                fclr.add_instr(VARIANT(prog::instr, READ_VAR, into_ptr(instr)));

                return { result, copy_type_local(var.type) };
            }

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                auto& type = *prog.global_vars[var_index]->tp;
                auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

                auto result = fclr.new_reg();
                auto instr = prog::read_global_var_instr { var_index, result };
                fclr.add_instr(VARIANT(prog::instr, READ_GLOBAL_VAR, into_ptr(instr)));

                return { result, move(type_local) };
            }

            case lvalue::TUPLE: {
                auto [values, types, all_confined] = fields(as_cref_vector(GET(lval, TUPLE)));

                auto result = fclr.new_reg();
                auto instr = prog::make_tuple_instr { move(values), result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_TUPLE, into_ptr(instr)));

                auto type = VARIANT(prog::type, TUPLE, into_ptr_vector(types));
                auto type_local = prog::type_local { into_ptr(type), all_confined };

                return { result, move(type_local) };
            }

            case lvalue::ARRAY: {
                auto [values, types, all_confined] = fields(as_cref_vector(GET(lval, ARRAY)));
                auto count = values.size();

                prog::type common_type = copy_type(prog::NEVER_TYPE);

                for (auto& type : types)
                    common_type = compiler_utils(clr).common_supertype(common_type, type, loc);

                for (size_t index = 0; index < count; index++)
                    values[index] = conversion_generator(fclr).convert(values[index], types[index], common_type, all_confined, loc);

                auto result = fclr.new_reg();
                auto instr = prog::make_array_instr { move(values), result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_ARRAY, into_ptr(instr)));

                auto array = prog::array_type { into_ptr(common_type), count };
                auto type = VARIANT(prog::type, ARRAY, into_ptr(array));
                auto type_local = prog::type_local { into_ptr(type), all_confined };

                return { result, move(type_local) };
            }

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                auto [values, types, all_confined] = fields(as_cref_vector(lval_ptrs));

                auto& st = *prog.struct_types[struct_index];
                auto count = values.size();

                for (size_t index = 0; index < count; index++) {
                    auto& type = types[index];
                    auto& field_type = *st.fields[index]->tp;
                    values[index] = conversion_generator(fclr).convert(values[index], type, field_type, all_confined, loc);
                }

                auto result = fclr.new_reg();
                auto instr = prog::make_struct_instr { struct_index, move(values), result };
                fclr.add_instr(VARIANT(prog::instr, MAKE_STRUCT, into_ptr(instr)));

                auto type = VARIANT(prog::type, STRUCT, struct_index);
                auto type_local = prog::type_local { into_ptr(type), all_confined };

                return { result, move(type_local) };
            }

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, type] = GET(lval, DEREFERENCE);
                auto type_local = prog::type_local { make_ptr(copy_type(type)), false };

                auto result = fclr.new_reg();
                auto instr = prog::ptr_read_instr { ptr_value, result };
                fclr.add_instr(VARIANT(prog::instr, PTR_READ, into_ptr(instr)));

                return { result, move(type_local) };
            }
        }

        UNREACHABLE;
    }

    void assignment_generator::add_write_from_swap(const lvalue& lval, prog::reg_index value, const prog::type_local& type, location loc) {
        switch (INDEX(lval)) {
            case lvalue::VAR: {
                auto var_index = GET(lval, VAR);
                auto& var = fclr.vars[var_index];

                if (type.confined && !type_trivial(prog, *type.tp) && var.outside_confinement > 0)
                    error(diags::variable_outside_confinement(var.name, loc));

                value = conversion_generator(fclr).convert(value, type, var.type, loc);

                auto write_instr = prog::write_var_instr { var_index, value };
                fclr.add_instr(VARIANT(prog::instr, WRITE_VAR, into_ptr(write_instr)));

                var.state = VAR_INITIALIZED;
                var.outside_loop = 0;
            } break;

            case lvalue::GLOBAL_VAR: {
                auto var_index = GET(lval, GLOBAL_VAR);
                auto& var_type = *prog.global_vars[var_index]->tp;

                value = conversion_generator(fclr).convert(value, type, var_type, loc);

                auto instr = prog::write_global_var_instr { var_index, value };
                fclr.add_instr(VARIANT(prog::instr, WRITE_GLOBAL_VAR, into_ptr(instr)));
            } break;

            case lvalue::TUPLE: {
                auto lvals = as_cref_vector(GET(lval, TUPLE));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, TUPLE))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::TUPLE, loc));

                auto field_types = as_cref_vector(GET(*type.tp, TUPLE));
                auto confined = type.confined;

                if (field_types.size() != count)
                    error(diags::invalid_tuple_size(field_types.size(), count, loc));

                for (size_t index = 0; index < count; index++) {
                    auto extracted = fclr.new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(field_types[index])), confined };
                    add_write_from_swap(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::ARRAY: {
                auto lvals = as_cref_vector(GET(lval, ARRAY));
                auto count = lvals.size();

                if (!INDEX_EQ(*type.tp, ARRAY))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::ARRAY, loc));

                auto& array_type = *GET(*type.tp, ARRAY);
                auto item_type = prog::type_local { make_ptr(copy_type(*array_type.tp)), type.confined };

                if (array_type.size != count)
                    error(diags::invalid_array_size(array_type.size, count, loc));

                for (size_t index = 0; index < count; index++) {
                    auto extracted = fclr.new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    add_write_from_swap(lvals[index], extracted, item_type, loc);
                }
            } break;

            case lvalue::STRUCT: {
                auto& [struct_index, lval_ptrs] = GET(lval, STRUCT);
                auto lvals = as_cref_vector(lval_ptrs);

                if (!INDEX_EQ(*type.tp, STRUCT))
                    error(diags::invalid_type(prog, move(*type.tp), diags::type_kind::STRUCT, loc));
                if (GET(*type.tp, STRUCT) != struct_index)
                    error(diags::invalid_struct(*prog.struct_types[GET(*type.tp, STRUCT)], *prog.struct_types[struct_index], loc));

                auto& st = *prog.struct_types[struct_index];
                auto count = st.fields.size();
                auto confined = type.confined;

                for (size_t index = 0; index < count; index++) {
                    auto extracted = fclr.new_reg();
                    auto instr = prog::extract_field_instr { value, index, extracted };
                    fclr.add_instr(VARIANT(prog::instr, EXTRACT_FIELD, into_ptr(instr)));

                    auto field_type = prog::type_local { make_ptr(copy_type(*st.fields[index]->tp)), confined };
                    add_write_from_swap(lvals[index], extracted, field_type, loc);
                }
            } break;

            case lvalue::DEREFERENCE: {
                auto& [ptr_value, target_type] = GET(lval, DEREFERENCE);

                value = conversion_generator(fclr).convert(value, type, target_type, loc);

                auto write_instr = prog::ptr_write_instr { ptr_value, value };
                fclr.add_instr(VARIANT(prog::instr, PTR_WRITE, into_ptr(write_instr)));
            } break;
        }
    }
}
