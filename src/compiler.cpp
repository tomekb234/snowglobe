#include "compiler.hpp"
#include "utils.hpp"
#include <utility>
#include <tuple>
#include <optional>
#include <variant>
#include <memory>
#include <exception>

namespace sg {
    using namespace sg::utils;

    using std::move;
    using std::pair;
    using std::tie;
    using std::get;
    using std::make_pair;
    using std::make_optional;
    using std::make_unique;
    using std::exception;

    prog::program compiler::compile(const ast::program& input) {
        program = { };

        for (auto& global_def : input.global_defs) {
            switch (global_def->value.index()) {
                case ast::global_def::VAR_DEF: {
                    auto global_var = compile_global_var(*get<ast::global_def::VAR_DEF>(global_def->value));
                    program.global_vars.push_back(into_ptr(global_var));
                    break;
                }

                case ast::global_def::FUNC_DEF: // TODO
                case ast::global_def::STRUCT_DEF:
                case ast::global_def::ENUM_DEF:
                    diags.add(make_unique<diags::not_implemented_error>());
                    throw exception();
            }
        }

        return move(program);
    }

    prog::global_var compiler::compile_global_var(const ast::var_def& input) {
        string name = input.name;

        if (global_names.count(name)) {
            diags.add(make_unique<diags::global_name_used_error>(name));
            throw exception();
        }

        prog::constant value;
        prog::type tp;
        tie(value, tp) = compile_constant(*input.value);

        if (input.tp) {
            auto declared_tp = compile_type(**input.tp);
            // TODO
        }

        diags.add(make_unique<diags::not_implemented_error>());
        throw exception();
    }

    pair<prog::constant, prog::type> compiler::compile_constant(const ast::expr& input) {
        switch (input.value.index()) {
            case ast::expr::TUPLE: // TODO
            case ast::expr::ARRAY:
            case ast::expr::APPLICATION:
            default:
                diags.add(make_unique<diags::not_implemented_error>());
                throw exception();
        }
    }

    prog::type compiler::compile_type(const ast::type& input) {
        switch (input.value.index()) {
            case ast::type::PRIMITIVE: {
                auto tp = compile_primitive_type(*get<ast::type::PRIMITIVE>(input.value));
                return VARIANT(prog::type, PRIMITIVE, into_ptr(tp));
            }

            case ast::type::USER_TYPE: // TODO
            case ast::type::TUPLE:
            case ast::type::ARRAY:
            case ast::type::OPTIONAL:
            case ast::type::PTR:
            case ast::type::INNER_PTR:
            case ast::type::FUNC:
            case ast::type::GLOBAL_FUNC:
            case ast::type::FUNC_WITH_PTR:
            default:
                diags.add(make_unique<diags::not_implemented_error>());
                throw exception();
        }
    }

    prog::primitive_type compiler::compile_primitive_type(const ast::primitive_type& input) {
        // TODO
        diags.add(make_unique<diags::not_implemented_error>());
        throw exception();
    }
}
