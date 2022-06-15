#ifndef UTILS_HPP
#define UTILS_HPP

#include <utility>
#include <memory>
#include <optional>
#include <vector>
#include <variant>
#include <functional>
#include <type_traits>

namespace sg::utils {
    using std::move;
    using std::unique_ptr;
    using std::make_unique;
    using std::optional;
    using std::make_optional;
    using std::vector;
    using std::in_place_index;
    using std::get;
    using std::remove_reference;

    #define UNREACHABLE { throw 0; }

    #define VARIANT(type, index, val) (type { decltype(type::value)(in_place_index<type::index>, val) })
    #define AST_VARIANT(type, index, loc, val) (type { { loc }, decltype(type::value)(in_place_index<type::index>, val) })
    #define INDEX(val) ((val).value.index())
    #define INDEX_EQ(val, index) (INDEX(val) == remove_reference<decltype(val)>::type::index)
    #define GET(val, index) (get<remove_reference<decltype(val)>::type::index>((val).value))

    template<typename T>
    unique_ptr<T> make_ptr(T&& value) {
        return make_unique<T>(move(value));
    }

    template<typename T>
    unique_ptr<T> into_ptr(T& value) {
        return make_unique<T>(move(value));
    }

    template<typename T>
    optional<unique_ptr<T>> into_optional_ptr(optional<T>& value) {
        return value ? make_optional(make_unique<T>(move(*value))) : optional<unique_ptr<T>>();
    }

    template<typename T>
    vector<unique_ptr<T>> into_ptr_vector(vector<T>& values) {
        vector<unique_ptr<T>> result;
        for (T& value : values)
            result.push_back(make_unique<T>(move(value)));
        return result;
    }

    template<typename T>
    vector<unique_ptr<T>> copy_ptr_vector(const vector<unique_ptr<T>>& vec) {
        vector<unique_ptr<T>> result;
        for (auto& ptr : vec)
            result.push_back(make_unique<T>(*ptr));
        return result;
    }

    template<typename T>
    vector<unique_ptr<T>> copy_ptr_vector(const vector<unique_ptr<T>>& vec, std::function<T(const T&)> inner_copy_func) {
        vector<unique_ptr<T>> result;
        for (auto& ptr : vec)
            result.push_back(make_unique<T>(inner_copy_func(*ptr)));
        return result;
    }
}

#endif
