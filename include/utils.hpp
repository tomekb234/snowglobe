#ifndef UTILS_HPP
#define UTILS_HPP

#include <utility>
#include <optional>
#include <variant>
#include <tuple>
#include <vector>
#include <queue>
#include <algorithm>
#include <memory>
#include <functional>
#include <limits>
#include <type_traits>

namespace sg::utils {
    using std::move;
    using std::optional;
    using std::make_optional;
    using std::get;
    using std::monostate;
    using std::in_place_index;
    using std::make_pair;
    using std::tuple;
    using std::make_tuple;
    using std::tie;
    using std::vector;
    using std::queue;
    using std::find;
    using std::unique_ptr;
    using std::make_unique;
    using std::reference_wrapper;
    using std::function;
    using std::numeric_limits;
    using std::remove_reference;
    using std::is_signed;
    using std::is_unsigned;

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

    template<typename T>
    optional<reference_wrapper<const T>> as_optional_cref(const optional<unique_ptr<T>>& value) {
        return value ? make_optional(reference_wrapper(**value)) : optional<reference_wrapper<const T>>();
    }

    template<typename T>
    vector<reference_wrapper<const T>> as_cref_vector(const vector<unique_ptr<T>>& vec) {
        vector<reference_wrapper<const T>> result;
        for (auto& ptr : vec)
            result.push_back(*ptr);
        return result;
    }

    template<typename T>
    unsigned long long encode_number(T number) {
        unsigned long long result = 0;
        auto ptr = reinterpret_cast<T*>(&result);
        *ptr = number;
        return result;
    }

    template<typename T>
    T decode_number(unsigned long long number) {
        auto ptr = reinterpret_cast<T*>(&number);
        return *ptr;
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
}

#endif
