#ifndef UTILS_HPP
#define UTILS_HPP

#include <utility>
#include <memory>
#include <optional>
#include <vector>
#include <variant>

#define VARIANT(type, index, val) { decltype(type::value)(in_place_index<type::index>, val) }

namespace sg::utils {
    using std::move;
    using std::unique_ptr;
    using std::make_unique;
    using std::optional;
    using std::make_optional;
    using std::vector;
    using std::in_place_index;
    using std::make_pair;

    template<typename T>
    static unique_ptr<T> make_ptr(T value) {
        return make_unique<T>(move(value));
    }

    template<typename T>
    static unique_ptr<T> into_ptr(T& value) {
        return make_unique<T>(move(value));
    }

    template<typename T>
    static optional<unique_ptr<T>> into_optional_ptr(optional<T>& value) {
        return value ? make_optional(make_unique<T>(move(*value))) : optional<unique_ptr<T>>();
    }

    template<typename T>
    static vector<unique_ptr<T>> into_ptr_vector(vector<T>& values) {
        vector<unique_ptr<T>> result;
        for (T& value : values)
            result.push_back(make_unique<T>(move(value)));
        return result;
    }
}

#endif
