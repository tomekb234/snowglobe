#ifndef LOCATION_HPP
#define LOCATION_HPP

#include <string>

namespace sg {
    using std::string;

    struct position {
        const string* file_name;
        size_t line;
        size_t column;
    };

    struct location {
        position begin;
        position end;
    };
}

#endif
