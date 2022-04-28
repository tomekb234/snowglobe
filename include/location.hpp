#ifndef LOCATION_HPP
#define LOCATION_HPP

#include <string>

namespace sg {
    using std::string;

    struct location {
        string file_name;
        size_t line;
        size_t column;
    };
}

#endif
