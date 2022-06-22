#ifndef LOCATION_HPP
#define LOCATION_HPP

#include <string>

namespace sg {
    using std::string;

    struct position {
        const string* file_name = nullptr;
        bool whole_file;
        size_t line;
        size_t column;
    };

    struct location {
        position begin;
        position end;
    };

    inline location whole_file(location loc) {
        loc.begin.whole_file = loc.end.whole_file = true;
        return loc;
    }
}

#endif
