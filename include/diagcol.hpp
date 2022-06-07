#ifndef DIAGCOL_HPP
#define DIAGCOL_HPP

#include "location.hpp"
#include <optional>
#include <ostream>
#include <vector>
#include <memory>
#include <utility>
#include <functional>
#include <istream>

namespace sg {
    using std::optional;
    using std::ostream;
    using std::vector;
    using std::unique_ptr;
    using std::move;
    using std::reference_wrapper;
    using std::istream;

    struct diagnostic {
        enum level_t {
            NOTE = 0,
            WARNING = 1,
            ERROR = 2,
        } level;

        optional<location> loc;

        diagnostic(level_t level) : level(level) { }
        diagnostic(level_t level, location loc) : level(level), loc(loc) { }
        virtual ~diagnostic() { }

        virtual void write(ostream& stream) const = 0;
    };

    class diagnostic_collector {
        vector<unique_ptr<diagnostic>> diags;

        public:

        inline void add(unique_ptr<diagnostic> diag) {
            diags.push_back(move(diag));
        }

        void report_all(ostream& stream, bool enable_colors, optional<reference_wrapper<istream>> source_file) const;
    };
}

#endif
