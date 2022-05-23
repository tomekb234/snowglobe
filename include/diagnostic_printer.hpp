#ifndef DIAGNOSTIC_PRINTER_HPP
#define DIAGNOSTIC_PRINTER_HPP

#include "diagnostics.hpp"

namespace sg {
    using std::reference_wrapper;
    using std::istream;

    class diagnostic_printer : public diagnostic_collector {
        vector<unique_ptr<diagnostic>> diags;

        public:

        void add(unique_ptr<diagnostic> diag) override;
        void report_all(ostream& stream, bool enable_colors, optional<reference_wrapper<istream>> source_file) const;
    };
}

#endif
