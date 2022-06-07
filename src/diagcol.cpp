#include "diagcol.hpp"
#include <sstream>
#include <ostream>

namespace sg {
    using std::ostringstream;
    using std::endl;
    using std::string_view;

    const string LEVELS[] = { "Note", "Warning", "Error" };

    struct color_map {
        string levels[3];
        string location;
        string code_context;
        string code;
        string reset;
    };

    const color_map ACTIVE_COLORS = {
        .levels = { "\e[1;36m", "\e[1;33m", "\e[1;31m" },
        .location = "\e[1;37m",
        .code_context = "\e[2m",
        .code = "\e[1;35m",
        .reset = "\e[0m"
    };

    void diagnostic_collector::report_all(ostream& stream, bool enable_colors, optional<reference_wrapper<istream>> source_file) const {
        // Select color table
        const auto colors = enable_colors ? ACTIVE_COLORS : color_map { };

        // Prepare line buffer
        vector<string> source_lines;
        if (source_file) {
            string source_line_buffer;
            while (!getline(source_file->get(), source_line_buffer).eof())
                source_lines.push_back(move(source_line_buffer));
        }

        for (auto& diag : diags) {
            // Header
            stream << colors.levels[diag->level] << LEVELS[diag->level] << colors.reset;

            // Location
            if (diag->loc) {
                stream << " at ";
                stream << colors.location;
                stream << *diag->loc->begin.file_name << ":";
                stream << diag->loc->begin.line << ":";
                stream << diag->loc->begin.column;
                stream << colors.reset;
            }

            stream << ":" << endl;

            // Code fragment
            if (diag->loc && source_file) {
                // TODO check if filename matches

                stream << colors.code_context;
                auto& [begin, end] = *diag->loc;

                for (size_t it = begin.line; it <= end.line; it++) {
                    auto line = string_view(source_lines[it - 1]);
                    auto fragment_begin = it==begin.line ? begin.column-1 : 0;
                    auto fragment_end = it==end.line ? end.column-1 : line.length();

                    stream << "\t| " << line.substr(0, fragment_begin);
                    stream << colors.reset << colors.code << line.substr(fragment_begin, fragment_end-fragment_begin);
                    stream << colors.reset << colors.code_context << line.substr(fragment_end) << endl;
                }

                stream << colors.reset;
            }

            // Indented message text
            ostringstream buf;
            diag->write(buf);
            auto text = buf.str();
            size_t it = 0, nit;
            while ((nit = text.find_first_of('\n', it)) != string::npos) {
                stream << "\t" << text.substr(it, nit - it + 1);
                it = nit + 1;
            }

            // Extra line feed / stream flush
            stream << endl;
        }
    }
}
