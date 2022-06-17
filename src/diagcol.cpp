#include "diagcol.hpp"
#include <sstream>

namespace sg {
    using std::ostringstream;
    using std::endl;

    struct color_map {
        string levels[3];
        string location;
        string code_context;
        string code;
        string reset;
    };

    const string LEVELS[] = { "Note", "Warning", "Error" };

    const color_map COLORS = {
        .levels = { "\e[1;36m", "\e[1;33m", "\e[1;31m" },
        .location = "\e[1;37m",
        .code_context = "\e[2m",
        .code = "\e[1;35m",
        .reset = "\e[0m"
    };

    void diagnostic_collector::report_all(ostream& stream, bool enable_colors, const unordered_map<string, ref<istream>>& files) const {
        const auto colors = enable_colors ? COLORS : color_map { };

        for (auto& diag : diags) {
            // Header

            stream << colors.levels[diag->level] << LEVELS[diag->level] << colors.reset;

            // Location

            if (diag->loc) {
                auto& pos = diag->loc->begin;
                stream << " at ";
                stream << colors.location;
                stream << *pos.file_name << ":";
                stream << pos.line << ":";
                stream << pos.column;
                stream << colors.reset;
            }

            stream << ":" << endl;

            // Code fragment

            if (diag->loc) {
                auto& [begin, end] = *diag->loc;
                auto iter = files.find(*begin.file_name);

                if (iter != files.end()) {
                    auto& file = iter->second.get();
                    file.clear();
                    file.seekg(0);

                    stream << colors.code_context;

                    string line;

                    for (size_t index = 1; index <= end.line; index++) {
                        getline(file, line);

                        if (index < begin.line)
                            continue;

                        auto fragment_begin = index == begin.line ? begin.column - 1 : 0;
                        auto fragment_end = index == end.line ? end.column - 1 : line.length();

                        stream << "\t| " << line.substr(0, fragment_begin);
                        stream << colors.reset << colors.code << line.substr(fragment_begin, fragment_end - fragment_begin);
                        stream << colors.reset << colors.code_context << line.substr(fragment_end) << endl;
                    }

                    stream << colors.reset;
                }
            }

            // Indented message text

            ostringstream buffer;
            diag->write(buffer);

            auto text = buffer.str();
            size_t index = 0, end_index;

            while ((end_index = text.find_first_of('\n', index)) != string::npos) {
                stream << "\t" << text.substr(index, end_index - index + 1);
                index = end_index + 1;
            }

            // Empty line

            stream << endl;
        }
    }
}
