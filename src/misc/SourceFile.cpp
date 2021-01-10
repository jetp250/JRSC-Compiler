#include "SourceFile.hpp"

#include <fstream>
#include <algorithm>

#include <cstdio>

std::optional<SourceFile> SourceFile::load(std::string_view full_path_to_file) {
    std::ifstream infile(full_path_to_file.data(), std::ios_base::binary);

    if (!infile) {
        return std::nullopt;
    }

    size_t last_separator = full_path_to_file.find_last_of('\\');
    size_t last_alt_separator = full_path_to_file.find_last_of('/');

    if (last_separator == std::string_view::npos) last_separator = last_alt_separator;
    else if (last_alt_separator != std::string_view::npos) last_separator = std::max(last_separator, last_alt_separator);
    
    std::string path;
    std::string filename;

    if (last_separator != std::string_view::npos) {
        path = full_path_to_file.substr(0, last_separator + 1);
        filename = full_path_to_file.substr(last_separator + 1, filename.size() - 1);
    } else {
        path = full_path_to_file;
        filename = full_path_to_file;
    }

    std::string contents(( std::istreambuf_iterator<char>(infile)),
                            (std::istreambuf_iterator<char>() ));

    contents.push_back('\n');

    return std::optional<SourceFile>(SourceFile { 
        std::move(filename), 
        std::move(path), 
        std::move(contents) 
    });
}

std::string_view SourceFile::get_token_at(const SourceRef &where) const noexcept {
    return std::string_view(contents.data() + where.src_index, where.length);
}
