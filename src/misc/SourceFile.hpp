#pragma once

#include "Common.hpp"
#include "misc/SourceRef.hpp"

#include <string>
#include <vector>

#include <optional>

struct SourceFile final
{
    static std::optional<SourceFile> load(std::string_view full_path_to_file);

    std::string filename_with_ext;
    std::string path;

    std::string contents;

    std::string_view get_token_at(const SourceRef& where) const noexcept;
};