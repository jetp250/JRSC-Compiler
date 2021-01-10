#pragma once

#include <cstdio>
#include <utility>
#include <vector>
#include <span>

#include <fmt/color.h>
#include <fmt/core.h>

#include "Common.hpp"
#include "SourceFile.hpp"
#include "SourceRef.hpp"

struct ErrorInfo final
{
    SourceFile const* file;
    SourceRef where;
    bool show_caret = true;
    bool underline = true;
};

// Represents an issue found in the code, i.e a warning, error or a note
// They're stored in a vector until parsing ends to ensure two parsing processes
// don't spit out their errors/warnings/infos concurrently
struct Issue final
{
    enum class Type { ERROR, WARNING, INFO };

    std::string message;
    std::string function_name;
    SourceRef where;
    Type type;
    bool show_caret = true;
    bool underline = true;

    Issue& at(SourceRef where) noexcept {
        this->where = where;
        return *this;
    }

    Issue& with_caret(bool caret = true) noexcept {
        show_caret = caret;
        return *this;
    }

    Issue& with_underline(bool underline = true) noexcept {
        this->underline = underline;
        return *this;
    }

    Issue& in_function(std::string name) noexcept {
        function_name = std::move(name);
        return *this;
    }
};

void log_issues(std::span<Issue> issues, const SourceFile& file) noexcept;

namespace Detail {
    void print_source_hint(ErrorInfo& err);
}

// Internal compiler error
template<typename ...Args>
[[noreturn]] void ice_abort(int code, std::string_view format, Args &&... args) {
    std::puts("Internal compiler error: ");
    fmt::print(format.data(), std::forward<Args>(args)...);
    std::puts("\nAborting\n\n");
    std::fflush(stdout);

    exit(code);
}

template<typename ...Args>
void log_error(ErrorInfo err, std::string_view format, Args &&... args) {
    fmt::print("{}:{}:{}: ", err.file->filename_with_ext.data(), err.where.line + 1, err.where.column + 1);
    fmt::print(fmt::fg(fmt::terminal_color::bright_red), "error: ");
    fmt::print(fmt::fg(fmt::terminal_color::white), format.data(), std::forward<Args>(args)...);
    std::putchar('\n');

    Detail::print_source_hint(err);
}

template<typename ...Args>
void log_warning(ErrorInfo err, std::string_view format, Args &&... args) {
    fmt::print("{}:{}:{}: ", err.file->filename_with_ext.data(), err.where.line + 1, err.where.column + 1);
    fmt::print(fmt::fg(fmt::terminal_color::bright_magenta), "warning: ");
    fmt::print(fmt::fg(fmt::terminal_color::white), format.data(), std::forward<Args>(args)...);
    std::putchar('\n');

    Detail::print_source_hint(err);
}

template<typename ...Args>
void log_clarification(ErrorInfo err, std::string_view format, Args &&... args) {
    fmt::print("{}:{}:{}: ", err.file->filename_with_ext.data(), err.where.line + 1, err.where.column + 1);
    fmt::print(fmt::fg(fmt::terminal_color::bright_cyan), "note: ");
    fmt::print(fmt::fg(fmt::terminal_color::white), format.data(), std::forward<Args>(args)...);
    std::putchar('\n');

    Detail::print_source_hint(err);
}