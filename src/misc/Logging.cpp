#include "Logging.hpp"

#include <cstring>
#include <mutex>

static int find_prev_endline(const SourceFile& file, int search_start) {
    for (int i = search_start; i >= 0; --i) {
        if (file.contents[i] == '\n') return i;
    }
    return -1;
}

static int find_next_endline(const SourceFile& file, int search_start) {
    for (size_t i = search_start; i < file.contents.size(); ++i) {
        if (file.contents[i] == '\r' || file.contents[i] == '\n') return i;
    }
    return file.contents.size();
}

/*static std::vector<std::string_view> word_wrap(std::string_view msg, int max_width) {
    std::vector<std::string_view> result;

    size_t word_start = 0;
    size_t last_split = 0;
    for (size_t i = 0; i < msg.size(); ++i) {
        if (msg[i] == ' ') word_start = i+1;

        if (i - last_split >= max_width) {
            // split at last word start instead of right here
            result.push_back(msg.substr(last_split, word_start-1));
            last_split = word_start;
        }
    }
    if (last_split != msg.size()-1) result.push_back(msg.substr(last_split, msg.size() - 1));

    return result;
}*/

void Detail::print_source_hint(ErrorInfo& err) {
    constexpr size_t BORDER = 5;
    constexpr size_t WIDTH = 80; // Don't print more than this of the code to console

    // Emulate GCC and Clang
    std::string line_str = std::to_string(err.where.line + 1);
    size_t padding = std::max(BORDER, line_str.size());

    char border_buf[10]; // doubt anybody is reaching 8-digit line numbers but hey, cheap
    std::memset(border_buf, ' ', sizeof(border_buf));
    border_buf[padding + 1] = '|';
    border_buf[padding + 3] = '\0';
    
    // Tricky one first: the one with the line number
    for (size_t i = 0; i < padding - line_str.size(); ++i) std::putchar(' ');
    fmt::print(fmt::fg(fmt::terminal_color::white), line_str.data());
    fmt::print(fmt::fg(fmt::terminal_color::white), " | ");

    // Calculate which portion of the code to show. Surprisingly complex?!
    int line_start = find_prev_endline(*err.file, err.where.src_index) + 1;
    int line_end = find_next_endline(*err.file, err.where.src_index);

    int line_length = line_end - line_start;

    int code_start = err.where.column - WIDTH/2;
    int code_end = err.where.column + WIDTH/2;

    if (code_start < 0 && code_end >= line_length) {
        // Easy case: overflow on both ends, aka the line of code is short, so show everything
        code_start = 0;
        code_end = line_length;
    }
    else if (code_end >= line_length) {
        // End overflowed but start didn't. Shift the region to the left by the amount of overflow
        code_start = std::max(0, code_start - (code_end - line_length));
        code_end = line_length;
    }
    else if (code_start < 0) {
        // Start < 0, end is fine. Shift the region to the right by the amount of overflow
        code_start = 0;
        code_end = std::min(line_length, code_end + (0 - code_start));
    }

    // Shift both to actual indices to the file contents by adding the line start index:
    code_start += line_start;
    code_end += line_start;

    // and finally print:
    char buf[WIDTH+1];
    std::memcpy(buf, err.file->contents.data() + code_start, code_end - code_start);
    buf[code_end - code_start] = '\0'; // Null-terminate

    fmt::print(fmt::fg(fmt::terminal_color::white), "{}\n{}", buf, border_buf);

    // Print the squiggles and the ^ that points to the error. This needs a buffer:
    char squiggle_buf[WIDTH+1];
    std::memset(squiggle_buf, ' ', sizeof(squiggle_buf));
    squiggle_buf[err.where.column] = err.show_caret ? '^' : (err.underline ? '~' : ' ');
    squiggle_buf[err.where.column + 1] = '\0';

    fmt::print(fmt::fg(fmt::terminal_color::bright_red), squiggle_buf);

    if (err.where.length == 0) {
        err.where.length = line_end - line_start;
    }

    if (err.underline && err.where.length > 1) {
        for (int i = 0; i < err.where.length - 1; ++i) squiggle_buf[i] = '~';
        squiggle_buf[err.where.length] = '\0';

        fmt::print(fmt::fg(fmt::terminal_color::bright_red), squiggle_buf);
    }
    std::putchar('\n');
    std::putchar('\n');
}

namespace {
    std::mutex logging_mutex;
}

void log_issues(std::span<Issue> issues, const SourceFile& src) noexcept {
    std::lock_guard<std::mutex> lock_guard(logging_mutex);

    fmt::print("Found {} issues in file '{}'\n", issues.size(), src.filename_with_ext);

    ErrorInfo info;
    info.file = &src;

    for (Issue& issue : issues) {
        info.show_caret = issue.show_caret;
        info.underline = issue.underline;
        info.where = issue.where;

        if (!issue.function_name.empty()) {
            fmt::print("{}: in function '{}':\n", src.filename_with_ext, issue.function_name);
        }

        switch (issue.type) {
            case Issue::Type::ERROR:
                log_error(info, issue.message);
                break;

            case Issue::Type::WARNING:
                log_warning(info, issue.message);
                break;

            case Issue::Type::INFO:
                log_clarification(info, issue.message);
                break;
        }
    }
}