#pragma once

struct SourceRef final
{
    // All zero-indexed
    int line;       // Line number
    int column;     // Column number
    int src_index;  // index of the char in the entire file
    int length;     // Length of the token, starting from src_index

    SourceRef whole_line() const noexcept {
        return SourceRef { line, 0, src_index - column, 0 };
    }
};