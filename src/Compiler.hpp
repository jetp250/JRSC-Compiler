#pragma once

// Main method is in Main.cpp.

// This is where the actual compilation happens once command-line
// arguments have been parsed in Main.cpp.

// The compiler only takes in the file with the main method,
// based on which it attempts to find and parse the imported files.

#include "misc/SourceFile.hpp"

namespace Compiler {
    /// Returns 0 on success and error code otherwise.
    int compile(const SourceFile &file);
}