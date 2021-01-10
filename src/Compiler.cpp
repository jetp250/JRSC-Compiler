#include "Compiler.hpp"

#include "lexing/Lexer.hpp"
#include "parsing/Parser.hpp"

#include <fmt/core.h>

#include <thread>

int Compiler::compile(const SourceFile& src) {
    std::optional<Lexer::LexResult> opt_lexed = Lexer::lex(src);
    if (!opt_lexed) {
        fmt::print("Failed to lex sourcecode");
        return 1;
    }

    Parser::parse(src, *opt_lexed);

    return 0;
}