#pragma once

#include "Common.hpp"
#include "misc/SourceRef.hpp"
#include "misc/SourceFile.hpp"
#include "misc/StringPool.hpp"
#include "misc/TokenTypes.hpp"

#include <memory>
#include <vector>
#include <optional>

namespace Lexer {
    struct TokenData final
    {
        enum class Type : u8 { STRING, CHAR, INT, UINT, FLOAT, IDENTIFIER };

        union {
            const char *string_literal;
            const char *identifier;
            char char_literal;
            i64 int_literal;
            u64 uint_literal;
            f64 float_literal;
        };
        Type type;
    };

    struct LexResult final
    {
        std::vector<TokenType> token_types;
        std::vector<TokenData> token_datas;
        std::vector<SourceRef> source_refs;

        StringPool string_pool;
        StringMemoryPool identifiers;
    };

    std::optional<LexResult> lex(const SourceFile &file);
}