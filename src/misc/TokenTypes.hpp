#pragma once

#include "Common.hpp"

#include <string_view>

enum class TokenType : u8
{
    LITERAL,            // 1234, "foo", 'x', 0.5f, 0xFFu

    PLUS_SIGN,          // +
    MINUS_SIGN,         // -
    ASTERISK,           // *
    FORWARD_SLASH,      // /
    MODULUS,            // %
    ASSIGN,             // =
    EXCLAMATION,        // !
    LOGICAL_AND,        // &&
    LOGICAL_OR,         // ||
    BITWISE_AND,        // &
    BITWISE_OR,         // |
    BITWISE_XOR,        // ^
    BITWISE_NOT,        // ~
    LSHIFT,             // <<
    RSHIFT,             // >>

    SET_ADD,            // +=
    SET_SUB,            // -=
    SET_MUL,            // *=
    SET_DIV,            // /=
    SET_MOD,            // %=
    SET_AND,            // &=
    SET_OR,             // |=
    SET_XOR,            // ^=
    SET_LSHIFT,         // <<=
    SET_RSHIFT,         // >>=

    LESS_THAN,          // <
    LEQUAL_TO,          // <=
    EQUAL_TO,           // ==
    NEQUAL_TO,          // !=
    GEQUAL_TO,          // >=
    GREATER_THAN,       // >

    IDENTIFIER,         // x, y, struct, fn

    SEMICOLON,          // ;
    COMMA,              // ,
    PERIOD,             // .
    COLON,              // :
    DOUBLE_COLON,       // ::
    RIGHT_ARROW,        // ->

    PAREN_OPEN,         // (
    PAREN_CLOSE,        // )
    SQUARE_OPEN,        // [
    SQUARE_CLOSE,       // ]
    BRACE_OPEN,         // {
    BRACE_CLOSE,        // }
};

namespace TokenTypes {
    std::string_view stringify(TokenType type) noexcept;
}