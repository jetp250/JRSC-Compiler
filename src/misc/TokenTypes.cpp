#include "TokenTypes.hpp"

std::string_view TokenTypes::stringify(TokenType type) noexcept {
    switch(type) {
        case TokenType::LITERAL: return "<literal>"sv;
        case TokenType::PLUS_SIGN: return "+"sv;
        case TokenType::MINUS_SIGN: return "-"sv;
        case TokenType::ASTERISK: return "*"sv;
        case TokenType::FORWARD_SLASH: return "/"sv;
        case TokenType::MODULUS: return "%"sv;
        case TokenType::ASSIGN: return "="sv;
        case TokenType::EXCLAMATION: return "!"sv;
        case TokenType::LOGICAL_AND: return "&&"sv;
        case TokenType::LOGICAL_OR: return "||"sv;
        case TokenType::BITWISE_AND: return "&"sv;
        case TokenType::BITWISE_OR: return "|"sv;
        case TokenType::BITWISE_XOR: return "^"sv;
        case TokenType::BITWISE_NOT: return "~"sv;
        case TokenType::LSHIFT: return "<<"sv;
        case TokenType::RSHIFT: return ">>"sv;
        case TokenType::SET_ADD: return "+="sv;
        case TokenType::SET_SUB: return "-="sv;
        case TokenType::SET_MUL: return "*="sv;
        case TokenType::SET_DIV: return "/="sv;
        case TokenType::SET_MOD: return "%="sv;
        case TokenType::SET_AND: return "&="sv;
        case TokenType::SET_OR: return "|="sv;
        case TokenType::SET_XOR: return "^="sv;
        case TokenType::SET_LSHIFT: return "<<="sv;
        case TokenType::SET_RSHIFT: return ">>="sv;
        case TokenType::LESS_THAN: return "<"sv;
        case TokenType::LEQUAL_TO: return "<="sv;
        case TokenType::EQUAL_TO: return "=="sv;
        case TokenType::NEQUAL_TO: return "!="sv;
        case TokenType::GEQUAL_TO: return ">="sv;
        case TokenType::GREATER_THAN: return ">";
        case TokenType::IDENTIFIER: return "<Identifier>"sv;
        case TokenType::SEMICOLON: return ";"sv;
        case TokenType::COMMA: return ","sv;
        case TokenType::PERIOD: return "."sv;
        case TokenType::COLON: return ":"sv;
        case TokenType::DOUBLE_COLON: return "::"sv;
        case TokenType::RIGHT_ARROW: return "->"sv;
        case TokenType::PAREN_OPEN: return "("sv;
        case TokenType::PAREN_CLOSE: return ")"sv;
        case TokenType::SQUARE_OPEN: return "["sv;
        case TokenType::SQUARE_CLOSE: return "]"sv;
        case TokenType::BRACE_OPEN: return "{"sv;
        case TokenType::BRACE_CLOSE: return "}"sv;
        default: return "<unknown>"sv; // UB case
    }
}