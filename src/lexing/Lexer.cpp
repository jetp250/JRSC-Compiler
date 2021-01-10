#include "Lexer.hpp"

#include "misc/Logging.hpp"

#include <filesystem>
#include <string>

#include <cstdio>
#include <cstring>

using namespace Lexer;

static bool lut_initialized = false;
static std::pair<char, TokenType> special_char_lut[256];

static unsigned char lut_key_a(char c) {
    return c;
}

static unsigned char lut_key_b(char a_key, char b) {
    return a_key ^ (b << 4);
}

static unsigned char lut_key_c(char ab_key, char c) {
    return ab_key + c;
}

static void add_lut_entry(char key_a, TokenType type) {
    unsigned char key = lut_key_a(key_a);
    if (special_char_lut[key].first != 0) {
        ice_abort(1, "add_lut_entry: duplicate entry for %c and %c", key, special_char_lut[key].first);
    }

    special_char_lut[key] = std::make_pair(key, type);
}

static void add_lut_entry(char key_a, char key_b, TokenType type) {
    unsigned char key = lut_key_b(lut_key_a(key_a), key_b);
    if (special_char_lut[key].first != 0) {
        ice_abort(1, "add_lut_entry 2: duplicate entry for (%c%c) and %c", key_a, key_b, special_char_lut[key].first);
    }

    special_char_lut[key] = std::make_pair(key_b, type);
}

static void add_lut_entry(char key_a, char key_b, char key_c, TokenType type) {
    unsigned char key = lut_key_c(lut_key_b(lut_key_a(key_a), key_b), key_c);
    if (special_char_lut[key].first != 0) {
        ice_abort(1, "add_lut_entry 3: duplicate entry for (%c%c%c) and %c", key_a, key_b, key_c, special_char_lut[key].first);
    }

    special_char_lut[key] = std::make_pair(key_c, type);
}

static void init_lut() {
    for (int i = 0; i < 256; ++i) special_char_lut[i] = std::make_pair('\0', (TokenType)0);

    add_lut_entry('.', TokenType::PERIOD);
    add_lut_entry(':', TokenType::COLON);
    add_lut_entry(';', TokenType::SEMICOLON);
    add_lut_entry(',', TokenType::COMMA);
    add_lut_entry('[', TokenType::SQUARE_OPEN);
    add_lut_entry(']', TokenType::SQUARE_CLOSE);
    add_lut_entry('(', TokenType::PAREN_OPEN);
    add_lut_entry(')', TokenType::PAREN_CLOSE);
    add_lut_entry('{', TokenType::BRACE_OPEN);
    add_lut_entry('}', TokenType::BRACE_CLOSE);

    add_lut_entry(':', ':', TokenType::DOUBLE_COLON);

    add_lut_entry('+', TokenType::PLUS_SIGN);
    add_lut_entry('-', TokenType::MINUS_SIGN);
    add_lut_entry('*', TokenType::ASTERISK);
    add_lut_entry('/', TokenType::FORWARD_SLASH);
    add_lut_entry('%', TokenType::MODULUS);
    add_lut_entry('=', TokenType::ASSIGN);
    add_lut_entry('!', TokenType::EXCLAMATION);
    add_lut_entry('&', TokenType::BITWISE_AND);
    add_lut_entry('|', TokenType::BITWISE_OR);
    add_lut_entry('^', TokenType::BITWISE_XOR);
    add_lut_entry('~', TokenType::BITWISE_NOT);

    add_lut_entry('+', '=', TokenType::SET_ADD);
    add_lut_entry('-', '=', TokenType::SET_SUB);
    add_lut_entry('*', '=', TokenType::SET_MUL);
    add_lut_entry('/', '=', TokenType::SET_DIV);
    add_lut_entry('%', '=', TokenType::SET_MOD);
    add_lut_entry('&', '=', TokenType::SET_AND);
    add_lut_entry('|', '=', TokenType::SET_OR);
    add_lut_entry('^', '=', TokenType::SET_XOR);

    add_lut_entry('>', '>',  TokenType::RSHIFT);
    add_lut_entry('>', '>', '=', TokenType::SET_RSHIFT);
    add_lut_entry('<', '<',  TokenType::LSHIFT);
    add_lut_entry('<', '<', '=',  TokenType::SET_LSHIFT);

    add_lut_entry('&', '&',  TokenType::LOGICAL_AND);
    add_lut_entry('|', '|',  TokenType::LOGICAL_OR);

    add_lut_entry('<', TokenType::LESS_THAN);
    add_lut_entry('>', TokenType::GREATER_THAN);
    add_lut_entry('<', '=', TokenType::LEQUAL_TO);
    add_lut_entry('>', '=', TokenType::GEQUAL_TO);
    add_lut_entry('=', '=', TokenType::EQUAL_TO);
    add_lut_entry('!', '=', TokenType::NEQUAL_TO);

    add_lut_entry('-', '>', TokenType::RIGHT_ARROW);
}

static bool is_valid_in_identifier(char c) {
    if ('a' <= c && c <= 'z') return true;
    if ('A' <= c && c <= 'Z') return true;
    if ('0' <= c && c <= '9') return true;
    
    return c == '_';
}

static bool is_number(char c) {
    return '0' <= c && c <= '9';
}

std::optional<LexResult> Lexer::lex(const SourceFile& file) {
    if (!lut_initialized) {
        lut_initialized = true;
        init_lut();
    }

    // test_func("\"text\":\"A 'weird' string\"", '\n');
    char const* src = file.contents.data();
    const int src_len = (int)file.contents.size();

    printf("Lexing file \"%s\"! Source length: %d\n", file.filename_with_ext.c_str(), src_len);
    printf("File contents: \n\"\n%s\n\"\n\n", file.contents.c_str());

    ErrorInfo err_info = {};
    err_info.file = &file;

    std::vector<TokenType> token_types;
    std::vector<TokenData> token_datas;
    std::vector<SourceRef> source_refs;

    StringPool string_pool;
    StringMemoryPool identifiers;

    int line_number = 0;
    int line_start = 0;
    int error_count = 0;

    for (int i = 0; i < src_len; ++i) {
        const char cp = src[i];

        if (cp == '\r' || cp == '\t' || cp == ' ') continue;

        if (cp == '\n') {
            line_number += 1;
            line_start = i + 1;
            continue;
        }

        if (cp == '/' && src[i+1] == '*') {
            for (int j = i + 3; j < src_len; ++j) {
                if (src[j] == '\n') {
                    line_number += 1;
                    line_start = j + 1;
                    continue;
                }

                if (src[j - 1] == '*' && src[j] == '/') {
                    // Found the closing pair
                    i = j + 1;
                    break;
                }
            }
            continue;
        }

        if (cp == '/' && src[i+1] == '/') {
            // Found a comment like this one. Skip to the next line
            for (int j = i + 1; j < src_len; ++j) {
                if (src[j] == '\n') {
                    line_number += 1;
                    line_start = j + 1;
                    i = line_start;
                    break;
                }
            }
            continue;
        }

        if (cp == '"') {
            // Found string
            for (int j = i + 1; j < src_len; ++j) {
                if (src[j] == '\n') {
                    int column = j - 1 - line_start;
                    err_info.where = SourceRef {line_number, column, j - 1, j - 1 - i };
                    log_error(err_info, "missing terminating \" character");

                    error_count += 1;

                    // attempt to recover by boldly skipping to the next line
                    line_number += 1;
                    line_start = j + 1;
                    i = line_start;
                    break;
                }

                if (src[j] == '"' && src[j-1] != '\\') {
                    // Found end of string. Copy string to output buffer:
                    std::string_view literal(src + i + 1, j - i - 1);
                    const char* pooled = string_pool.get_pooled(literal);

                    token_types.push_back(TokenType::LITERAL);
                    token_datas.push_back(TokenData{ .string_literal = pooled, .type = TokenData::Type::STRING });
                    
                    source_refs.push_back(SourceRef{ .line = line_number, .column = i - line_start, .src_index = i, .length = j - i + 1 });

                    //printf("Found string: \t\t\"%s\"\n", data.data() + data_pos - 1 - (j-i));

                    i = j;
                    break;
                }
            }
            continue;
        }

        if (cp == '\'') {
            // Found char
            // TODO handle UTF
            if ((i + 2 >= src_len-1) || (src[i+1] == '\n') || (src[i+2] != '\'')) { // Require at least two more chars
                int column = i + 2 - line_start;
                err_info.where = SourceRef {line_number, column, i + 2, 2 };
                log_error(err_info, "missing terminating ' character");

                error_count += 1;
                
                // Attempt to recover by assuming it was correct
            }

            token_types.push_back(TokenType::LITERAL);
            token_datas.push_back(TokenData{ .char_literal = src[i+1], .type = TokenData::Type::CHAR });

            source_refs.push_back(SourceRef{ .line = line_number, .column = i - line_start, .src_index = i, .length = 3 });

            //printf("Found char literal: '%c'\n", src[i+1]);

            i += 2; // Skip the char and the closing quote
            continue;
        }

        if (is_number(cp)) {
            u64 parsed = cp - '0';
            int first_non_number = 0;
            // This loop always succeeds because a newline is inserted at the end of each file in SourceFile::load()
            for (int j = i + 1; j < src_len; ++j) {
                // Allow underscore only if there aren't multiple in a row
                if (!is_number(src[j]) && (src[j] != '_' || (src[j-1] == '_' || src[j+1] == '_'))) {
                    first_non_number = j;
                    break;
                }

                parsed *= 10;
                parsed += src[j] - '0';
            }

            char non_digit = src[first_non_number];

            if (non_digit == '.') {
                // Float
                u64 decimals = 0;
                u64 divisor = 1;
                for (int j = first_non_number + 1; j < (int) src_len; ++j) {
                    if (!is_number(src[j])) {
                        first_non_number = j;
                        break; 
                    }
            
                    decimals *= 10;
                    divisor *= 10;
                    decimals += src[j] - '0';
                }

                //if (src[first_non_number] == 'f' || src[first_non_number] == 'F') {
                    //first_non_number += 1; // It *was* part of the number.

                f64 literal = parsed + decimals / (f64)divisor;

                token_types.push_back(TokenType::LITERAL);
                token_datas.push_back(TokenData{ .float_literal = literal, .type = TokenData::Type::FLOAT });
                    //printf("Found float literal: \t%f\n", literal);
                /*}
                else {
                    f64 literal = parsed + decimals / (f64)divisor;

                    token_types.push_back(TokenType::LITERAL);
                    token_datas.push_back(TokenData{ .double_literal = literal, .type = TokenData::Type::DOUBLE });

                    //printf("Found double literal: \t%f\n", literal);
                }*/

                source_refs.push_back(SourceRef{ .line = line_number, .column = i - line_start, .src_index = i, .length = first_non_number - i });
                i = first_non_number - 1;
                continue; // Continue the main loop
            } 
            
            if (first_non_number == i + 1) { // if only one digit was valid...
                if (non_digit == 'x') {
                    parsed = 0;
                    // Hex int
                    for (int j = first_non_number + 1; j < src_len; ++j) {
                        const char next = src[j];
                        if ('a' <= next && next <= 'f') {
                            parsed <<= 4;
                            parsed |= next - 'a' + 10;
                        } 
                        else if ('A' <= next && next <= 'F') {
                            parsed <<= 4;
                            parsed |= next - 'A' + 10;
                        } 
                        else if ('0' <= next && next <= '9') {
                            parsed <<= 4;
                            parsed |= next - '0';
                        }
                        else if (src[j] != '_' || (src[j-1] == '_' || src[j+1] == '_')) {
                            first_non_number = j;
                            break;
                        }
                    }
                
                } 
                else if (non_digit == 'b') {
                    parsed = 0;
                    // Binary int
                    for (int j = first_non_number + 1; j < src_len; ++j) {
                        const char next = src[j];
                        if (next == '1') {
                            parsed <<= 1;
                            parsed |= 1;
                        }
                        else if (next == '0') {
                            parsed <<= 1;
                        } 
                        else if (src[j] != '_' || (src[j-1] == '_' || src[j+1] == '_')) {
                            first_non_number = j;
                            break;
                        }
                    }
                } 
                else if (non_digit == 'o') {
                    parsed = 0;
                    // Octal int
                    printf("Octal NOT IMPLEMENTED\n");
                }

                non_digit = src[first_non_number];
            }

            if (non_digit == 'u' || non_digit == 'U') {
                token_types.push_back(TokenType::LITERAL);
                token_datas.push_back(TokenData{ .uint_literal = parsed, .type = TokenData::Type::UINT });
                first_non_number += 1; // skip the suffix

                //printf("Found uint literal: \t%lldu\n", parsed);
            } else {
                token_types.push_back(TokenType::LITERAL);
                token_datas.push_back(TokenData{ .int_literal = (i64)parsed, .type = TokenData::Type::INT });
                //printf("Found int literal: \t%lld\n", parsed);
            }

            source_refs.push_back(SourceRef{ .line = line_number, .column = i - line_start, .src_index = i, .length = first_non_number - i });
            i = first_non_number - 1;
            continue; // Continue the main loop
        }

        // the lut system is basically a perfect hash map:
        // none of the entries have hash collisions with each other.
        // However, random characters like cp here are likely to collide
        // (as in, find an entry even though the original key was different)
        // so on each find the keys must be compared to make sure they match.
        // That's what 'lookup_a.first == cp' is for.

        unsigned char a_key = lut_key_a(cp);
        auto& lookup_a = special_char_lut[a_key];

        if (lookup_a.first == cp) {
            unsigned char b_key = lut_key_b(a_key, src[i+1]);
            auto& lookup_b = special_char_lut[b_key];
        
            if (i+1 < src_len && lookup_b.first == src[i+1]) {
                unsigned char c_key = lut_key_c(b_key, src[i+2]);
                auto& lookup_c = special_char_lut[c_key];

                if (i+2 < src_len && lookup_c.first == src[i+2]) {
                    // Operator length is 3
                    //printf("Found symbol: \t\t%c%c%c\n", cp, src[i+1], src[i+2]);
                    token_types.push_back(lookup_c.second);
                    source_refs.push_back(SourceRef{ .line = line_number, .column = i - line_start, .src_index = i, .length = 3 });
                    i += 2;
                }
                else {
                    // Operator length is 2
                    //printf("Found symbol: \t\t%c%c\n", cp, src[i+1]);
                    token_types.push_back(lookup_b.second);
                    source_refs.push_back(SourceRef{ .line = line_number, .column = i - line_start, .src_index = i, .length = 2 });
                    i += 1;
                }
            }
            else {
                // Operator length is 1
                //printf("Found symbol: \t\t%c\n", cp);
                token_types.push_back(lookup_a.second);
                source_refs.push_back(SourceRef{ .line = line_number, .column = i - line_start, .src_index = i, .length = 1 });
            }
            continue;
        }

        // Has to be an identifier if none of the above matched
        if (is_valid_in_identifier(cp)) {
            int first_invalid = i;
            for (int j = i; j < src_len; ++j) {
                if (!is_valid_in_identifier(src[j])) {
                    first_invalid = j;
                    break;
                }
            }

            std::string_view identifier(src + i, first_invalid - i);

            // store() returns null-terminated view to persistent storage
            std::string_view stored = identifiers.store(identifier); 

            token_types.push_back(TokenType::IDENTIFIER);
            token_datas.push_back(TokenData{ .identifier = stored.data(), .type = TokenData::Type::IDENTIFIER });
            source_refs.push_back(SourceRef{ .line = line_number, .column = i - line_start, .src_index = i, .length = first_invalid - i });

            /*err_info.where = source_refs.back();
            err_info.show_caret = false;
            log_warning(err_info, "Test");*/

            //printf("Found identifier: \t%s\n", data.data() + data_pos - 1 - (first_invalid - i));
            
            i = first_invalid - 1;
            continue;
        }
    }

    // Don't continue to parsing if there were errors
    if (error_count != 0) {
        printf("Found %d errors, aborting.\n", error_count);
        return std::nullopt;
    }

    token_types.shrink_to_fit();
    token_datas.shrink_to_fit();
    source_refs.shrink_to_fit();

    LexResult result;
    result.token_types = std::move(token_types);
    result.token_datas = std::move(token_datas);
    result.source_refs = std::move(source_refs);
    result.string_pool = std::move(string_pool);
    result.identifiers = std::move(identifiers);
    return result;
}