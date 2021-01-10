#pragma once

#include "lexing/Lexer.hpp"
#include "misc/SourceFile.hpp"

#include <optional>
#include <string_view>
#include <vector>
#include <memory>

#include "tsl/sparse_map.h"

namespace Parser {
    // Basically equivalent to std::string_view (used to be an alias),
    // but is trivially constructible. Maybe unions weren't a great idea?
    struct Identifier final {
        const char *string;
        size_t length;

        std::string_view str_view() const noexcept {
            return std::string_view(string, length);
        }

        Identifier &operator=(std::string_view rhs) noexcept {
            string = rhs.data();
            length = rhs.size();
            return *this;
        }

        friend bool operator==(const Identifier lhs, const Identifier rhs) noexcept {
            return lhs.str_view() == rhs.str_view();
        }
    };

    enum class BinaryOp : u8 {
        ADD, SUB, MUL, DIV, MOD,
        AND, OR,
        RSHIFT, LSHIFT, BIT_AND, BIT_OR, BIT_XOR,
        LTHAN, LEQUAL_TO, EQUAL_TO, NEQUAL_TO, GEQUAL_TO, GTHAN
    };

    enum class UnaryOp : u8 {
        NEGATE, // -x
        NOT,    // !x
        BIT_NOT // ~x
    };

    struct Literal final {
        enum class Type : u8 {
            STRING, CHAR, UINT, INT, FLOAT
        };
        Type type;

        union {
            const char *string_literal;
            char char_literal;
            i64 int_literal;
            u64 uint_literal;
            f32 float_literal;
        };
    };

    // Used in function parameters and struct members:
    // fn my_func(var_1: [String], var_2: mut String) {}
    // struct MyStruct { pub var: int }
    struct Type final
    {
        enum Flags { ARRAY = 0x1, MUTABLE = 0x4 };

        Identifier type_name;
        u8 flags;
    };

    struct Expression final {
        enum class Type : u8 {
            EMPTY, LITERAL, VAR_REF, MEMBER_VAR_REF, FUNC_CALL, MEMBER_FUNC_CALL, BINARY_OP, UNARY_OP
        };

        union {
            Literal literal;

            Identifier var_ref;

            struct {
                Expression *variable; // Expression to allow f.e `get_foo()->var`
                Identifier member_name;
            } member_var_ref;

            struct {
                Identifier fn_name;
                size_t num_params;
                Expression *array_of_params;
            } func_call;

            struct {
                Expression *variable;
                Identifier fn_name;
                size_t num_params;
                Expression *array_of_params;
            } member_func_call; // also known as 'method call'

            struct {
                Expression *lhs; // Non-owning, points to AST::Storage::expressions
                Expression *rhs;
                BinaryOp op;
            } binary_op;

            struct {
                Expression *value;
                UnaryOp op;
            } unary_op;
        };
        Type type;
    };

    struct VarDecl final {
        Identifier name;
        Expression* value;
    };

    struct Statement; // forward decl
    // Technically also a statement, but I don't want to
    // accept any statement in place of code block
    struct CodeBlock final {
        size_t num_statements; // can be 0
        Statement* statements;
    };

    struct Statement final {
        enum class Type : u8 {
            IF_STATEMENT, VAR_DECL, ASSIGNMENT, FUNC_CALL, LOOP, CODE_BLOCK,
            RETURN,
        };
        union {
            VarDecl var_decl;

            Expression* func_call;

            struct {
                Expression* variable;
                Expression* value;
            } assignment;

            struct {
                Statement* code;
                Statement* else_statement_code; // null if no else block
                Expression* condition;
            } if_statement;

            CodeBlock loop;

            CodeBlock code_block;

            Expression* return_statement; // null if no value is returned
        };
        Type type;
    };

    struct FuncDecl final {
        struct Param final {
            Identifier name;
            Identifier type_name;
        };

        Identifier name;
        std::vector<Param> params;
        Identifier return_type_name;
        CodeBlock code_block;

        SourceRef where;
    };

    struct StructDecl final {

    };
}

// Hash needs to be declared before it's instantiated, so..
namespace std {
    template<>
    struct hash<Parser::Identifier>
    {
        // Hash it as std::string_view
        size_t operator()(const Parser::Identifier& ident) const {
            std::string_view str(ident.string, ident.length);
            return std::hash<string_view>{}(str);
        }
    };
}

namespace Parser {
    struct AST final
    {
        // Here just so that structs like Expression can store pointers
        // to other structs without non-trivial types or manual memory management
        struct Storage final
        {
            // Linked lists: first item in pair is the data, second is pointer to the next
            std::vector<Expression> expressions;
            std::vector<Statement> statements;
        } storage;

        tsl::sparse_map<Identifier, FuncDecl> functions;
        tsl::sparse_map<Identifier, StructDecl> structs;
    };

    std::optional<AST> parse(const SourceFile& src, Lexer::LexResult&);
}
