#include "Parser.hpp"

#include "misc/Logging.hpp"

#include <algorithm>

using namespace Lexer; // Brings in LexResult, TokenType, TokenData
using namespace Parser;

static bool strequals(const char* a, const char* b) noexcept {
    while(*a && *b) {
        if (*a != *b) return false;

        a++;
        b++;
    }
    return true;
}

static int scope = 0;

static char _scope_buf[128]{};

template<typename ... Args>
static void log(std::string_view format, Args&&... args) {
    std::memset(_scope_buf, ' ', 4 * scope);
    _scope_buf[4 * scope] = 0;

    printf(_scope_buf);
    fmt::print(format, std::forward<Args>(args)...);

    fflush(stdout);
}

template<typename ... Args>
static void start_scope(std::string_view format, Args&&... args) {
    log(format, std::forward<Args>(args)...);
    scope += 1;
}

template<typename ... Args>
static void end_scope(std::string_view format, Args&&... args) {
    scope -= 1;
    log(format, std::forward<Args>(args)...);
}

static std::string to_string(Parser::BinaryOp op) {
    switch(op) {
        case BinaryOp::ADD: return "+";
        case BinaryOp::SUB: return "-";
        case BinaryOp::MUL: return "*";
        case BinaryOp::DIV: return "/";
        case BinaryOp::MOD: return "%";
        case BinaryOp::AND: return "&&";
        case BinaryOp::OR: return "||";
        case BinaryOp::RSHIFT: return ">>";
        case BinaryOp::LSHIFT: return "<<";
        case BinaryOp::BIT_AND: return "&";
        case BinaryOp::BIT_OR: return "|";
        case BinaryOp::BIT_XOR: return "^";
        case BinaryOp::LTHAN: return "<";
        case BinaryOp::LEQUAL_TO: return "<=";
        case BinaryOp::EQUAL_TO: return "==";
        case BinaryOp::NEQUAL_TO: return "!=";
        case BinaryOp::GEQUAL_TO: return ">=";
        case BinaryOp::GTHAN: return ">";
    }
    return "<invalid op>";
}

static std::string to_string(const Parser::Expression& expression) {
    fflush(stdout);
    switch(expression.type) {
        case Expression::Type::EMPTY: return "()";
        case Expression::Type::LITERAL:
            switch(expression.literal.type) {
                case Literal::Type::STRING: return "\"" + std::string(expression.literal.string_literal) + "\"";
                case Literal::Type::CHAR: return "'" + std::string(1, expression.literal.char_literal) + "'";
                case Literal::Type::UINT: return std::to_string(expression.literal.uint_literal) + "u";
                case Literal::Type::INT: return std::to_string(expression.literal.int_literal);
                case Literal::Type::FLOAT: return std::to_string(expression.literal.float_literal) + "f";
            }
            return "<invalid literal>";

        case Expression::Type::VAR_REF: return std::string(expression.var_ref.str_view());
        case Expression::Type::MEMBER_VAR_REF: return to_string(*expression.member_var_ref.variable) + "->"
                                                      + std::string(expression.member_var_ref.member_name.str_view());
        case Expression::Type::FUNC_CALL: {
            std::string fcres = std::string(expression.func_call.fn_name.str_view());
            fcres += "(";
            for (size_t i = 0; i < expression.func_call.num_params; ++i) {
                fcres += to_string(expression.func_call.array_of_params[i]);
                if (i != expression.func_call.num_params - 1) fcres += ", ";
            }
            fcres += ")";
            return fcres; }

        case Expression::Type::MEMBER_FUNC_CALL: {
            std::string mfcres = to_string(*expression.member_func_call.variable) + "->";
            mfcres += std::string(expression.member_func_call.fn_name.str_view());
            mfcres += "(";
            for (size_t i = 0; i < expression.member_func_call.num_params; ++i) {
                mfcres += to_string(expression.member_func_call.array_of_params[i]);
                if (i != expression.member_func_call.num_params - 1) mfcres += ", ";
            }
            mfcres += ")";
            return mfcres; }

        case Expression::Type::BINARY_OP: {
            std::string bopres;
            if (expression.binary_op.lhs->type == Expression::Type::BINARY_OP) {
                bopres += "(" + to_string(*expression.binary_op.lhs) + ")";
            } else bopres += to_string(*expression.binary_op.lhs);

            bopres += " ";
            bopres += to_string(expression.binary_op.op);
            bopres += " ";

            if (expression.binary_op.rhs->type == Expression::Type::BINARY_OP) {
                bopres += "(" + to_string(*expression.binary_op.rhs) + ")";
            } else bopres += to_string(*expression.binary_op.rhs);

            return bopres;
        }

        case Expression::Type::UNARY_OP: {
            UnaryOp op = expression.unary_op.op;
            return (op == UnaryOp::NOT ? "!" : op == UnaryOp::NEGATE ? "-" : "~") + to_string(*expression.unary_op.value);
        }
    }
    return "<invalid expression>";
}

static std::string to_string(const Parser::CodeBlock& code);

static std::string to_string(const Parser::Statement& stmt) {
    switch(stmt.type) {
        case Statement::Type::IF_STATEMENT: {
            std::string ires = "if ";

            ires += to_string(*stmt.if_statement.condition);
            ires += " ";
            if (stmt.if_statement.code->type != Statement::Type::CODE_BLOCK) ires += "{ ";
            ires += to_string(*stmt.if_statement.code);
            if (stmt.if_statement.code->type != Statement::Type::CODE_BLOCK) ires += " }";

            if (stmt.if_statement.else_statement_code != nullptr) {
                ires += " else ";
                if (stmt.if_statement.else_statement_code->type != Statement::Type::CODE_BLOCK) ires += "{ ";
                ires += to_string(*stmt.if_statement.else_statement_code);
                if (stmt.if_statement.else_statement_code->type != Statement::Type::CODE_BLOCK) ires += " }";
            }
            return ires;
        }

        case Statement::Type::VAR_DECL: {
            std::string vdres = "let ";
            vdres += stmt.var_decl.name.str_view();
            if (!stmt.var_decl.value) return vdres + ";";

            vdres += " = ";
            vdres += to_string(*stmt.var_decl.value);
            vdres += ";";
            return vdres;
        }

        case Statement::Type::ASSIGNMENT: {
            std::string ares = to_string(*stmt.assignment.variable);
            ares += " = ";
            ares += to_string(*stmt.assignment.value);
            ares += ";";
            return ares;
        }

        case Statement::Type::FUNC_CALL:
            return to_string(*stmt.func_call) + ";";

        case Statement::Type::RETURN: {
            std::string retres = "return";
            if (stmt.return_statement != nullptr) retres += " " + to_string(*stmt.return_statement);
            return retres + ";";
        }

        case Statement::Type::LOOP: {
            return "loop " + to_string(stmt.loop);
        }
        case Statement::Type::CODE_BLOCK:
            return to_string(stmt.code_block);
    }
    return "<invalid statement>";
}

static std::string to_string(const Parser::CodeBlock& code) {
    std::string result;
    result += "{\n";
    for (size_t i = 0; i < code.num_statements; ++i) {
        Statement& stmt = code.statements[i];
        result += "   ";
        result += to_string(stmt);
        result += "\n";
    }
    result += "}\n";
    return result;
}

static std::string to_string(const Parser::FuncDecl& fn) {
    std::string result = "fn " + std::string(fn.name.str_view()) + "(";
    for (size_t i = 0; i < fn.params.size(); ++i) {
        result += fn.params[i].name.str_view();
        result += ": ";
        result += fn.params[i].type_name.str_view();

        if (i != fn.params.size()-1) result += ", ";
    }
    result += ") ";

    if (fn.return_type_name.length != 0) {
        result += "-> ";
        result += fn.return_type_name.str_view();
        result += " ";
    }

    result += to_string(fn.code_block);

    return result;
}

static int get_precedence(BinaryOp op) noexcept {
    switch(op) {
        case BinaryOp::MUL:
        case BinaryOp::DIV:
        case BinaryOp::MOD: return 9;

        case BinaryOp::ADD:
        case BinaryOp::SUB: return 8;

        case BinaryOp::RSHIFT:
        case BinaryOp::LSHIFT: return 7;

        case BinaryOp::LTHAN:
        case BinaryOp::LEQUAL_TO:
        case BinaryOp::GEQUAL_TO:
        case BinaryOp::GTHAN: return 6;

        case BinaryOp::EQUAL_TO:
        case BinaryOp::NEQUAL_TO: return 5;

        case BinaryOp::BIT_AND: return 4;
        case BinaryOp::BIT_XOR: return 3;
        case BinaryOp::BIT_OR: return 2;
        case BinaryOp::AND: return 1;
        case BinaryOp::OR: return 0;
    }
    ice_abort(1, "get_precedence(BinaryOp): invalid operator");
}

// Represents one active parsing process (for a single file).
// Multiple may be running in parallel.
// A parser simply takes in a token stream and
// converts it to an AST. AST->IR conversion
// is done elsewhere.
class ParserUnit final
{
public:

    ParserUnit(const SourceFile& src, LexResult& lex) noexcept
        : index(0),
        num_tokens(lex.token_types.size()),
        tokens(lex.token_types.data()),
        datas(lex.token_datas.data()),
        source_refs(lex.source_refs.data()),
        src(src),
        current_function_name(Identifier{})
        {
            expressions.reserve(num_tokens); // a single token is enough for an expression: `1`
            statements.reserve(num_tokens / 3) ;// 3 is the minimum number of tokens needed for a statement: `x = 5`
        }

    std::optional<AST> parse() {
        //fmt::print("sizeof(Expression) = {}. sizeof(Statement) = {}\n", sizeof(Parser::Expression), sizeof(Parser::Statement));

        start_scope("parse_imports start\n");
        parse_imports(); // Kicks off lexing & parsing of found imports in parallel
        end_scope("parse_imports end\n");

        log("Next token: {}\n", TokenTypes::stringify(peek_token()));

        while(true) {
            if (consume_identifier("fn")) {
                parse_function();
            }
            else if (consume_identifier("struct")) {
                parse_struct();
            }
            else {
                break;
            }
        }

        if (!issues.empty()) {
            log_issues(issues, src);
            return {};
        }

        AST ast;
        ast.storage.expressions = std::move(expressions);
        ast.storage.statements = std::move(statements);
        ast.functions = std::move(functions);
        ast.structs = std::move(structs);

        printf("Done parsing\n");
        return ast;
    }

private:
    void parse_function() noexcept {
        start_scope("Parsing function\n");

        FuncDecl fn;
        expect_identifier(fn.name);

        log("Expecting (");

        expect(TokenType::PAREN_OPEN);

        log("Found )");

        // Even if the user forgets the whole parameter pack and the return value (which isn't always there though)
        // (e.g. 'fn func { println("hi"); }')
        // try to recover by looking for next expected tokens
        start_scope("Parsing parameters\n");
        while (!consume_token(TokenType::PAREN_CLOSE) && !consume_token(TokenType::RIGHT_ARROW) &&
               !consume_token(TokenType::BRACE_OPEN)) {
            start_scope("Parsing parameter\n");
            Identifier param_name{};
            expect_identifier(param_name);

            expect(TokenType::COLON);

            Identifier type_name{};
            expect_identifier(type_name);

            fn.params.push_back(FuncDecl::Param{param_name, type_name});

            consume_token(TokenType::COMMA);
            end_scope("Done parsing parameter\n");
        }
        end_scope("Done parsing parameters\n");

        if (consume_token(TokenType::RIGHT_ARROW)) { // Optional
            expect_identifier(fn.return_type_name);
        }

        fn.code_block = parse_code_block();

        functions.insert({ fn.name, fn });
        log("Function parsed\n");
        fflush(stdout);

        current_function_name = Identifier{};

        end_scope("Done parsing function: \n\n{}\n", to_string(fn));
    }

    CodeBlock parse_code_block() noexcept {
        start_scope("Parsing code block\n");

        std::vector<Statement> statements;
        statements.reserve(8); // Totally arbitrary. Should be over the 256 byte allocation threshold though

        expect(TokenType::BRACE_OPEN);
        while(!consume_token(TokenType::BRACE_CLOSE)) {
            statements.push_back(parse_statement());
        }

        CodeBlock code_block{};
        code_block.statements = &(this->statements.back()) + 1;
        code_block.num_statements = statements.size();

        for (Statement& statement : statements) {
            push_statement(statement);
        }

        end_scope("Done parsing code block\n");
        return code_block;
    }

    Statement parse_statement() noexcept {
        start_scope("Parsing statement. Next token: {}\n", TokenTypes::stringify(peek_token()));

        Statement statement{};
        if (peek_token() == TokenType::BRACE_OPEN) {
            statement.type = Statement::Type::CODE_BLOCK;
            statement.code_block = parse_code_block();
        }
        if (consume_identifier("let")) {
            statement.type = Statement::Type::VAR_DECL;
            statement.var_decl = parse_var_decl();
        }
        else if (consume_identifier("if")) {
            statement.type = Statement::Type::IF_STATEMENT;
            statement.if_statement.condition = push_expression(parse_expression());
            statement.if_statement.code = push_statement(parse_statement());
            statement.if_statement.else_statement_code = nullptr;

            if (consume_identifier("else")) {
                statement.if_statement.else_statement_code = push_statement(parse_statement());
            }
        }
        else if (consume_identifier("loop")) {
            statement.type = Statement::Type::LOOP;
            statement.loop = parse_code_block();
        }
        else if (consume_identifier("return")) {
            statement.type = Statement::Type::RETURN;
            statement.return_statement = nullptr;
            if (!consume_token(TokenType::SEMICOLON)) {
                statement.return_statement = push_expression(parse_expression());
                expect(TokenType::SEMICOLON);
            }
        }
        else {
            // If nothing else matches, try an expression.
            // Could be `x = ..`, `foo->bar = baz`, `foo(bar, baz)`, ...
            Expression left = parse_expression(); // 'left' as in left hand side
            if (left.type == Expression::Type::VAR_REF || left.type == Expression::Type::MEMBER_VAR_REF) {
                statement.type = Statement::Type::ASSIGNMENT;
                statement.assignment.variable = push_expression(left);

                if (consume_token(TokenType::ASSIGN)) {
                    statement.assignment.value = push_expression(parse_expression());
                }
                else if (BinaryOp op; consume_any_assignment_bin_op(op)) {
                    // Convert `x += 5` to `x = x + 5`
                    Expression rhs{};
                    rhs.type = Expression::Type::BINARY_OP;
                    rhs.binary_op.lhs = statement.assignment.variable;
                    rhs.binary_op.rhs = push_expression(parse_expression());
                    rhs.binary_op.op = op;

                    statement.assignment.value = push_expression(rhs);
                }
            }
            else if (left.type == Expression::Type::FUNC_CALL || left.type == Expression::Type::MEMBER_FUNC_CALL) {
                statement.type = Statement::Type::FUNC_CALL;
                statement.func_call = push_expression(left);
            }

            expect(TokenType::SEMICOLON);
        }

        end_scope("Done parsing statement: '{}'\n", to_string(statement));
        return statement;
    }

    // Variable declarations are statements starting with "let".
    VarDecl parse_var_decl() noexcept {
        start_scope("Parsing variable declaration\n");
        VarDecl var_decl{};
        expect_identifier(var_decl.name);

        log("Variable name: {}\n", var_decl.name.str_view());

        if (consume_token(TokenType::SEMICOLON)) {
            var_decl.value = nullptr;
            end_scope("Done parsing variable declaration (empty)\n");
            return var_decl;
        }

        expect(TokenType::ASSIGN);
        var_decl.value = push_expression(parse_expression());

        expect(TokenType::SEMICOLON);

        Statement temp = {};
        temp.type = Statement::Type::VAR_DECL;
        temp.var_decl = var_decl;
        end_scope("Done parsing variable declaration: '{}'\n", to_string(temp));
        return var_decl;
    }

    // Parses and performs constant folding where trivial
    Expression parse_expression() noexcept {
        start_scope("Parsing expression\n");
        Expression lhs = {};
        lhs.type = Expression::Type::EMPTY;

        // Expression can start with a literal: `1 + 2`..
        if (consume_token(TokenType::LITERAL)) {

            log("Parsed literal. Next token: '{}'\n", TokenTypes::stringify(peek_token()));
            // Strings are literals too, but no need to make a distinction
            lhs.type = Expression::Type::LITERAL;
            lhs.literal = get_literal();

        } // .. or an identifier: `x + 5`, `func()`..
        else if (consume_token(TokenType::IDENTIFIER)) {
            Identifier identifier = get_identifier();

            if (consume_token(TokenType::PAREN_OPEN)) {
                // Function call
                lhs.type = Expression::Type::FUNC_CALL;
                lhs.func_call.fn_name = identifier;

                // The expressions vector is always bigger than the maximum number of expressions,
                // so pushing will never allocate and thereby never invalidate pointers
                lhs.func_call.num_params = 0;

                if(!consume_token(TokenType::PAREN_CLOSE)) {
                    int param_count = recursive_parse_func_params();

                    auto start = expressions.end() - param_count;
                    auto end = expressions.end();
                    std::reverse(start, end);

                    lhs.func_call.num_params = param_count;
                    lhs.func_call.array_of_params = start.base();

                    expect(TokenType::PAREN_CLOSE);
                }
            }
            else {
                lhs.type = Expression::Type::VAR_REF;
                lhs.var_ref = identifier;
            }
        } // .. or an open paren, aka sub-expression: `(x-3)`..
        else if (consume_token(TokenType::PAREN_OPEN)) {
            lhs = parse_expression();
            expect(TokenType::PAREN_CLOSE);
        } // .. or an unary operator: `-x`, `~x`
        else if (UnaryOp op; consume_any_unary_op(op)) {
            lhs.type = Expression::Type::UNARY_OP;
            lhs.unary_op.op = op;
            lhs.unary_op.value = push_expression(parse_expression());
        }
        else {
            // Syntax error
            error("Unexpected token. Expected one of: -, ~, (, literal or identifier");

            // Unless the token is a semicolon...
            if (consume_token(TokenType::SEMICOLON)) {
                // if this wasn't a non-recursive call to parse_expression(),
                // this is an ICE (because the semicolon should've been detected).
                // Either way, the returned expression shouldn't be needed,
                // because compilation will abort as errors were found
                end_scope("Done parsing expression, error: '{}'\n", to_string(lhs));
                return lhs;
            }

            // Attempt recovery by starting over from the next token
            skip_token();
            end_scope("Done parsing expression, recovering from error by starting over\n");
            Expression expr = parse_expression();
            return expr;
        }

        // Quick exit if ) is found. Not strictly necessary
        if (peek_token() == TokenType::PAREN_CLOSE) {
            end_scope("Done parsing expression (')' found): '{}'\n", to_string(lhs));
            return lhs;
        }

        log("First half of expression parsing done, found '{}'\n", to_string(lhs));

        // See if there's an operator used on the left hand side expression

        if (consume_token(TokenType::RIGHT_ARROW)) {
            // explicitly disallow `(` here because otherwise
            // syntax like `foo->(bar)` would work
            if (consume_token(TokenType::PAREN_OPEN)) {
                error("Unexpected token. Expected variable name or function call after ->");

                Expression exp = parse_expression(); // Try to parse the rest still to see if there are more errors
                end_scope("Done parsing expression (error): '{}'\n", to_string(exp));
                return exp;
            }

            Expression rhs = parse_expression();
            if (rhs.type == Expression::Type::VAR_REF) {
                Expression* variable = push_expression(lhs);

                lhs.type = Expression::Type::MEMBER_VAR_REF;
                lhs.member_var_ref.member_name = rhs.var_ref;
                lhs.member_var_ref.variable = variable;

                end_scope("Done parsing expression, #1: '{}'\n", to_string(lhs));
                return lhs;
            }
            else if (rhs.type == Expression::Type::FUNC_CALL) {
                log("Rhs was FUNC_CALL. Name length: {}\n", rhs.func_call.fn_name.length);

                Expression* variable = push_expression(lhs);

                lhs.type = Expression::Type::MEMBER_FUNC_CALL;
                lhs.member_func_call.variable = variable;
                lhs.member_func_call.fn_name = rhs.func_call.fn_name;
                lhs.member_func_call.array_of_params = rhs.func_call.array_of_params;
                lhs.member_func_call.num_params = rhs.func_call.num_params;

                end_scope("Done parsing expression, #2: '{}'\n", to_string(lhs));
                return lhs;
            }
            // Complex cases
            else if (rhs.type == Expression::Type::MEMBER_FUNC_CALL) {
                // This could mean either `foo->bar->baz()` or `foo->bar()->baz()`.
                // However, right now it's constructed as
                // `foo->(bar->baz())` or `foo->(bar()->baz())`, both of which
                // are wrong, should be like `(foo->bar)->baz()` or `(foo->bar())->baz()`

                // NOTE: `foo` in any of these examples can be `foo` or `foo()`!!
                // (or any more complex expression!)

                // First store 'foo' persistently.
                // Note that any modifications to 'lhs' after this
                // won't change the value in 'expressions'
                Expression* foo = push_expression(lhs);

                // This is the function we're actually calling on lhs:
                Expression* rhs_var = rhs.member_func_call.variable;
                if (rhs_var->type == Expression::Type::VAR_REF) {
                    // `foo->bar->baz()`
                    // 'rhs_var' is `bar` here, *not* `bar->baz()`.
                    lhs.type = Expression::Type::MEMBER_VAR_REF;
                    lhs.member_var_ref.variable = foo;
                    lhs.member_var_ref.member_name = rhs_var->var_ref;

                    // Store `(foo->bar)` and rewrite so that result is `(foo->bar)->baz()`
                    rhs.member_func_call.variable = push_expression(lhs);

                    end_scope("Done parsing expression, #3: '{}'\n", to_string(rhs));
                    return rhs;
                }
                else if (rhs_var->type == Expression::Type::FUNC_CALL) {
                    // `foo->bar()->baz()`
                    // 'rhs_var' is `bar()` here, *not* `bar()->baz()`.
                    lhs.type = Expression::Type::MEMBER_FUNC_CALL;
                    lhs.member_func_call.variable = foo;
                    lhs.member_func_call.num_params = rhs_var->func_call.num_params;
                    lhs.member_func_call.array_of_params = rhs_var->func_call.array_of_params;
                    lhs.member_func_call.fn_name = rhs_var->func_call.fn_name;

                    // Store `(foo->bar())` and rewrite so that result is `(foo->bar())->baz()`
                    rhs.member_func_call.variable = push_expression(lhs);

                    end_scope("Done parsing expression, #4: '{}'\n", to_string(rhs));
                    return rhs;
                }
                else {
                    // Syntax error. Something like `x->(y+5)->z()`?
                    error( "Unexpected token. Expected variable name or function call after ->");
                }
            }
            else if (rhs.type == Expression::Type::MEMBER_VAR_REF) {
                // `foo->bar->baz` or `foo->bar()->baz`. See handling of member_func_call above.

                Expression* variable = push_expression(lhs);

                Expression* rhs_var = lhs.member_var_ref.variable;
                if (rhs_var->type == Expression::Type::VAR_REF) {
                    // `foo->bar->baz`
                    lhs.type = Expression::Type::MEMBER_VAR_REF;
                    lhs.member_var_ref.member_name = rhs_var->var_ref;
                    lhs.member_var_ref.variable = variable;

                    rhs.member_var_ref.variable = push_expression(lhs);
                    end_scope("Done parsing expression, #5: '{}'\n", to_string(rhs));
                    return rhs;
                }
                else if (rhs_var->type == Expression::Type::FUNC_CALL) {
                    lhs.type = Expression::Type::MEMBER_FUNC_CALL;
                    lhs.member_func_call.variable = variable;
                    lhs.member_func_call.fn_name = rhs_var->func_call.fn_name;
                    lhs.member_func_call.array_of_params = rhs_var->func_call.array_of_params;
                    lhs.member_func_call.num_params = rhs_var->func_call.num_params;

                    rhs.member_var_ref.variable = push_expression(lhs);
                    end_scope("Done parsing expression, #6: '{}'\n", to_string(rhs));
                    return rhs;
                }
                else {
                    error("Unexpected token. Expected variable name or function call after ->");
                }

            }
        } // end of `if (consume_token(TokenType::RIGHT_ARROW))`

        if (BinaryOp op; consume_any_binary_op(op)) {
            Expression* left = push_expression(lhs);

            lhs.type = Expression::Type::BINARY_OP;
            lhs.binary_op.lhs = left;
            lhs.binary_op.op = op;

            // Binary operators woo!
            Expression rhs = parse_expression();
            if (rhs.type == Expression::Type::BINARY_OP) {
                if (get_precedence(op) > get_precedence(rhs.binary_op.op)) {
                    // Convert `x OP (y OP z)` to `(x OP y) OP z`
                    lhs.binary_op.rhs = rhs.binary_op.lhs;
                    rhs.binary_op.lhs = push_expression(lhs);
                    end_scope("Done parsing expression (binary op; swapped order): '{}'\n", to_string(rhs));
                    return rhs;
                }
            }
            lhs.binary_op.rhs = push_expression(rhs);
            end_scope("Done parsing expression (binary op): '{}'\n", to_string(lhs));
            return lhs;
        }

        end_scope("Done parsing expression: '{}'\n", to_string(lhs));
        return lhs;
    }

    // Tolerates push_expressions while parsing parameters,
    // and builds a contiguous array of parameters at the end of the 'expressions' vector.
    // Kind of relies on there not being enough function parameters to overflow stack?
    int recursive_parse_func_params(int D = 0) noexcept {
        Expression expression = parse_expression(); // 48 bytes on stack
        start_scope("Starting D={}, parsed '{}'\n", D, to_string(expression));

        bool more = consume_token(TokenType::COMMA);

        int count = 1; // .. 52 bytes on stack per recursion, approximately
        if (more) {
            count += recursive_parse_func_params(D+1);
        }

        // Note that since this is pushed last, the parameters are pushed in reverse order.
        // This is why we're reversing the array after pushing the last one
        push_expression(expression);

        end_scope("Ending D={}. Count: {}\n", D, count);
        return count;
    }

    void parse_struct() noexcept {
        log("Parsing struct -- done, not implemented\n");
    }

    void parse_imports() noexcept {
        std::unordered_map<std::string, SourceRef> paths;

        while(consume_identifier("import")) {
            const SourceRef& source_pos = source_refs[index].whole_line();
            //token(); data(); // advance both

            std::string path;

            TokenType expected = TokenType::IDENTIFIER;
            while(!consume_token(TokenType::SEMICOLON)) {
                if (!expect(expected)) token(); // consume either way to not get stuck

                if (expected == TokenType::IDENTIFIER) {
                    expected = TokenType::DOUBLE_COLON;
                    path += data().identifier;
                } else { // Found ::
                    expected = TokenType::IDENTIFIER;
                    path += '/';
                }
            }
            path += ".rsc";

            if (auto it = paths.find(path); it != paths.end()) {
                warn("Duplicate import '{}'", path)
                    .at(source_pos)
                    .with_caret(false);

                note( "first occurrence here:")
                    .at(it->second)
                    .with_caret(false)
                    .with_underline(false);
            } else {
                paths.insert({ std::move(path), source_pos });
            }
        }

        for (auto& [path, src_ref] : paths) {
            log("Import statement found! Path: \"{}\"\n", path);
        }
    }

private:
    // Conversion methods

    Literal get_literal() noexcept {
        // Assumes token() == TokenType::LITERAL. Check first!
        Literal literal{};

        // This is stupid
        TokenData token_data = data();
        switch(token_data.type) {
            case TokenData::Type::UINT:
                literal.type = Literal::Type::UINT;
                literal.uint_literal = token_data.uint_literal;
                break;

            case TokenData::Type::INT:
                literal.type = Literal::Type::INT;
                literal.int_literal = token_data.int_literal;
                break;

            case TokenData::Type::FLOAT:
                literal.type = Literal::Type::FLOAT;
                literal.float_literal = token_data.float_literal;
                break;

            case TokenData::Type::STRING:
                literal.type = Literal::Type::STRING;
                literal.string_literal = token_data.string_literal;
                break;

            case TokenData::Type::CHAR:
                literal.type = Literal::Type::CHAR;
                literal.char_literal = token_data.char_literal;
                break;

            default:
                ice_abort(1, "get_literal: invalid token data");
        }
        return literal;
    }

    Identifier get_identifier() noexcept {
        // Assumes token() == TokenType::IDENTIFIER. Check first!
        Identifier identifier{};

        TokenData token_data = data();

        // Safety check even though this should never be true
        if (token_data.type != TokenData::Type::IDENTIFIER) {
            ice_abort(1, "get_identifier: invalid token data");
        }

        identifier.string = token_data.identifier;
        identifier.length = source_refs[index-1].length; // Hack
        return identifier;
    }

    bool consume_any_assignment_bin_op(BinaryOp& out) noexcept {
        switch(peek_token()) {
            case TokenType::SET_ADD:        out = BinaryOp::ADD;        break; // +=
            case TokenType::SET_SUB:        out = BinaryOp::SUB;        break; // -=
            case TokenType::SET_MUL:        out = BinaryOp::MUL;        break; // *=
            case TokenType::SET_DIV:        out = BinaryOp::DIV;        break; // /=
            case TokenType::SET_MOD:        out = BinaryOp::MOD;        break; // %=
            case TokenType::SET_OR:         out = BinaryOp::BIT_OR;     break; // |=
            case TokenType::SET_AND:        out = BinaryOp::BIT_AND;    break; // &=
            case TokenType::SET_XOR:        out = BinaryOp::BIT_XOR;    break; // ^=
            case TokenType::SET_LSHIFT:     out = BinaryOp::LSHIFT;     break; // <<=
            case TokenType::SET_RSHIFT:     out = BinaryOp::RSHIFT;     break; // >>=
            default: return false;
        }
        token(); // Consume the token
        return true;
    }

    bool consume_any_binary_op(BinaryOp& out) noexcept {
        switch(peek_token()) {
            case TokenType::PLUS_SIGN:      out = BinaryOp::ADD;        break; // +
            case TokenType::MINUS_SIGN:     out = BinaryOp::SUB;        break; // -
            case TokenType::ASTERISK:       out = BinaryOp::MUL;        break; // *
            case TokenType::FORWARD_SLASH:  out = BinaryOp::DIV;        break; // /
            case TokenType::MODULUS:        out = BinaryOp::MOD;        break; // %
            case TokenType::BITWISE_OR:     out = BinaryOp::BIT_OR;     break; // |
            case TokenType::BITWISE_AND:    out = BinaryOp::BIT_AND;    break; // &
            case TokenType::BITWISE_XOR:    out = BinaryOp::BIT_XOR;    break; // ^
            case TokenType::LSHIFT:         out = BinaryOp::LSHIFT;     break; // <<
            case TokenType::RSHIFT:         out = BinaryOp::RSHIFT;     break; // >>
            case TokenType::LOGICAL_AND:    out = BinaryOp::AND;        break; // &&
            case TokenType::LOGICAL_OR:     out = BinaryOp::OR;         break; // ||
            case TokenType::LESS_THAN:      out = BinaryOp::LTHAN;      break; // <
            case TokenType::LEQUAL_TO:      out = BinaryOp::LEQUAL_TO;  break; // <=
            case TokenType::EQUAL_TO:       out = BinaryOp::EQUAL_TO;   break; // ==
            case TokenType::NEQUAL_TO:      out = BinaryOp::NEQUAL_TO;  break; // !=
            case TokenType::GEQUAL_TO:      out = BinaryOp::GEQUAL_TO;  break; // >=
            case TokenType::GREATER_THAN:   out = BinaryOp::GTHAN;      break; // >
            default: return false;
        }
        token(); // Consume the token
        return true;
    }

    bool consume_any_unary_op(UnaryOp& out) noexcept {
        switch(peek_token()) {
            case TokenType::BITWISE_NOT:    out = UnaryOp::BIT_NOT;     break; // ~x
            case TokenType::MINUS_SIGN:     out = UnaryOp::NEGATE;      break; // -x
            case TokenType::EXCLAMATION:    out = UnaryOp::NOT;         break; // !x
            default: return false;
        }
        token(); // Consume the token
        return true;
    }

private:
    // Methods for consuming and checking tokens

    void skip_token() noexcept {
        // Some tokens have data associated, and that needs to be consumed too.
        switch(token()) {
            case TokenType::LITERAL:
            case TokenType::IDENTIFIER:
                data();
                break;
            default:
                break;
        }
    }

    void skip_until(TokenType type) noexcept {
        while(!consume_token(type)) {
            skip_token();
        }
    }

    bool consume_identifier(const char* name) noexcept {
        if (peek_token() == TokenType::IDENTIFIER && strequals(peek_data().identifier, name)) {
            token(); data(); // Consume found token and its data
            return true;
        }
        return false;
    }

    bool consume_token(TokenType type) noexcept {
        if (peek_token() == type) {
            token(); // Consume
            return true;
        }
        return false;
    }

    bool expect_identifier(Identifier& out) noexcept {
        if (!consume_token(TokenType::IDENTIFIER)) {
            std::string_view found = src.get_token_at(source_refs[index]);
            std::string_view expected = TokenTypes::stringify(TokenType::IDENTIFIER);

            error("Unexpected token '{}', expected '{}'", found, expected)
                .with_caret(false);

            return false;
        }
        // TODO Use get_identifier()?
        out.string = data().identifier;
        out.length = source_refs[index-1].length; // bit of a hack. -2 because "" are included
        return true;
    }

    bool expect(TokenType type) noexcept {
        if (!consume_token(type)) {
            std::string_view found = src.get_token_at(source_refs[index]);
            std::string_view expected = TokenTypes::stringify(type);

            log("Logging issue\n");
            error( "Unexpected token '{}', expected '{}'", found, expected)
                .with_caret(false);
            log("issue logged\n");
            return false;
        }
        return true;
    }

    TokenType token() noexcept {
        return tokens[index++];
    }

    TokenData data() noexcept {
        return *datas++; }

    [[nodiscard]] TokenType peek_token() const noexcept { return tokens[index]; }
    [[nodiscard]] TokenData peek_data() const noexcept { return *datas;}

    ErrorInfo error_info_here() const noexcept {
        ErrorInfo info;
        info.file = &src;
        info.where = source_refs[index];

        if (info.where.length == 1) info.show_caret = true;

        return info;
    }

private:
    Expression* push_expression(const Expression& expression) noexcept {
        if (expressions.size() == expressions.capacity()) {
            fmt::print("WELP: {}/{}\n", expressions.size(), num_tokens);
            return nullptr;
        }
        expressions.push_back(expression);
        return &expressions.back();
    }

    Statement* push_statement(const Statement& statement) noexcept {
        if (expressions.size() == expressions.capacity()) {
            fmt::print("WELP: {}/{}\n", expressions.size(), num_tokens);
            return nullptr;
        }
        statements.push_back(statement);
        return &statements.back();
    }

private:
    // Logging

    template<typename ... Args>
    Issue& warn(std::string_view format, Args&&... args) noexcept {
        Issue issue;
        issue.where = source_refs[index];
        issue.message = fmt::format(format, std::forward<Args>(args)...);
        issue.type = Issue::Type::WARNING;
        issue.underline = true;
        issue.show_caret = false;
        if (current_function_name.length != 0) {
            issue.function_name = current_function_name.str_view();
        }

        if (issue.where.length == 1) issue.show_caret = true;

        issues.push_back(std::move(issue));
        return issues.back();
    }

    template<typename ... Args>
    Issue& error(std::string_view format, Args&&... args) noexcept {
        Issue issue;
        issue.where = source_refs[index];
        issue.message = fmt::format(format, std::forward<Args>(args)...);
        issue.type = Issue::Type::ERROR;
        issue.underline = true;
        issue.show_caret = false;

        log("issue created\n");

        if (current_function_name.length != 0) {
            issue.function_name = current_function_name.str_view();
        }

        if (issue.where.length == 1) issue.show_caret = true;

        log("Pushing issue\n");

        issues.push_back(std::move(issue));
        log("Pushed, returning\n");
        return issues.back();
    }

    template<typename ... Args>
    Issue& note(std::string_view format, Args&&... args) noexcept {
        Issue issue;
        issue.where = source_refs[index];
        issue.message = fmt::format(format, std::forward<Args>(args)...);
        issue.type = Issue::Type::INFO;

        issues.push_back(std::move(issue));
        return issues.back();
    }

private:
    size_t index;
    size_t num_tokens;

    TokenType* tokens;
    TokenData* datas;

    SourceRef* source_refs;
    const SourceFile& src;

    // Neither of these are ever reallocated so pointers are stable
    std::vector<Expression> expressions;
    std::vector<Statement> statements;

    std::vector<Issue> issues;
    Identifier current_function_name;

    tsl::sparse_map<Identifier, FuncDecl> functions;
    tsl::sparse_map<Identifier, StructDecl> structs;
};

std::optional<AST> Parser::parse(const SourceFile& src, LexResult& lex) {
    if (lex.token_types.empty()) return std::nullopt;

    printf("Tokens (%lld) {\n", lex.token_types.size());
    int data_pos = 0;
    for (TokenType token : lex.token_types) {
        if (token == TokenType::LITERAL) {
            fmt::print("   literal ");
            switch(lex.token_datas[data_pos].type) {
                case TokenData::Type::STRING:
                    fmt::print("(string): \"{}\"\n", lex.token_datas[data_pos++].string_literal);
                    break;

                case TokenData::Type::CHAR:
                    fmt::print("(char): '{}'\n", lex.token_datas[data_pos++].char_literal);
                    break;

                case TokenData::Type::INT:
                    fmt::print("(int): {}\n", lex.token_datas[data_pos++].int_literal);
                    break;

                case TokenData::Type::UINT:
                    fmt::print("(uint): {}\n", lex.token_datas[data_pos++].uint_literal);
                    break;

                case TokenData::Type::FLOAT:
                    fmt::print("(float): {}\n", lex.token_datas[data_pos++].float_literal);
                    break;

                case TokenData::Type::IDENTIFIER:
                    fmt::print("(identifier): \"{}\"\n", lex.token_datas[data_pos++].identifier);
                    break;

            }
        }
        else if (token == TokenType::IDENTIFIER) {
            fmt::print("   identifier: \"{}\"\n", lex.token_datas[data_pos++].identifier);
        }
        else fmt::print("   '{}'\n", TokenTypes::stringify(token));
    }
    printf("}\n");

    std::optional<AST> ast = ParserUnit(src, lex).parse();
    return ast;
}