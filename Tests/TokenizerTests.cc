#include "Meter/Tokenizer.hh"
#include "Meter/CompileError.hh"

#include "gtest/gtest.h"

using namespace std::literals;
using namespace Meter::Tokens::Literals;

auto tokenizeString(std::string_view input,
                    std::vector<Meter::Tokens::Token> const &expectedTokens) {
  Meter::Tokens::TokenizerContext context;
  context.view = input;
  auto contextBackup = context;
  for(auto it = expectedTokens.begin(); it != expectedTokens.end(); ++ it, contextBackup = context) {
    if(it->index() != Meter::Tokens::consumeToken(context).index()) {
      ADD_FAILURE();
      std::cout << "Test string \"" << input << "\" failed on token " <<
                   it - expectedTokens.begin() << ".\n"
                   "Before consuming: " << contextBackup.view << "\n"
                   "After consuming: " << context.view << "\n";
    }
  }

  auto endtoken = Meter::Tokens::consumeToken(context);
  if(!std::get_if<Meter::Tokens::EOF_T>(&endtoken)) {
    std::cout << "Test string \"" << input << "\" produced extra tokens.\n"
                 "Before consuming: " << contextBackup.view << "\n"
                 "After consuming: " << context.view << "\n";
    ADD_FAILURE();
  }
}

TEST(Tokenizer, TokenizeStrings) {
  tokenizeString("", {});
  tokenizeString("++", {"++"_token});
  tokenizeString("+++", {"++"_token, "+"_token});
  tokenizeString("++++", {"++"_token, "++"_token});
  tokenizeString("----aa-", {"--"_token, "--"_token, Meter::Tokens::Identifier{}, "-"_token});
  tokenizeString("----aa--", {"--"_token, "--"_token, Meter::Tokens::Identifier{}, "--"_token});
  tokenizeString("---aa", {"--"_token, "-"_token, Meter::Tokens::Identifier{}});
  tokenizeString("return", {"return"_token});
  tokenizeString("returna", {Meter::Tokens::Identifier{}});
  tokenizeString("-->;", {"--"_token, ">"_token, ";"_token});
  tokenizeString("--->;", {"--"_token, "->"_token, ";"_token});
  tokenizeString("//\n+", {"+"_token});
  tokenizeString("/**/+", {"+"_token});
  tokenizeString("//+++++", {});
  tokenizeString("/*+++++", {});
  tokenizeString("//+++++\n", {});
  tokenizeString("/*+++++*/", {});
  tokenizeString("//+++++\n+", {"+"_token});
  tokenizeString("/*+++++*/+", {"+"_token});
}

auto getTokens(std::string_view input) {
  Meter::Tokens::TokenizerContext context;
  context.view = input;
  std::vector<Meter::Tokens::Token> tokens;
  while(1) {
    auto tok = Meter::Tokens::consumeToken(context);
    if(std::holds_alternative<Meter::Tokens::EOF_T>(tok))
      break;
    tokens.emplace_back(std::move(tok));
  }
  return tokens;
}

TEST(Tokenizer, TokenContext) {
  {
    auto sv = "ident"sv;
    // Expected result
    tokenizeString(sv, {Meter::Tokens::Identifier{}});

    auto toks = getTokens(sv);
    auto &ident = std::get<Meter::Tokens::Identifier>(toks[0]);
    EXPECT_EQ(ident.context.view, sv);
    EXPECT_EQ(ident.context.line, 1);
    EXPECT_EQ(ident.context.columnStart, 1);
    EXPECT_EQ(ident.context.columnEnd, 6);
  }

  {
    auto sv = "    ident"sv;
    // Expected result
    tokenizeString(sv, {Meter::Tokens::Identifier{}});

    auto toks = getTokens(sv);
    auto &ident = std::get<Meter::Tokens::Identifier>(toks[0]);
    EXPECT_EQ(ident.context.view, sv.substr(4));
    EXPECT_EQ(ident.context.line, 1);
    EXPECT_EQ(ident.context.columnStart, 5);
    EXPECT_EQ(ident.context.columnEnd, 10);
  }

  {
    auto sv = "   \nident"sv;
    // Expected result
    tokenizeString(sv, {Meter::Tokens::Identifier{}});

    auto toks = getTokens(sv);
    auto &ident = std::get<Meter::Tokens::Identifier>(toks[0]);
    EXPECT_EQ(ident.context.view, sv.substr(4));
    EXPECT_EQ(ident.context.line, 2);
    EXPECT_EQ(ident.context.columnStart, 1);
    EXPECT_EQ(ident.context.columnEnd, 6);
  }
}

TEST(Tokenizer, SyntaxErr) {
  ASSERT_THROW(tokenizeString("\"", {}), Meter::SyntaxError);
  ASSERT_THROW(tokenizeString("\x07", {}), Meter::SyntaxError);
}

#define ParseOperator(TestName, Token)                                    \
TEST(Tokenizer, TestName) {                                               \
  for(auto sv: { Token##sv, "     " Token##sv, "\n\n\t   " Token##sv }) { \
    auto context = Meter::Tokens::TokenizerContext{sv};                   \
    auto tokvar = Meter::Tokens::consumeToken(context);                   \
    EXPECT_TRUE(context.view.empty());                                    \
    auto tok = std::get_if<decltype(Token##_token)>(&tokvar);             \
    EXPECT_NE(tok, nullptr);                                              \
    if(tok != nullptr) {                                                  \
      auto tokenPos = std::find_if_not(sv.begin(), sv.end(),              \
        [](auto val){ return std::isspace(val); });                       \
      EXPECT_EQ(tokenPos, tok->context.view.begin());                     \
      EXPECT_EQ(sv.end(), tok->context.view.end());                       \
    }                                                                     \
  }                                                                       \
}

ParseOperator(Semicolon, ";")
ParseOperator(Dot, ".")
ParseOperator(Colon, ":")
ParseOperator(Comma, ",")
ParseOperator(Plus, "+")
ParseOperator(Minus, "-")
ParseOperator(Multiply, "*")
ParseOperator(Division, "/")
ParseOperator(Modulus, "%")
ParseOperator(QMark, "?")
ParseOperator(PointerAccess, "->")
ParseOperator(OParen, "(")
ParseOperator(CParen, ")")
ParseOperator(OBrace, "{")
ParseOperator(CBrace, "}")
ParseOperator(OSqBr, "[")
ParseOperator(CSqBr, "]")
ParseOperator(ShiftLeft, "<<")
ParseOperator(ShiftRight, ">>")
ParseOperator(Bitand, "&")
ParseOperator(Bitor, "|")
ParseOperator(Bitxor, "^")
ParseOperator(LessThan, "<")
ParseOperator(LessThanEq, "<=")
ParseOperator(GreaterThan, ">")
ParseOperator(BitNot, "~")
ParseOperator(GreaterThanEq, ">=")
ParseOperator(LogicalAnd, "&&")
ParseOperator(LogicalOr, "||")
ParseOperator(Equals, "==")
ParseOperator(NotEquals, "!=")
ParseOperator(Assignment, "=")
ParseOperator(LogicalNot, "!")
ParseOperator(Increment, "++")
ParseOperator(Decrement, "--")
ParseOperator(AddAssign, "+=")
ParseOperator(SubAssign, "-=")
ParseOperator(DivAssign, "/=")
ParseOperator(MultiplyAssign, "*=")
ParseOperator(ModAssign, "%=")
ParseOperator(OCBrace, "%{")
ParseOperator(ShiftLeftAssign, "<<=")
ParseOperator(ShiftRightAssign, ">>=")
ParseOperator(BitandAssign, "&=")
ParseOperator(BitxorAssign, "^=")
ParseOperator(BitorAssign, "|=")
ParseOperator(Compare, "<=>")
ParseOperator(RetBlock, "=>")
#undef ParseOperator

#define ParseKeyword(TestName, Keyword)                                 \
TEST(Tokenizer, TestName) {                                             \
  auto context = Meter::Tokens::TokenizerContext{Keyword##sv};          \
  auto &sv = context.view, svbefore = sv;                               \
  auto tokvar = Meter::Tokens::consumeToken(context);                   \
  EXPECT_TRUE(sv.empty());                                              \
  auto tok = std::get_if<decltype(Keyword##_token)>(&tokvar);           \
  EXPECT_NE(tok, nullptr);                                              \
  if(tok != nullptr) {                                                  \
    EXPECT_EQ(svbefore.begin(), tok->context.view.begin());             \
    EXPECT_EQ(svbefore.end(),   tok->context.view.end());               \
  }                                                                     \
  sv = svbefore = Keyword "aääa";                                       \
  tokvar = Meter::Tokens::consumeToken(context);                        \
  EXPECT_TRUE(sv.empty());                                              \
  auto tok2 = std::get_if<Meter::Tokens::Identifier>(&tokvar);          \
  EXPECT_NE(tok2, nullptr);                                             \
  if(tok2 != nullptr) {                                                 \
    EXPECT_EQ(svbefore.begin(), tok2->context.view.begin());            \
    EXPECT_EQ(svbefore.end(),   tok2->context.view.end());              \
  }                                                                     \
}

ParseKeyword(For, "for");
ParseKeyword(If, "if");
ParseKeyword(Else, "else");
ParseKeyword(While, "while");
ParseKeyword(Struct, "struct");
ParseKeyword(Do, "do");
ParseKeyword(Return, "return");
ParseKeyword(Scope_Exit, "scope_exit");
ParseKeyword(Scope_Fail, "scope_fail");
ParseKeyword(Scope_Success, "scope_success");
#undef ParseKeyword

TEST(Tokenizer, EndOfFile) {
  for(auto sv: { ""sv, "     "sv, "\n\n\t   "sv }) {
    auto context = Meter::Tokens::TokenizerContext{sv};
    auto svbefore = sv;
    auto tokvar = Meter::Tokens::consumeToken(context);
    EXPECT_EQ(sv, svbefore);
    auto tok = std::get_if<Meter::Tokens::EOF_T>(&tokvar);
    EXPECT_NE(tok, nullptr);
  }
}
