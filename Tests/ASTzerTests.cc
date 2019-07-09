#include "Meter/ASTizer.hh"
#include "Meter/CompileError.hh"

#include "gtest/gtest.h"

using namespace Meter::Tokens::Literals;
using namespace std::literals;

void expect_match(Meter::AST::Statements const &actual, Meter::AST::Statements const &expected);
void expect_match(Meter::AST::Statement const &actual, Meter::AST::Statement const &expected);

void expect_match(Meter::AST::Statement const &actual, Meter::AST::Statement const &expected) {
  if(actual.index() != expected.index()) {
    std::cerr <<
        "Statement mismatch: "
        " Expected " << expected <<
        ", but got " << actual << ".\n";
    ADD_FAILURE();
    return;
  }

  /*std::visit([&expected](auto &actual) {
    [expected = std::get<decltype(actual)>(expected), &actual]() {
    }();
  }, actual);*/
}

void expect_match(Meter::AST::Statements const &actual, Meter::AST::Statements const &expected) {
  EXPECT_EQ(actual.size(), expected.size());

  if(actual.size() != expected.size())
    return;

  auto m = std::min(actual.size(), expected.size());

  for(auto i = 0; i < m; ++ i)
    expect_match(actual[i], expected[i]);
}

#include <random>

namespace {
  std::mt19937_64 mt;
  auto randomNameSource = "totallyRandomName"sv;
  std::uniform_int_distribution<int> lenDist{3, 9};
  std::uniform_int_distribution<int> lineDist{5, 20};
  std::uniform_int_distribution<int> colDist{1, 200};
}

auto randomName() {
  return randomNameSource.substr(lenDist(mt), lenDist(mt));
}

Meter::Tokens::Identifier randomIdent() {
  Meter::Tokens::TokenContext ctx;
  ctx.filename = randomName();
  ctx.view = randomName();
  ctx.line = lineDist(mt);
  ctx.columnStart = lineDist(mt);
  ctx.columnEnd = ctx.columnStart + ctx.view.size();

  Meter::Tokens::Identifier ident;
  ident.length = ctx.view.size();
  ident.value = ctx.view;
  ident.context = std::move(ctx);

  return ident;
}

void ASTize(std::vector<Meter::Tokens::Token> tokens,
  Meter::AST::Statements const &expectedStatements) {

  Meter::AST::ParserContext ctx;
  std::copy(tokens.begin(), tokens.end(), std::back_inserter(ctx.tokenQueue));

  expect_match(Meter::AST::makeAST(ctx), expectedStatements);
}

#define TestUnaryWIdent(Name, Type)                  \
TEST(ASTizer, Name) {                                \
  Meter::AST::Type pi;                               \
  Meter::AST::ExpressionStatment est{std::move(pi)}; \
  Meter::AST::Statements st;                         \
  st.emplace_back(std::move(est));                   \
                                                     \
  for(int i = 0; i < 100; ++ i) {                    \
    auto ident = randomIdent();                      \
    ASTize({ident, "++"_token, ";"_token}, st);      \
  }                                                  \
}

TestUnaryWIdent(PreIncIdent, Preincrement);
TestUnaryWIdent(PostIncIdent, Postincrement);
#undef TestUnaryWIdent
