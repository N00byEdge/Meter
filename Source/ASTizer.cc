#include "Meter/AST.hh"

#include <optional>

namespace {
  struct BadParse {};
  template<typename TokenT>
  TokenT expectToken(TokenT, char const *&cp) {
    auto t = Meter::Tokens::consumeToken(cp);
    if(auto vp = std::get_if<TokenT>(&t); !vp) {
      // os << error(t) << " expected " << friendlyName<TokenT> << ", got ";
      // std::visit([&](auto tok){ os << friendlyName<decltype(tok); >});
      // os << ".\n";
      throw BadParse{};
    } else {
      return *vp;
    }
  }

  Meter::Tokens::Token lookaheadToken(char const *cp) {
    return Meter::Tokens::consumeToken(cp);
  }

  template<typename TokenT>
  bool lookaheadMatch(TokenT, char const *cp) {
    return std::holds_alternative<TokenT>(lookaheadToken(cp));
  }

  template<typename ...Fs>
  struct Overload: Fs... {
    using Fs::operator()...;
  };

  template<typename ...Fs> Overload(Fs ...vals) -> Overload<Fs...>;

  Meter::AST::Expression parseExpr(char const *&cp) {
    return Meter::AST::NoOp{};
  }

  Meter::AST::Statement parseStmt(char const *&cp) {
    using namespace Meter::Tokens::Literals;
    return std::visit(Overload{
        [&](decltype("for"_token)) -> Meter::AST::Statement {
          Meter::Tokens::consumeToken(cp);
          Meter::AST::ForStatement stmt;
          expectToken("("_token, cp);
          stmt.init = parseExpr(cp);
          expectToken(";"_token, cp);
          stmt.cond = parseExpr(cp);
          expectToken(";"_token, cp);
          stmt.iter = parseExpr(cp);
          expectToken(")"_token, cp);
          stmt.body = std::make_unique<Meter::AST::Statement>(parseStmt(cp));
          return Meter::AST::Statement{std::move(stmt)};
      }
      , [&](decltype("if"_token)) -> Meter::AST::Statement {
          Meter::Tokens::consumeToken(cp);
          Meter::AST::IfStatement stmt;
          expectToken("("_token, cp);
          stmt.condition = parseExpr(cp);
          expectToken(")"_token, cp);
          stmt.taken = std::make_unique<Meter::AST::Statement>(parseStmt(cp));
          return Meter::AST::Statement{std::move(stmt)};
      }
      , [&](decltype("{"_token)) -> Meter::AST::Statement {
        Meter::Tokens::consumeToken(cp);
        Meter::AST::CompoundStatement stmt;
        while(!lookaheadMatch("}"_token, cp))
          stmt.stmts.emplace_back(std::make_unique<Meter::AST::Statement>(parseStmt(cp)));
        Meter::Tokens::consumeToken(cp);
        return Meter::AST::Statement{std::move(stmt)};
      }
      , [&](auto otherToken) -> Meter::AST::Statement {
        // No idea what this is, assume it's an expression statement.
        return Meter::AST::ExpressionStatment{parseExpr(cp)};
      }
    }, lookaheadToken(cp));
  }
}

std::deque<Meter::AST::Statement> Meter::AST::makeAST(char const *&cp, std::ostream &os) {
  std::deque<Meter::AST::Statement> ret;

  for(;;) {
    /*auto token = Meter::Tokens::consumeToken(cp);
    if(std::holds_alternative<Meter::Tokens::EOF_T>(token))
      break;*/
    try {
      parseStmt(cp);
    } catch(BadParse bp) {
      os << "Parsing failed.\n";
      return ret;
    }
  }

  return ret;
}
