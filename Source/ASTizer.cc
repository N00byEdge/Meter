#include "Meter/AST.hh"
#include "Meter/Types.hh"

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

  template<typename ...Fs> Overload(Fs &&...vals) -> Overload<Fs...>;

  Meter::AST::Expression parseExpr(char const *&cp, int maxPrec = 55555);

  template<typename PrefOp>
  Meter::AST::Expression handlePrefix(char const *&cp) {
    Meter::Tokens::consumeToken(cp);
    auto op = PrefOp { std::make_unique<Meter::AST::Expression>(parseExpr(cp, PrefOp::prec)) };
    return std::move(op);
  }

  template<typename PostOp>
  void handlePostfix(char const *&cp, Meter::AST::Expression &currExpr) {
    Meter::Tokens::consumeToken(cp);
    PostOp pop{std::make_unique<Meter::AST::Expression>(std::move(currExpr))};
    currExpr = std::move(pop);
  }

  template<typename BOp>
  void handleBOp(char const *&cp, int maxPrec, bool &terminate, Meter::AST::Expression &currExpr) {
    if((maxPrec >= BOp::prec && std::is_same_v<typename BOp::assoc, Meter::AST::RightAssociative>)
     || maxPrec >  BOp::prec) {
      Meter::Tokens::consumeToken(cp);
      BOp bop;
      bop.lhs = std::make_unique<Meter::AST::Expression>(std::move(currExpr));
      bop.rhs = std::make_unique<Meter::AST::Expression>(parseExpr(cp, BOp::prec));
      currExpr = std::move(bop);
    }
    else {
      terminate = true;
    }
  }

  template<typename FOp>
  void handleFOp(char const *&cp, Meter::AST::Expression &currExpr) {
    Meter::Tokens::consumeToken(cp);
    FOp fop;
    fop.callee = std::make_unique<Meter::AST::Expression>(std::move(currExpr));
    while(!lookaheadMatch<typename FOp::end>({}, cp)) {
      using namespace Meter::Tokens::Literals;
      expectToken(","_token, cp);
      fop.arguments.emplace_back(parseExpr(cp));
    }
    Meter::Tokens::consumeToken(cp);
    currExpr = std::move(fop);
  }

  Meter::AST::Expression parseExpr(char const *&cp, int maxPrec) {
    using namespace Meter::Tokens::Literals;
    return std::visit(Overload{
        [&](decltype(";"_token)) -> Meter::AST::Expression {
          Meter::Tokens::consumeToken(cp);
          return Meter::AST::NoOp{};
      }
      , [&](decltype("("_token)) -> Meter::AST::Expression {
          Meter::Tokens::consumeToken(cp);
          auto ret = parseExpr(cp);
          expectToken(")"_token, cp);
          return ret;
      }
      , [&](Meter::Tokens::EOF_T)    -> Meter::AST::Expression { throw BadParse{}; }
      // Prefix operators
      , [&](decltype("++"_token)) -> Meter::AST::Expression { return handlePrefix<Meter::AST::Preincrement>(cp); }
      , [&](decltype("--"_token)) -> Meter::AST::Expression { return handlePrefix<Meter::AST::Predecrement>(cp); }
      , [&](decltype("+"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::UnaryPlus>   (cp); }
      , [&](decltype("-"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::UnaryMinus>  (cp); }
      , [&](decltype("!"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::UnaryNot>    (cp); }
      , [&](decltype("~"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::BitNot>      (cp); }
      , [&](decltype("*"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::Dereference> (cp); }
      , [&](decltype("&"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::Addressof>   (cp); }
      , [&](auto anythingElse)    -> Meter::AST::Expression {
        auto expr = std::visit(Overload{
          // Non-operators
            [&](Meter::Tokens::Identifier ident) { return Meter::AST::Expression{ident}; }
          , [&](Meter::Tokens::Number     num)   { return Meter::AST::Expression{num};   }
          , [&](Meter::Tokens::Literal    lit)   { return Meter::AST::Expression{lit};   }
          , [&](Meter::Tokens::Float      flt)   { return Meter::AST::Expression{flt};   }
          , [&](auto) -> Meter::AST::Expression  { throw BadParse{}; }
        }, Meter::Tokens::consumeToken(cp));
        // Binary operators, postfix operators
        bool exprEnd = false;
        while(!exprEnd) {
          std::visit(Overload{
              // Postfix operators
              [&](decltype("++"_token)) { handlePostfix<Meter::AST::Postincrement>(cp, expr); }
            , [&](decltype("--"_token)) { handlePostfix<Meter::AST::Postdecrement>(cp, expr); }
              // Functionlike operators
            , [&](decltype("["_token))  { handleFOp<Meter::AST::FCall>    (cp, expr); }
            , [&](decltype("("_token))  { handleFOp<Meter::AST::Subscript>(cp, expr); }
              // Left assoc binary ops
            , [&](decltype("->"_token))  { handleBOp<Meter::AST::MemberDeref>   (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("*"_token))   { handleBOp<Meter::AST::Multiplication>(cp, maxPrec, exprEnd, expr); }
            , [&](decltype("/"_token))   { handleBOp<Meter::AST::Division>      (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("%"_token))   { handleBOp<Meter::AST::Modulus>       (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("+"_token))   { handleBOp<Meter::AST::Addition>      (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("-"_token))   { handleBOp<Meter::AST::Subtraction>   (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("<<"_token))  { handleBOp<Meter::AST::LeftShift>     (cp, maxPrec, exprEnd, expr); }
            , [&](decltype(">>"_token))  { handleBOp<Meter::AST::RightShift>    (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("<=>"_token)) { handleBOp<Meter::AST::Compare>       (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("<"_token))   { handleBOp<Meter::AST::Less>          (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("<="_token))  { handleBOp<Meter::AST::LessEquals>    (cp, maxPrec, exprEnd, expr); }
            , [&](decltype(">"_token))   { handleBOp<Meter::AST::Greater>       (cp, maxPrec, exprEnd, expr); }
            , [&](decltype(">="_token))  { handleBOp<Meter::AST::GreaterEquals> (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("=="_token))  { handleBOp<Meter::AST::Equals>        (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("!="_token))  { handleBOp<Meter::AST::NotEquals>     (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("&"_token))   { handleBOp<Meter::AST::BitAnd>        (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("^"_token))   { handleBOp<Meter::AST::BitXor>        (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("|"_token))   { handleBOp<Meter::AST::BitOr>         (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("&&"_token))  { handleBOp<Meter::AST::LogicalAnd>    (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("||"_token))  { handleBOp<Meter::AST::LogicalOr>     (cp, maxPrec, exprEnd, expr); }
              // Misc
            , [&](decltype("?"_token))  {
              if(maxPrec >= Meter::AST::Ternary::prec) {
                Meter::Tokens::consumeToken(cp);
                Meter::AST::Ternary ex;
                ex.cond = std::make_unique<Meter::AST::Expression>(std::move(expr));
                // Parse middle without precedence
                ex.taken = std::make_unique<Meter::AST::Expression>(parseExpr(cp));
                expectToken(":"_token, cp);
                // Parse last part with precedence
                ex.notTaken = std::make_unique<Meter::AST::Expression>(parseExpr(cp, Meter::AST::Ternary::prec));
                expr = std::move(ex);
              } else {
                exprEnd = true;
              }
            }
              // Right assoc binary ops
            , [&](decltype("="_token))   { handleBOp<Meter::AST::Assignment>       (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("+="_token))  { handleBOp<Meter::AST::AddAssign>        (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("-="_token))  { handleBOp<Meter::AST::SubAssign>        (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("*="_token))  { handleBOp<Meter::AST::MulAssign>        (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("/="_token))  { handleBOp<Meter::AST::DivAssign>        (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("%="_token))  { handleBOp<Meter::AST::ModAssign>        (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("<<="_token)) { handleBOp<Meter::AST::ShiftLeftAssign>  (cp, maxPrec, exprEnd, expr); }
            , [&](decltype(">>="_token)) { handleBOp<Meter::AST::ShiftRightAssign> (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("&="_token))  { handleBOp<Meter::AST::BitAndAssign>     (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("^="_token))  { handleBOp<Meter::AST::BitXorAssign>     (cp, maxPrec, exprEnd, expr); }
            , [&](decltype("|="_token))  { handleBOp<Meter::AST::BitOrAssign>      (cp, maxPrec, exprEnd, expr); }
            , [&](auto anythingElse) { exprEnd = true; }
          }, lookaheadToken(cp));
        }
        return expr;
      }
    }, lookaheadToken(cp));
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
          if(lookaheadMatch("else"_token, cp)) {
            Meter::Tokens::consumeToken(cp);
            stmt.notTaken = std::make_unique<Meter::AST::Statement>(parseStmt(cp));
          }
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
        auto expr = Meter::AST::ExpressionStatment{parseExpr(cp)};
        expectToken(";"_token, cp);
        return std::move(expr);
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
