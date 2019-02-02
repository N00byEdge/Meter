#include "Meter/AST.hh"
#include "Meter/Types.hh"

#include <optional>
#include <sstream>

namespace {
  using namespace Meter::Tokens::Literals;
  struct BadParse { std::string s; };

  template<typename ...Ts>
  BadParse throwBadParse(Ts &&...vs) {
    BadParse bp;
    std::stringstream ss;
    ((ss << std::forward<Ts>(vs)), ...);
    bp.s = ss.str();
    throw bp;
    return bp;
  }

  template<typename TokenT>
  decltype(auto) expectToken(TokenT, Meter::Tokens::ParserContext &ctx) {
    return std::visit(Meter::Overload{
        [](TokenT &&t)   -> TokenT && { return std::move(t); }
      , [](auto &&other) -> TokenT && {
          throw throwBadParse("Unexpected '", Meter::Tokens::tokenName(other)
            , "', expected '", Meter::Tokens::tokenName(TokenT{}), "'.\n");
        }
    }, std::move(ctx.consume()));
  }

  Meter::AST::Expression parseExpr(Meter::Tokens::ParserContext &ctx, int maxPrec = 55555);

  template<typename PrefOp>
  Meter::AST::Expression handlePrefix(Meter::Tokens::ParserContext &ctx) {
    auto op = PrefOp { std::make_unique<Meter::AST::Expression>(parseExpr(ctx, PrefOp::prec)) };
    return std::move(op);
  }

  template<typename PostOp>
  void handlePostfix(Meter::Tokens::ParserContext &ctx, Meter::AST::Expression &currExpr) {
    ctx.pop();
    PostOp pop{std::make_unique<Meter::AST::Expression>(std::move(currExpr))};
    currExpr = std::move(pop);
  }

  template<typename BOp>
  void handleBOp(Meter::Tokens::ParserContext &ctx, int maxPrec, bool &terminate, Meter::AST::Expression &currExpr) {
    if((maxPrec >= BOp::prec && std::is_same_v<typename BOp::assoc, Meter::AST::RightAssociative>)
     || maxPrec >  BOp::prec) {
      ctx.pop();
      BOp bop;
      bop.lhs = std::make_unique<Meter::AST::Expression>(std::move(currExpr));
      bop.rhs = std::make_unique<Meter::AST::Expression>(parseExpr(ctx, BOp::prec));
      currExpr = std::move(bop);
    }
    else {
      terminate = true;
    }
  }

  template<typename FOp>
  void handleFOp(Meter::Tokens::ParserContext &ctx, Meter::AST::Expression &currExpr) {
    ctx.pop();
    FOp fop;
    fop.callee = std::make_unique<Meter::AST::Expression>(std::move(currExpr));
    while(ctx.lookaheadMatch<0, typename FOp::end>({})) {
      expectToken(","_token, ctx);
      fop.arguments.emplace_back(parseExpr(ctx));
    }
    ctx.pop();
    currExpr = std::move(fop);
  }

  Meter::AST::Expression parseDecl(Meter::Tokens::ParserContext &ctx, Meter::AST::Expression type) {
    auto ident = expectToken(Meter::Tokens::Identifier{}, ctx);
    auto tp = std::make_unique<Meter::AST::Expression>(std::move(type));
    return std::visit(Meter::Overload{
        [&](decltype("("_token)) -> Meter::AST::Expression {
        ctx.pop();
        Meter::AST::FunctionDecl decl;
        decl.ident = std::move(ident);
        decl.retType = std::move(tp);
        while(!ctx.lookaheadMatch(")"_token)) {
          auto arg = parseExpr(ctx);
          if (auto argg = std::get_if<Meter::AST::Decl>(&arg); argg) {
            decl.parameters.emplace_back(std::move(*argg));
          } else {
            throwBadParse("Expected parameter declaration.\n");
          }
        }
        // Todo: things like const on member functions here
        return std::move(decl);
      }
      , [&](decltype("="_token)) -> Meter::AST::Expression {
        ctx.pop();
        Meter::AST::Decl d;
        d.ident = std::move(ident);
        d.type = std::move(tp);
        d.arguments.emplace_back(parseExpr(ctx, Meter::AST::Assignment::prec));
        return std::move(d);
      }
      , [&](decltype("{")) -> Meter::AST::Expression {
        ctx.pop();
        Meter::AST::Decl d;
        d.ident = std::move(ident);
        d.type = std::move(tp);

        while(!ctx.lookaheadMatch("}"_token))
          d.arguments.emplace_back(parseExpr(ctx));

        return std::move(d);
      }
      , [&](auto otherwise) -> Meter::AST::Expression {
        Meter::AST::Decl d;
        d.type = std::move(tp);
        d.ident = std::move(ident);
        return std::move(d);
      }
    }, ctx.lookahead());
  }

  Meter::AST::Expression parseExpr(Meter::Tokens::ParserContext &ctx, int maxPrec) {
    auto expr = std::visit(Meter::Overload{
        // Non-operators
        [&](Meter::Tokens::Identifier ident) -> Meter::AST::Expression { return Meter::AST::Expression{ident}; }
      , [&](Meter::Tokens::Number     num)   -> Meter::AST::Expression { return Meter::AST::Expression{num};   }
      , [&](Meter::Tokens::Literal    lit)   -> Meter::AST::Expression { return Meter::AST::Expression{lit};   }
      , [&](Meter::Tokens::Float      flt)   -> Meter::AST::Expression { return Meter::AST::Expression{flt};   }
        // Prefix operators
      , [&](decltype("++"_token)) -> Meter::AST::Expression { return handlePrefix<Meter::AST::Preincrement>(ctx); }
      , [&](decltype("--"_token)) -> Meter::AST::Expression { return handlePrefix<Meter::AST::Predecrement>(ctx); }
      , [&](decltype("+"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::UnaryPlus>   (ctx); }
      , [&](decltype("-"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::UnaryMinus>  (ctx); }
      , [&](decltype("!"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::UnaryNot>    (ctx); }
      , [&](decltype("~"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::BitNot>      (ctx); }
      , [&](decltype("*"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::Dereference> (ctx); }
      , [&](decltype("&"_token))  -> Meter::AST::Expression { return handlePrefix<Meter::AST::Addressof>   (ctx); }
        // Parens
      , [&](decltype("("_token)) -> Meter::AST::Expression {
          auto ret = parseExpr(ctx); // Parse without precedence
          expectToken(")"_token, ctx);
          return ret;
      }
      , [&](auto) -> Meter::AST::Expression  { throw throwBadParse("Expected primary expression.\n"); }
    }, ctx.consume());
    // Binary operators, postfix operators
    for(bool exprEnd = false; !exprEnd;) {
      std::visit(Meter::Overload{
          // Postfix operators
          [&](decltype("++"_token)) { handlePostfix<Meter::AST::Postincrement>(ctx, expr); }
        , [&](decltype("--"_token)) { handlePostfix<Meter::AST::Postdecrement>(ctx, expr); }
          // Functionlike operators
        , [&](decltype("["_token))  { handleFOp<Meter::AST::FCall>    (ctx, expr); }
        , [&](decltype("("_token))  { handleFOp<Meter::AST::Subscript>(ctx, expr); }
          // Left assoc binary ops
        , [&](decltype("->"_token))  { handleBOp<Meter::AST::MemberDeref>   (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("*"_token))   { handleBOp<Meter::AST::Multiplication>(ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("/"_token))   { handleBOp<Meter::AST::Division>      (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("%"_token))   { handleBOp<Meter::AST::Modulus>       (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("+"_token))   { handleBOp<Meter::AST::Addition>      (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("-"_token))   { handleBOp<Meter::AST::Subtraction>   (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("<<"_token))  { handleBOp<Meter::AST::LeftShift>     (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype(">>"_token))  { handleBOp<Meter::AST::RightShift>    (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("<=>"_token)) { handleBOp<Meter::AST::Compare>       (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("<"_token))   { handleBOp<Meter::AST::Less>          (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("<="_token))  { handleBOp<Meter::AST::LessEquals>    (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype(">"_token))   { handleBOp<Meter::AST::Greater>       (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype(">="_token))  { handleBOp<Meter::AST::GreaterEquals> (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("=="_token))  { handleBOp<Meter::AST::Equals>        (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("!="_token))  { handleBOp<Meter::AST::NotEquals>     (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("&"_token))   { handleBOp<Meter::AST::BitAnd>        (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("^"_token))   { handleBOp<Meter::AST::BitXor>        (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("|"_token))   { handleBOp<Meter::AST::BitOr>         (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("&&"_token))  { handleBOp<Meter::AST::LogicalAnd>    (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("||"_token))  { handleBOp<Meter::AST::LogicalOr>     (ctx, maxPrec, exprEnd, expr); }
          // Misc
        , [&](decltype("?"_token))  {
          if(maxPrec >= Meter::AST::Ternary::prec) {
            ctx.pop();
            Meter::AST::Ternary ex;
            ex.cond = std::make_unique<Meter::AST::Expression>(std::move(expr));
            // Parse middle without precedence
            ex.taken = std::make_unique<Meter::AST::Expression>(parseExpr(ctx));
            expectToken(":"_token, ctx);
            // Parse last part with precedence
            ex.notTaken = std::make_unique<Meter::AST::Expression>(parseExpr(ctx, Meter::AST::Ternary::prec));
            expr = std::move(ex);
          } else {
            exprEnd = true;
          }
        }
          // Right assoc binary ops
        , [&](decltype("="_token))   { handleBOp<Meter::AST::Assignment>       (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("+="_token))  { handleBOp<Meter::AST::AddAssign>        (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("-="_token))  { handleBOp<Meter::AST::SubAssign>        (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("*="_token))  { handleBOp<Meter::AST::MulAssign>        (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("/="_token))  { handleBOp<Meter::AST::DivAssign>        (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("%="_token))  { handleBOp<Meter::AST::ModAssign>        (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("<<="_token)) { handleBOp<Meter::AST::ShiftLeftAssign>  (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype(">>="_token)) { handleBOp<Meter::AST::ShiftRightAssign> (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("&="_token))  { handleBOp<Meter::AST::BitAndAssign>     (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("^="_token))  { handleBOp<Meter::AST::BitXorAssign>     (ctx, maxPrec, exprEnd, expr); }
        , [&](decltype("|="_token))  { handleBOp<Meter::AST::BitOrAssign>      (ctx, maxPrec, exprEnd, expr); }
        , [&](Meter::Tokens::Identifier ident) {
          // We just had an expression, now we have an identifier.
          // This is a declaration with the last expression as the type.
          expr = parseDecl(ctx, std::move(expr));
          exprEnd = true;
        }
        , [&](auto anythingElse) { exprEnd = true; }
      }, ctx.lookahead());
    }
    return expr;
  }

  Meter::AST::Statement parseStmt(Meter::Tokens::ParserContext &ctx) {
    return std::visit(Meter::Overload{
        [&](decltype("for"_token)) -> Meter::AST::Statement {
          ctx.pop();
          Meter::AST::ForStatement stmt;
          expectToken("("_token, ctx);
          stmt.init = parseExpr(ctx);
          expectToken(";"_token, ctx);
          stmt.cond = parseExpr(ctx);
          expectToken(";"_token, ctx);
          stmt.iter = parseExpr(ctx);
          expectToken(")"_token, ctx);
          stmt.body = std::make_unique<Meter::AST::Statement>(parseStmt(ctx));
          return Meter::AST::Statement{std::move(stmt)};
      }
      , [&](decltype("if"_token)) -> Meter::AST::Statement {
          ctx.pop();
          Meter::AST::IfStatement stmt;
          expectToken("("_token, ctx);
          stmt.condition = parseExpr(ctx);
          expectToken(")"_token, ctx);
          stmt.taken = std::make_unique<Meter::AST::Statement>(parseStmt(ctx));
          if(ctx.lookaheadMatch("else"_token)) {
            ctx.pop();
            stmt.notTaken = std::make_unique<Meter::AST::Statement>(parseStmt(ctx));
          }
          return Meter::AST::Statement{std::move(stmt)};
      }
      , [&](decltype("struct"_token)) -> Meter::AST::Statement {
        ctx.pop();
        Meter::AST::StructDeclaration str;
        auto ident = expectToken(Meter::Tokens::Identifier{}, ctx);
        str.ident = std::move(ident);
        str.contents = std::make_unique<Meter::AST::Statement>(parseStmt(ctx));
        return Meter::AST::Statement{std::move(str)};
      }
      , [&](decltype("do"_token)) -> Meter::AST::Statement {
        ctx.pop();
        Meter::AST::DoWhile dw;
        dw.body = std::make_unique<Meter::AST::Statement>(parseStmt(ctx));
        expectToken("while"_token, ctx);
        expectToken("("_token, ctx);
        dw.cond = parseExpr(ctx);
        expectToken(")"_token, ctx);
        expectToken(";"_token, ctx);
        return Meter::AST::Statement{std::move(dw)};
      }
      , [&](decltype("{"_token)) -> Meter::AST::Statement {
        ctx.pop();
        Meter::AST::CompoundStatement stmt;
        while (!ctx.lookaheadMatch("}"_token))
          stmt.stmts.emplace_back(parseStmt(ctx));
        ctx.pop();
        return Meter::AST::Statement{std::move(stmt)};
      }
      , [&](auto otherToken) -> Meter::AST::Statement {
        // No idea what this is, assume it's an expression statement.
        auto expr = Meter::AST::ExpressionStatment{parseExpr(ctx)};
        expectToken(";"_token, ctx);
        return std::move(expr);
      }
    }, ctx.lookahead());
  }
}

std::deque<Meter::AST::Statement> Meter::AST::makeAST(Meter::Tokens::ParserContext &ctx, std::ostream &os) {
  std::deque<Meter::AST::Statement> ret;

  for(; !ctx.lookaheadMatch(Tokens::EndOfFile);) {
    try {
      ret.emplace_back(parseStmt(ctx));
    } catch(BadParse bp) {
      os << "Parsing failed: " + bp.s;
      return ret;
    }
  }

  return ret;
}
