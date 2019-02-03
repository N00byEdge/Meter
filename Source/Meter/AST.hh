#pragma once

#include "Meter/Token.hh"

#include <memory>
#include <iostream>
#include <iterator>

namespace Meter::AST {
  class Expression;
  struct LeftAssociative;
  struct RightAssociative;
  namespace Impl {
    using ExprRef = std::unique_ptr<Expression>;

    template<typename TriggerToken, typename Associativity, int precedence>
    struct BinaryOperator {
      inline constexpr static auto prec = precedence;
      using trigger = TriggerToken;
      using assoc = Associativity;
      ExprRef lhs, rhs;
    };

    template<typename TriggerToken, typename Associativity, int precedence>
    struct UnaryOperator {
      inline constexpr static auto prec = precedence;
      using trigger = TriggerToken;
      using assoc = Associativity;
      ExprRef operand;
    };
  }

  using namespace Meter::Tokens::Literals;
  using MemberAccess     = Impl::BinaryOperator<decltype("."_token),   LeftAssociative, 2>;
  using Postincrement    = Impl::UnaryOperator <decltype("++"_token),  LeftAssociative, 2>;
  using Postdecrement    = Impl::UnaryOperator <decltype("--"_token),  LeftAssociative, 2>;
  struct FCall           {
                           inline constexpr static auto prec = 2;
                           using begin = decltype("("_token);
                           using end   = decltype(")"_token);
                           Impl::ExprRef callee;
                           std::deque<Expression> arguments;
                         };
  struct Subscript       {
                           inline constexpr static auto prec = 2;
                           using begin = decltype("["_token);
                           using end   = decltype("]"_token);
                           Impl::ExprRef callee;
                           std::deque<Expression> arguments;
                         };
  using Preincrement     = Impl::UnaryOperator <decltype("++"_token),  RightAssociative, 3>;
  using Predecrement     = Impl::UnaryOperator <decltype("--"_token),  RightAssociative, 3>;
  using UnaryPlus        = Impl::UnaryOperator <decltype("+"_token),   RightAssociative, 3>;
  using UnaryMinus       = Impl::UnaryOperator <decltype("-"_token),   RightAssociative, 3>;
  using UnaryNot         = Impl::UnaryOperator <decltype("!"_token),   RightAssociative, 3>;
  using BitNot           = Impl::UnaryOperator <decltype("~"_token),   RightAssociative, 3>;
  using Dereference      = Impl::UnaryOperator <decltype("*"_token),   RightAssociative, 3>;
  using Addressof        = Impl::UnaryOperator <decltype("&"_token),   RightAssociative, 3>;
  using MemberDeref      = Impl::BinaryOperator<decltype("->"_token),  LeftAssociative, 4>;
  using Multiplication   = Impl::BinaryOperator<decltype("*"_token),   LeftAssociative, 5>;
  using Division         = Impl::BinaryOperator<decltype("/"_token),   LeftAssociative, 5>;
  using Modulus          = Impl::BinaryOperator<decltype("%"_token),   LeftAssociative, 5>;
  using Addition         = Impl::BinaryOperator<decltype("+"_token),   LeftAssociative, 6>;
  using Subtraction      = Impl::BinaryOperator<decltype("-"_token),   LeftAssociative, 6>;
  using LeftShift        = Impl::BinaryOperator<decltype("<<"_token),  LeftAssociative, 7>;
  using RightShift       = Impl::BinaryOperator<decltype(">>"_token),  LeftAssociative, 7>;
  using Compare          = Impl::BinaryOperator<decltype("<=>"_token), LeftAssociative, 8>;
  using Less             = Impl::BinaryOperator<decltype("<"_token),   LeftAssociative, 9>;
  using LessEquals       = Impl::BinaryOperator<decltype("<="_token),  LeftAssociative, 9>;
  using Greater          = Impl::BinaryOperator<decltype(">"_token),   LeftAssociative, 9>;
  using GreaterEquals    = Impl::BinaryOperator<decltype(">="_token),  LeftAssociative, 9>;
  using Equals           = Impl::BinaryOperator<decltype("=="_token),  LeftAssociative, 10>;
  using NotEquals        = Impl::BinaryOperator<decltype("!="_token),  LeftAssociative, 10>;
  using BitAnd           = Impl::BinaryOperator<decltype("&"_token),   LeftAssociative, 11>;
  using BitXor           = Impl::BinaryOperator<decltype("^"_token),   LeftAssociative, 12>;
  using BitOr            = Impl::BinaryOperator<decltype("|"_token),   LeftAssociative, 13>;
  using LogicalAnd       = Impl::BinaryOperator<decltype("&&"_token),  LeftAssociative, 14>;
  using LogicalOr        = Impl::BinaryOperator<decltype("||"_token),  LeftAssociative, 15>;
  struct Ternary         {
                           inline constexpr static auto prec = 16;
                           using trigger = decltype("?"_token);
                           Impl::ExprRef cond, taken, notTaken;
                         };
  using Assignment       = Impl::BinaryOperator<decltype("="_token),   RightAssociative, 16>;
  using AddAssign        = Impl::BinaryOperator<decltype("+="_token),  RightAssociative, 16>;
  using SubAssign        = Impl::BinaryOperator<decltype("-="_token),  RightAssociative, 16>;
  using MulAssign        = Impl::BinaryOperator<decltype("*="_token),  RightAssociative, 16>;
  using DivAssign        = Impl::BinaryOperator<decltype("/="_token),  RightAssociative, 16>;
  using ModAssign        = Impl::BinaryOperator<decltype("%="_token),  RightAssociative, 16>;
  using ShiftLeftAssign  = Impl::BinaryOperator<decltype("<<="_token), RightAssociative, 16>;
  using ShiftRightAssign = Impl::BinaryOperator<decltype(">>="_token), RightAssociative, 16>;
  using BitAndAssign     = Impl::BinaryOperator<decltype("&="_token),  RightAssociative, 16>;
  using BitXorAssign     = Impl::BinaryOperator<decltype("^="_token),  RightAssociative, 16>;
  using BitOrAssign      = Impl::BinaryOperator<decltype("|="_token),  RightAssociative, 16>;
  struct Decl {
    Impl::ExprRef type;
    Tokens::Identifier ident;
    std::deque<Expression> arguments;
  };
  struct FunctionDecl {
    Impl::ExprRef retType;
    Tokens::Identifier ident;
    std::deque<Decl> parameters;
  };
  struct NoOp            {
                           inline constexpr static auto prec = 0;
                           using trigger = decltype(";"_token);
                         };

  namespace Impl {
    using ExpressionVar = std::variant<
        MemberAccess
      , Postincrement
      , Postdecrement
      , FCall
      , Subscript
      , Preincrement
      , Predecrement
      , UnaryPlus
      , UnaryMinus
      , UnaryNot
      , BitNot
      , Dereference
      , Addressof
      , MemberDeref
      , Multiplication
      , Division
      , Modulus
      , Addition
      , Subtraction
      , LeftShift
      , RightShift
      , Compare
      , Less
      , LessEquals
      , Greater
      , GreaterEquals
      , Equals
      , NotEquals
      , BitAnd
      , BitXor
      , BitOr
      , LogicalAnd
      , LogicalOr
      , Ternary
      , Assignment
      , AddAssign
      , SubAssign
      , MulAssign
      , DivAssign
      , ModAssign
      , ShiftLeftAssign
      , ShiftRightAssign
      , BitAndAssign
      , BitXorAssign
      , BitOrAssign
      , Tokens::Identifier
      , Tokens::Literal
      , Tokens::Number
      , Tokens::Float
      , Decl
      , FunctionDecl
      , NoOp
    >;
  }

  struct Expression: Impl::ExpressionVar {
    using Impl::ExpressionVar::ExpressionVar;
  };

  struct Statement;

  namespace Impl {
    using StmtRef = std::unique_ptr<Statement>;
  }

  struct IfStatement { Expression condition; Impl::StmtRef taken, notTaken; };
  struct ForStatement { Expression init, cond, iter; Impl::StmtRef body; };
  struct ExpressionStatment { Expression expr; };
  struct CompoundStatement { std::deque<Statement> stmts; };
  struct StructDeclaration { Tokens::Identifier ident; Impl::StmtRef contents; };
  struct DoWhile { Impl::StmtRef body; Expression cond; };
  struct ReturnStatement { Expression expr; };

  namespace Impl {
    using StatementVar = std::variant<
        IfStatement
      , ForStatement
      , ExpressionStatment
      , CompoundStatement
      , StructDeclaration
      , DoWhile
      , ReturnStatement
    >;
  }

  struct Statement: Impl::StatementVar {
    using Impl::StatementVar::StatementVar;
  };

  std::deque<Statement> makeAST(Tokens::ParserContext &ctx, std::ostream &os);

  std::ostream &operator<<(std::ostream &os, Impl::ExpressionVar const &exp);
  template<typename T>
  void printExprs(std::ostream &os, T const &exprs) {
    if(!std::distance(std::begin(exprs), std::end(exprs))) return;
    std::copy(std::begin(exprs), std::end(exprs) - 1, std::ostream_iterator<typename T::value_type>(os, ","));
    os << exprs.back();
  }

  template <typename Token, typename Assoc, int prec>
  std::ostream &operator<<(std::ostream &os, Impl::BinaryOperator<Token, Assoc, prec> const &bop) {
    os << '(' << *bop.lhs << Token::value << *bop.rhs << ')';
    return os;
  }

  template<typename Token, typename Assoc, int prec>
  std::ostream &operator<<(std::ostream &os, Impl::UnaryOperator<Token, Assoc, prec> const &uop) {
    if constexpr (std::is_same_v<Assoc, LeftAssociative>) {
      os << '(' << *uop.operand << Token::value << ')';
    } else {
      os << '(' << Token::value << *uop.operand << ')';
    }
    return os;
  }

  inline std::ostream &operator<<(std::ostream &os, Decl const &decl) {
    os << '[' << *decl.type << " <" << decl.ident.value << ">{";
    printExprs(os, decl.arguments);
    os << "}]";
    return os;
  }

  inline std::ostream &operator<<(std::ostream &os, FunctionDecl const &decl) {
    os << '[' << *decl.retType << " <" << decl.ident.value << ">(";
    printExprs(os, decl.parameters);
    os << ")]";
    return os;
  }

  inline std::ostream &operator<<(std::ostream &os, Impl::ExpressionVar const &exp) {
    std::visit(Overload{
        [&](NoOp)               { os << "<NoOp>"; }
      , [&](Ternary const &t)   { os << '(' << *t.cond << '?' << *t.taken << ':' << *t.notTaken << ')'; }
      , [&](FCall const &f)     { os << *f.callee << '('; printExprs(os, f.arguments); os << ')'; }
      , [&](Subscript const &s) { os << *s.callee << '['; printExprs(os, s.arguments); os << ']'; }
      , [&](auto const &op)     { os << op; }

      , [&](Tokens::Literal const &l)    { os << '"' << l.value << '"'; }
      , [&](Tokens::Number const &n)     { os << n.value << 'i'; }
      , [&](Tokens::Float const &n)      { os << n.value << 'f'; }
      , [&](Tokens::Identifier const &n) { os << '<' << n.value << '>'; }
    }, exp);
    return os;
  }

  std::ostream &operator<<(std::ostream &os, Impl::StatementVar const &stmt);
  template<typename T>
  void printStmts(std::ostream &os, T const &stmts) {
    std::copy(std::begin(stmts), std::end(stmts), std::ostream_iterator<typename T::value_type>(os));
  }

  inline std::ostream &operator<<(std::ostream &os, IfStatement const &fi) {
    os << "if (" << fi.condition << ") " << *fi.taken;
    if(fi.notTaken) os << " else " << *fi.taken;
    return os;
  }
  inline std::ostream &operator<<(std::ostream &os, ForStatement const &fr) {
    os << "for (" << fr.init << "; " << fr.cond << "; " << fr.iter << ") " << *fr.body;
    return os;
  }
  inline std::ostream &operator<<(std::ostream &os, ExpressionStatment const &fe) {
    os << fe.expr << ';';
    return os;
  }
  inline std::ostream &operator<<(std::ostream &os, CompoundStatement const &fc) {
    os << "{ "; printStmts(os, fc.stmts); os << " }";
    return os;
  }
  inline std::ostream &operator<<(std::ostream &os, StructDeclaration const &str) {
    os << "struct " << str.ident.value << " " << *str.contents;
    return os;
  }
  inline std::ostream &operator<<(std::ostream &os, DoWhile const &dw) {
    os << "do " << *dw.body << " while (" << dw.cond << ");";
    return os;
  }
  inline std::ostream &operator<<(std::ostream &os, ReturnStatement const &ret) {
    os << "return " << ret.expr << ';';
    return os;
  }
  inline std::ostream &operator<<(std::ostream &os, Impl::StatementVar const &stmt) {
    std::visit([&](auto const &stmt) { os << stmt; }, stmt);
    return os;
  }
}
