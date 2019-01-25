#pragma once

#include "Meter/Token.hh"

#include <memory>
#include <deque>
#include <iostream>

namespace Meter::AST {
  class Expression;
  namespace Impl {
    using ExprRef = std::unique_ptr<Expression>;

    template<typename Token, typename Associativity>
    struct BinaryOperator {
      using tok = Token;
      using assoc = Associativity;
      ExprRef lhs, rhs;
    };

    template<typename Token, typename Associativity>
    struct UnaryOperator {
      using tok = Token;
      using assoc = Associativity;
      ExprRef operand;
    };
  }

  struct LeftAssociative;
  struct RightAssociative;
  using namespace Meter::Tokens::Literals;
  using MemberAccess     = Impl::BinaryOperator<decltype("."_token),   LeftAssociative>;
  using Postincrement    = Impl::UnaryOperator <decltype("++"_token),  LeftAssociative>;
  using Postdecrement    = Impl::UnaryOperator <decltype("--"_token),  LeftAssociative>;
  using Preincrement     = Impl::UnaryOperator <decltype("++"_token),  RightAssociative>;
  using Predecrement     = Impl::UnaryOperator <decltype("--"_token),  RightAssociative>;
  using UnaryPlus        = Impl::UnaryOperator <decltype("+"_token),   RightAssociative>;
  using UnaryMinus       = Impl::UnaryOperator <decltype("-"_token),   RightAssociative>;
  using UnaryNot         = Impl::UnaryOperator <decltype("!"_token),   RightAssociative>;
  using BitNot           = Impl::UnaryOperator <decltype("~"_token),   RightAssociative>;
  using Dereference      = Impl::UnaryOperator <decltype("*"_token),   RightAssociative>;
  using Addressof        = Impl::UnaryOperator <decltype("&"_token),   RightAssociative>;
  using MemberDeref      = Impl::BinaryOperator<decltype("->"_token),  LeftAssociative>;
  using Multiplication   = Impl::BinaryOperator<decltype("*"_token),   LeftAssociative>;
  using Division         = Impl::BinaryOperator<decltype("/"_token),   LeftAssociative>;
  using Modulus          = Impl::BinaryOperator<decltype("%"_token),   LeftAssociative>;
  using Addition         = Impl::BinaryOperator<decltype("+"_token),   LeftAssociative>;
  using Subtraction      = Impl::BinaryOperator<decltype("-"_token),   LeftAssociative>;
  using LeftShift        = Impl::BinaryOperator<decltype("<<"_token),  LeftAssociative>;
  using RightShift       = Impl::BinaryOperator<decltype(">>"_token),  LeftAssociative>;
  using Compare          = Impl::BinaryOperator<decltype("<=>"_token), LeftAssociative>;
  using Less             = Impl::BinaryOperator<decltype("<"_token),   LeftAssociative>;
  using LessEquals       = Impl::BinaryOperator<decltype("<="_token),  LeftAssociative>;
  using Greater          = Impl::BinaryOperator<decltype(">"_token),   LeftAssociative>;
  using GreaterEquals    = Impl::BinaryOperator<decltype(">="_token),  LeftAssociative>;
  using Equals           = Impl::BinaryOperator<decltype("=="_token),  LeftAssociative>;
  using NotEquals        = Impl::BinaryOperator<decltype("!="_token),  LeftAssociative>;
  using BitAnd           = Impl::BinaryOperator<decltype("&"_token),   LeftAssociative>;
  using BitXor           = Impl::BinaryOperator<decltype("^"_token),   LeftAssociative>;
  using BitOr            = Impl::BinaryOperator<decltype("|"_token),   LeftAssociative>;
  using LogicalAnd       = Impl::BinaryOperator<decltype("&&"_token),  LeftAssociative>;
  using LogicalOr        = Impl::BinaryOperator<decltype("||"_token),  LeftAssociative>;
  struct Ternary         {
                           using tok = decltype("?");
                           using assoc = RightAssociative;
                           Impl::ExprRef cond, taken, notTaken;
                         };
  using Assignment       = Impl::BinaryOperator<decltype("="_token),   RightAssociative>;
  using AddAssign        = Impl::BinaryOperator<decltype("+="_token),  RightAssociative>;
  using SubAssign        = Impl::BinaryOperator<decltype("-="_token),  RightAssociative>;
  using MulAssign        = Impl::BinaryOperator<decltype("*="_token),  RightAssociative>;
  using DivAssign        = Impl::BinaryOperator<decltype("/="_token),  RightAssociative>;
  using ModAssign        = Impl::BinaryOperator<decltype("%="_token),  RightAssociative>;
  using ShiftLeftAssign  = Impl::BinaryOperator<decltype("<<="_token), RightAssociative>;
  using ShiftRightAssign = Impl::BinaryOperator<decltype(">>="_token), RightAssociative>;
  using BitAndAssign     = Impl::BinaryOperator<decltype("&="_token),  RightAssociative>;
  using BitXorAssign     = Impl::BinaryOperator<decltype("^="_token),  RightAssociative>;
  using BitOrAssign      = Impl::BinaryOperator<decltype("|="_token),  RightAssociative>;
  struct NoOp            { };

  namespace Impl {
    using ExpressionVar = std::variant<
        MemberAccess
      , Postincrement
      , Postdecrement
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
  struct CompoundStatement { std::deque<Impl::StmtRef> stmts; };

  namespace Impl {
    using StatementVar = std::variant<
        IfStatement
      , ForStatement
      , ExpressionStatment
      , CompoundStatement
    >;
  }

  struct Statement: Impl::StatementVar {
    using Impl::StatementVar::StatementVar;
  };

  std::deque<Statement> makeAST(char const *&, std::ostream &os);
}
