#pragma once

#include "Meter/Token.hh"

#include <memory>
#include <deque>
#include <iostream>

namespace Meter::AST {
  class Expression;
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

  struct LeftAssociative;
  struct RightAssociative;
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
