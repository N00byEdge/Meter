#pragma once

#include <memory>
#include <iterator>
#include <deque>
#include <vector>
#include <variant>

#include "Meter/Identifier.hh"
#include "Meter/Overload.hh"

namespace Meter::AST {
  namespace Impl {
    struct Expression;
    struct Statement;

    using ExprRef = std::unique_ptr<Expression>;
    using StmtRef = std::unique_ptr<Statement>;
  }

  template<typename tag>
  struct BinaryOperator {
    Impl::ExprRef lhs, rhs;
  };

  template<typename tag>
  struct UnaryOperator {
    Impl::ExprRef operand;
  };

  using Statements = std::deque<Impl::Statement>;
  using Expressions = std::deque<Impl::Expression>;

  struct NominalCallParameter {
    Identifier ident;
    Impl::ExprRef value;
  };

  struct PositionalCallParameter {
    Impl::ExprRef value;
  };

  using NominalCallParameters = std::deque<NominalCallParameter>;
  using PositionalCallParameters = std::deque<PositionalCallParameter>;

  struct CallParamaterList {
    PositionalCallParameters positional_parameters;
    NominalCallParameters nominal_parameters;
  };

  template<typename T>
  bool isUnaryOp = false;
  template<typename tag>
  bool isUnaryOp<UnaryOperator<tag>> = true;

  template<typename T>
  bool isBinaryOp = false;
  template<typename tag>
  bool isBinaryOp<BinaryOperator<tag>> = true;

  struct MemberAccess     : BinaryOperator<MemberAccess> { };
  struct Postincrement    : UnaryOperator <Postincrement> { };
  struct Postdecrement    : UnaryOperator <Postdecrement> { };
  struct FCall            {
                            Impl::ExprRef callee;
                            CallParamaterList arguments;
                          };
  struct Subscript        {
                            Impl::ExprRef callee;
                            CallParamaterList arguments;
                          };
  struct Preincrement     : UnaryOperator <Preincrement> { };
  struct Predecrement     : UnaryOperator <Predecrement> { };
  struct UnaryPlus        : UnaryOperator <UnaryPlus> { };
  struct UnaryMinus       : UnaryOperator <UnaryMinus> { };
  struct UnaryNot         : UnaryOperator <UnaryNot> { };
  struct BitNot           : UnaryOperator <BitNot> { };
  struct Dereference      : UnaryOperator <Dereference> { };
  struct Addressof        : UnaryOperator <Addressof> { };
  struct MemberDeref      : BinaryOperator<MemberDeref> { };
  struct Multiplication   : BinaryOperator<Multiplication> { };
  struct Division         : BinaryOperator<Division> { };
  struct Modulus          : BinaryOperator<Modulus> { };
  struct Addition         : BinaryOperator<Addition> { };
  struct Subtraction      : BinaryOperator<Subtraction> { };
  struct LeftShift        : BinaryOperator<LeftShift> { };
  struct RightShift       : BinaryOperator<RightShift> { };
  struct Compare          : BinaryOperator<Compare> { };
  struct Less             : BinaryOperator<Less> { };
  struct LessEquals       : BinaryOperator<LessEquals> { };
  struct Greater          : BinaryOperator<Greater> { };
  struct GreaterEquals    : BinaryOperator<GreaterEquals> { };
  struct Equals           : BinaryOperator<Equals> { };
  struct NotEquals        : BinaryOperator<NotEquals> { };
  struct BitAnd           : BinaryOperator<BitAnd> { };
  struct BitXor           : BinaryOperator<BitXor> { };
  struct BitOr            : BinaryOperator<BitOr> { };
  struct LogicalAnd       : BinaryOperator<LogicalAnd> { };
  struct LogicalOr        : BinaryOperator<LogicalOr> { };
  struct Ternary          {
                            Impl::ExprRef cond, taken, notTaken;
                          };
  struct Assignment       : BinaryOperator<Assignment> { };
  struct AddAssign        : BinaryOperator<AddAssign> { };
  struct SubAssign        : BinaryOperator<SubAssign> { };
  struct MulAssign        : BinaryOperator<MulAssign> { };
  struct DivAssign        : BinaryOperator<DivAssign> { };
  struct ModAssign        : BinaryOperator<ModAssign> { };
  struct ShiftLeftAssign  : BinaryOperator<ShiftLeftAssign> { };
  struct ShiftRightAssign : BinaryOperator<ShiftRightAssign> { };
  struct BitAndAssign     : BinaryOperator<BitAndAssign> { };
  struct BitXorAssign     : BinaryOperator<BitXorAssign> { };
  struct BitOrAssign      : BinaryOperator<BitOrAssign> { };

  struct Decl {
    Impl::ExprRef type;
    Identifier ident;
    Impl::ExprRef initValue;
  };

  struct FunctionDeclParameter {
    Impl::ExprRef type;
    Identifier ident;
    Impl::ExprRef default_value;
  };

  using FunctionDeclParameters = std::deque<FunctionDeclParameter>;

  struct ParameterList {
    FunctionDeclParameters positional_parameters;
    FunctionDeclParameters nominal_parameters;
  };

  struct FunctionDecl {
    Impl::ExprRef retType;
    Identifier ident;
    ParameterList parameters;
    Impl::StmtRef funcBody;
  };

  struct NoOp { };

  struct LiteralExpression {
    std::string_view text;
  };

  struct CompoundStatement { Statements stmts; };
  
  using Expression = std::variant<
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
    , Identifier
    , LiteralExpression
    , Decl
    , FunctionDecl
    , NoOp
  >;

  namespace Impl {
    struct Expression: Meter::AST::Expression {
      using Meter::AST::Expression::Expression;
      Expression(Meter::AST::Expression &&sv): Meter::AST::Expression{std::move(sv)} { }
    };
  }

  inline auto makeExpression(Meter::AST::Expression &&e) {
    return std::make_unique<Meter::AST::Impl::Expression>(std::move(e));
  }

  struct IfStatement { Expression condition; Impl::StmtRef taken, notTaken; };
  struct ForStatement { Expression init, cond, iter; Impl::StmtRef body; };
  struct ExpressionStatment { Expression expr; };
  struct StructDeclaration { Identifier ident; Impl::StmtRef contents; };
  struct DoWhile { Impl::StmtRef body; Expression cond; };
  struct ReturnStatement { Expression expr; };

  using Statement = std::variant<
      IfStatement
    , ForStatement
    , ExpressionStatment
    , CompoundStatement
    , StructDeclaration
    , DoWhile
    , ReturnStatement
  >;

  namespace Impl {
    struct Statement: Meter::AST::Statement {
      using Meter::AST::Statement::Statement;
      Statement(Meter::AST::Statement &&sv): Meter::AST::Statement{std::move(sv)} { }
    };
  }

  inline auto makeStatement(Meter::AST::Statement &&s) {
    return std::make_unique<Meter::AST::Impl::Statement>(std::move(s));
  }

  using Statement = std::variant<
      IfStatement
    , ForStatement
    , ExpressionStatment
    , CompoundStatement
    , StructDeclaration
    , DoWhile
    , ReturnStatement
  >;
}
