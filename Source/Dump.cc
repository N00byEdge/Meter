#include "Meter/AST.hh"
#include "Meter/Dump.hh"
#include "Meter/Source.hh"
#include "Meter/Overload.hh"
#include "Meter/Impl/String.hh"

#include <iostream>

namespace Dump {
  namespace {
    void dump(std::string_view const &str) {
      std::cerr << str;
    }

    template<typename T1, typename T2, typename ...Ts>
    void dump(T1 const &v1, T2 const &v2, Ts &&...vs);

    void dump(Meter::AST::Expression const &);
    void dump(Meter::AST::Statement const &);

    template<typename BOp>
    void dump(Meter::AST::BinaryOperator<BOp> const &bop) {
      dump(typeid(BOp).name(), "(", *bop.lhs, ", ", *bop.rhs, ")");
    }

    template<typename UOp>
    void dump(Meter::AST::UnaryOperator<UOp> const &uop) {
      dump(typeid(UOp).name(), "(", *uop.operand, ")");
    }

    void dump(Meter::AST::LiteralExpression const &lit) {
      dump("_", lit.text, "_");
    }

    void dump(Meter::Identifier const &ident) {
      dump("'", ident.name(), "'");
    }

    void dump(Meter::AST::Statements const &statements) {
      for(auto &stmt: statements) dump(stmt);
    }

    void dump(Meter::AST::FunctionDeclParameter const &decl) {
      dump(decl.ident, " [", *decl.type, "] = ", *decl.default_value);
    };

    void dump(Meter::AST::FunctionDeclParameters const &params) {
      for(auto &p: params) dump(p);
    };

    void dump(Meter::AST::Decl const &decl) {
      dump(decl.ident, " [", *decl.type, "] = ", *decl.initValue);
    }

    void dump(Meter::AST::FunctionDecl const &decl) {
      dump("[", *decl.retType, decl.ident, "](", decl.parameters, ")]", *decl.funcBody);
    }

    void dump(Meter::AST::NoOp const &noop) {
      dump("<NoOp>");
    }

    void dump(Meter::AST::Ternary const &t) {
      dump("(", *t.cond, "?", *t.taken, ":", *t.notTaken, ")");
    }

    void dump(Meter::AST::Subscript const &s) {
      dump(*s.callee, "[", s.arguments, "]");
    }

    void dump(Meter::AST::PositionalCallParameter const &param) {
      dump("{", *param.value, "}");
    } 

    void dump(Meter::AST::PositionalCallParameters const &params) {
      for(auto &p: params) dump(p);
    }

    void dump(Meter::AST::NominalCallParameter const &param) {
      dump(param.ident, " = {", *param.value, "}");
    }

    void dump(Meter::AST::NominalCallParameters const &params) {
      for(auto &p: params) dump(p);
    }

    void dump(Meter::AST::FCall const &f) {
      dump(*f.callee, "[", f.arguments, "]");
    }

    void dump(Meter::AST::ParameterList const &pl) {
      dump("[Pos:", pl.positional_parameters, "], [Nom:", pl.nominal_parameters, "]");
    }

    void dump(Meter::AST::CallParamaterList const &pl) {
      dump("[Pos: ", pl.positional_parameters, "], [Nom: ", pl.nominal_parameters, "]");
    }

    void dump(Meter::AST::IfStatement const &fi) {
      dump("if (", fi.condition, ") ", *fi.taken);
      if(fi.notTaken)
        dump(*fi.notTaken);
    }

    void dump(Meter::AST::ForStatement const &fr) {
      dump("for (", fr.init, "; ", fr.cond, "; ", fr.iter, ") ", *fr.body);
    }

    void dump(Meter::AST::ExpressionStatment const &es) {
      dump(es.expr, ";");
    }

    void dump(Meter::AST::CompoundStatement const &cs) {
      dump("{ ", cs.stmts, " }");
    }

    void dump(Meter::AST::StructDeclaration const &sd) {
      dump("struct ", sd.ident, " ", *sd.contents);
    }

    void dump(Meter::AST::DoWhile const &dw) {
      dump("do ", *dw.body, " while (", dw.cond, ")");
    }

    void dump(Meter::AST::ReturnStatement const &rs) {
      dump("return ", rs.expr, ";");
    }

    void dump(Meter::AST::Expression const &expr) {
      std::visit([](auto const &a) { dump(a); }, expr);
    }

    void dump(Meter::AST::Statement const &stmt) {
      std::visit([](auto const &a) { dump(a); }, stmt);
    }

    /*
    std::ostream &operator<<(std::ostream &os, Meter::AST::CompoundStatement const &fc) {
      os << "{ "; printStmts(os, fc.stmts); os << " }";
      return os;
    }

    void dump(Meter::AST::Statement const &stmt) {
      std::visit([](auto const &val) { std::cerr << val; });
      std::cerr << ";\n";
    }

    void dump(Meter::AST::Statements const &stmts) {
      std::cerr << "AST:\n";
      std::cerr << "{\n";
      for(auto &s: stmts) {
        dump(s);
      }
      std::cerr << "}\n";
    }
    */

    void dump(Meter::StringStore const &ss) {
      std::cerr << "STRINGS: \n";
      for(auto &str: ss.strings) {
        std::cerr << "\t\t[" << str.use_count() - 1 << "]: " << str->data << "\n";
      }
    }

    template<typename T1, typename T2, typename ...Ts>
    void dump(T1 const &v1, T2 const &v2, Ts &&...vs) {
      dump(v1); dump(v2);
      (dump(vs),...);
    }
  }
}

void Meter::dump(Meter::Source const &source) {
  std::cerr << "SOURCE DUMP\n";
  Dump::dump(source.strings);
  Dump::dump(source.ast);
}
