#include "med/SourceRenderer.hh"

#include "Meter/AST.hh"
#include "Meter/Dump.hh"

#include "imgui/imgui.h"

#include "med/Style.hh"

namespace {
  struct RendererImpl {
    RendererImpl(Style &style): style{style} { }

    void render(Meter::AST::CompoundStatement &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "CompoundStatement"); }
    void render(Meter::AST::ReturnStatement &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "ReturnStatement"); }
    void render(Meter::AST::DoWhile &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "DoWhile"); }
    void render(Meter::AST::StructDeclaration &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "StructDeclaration"); }

    void render(Meter::AST::ExpressionStatment &es) {
      render(es.expr);
      if(style.expression_statement_semicolons)
        noline(), ImGui::TextColored(style.text_color, ";");
    }

    void render(Meter::AST::IfStatement &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "IfStatement"); }
    void render(Meter::AST::FunctionDecl &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "FunctionDecl"); }
    void render(Meter::AST::Decl &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Decl"); }

    void render(Meter::Identifier &id) {
      ImGui::TextColored(style.identifier_color, "%s", id.name().data());
    }

    void render(Meter::AST::LiteralExpression &lit) {
      ImGui::TextColored(style.literal_color, "%s", lit.text.data());
    }

    void render(Meter::AST::Ternary &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Ternary"); }
    void render(Meter::AST::Subscript &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Subscript"); }
    void render(Meter::AST::FCall &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Function call"); }
    void render(Meter::AST::Addressof &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Addressof"); }
    void render(Meter::AST::Dereference &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Dereference"); }
    void render(Meter::AST::BitNot &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "BitNot"); }
    void render(Meter::AST::UnaryNot &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "UnaryNot"); }
    void render(Meter::AST::UnaryMinus &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "UnaryMinus"); }
    void render(Meter::AST::UnaryPlus &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "UnaryPlus"); }
    void render(Meter::AST::Predecrement &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Predecrement"); }

    void render(Meter::AST::Preincrement &pi) {
      ImGui::TextColored(style.text_color, "++");
      noline(), render(pi.operand.get());
    }

    void render(Meter::AST::Postdecrement &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Postdecrement"); }
    void render(Meter::AST::Postincrement &) { ImGui::TextColored(ImVec4(1, 0, 0, 1), "Postincrement"); }

    template<typename BOp> void render(Meter::AST::BinaryOperator<BOp> &bop) { ImGui::TextColored(ImVec4(1, 0, 0, 1), typeid(BOp).name()); }

    void render(Meter::AST::Less &lt) {
      render(lt.lhs.get());
      if(style.space_around_bops)
        noline(), ImGui::Text(" ");
      noline(), ImGui::TextColored(style.text_color, "<");
      if(style.space_around_bops)
        noline(), ImGui::Text(" ");
      noline(), render(lt.rhs.get());
    }

    void render(Meter::AST::Assignment &ass) {
      render(ass.lhs.get());
      if(style.space_around_bops)
        noline(), ImGui::Text(" ");
      noline(), ImGui::TextColored(style.text_color, "=");
      if(style.space_around_bops)
        noline(), ImGui::Text(" ");
      noline(), render(ass.rhs.get());
    }

    void noline() {
      ImGui::SameLine(0.f, 0.f);
    }

    void keyword(Style::Color const &color, char const *kw) {
      ImGui::TextColored(color, kw);
      if(style.space_after_statement)
        noline(), ImGui::Text(" ");
    }

    void render(Meter::AST::MemberAccess &ma) {
      render(ma.lhs.get());
      noline(), ImGui::TextColored(style.text_color, ".");
      noline(), render(ma.rhs.get());
    }

    void render(Meter::AST::NoOp &noop) {
      ImGui::Text("%s", ""); // To feed line in case there is a current one
    }

    void render(Meter::AST::Expression &ex) {
      ImGui::PushID(&ex);
      std::visit([&](auto &e) { render(e); }, ex);
      ImGui::PopID();
    }

    void render(Meter::AST::Expression *ex) {
      if(!ex) ImGui::TextColored(ImVec4(1, 0, 0, 1), "NULL EXPRESSION");
      else render(*ex);
    }

    void render(Meter::AST::ForStatement &fs) {
      keyword(style.for_color(), "for");
      noline(), ImGui::TextColored(style.text_color, "(");
      noline(), render(fs.init);
      noline(), ImGui::TextColored(style.text_color, "; ");
      noline(), render(fs.cond);
      noline(), ImGui::TextColored(style.text_color, "; ");
      noline(), render(fs.iter);
      noline(), ImGui::TextColored(style.text_color, ")");
      if(style.space_before_curly_brace)
        noline(), ImGui::Text(" ");
      if(!style.curly_braces_on_new_line)
        noline();

      ImGui::TextColored(style.text_color, "{");
      render(fs.body.get());
      ImGui::TextColored(style.text_color, "}");
    }

    void render(Meter::AST::Statement *stmt) {
      if(!stmt) ImGui::TextColored(ImVec4(1, 0, 0, 1), "NULL STATEMENT");
      else render(*stmt);
    }

    void render(Meter::AST::Statement &stmt) {
      ImGui::PushID(&stmt);
      std::visit([&](auto &s) { render(s); }, stmt);
      ImGui::PopID();
    }

  private:
    Style &style;
  };
}

void render_source(std::string const &name, Meter::Source &source, Style &style) {
  ImGui::Begin(name.c_str());

  {
    RendererImpl renderer{style};
    for(auto &stmt: source.ast) {
      renderer.render(stmt);
    }
  }
  if(ImGui::Button("Add for loop")) {
    Meter::AST::ForStatement fs{};
    auto i = source.add_identifier("best variable name");
    fs.init = Meter::AST::Assignment{
      Meter::AST::makeExpression(Meter::Identifier{i}),
      Meter::AST::makeExpression(Meter::AST::LiteralExpression{"0"}),
    };
    fs.cond = Meter::AST::Less {
      Meter::AST::makeExpression(Meter::Identifier{i}),
      Meter::AST::makeExpression(Meter::AST::LiteralExpression{"5"}),
    };
    fs.iter = Meter::AST::Preincrement {
      Meter::AST::makeExpression(Meter::Identifier{i}),
    };
    fs.body = Meter::AST::makeStatement(Meter::AST::ExpressionStatment{Meter::AST::NoOp{}});
    source.ast.emplace_back(std::move(fs));
  }
  if(ImGui::Button("Dump"))
    dump(source);
  ImGui::End();
}
