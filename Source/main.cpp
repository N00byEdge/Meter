#include "Meter/AST.hh"
#include "Meter/Language.hh"

#include <fstream>

template<typename T>
std::string loadFile(T &&v) {
  std::ifstream is{v};
  std::string s{std::istreambuf_iterator<char>{is}, {}};
  return s;
}

#include <iostream>

template<typename T>
void test(T &&v) {
  Meter::Language::Scope sc;

  auto s = loadFile(v);
  Meter::Tokens::ParserContext ctx{s.c_str()};
  while(1) {
    auto token = ctx.consume();
    std::visit([](auto token){
      if constexpr(std::is_same_v<decltype(token), Meter::Tokens::Literal>) {
        std::cout << "\nLiteral: \"" << token.value << "\", bytes: " << token.value.size() << std::endl;
      } else {
        std::cout << token.value << ' ' << std::flush;
      }
    }, token);
    if(std::holds_alternative<Meter::Tokens::EOF_T>(token))
      break;
  }
  std::cout << '\n';
  ctx = Meter::Tokens::ParserContext{s.c_str()};
  auto ast = Meter::AST::makeAST(ctx, std::cerr);
  for(auto &o: ast) {
    std::cout << o << '\n';
  }
}

int main() {
  test("Tests/Unicode.met");
  test("Tests/Maffs.met");
  test("Tests/Struct.met");
  test("Tests/EdgeCases.met");
}
