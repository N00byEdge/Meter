#include "Meter/Token.hh"
#include "Meter/AST.hh"

#include <fstream>

template<typename T>
std::string loadFile(T &&v) {
  std::ifstream is{v};
  std::string s{std::istreambuf_iterator<char>{is}, {}};
  return s;
}

#include <queue>
#include <iostream>

template<typename T>
void test(T &&v) {
  std::deque<Meter::Tokens::Token> tokens;
  auto s = loadFile(v);
  auto fp = s.c_str();
  while(1) {
    auto &token = tokens.emplace_back(Meter::Tokens::consumeToken(fp));
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
}

int main() {
  test("Tests/Unicode.met");
  test("Tests/Maffs.met");
  test("Tests/Struct.met");
  test("Tests/EdgeCases.met");
}
