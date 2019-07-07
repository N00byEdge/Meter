#pragma once

#include "Meter/Types.hh"

#include <variant>
#include <string>
#include <queue>

namespace Meter::Tokens {
  struct TokenContext {
    std::string_view view;
    std::string_view filename;
    int line, columnStart, columnEnd;
  };

  struct EOF_T {
    inline static constexpr std::intptr_t length = 0;
    inline static constexpr char const value[] = "[END OF FILE]";
    TokenContext context;
  };
  inline EOF_T EndOfFile;

  namespace Impl {
    template<char ...c>
    struct tok_ {
      inline static constexpr std::intptr_t length = sizeof...(c);
      inline static constexpr char value[] = {c..., '\0'};
      TokenContext context;
    };
    template<typename Tag>
    struct VarLenToken {
      std::intptr_t length;
      std::string_view value;
      TokenContext context;
    };
  }

  struct Literal:    Impl::VarLenToken<Literal>{};
  struct Identifier: Impl::VarLenToken<Identifier>{
    bool operator<(Identifier const &other) const { return std::string_view{value} < other.value; }
  };
  struct Number:     Impl::VarLenToken<Number>{};
  struct Float:      Impl::VarLenToken<Float>{};

  namespace Impl {
    template<typename ...Ts>
    std::variant<
        EOF_T
      , Literal
      , Identifier
      , Number
      , Float
      , Ts ...> makeTokens(Ts ...);
  }

  namespace Literals {
    // GCC only, goal is for the language we're making to not need this :^)
    template<typename T, T... chars>
    constexpr Impl::tok_<chars...> operator""_token() { return {}; }
  }

  using namespace Literals;

  using Token = decltype(Impl::makeTokens(
      ";"_token
    , "."_token
    , ":"_token
    , ","_token
    , "+"_token
    , "-"_token
    , "*"_token
    , "/"_token
    , "%"_token
    , "?"_token
    , "->"_token
    , "("_token
    , ")"_token
    , "{"_token
    , "}"_token
    , "["_token
    , "]"_token
    , "<<"_token
    , ">>"_token
    , "&"_token
    , "|"_token
    , "^"_token
    , "<"_token
    , "<="_token
    , ">"_token
    , "~"_token
    , ">="_token
    , "&&"_token
    , "||"_token
    , "=="_token
    , "!="_token
    , "="_token
    , "!"_token
    , "++"_token
    , "--"_token
    , "+="_token
    , "-="_token
    , "/="_token
    , "*="_token
    , "%="_token
    , "%{"_token
    , "<<="_token
    , ">>="_token
    , "&="_token
    , "^="_token
    , "|="_token
    , "<=>"_token
    , "for"_token
    , "if"_token
    , "else"_token
    , "while"_token
    , "struct"_token
    , "do"_token
    , "return"_token
    , "scope_exit"_token
    , "scope_fail"_token
    , "scope_success"_token
    , "=>"_token
  ));

  inline std::string_view tokenName(Token const &tok) {
    return std::visit(Overload{
        [](auto op)          { return decltype(op)::value; }
      , [](Literal lit)      { return "literal";   }
      , [](Number num)       { return "number";    }
      , [](Float flt)        { return "float";     }
      , [](Identifier ident) { return "identifier"; }
    }, tok);
  }

  inline std::string_view tokenValue(Token const &tok) {
    return std::visit([](auto op) -> std::string_view { return op.value; }, tok);
  }
}
