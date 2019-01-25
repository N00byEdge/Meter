#pragma once

#include <variant>
#include <string>

namespace Meter::Tokens {
  struct TokenContext { int line; int column; };

  struct EOF_T{
    inline static constexpr std::intptr_t length = 0;
    inline static constexpr char value[] = "[END OF FILE]";
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
    template<typename T, typename Tag>
    struct VarLenToken {
      std::intptr_t length;
      T value;
      TokenContext context;
    };
  }

  struct Literal:    Impl::VarLenToken<std::string_view, Literal>{};
  struct Identifier: Impl::VarLenToken<std::string_view, Identifier>{};
  struct Number:     Impl::VarLenToken<std::string_view, Number>{};
  struct Float:      Impl::VarLenToken<std::string_view, Float>{};

  namespace Impl {
    template<typename ...Ts>
    std::variant<
        Literal
      , Identifier
      , Number
      , Float
      , EOF_T
      , Ts ...> makeTokens(Ts ...);
  }

  namespace Literals {
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
  ));

  Meter::Tokens::Token consumeToken(char const *&s);
}
