#pragma once

#include "Meter/Types.hh"

#include <variant>
#include <string>
#include <queue>

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
  struct Identifier: Impl::VarLenToken<std::string_view, Identifier>{
    bool operator<(Identifier const &other) const { return std::string_view{value} < other.value; }
  };
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

  Meter::Tokens::Token consumeToken(char const *&s);

  struct ParserContext {
    char const *file;
    std::deque<Meter::Tokens::Token> tokenQueue;

    template<int idx = 0>
    void ensureQueued() {
      while(tokenQueue.size() <= idx) {
        if (!file)
          tokenQueue.push_back(Meter::Tokens::EndOfFile);

        tokenQueue.emplace_back(Meter::Tokens::consumeToken(file));
        if(std::holds_alternative<Meter::Tokens::EOF_T>(tokenQueue.back()))
          file = nullptr;
      }
    }

    template<int numAhead = 0>
    [[nodiscard]] Meter::Tokens::Token &lookahead() {
      ensureQueued<numAhead>();
      return tokenQueue[numAhead];
    }

    template<int numAhead = 0, typename TokenT>
    [[nodiscard]] bool lookaheadMatch(TokenT = TokenT{}) {
      return std::holds_alternative<TokenT>(lookahead<numAhead>());
    }

    template<typename TokenT>
    bool tryPop(TokenT = TokenT{}) {
      if(lookaheadMatch<0, TokenT>()) {
        pop();
        return true;
      }
      return false;
    }

    void pop() {
      ensureQueued();
      tokenQueue.pop_front();
    }

    [[nodiscard]] Meter::Tokens::Token consume() {
      auto ret = lookahead();
      pop();
      return ret;
    }
  };
}
