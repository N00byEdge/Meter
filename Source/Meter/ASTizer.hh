#include "Meter/Tokenizer.hh"
#include "Meter/AST.hh"

namespace Meter::AST {
  struct ParserContext {
    Tokens::TokenizerContext inputData;
    std::deque<Meter::Tokens::Token> tokenQueue;

    template<int idx = 0>
    void ensureQueued() {
      while(tokenQueue.size() <= idx) {
        if (inputData.view.empty()) tokenQueue.emplace_back(Meter::Tokens::EndOfFile);
        else                        tokenQueue.emplace_back(Meter::Tokens::consumeToken(inputData));
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

  Meter::AST::Statements makeAST(ParserContext &ctx, std::ostream &os);
}
