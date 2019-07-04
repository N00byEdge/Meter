#pragma once

#include "Meter/Token.hh"

namespace Meter::Tokens {
  struct TokenizerContext {
    std::string_view view;
    int columnLoc = 0;
    int rowLoc = 0;
    void cols(int num = 1) {
      columnLoc += num;
    }
    void rows(int num = 1) {
      columnLoc = 0;
      rowLoc += num;
    }
  };

  Meter::Tokens::Token consumeToken(TokenizerContext &context);
}
