#pragma once

#include "Meter/Token.hh"

namespace Meter::Tokens {
  struct TokenizerContext {
    std::string_view view;
    std::string_view filename = "unknown";
    int columnLoc = 1;
    int rowLoc = 1;
    void cols(int num = 1) {
      columnLoc += num;
    }
    void rows(int num = 1) {
      columnLoc = 1;
      rowLoc += num;
    }
  };

  Meter::Tokens::Token consumeToken(TokenizerContext &context);
}
