#pragma once

#include "Meter/Token.hh"

namespace Meter {
  struct SyntaxError {
    Meter::Tokens::TokenContext context;
    std::string_view errorMessage;

    void print();
  };
}
