#pragma once

#include <iostream>
#include <string_view>

#include "Meter/Identifier.hh"
#include "Meter/AST.hh"
#include "Meter/Impl/String.hh"

namespace Meter {
  struct Source {
    void serialize(std::ostream &os);

    static Source deserialize(std::istream &is);

    Identifier add_identifier(std::string_view name);

    Meter::AST::Statements ast;
    StringStore strings;
  };
}
