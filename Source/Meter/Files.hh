#pragma once

#include <string>
#include <fstream>
#include <iostream>

#include "Meter/Unicode.hh"

namespace Meter::Files {
  template<typename T>
  [[nodiscard]] auto loadFile(T &&v) {
    std::ifstream is{v};
    Unicode::str8 s{std::istreambuf_iterator<char>{is}, {}};
    return s;
  }
}
