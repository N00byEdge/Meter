#include "Meter/CompileError.hh"

#include <iostream>

void Meter::SyntaxError::print() {
  std::cerr << "\033[0;31mError: "
    << context.filename << ":"
    << context.line << ":"
    << context.columnStart << "-" << context.columnEnd << ": "
    << errorMessage << "\033[0m\n";
}