#pragma once

#include <string_view>
#include <memory>

#include "Meter/Impl/String.hh"

namespace Meter {
  struct Source;

  /* References an identifier in program source */
  struct Identifier {
    Identifier(std::shared_ptr<StringRef> name): name_ptr{name} { }

    void rename(std::string_view sv) { name_ptr->change_string(sv); }

    std::string_view name() const;

  private:
    std::shared_ptr<StringRef> name_ptr;
  };
}
