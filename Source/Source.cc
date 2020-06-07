#include "Meter/Source.hh"

Meter::Identifier Meter::Source::add_identifier(std::string_view name) {
  auto string_ref = std::make_shared<Meter::StringRef>(strings.reference_string(name));
  return Meter::Identifier{string_ref};
}
