#include "Meter/Identifier.hh"

std::string_view Meter::Identifier::name() const {
  return name_ptr->stored_string->data;
}
