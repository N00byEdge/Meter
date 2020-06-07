#include "Meter/Impl/String.hh"

using StoredString = Meter::Impl::StoredString;

namespace {
  [[maybe_unused]] std::string_view get_string(std::shared_ptr<StoredString> const &sp) { return sp->data; }
  [[maybe_unused]] std::string_view get_string(StoredString const &ss) { return ss.data; }
  [[maybe_unused]] std::string_view get_string(std::string const &str) { return str; }
  [[maybe_unused]] std::string_view get_string(std::string_view const &view) { return view; }
}

template<typename Lhs, typename Rhs>
bool Meter::StoreCompare::operator()(Lhs const &lhs, Rhs const &rhs) const {
  return get_string(lhs) < get_string(rhs);
}

Meter::StringRef Meter::StringStore::reference_string(std::string_view sv) {
  auto it = strings.find(sv);
  if(it == strings.end()) {
    auto str = std::make_shared<StoredString>(std::string{sv}, *this);
    strings.insert(str);
    return {std::move(str)};
  }
  return {(*it)->shared_from_this()};
}

namespace {
  void cleanup(std::shared_ptr<StoredString> str) {
    str->store.strings.erase(str);
  }
}

namespace {
  void cleanup(Meter::StringRef &sr) {
    // If only this and the StringStore references it, kill it with fire.
    if(sr.stored_string.use_count() == 2) {
      cleanup(sr.stored_string);
    }
    sr.stored_string.reset();
  }
}

Meter::StringRef::~StringRef() {
  cleanup(*this);
}

void Meter::StringRef::change_string(std::string_view sv) {
  if(stored_string.use_count() == 2) { // Only us using the string, we can change it
    stored_string->data = sv;
  } else {
    auto &store = stored_string->store;
    cleanup(*this);
    *this = store.reference_string(sv);
  }
}
