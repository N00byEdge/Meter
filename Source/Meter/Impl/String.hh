#pragma once

#include <set>
#include <memory>
#include <string>

namespace Meter {
  using StringID = unsigned long long;

  struct StringStore;

  namespace Impl {
    struct StoredString: std::enable_shared_from_this<StoredString> {
      StoredString(std::string data, StringStore &store)
        : data{data}, store{store} { }
      std::string data;
      StringStore &store;
    };
  }

  struct StringRef {
    ~StringRef();
    std::shared_ptr<Impl::StoredString> stored_string;
    void change_string(std::string_view str);
  };

  struct StoreCompare {
    template<typename Lhs, typename Rhs>
    bool operator()(Lhs const &, Rhs const &) const;

    using is_transparent = void *;
  };

  struct StringStore {
    std::set<std::shared_ptr<Impl::StoredString>, struct StoreCompare> strings;
    StringRef reference_string(std::string_view sv);
  };
}
