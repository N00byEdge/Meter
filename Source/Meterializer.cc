#include "Meter/Language.hh"

#include "Meter/AST.hh"

#include <map>
#include <variant>
#include <string_view>

using namespace std::literals;

namespace {
  template<typename F>
  void feedNamedValues(Meter::Language::Scope const &scope, F &&f) {
    for(auto &v: scope.namedValues) {
      std::forward<F>(f)(v);
    }
  }

  auto iptrScope = std::make_shared<Meter::Language::Scope>();

  /*auto ListScope = std::make_shared<Meter::Language::Scope>({
    Meter::Language::NamedValue{-1, "List"}, Meter::Language::TypeRef{
      std::make_shared<Meter::Language::Scope>({
        Meter::Language::NamedValue{{-1, "length"}}
      })
    }
  });*/

  Meter::Language::Scope rootScope{{
    Meter::Language::NamedValue{{-1, "CompilerIntrinsics"}, Meter::Language::Scope{{
      Meter::Language::NamedValue{{-1, "void"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "f32"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "f64"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "i64"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "u64"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "iptr"}, Meter::Language::TypeRef{iptrScope}},
      Meter::Language::NamedValue{{-1, "uptr"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "Type"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "Function"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "Scope"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "Number"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "StringLiteral"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "Identifier"}, Meter::Language::TypeRef{nullptr}},
      Meter::Language::NamedValue{{-1, "Impl"}, Meter::Language::Scope{{

      }}}
    }}}
  }};

  //auto val = getIfType(*rootScope.namedValues.begin());
}

namespace Meter::Language {
  MeterState::MeterState(): states{{/*rootScope*/}} { }

  void Meterialize(MeterState &languageState, Statements statements) {
    
  }
}
