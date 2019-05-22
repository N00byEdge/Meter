#pragma once

#include "Meter/Token.hh"
#include "Meter/AST.hh"

#include <variant>
#include <set>
#include <cstdint>
#include <memory>
#include <functional>

namespace Meter::Language {
  using Identifier = Meter::Tokens::Identifier;
  using Statements = Meter::AST::Statements;
  struct NamedValue;

  struct Scope {
    std::set<NamedValue, std::less<>> namedValues;
  };

  namespace Impl {
    template<typename T>
    struct FloatTraits;

    template<>
    struct FloatTraits<float> {
      static constexpr int exponentBits = 8;
      static constexpr int mantissaBits = 23;
      static_assert(1 + exponentBits + mantissaBits == sizeof(float) * 8);
    };

    template<>
    struct FloatTraits<double> {
      static constexpr int exponentBits = 11;
      static constexpr int mantissaBits = 52;
      static_assert(1 + exponentBits + mantissaBits == sizeof(double) * 8);
    };
  }

  template<int sz>
  auto uintOfAtLeastSize() {
    if constexpr(sz < 2) return std::uint8_t{};
    if constexpr(sz < 3) return std::uint16_t{};
    if constexpr(sz < 5) return std::uint32_t{};
    if constexpr(sz < 9) return std::uint64_t{};
  }

  template<typename T>
  struct FloatTraits: Impl::FloatTraits<T> {
    using IntT = decltype(uintOfAtLeastSize<sizeof(T)>());
    static constexpr IntT exponentBias =
      (1 << (Impl::FloatTraits<T>::exponentBits - 1)) - 1;
  };

  template<typename T, typename = void>
  struct BuiltinType {
    mutable T value;
  };

  template<typename T>
  struct BuiltinType<T, std::enable_if<std::is_floating_point_v<T>>> {
    struct Repr {
      typename FloatTraits<T>::IntT sign     : 1;
      typename FloatTraits<T>::IntT exponent : FloatTraits<T>::exponentBits;
      typename FloatTraits<T>::IntT mantissa : FloatTraits<T>::mantissaBits;
    };
    
    union {
      mutable Repr repr;
      mutable T value;
    };

    auto sign() { return repr.sign ? -1 : 1; }
    auto exponent() { return repr.exponent - FloatTraits<T>::exponentBias; }
    auto mantissa() { return repr.mantissa; }
  };

  template<typename T>
  struct BuiltinType<void, T> {
    mutable std::monostate value;
  };

  struct TypeRef {
    mutable std::shared_ptr<Scope const> value;
  };

  struct TypeValue: TypeRef {
    mutable Scope members;
  };

  struct FunctionType {
    mutable TypeRef returnType;
    mutable std::set<NamedValue> arguments;
  };

  using Value = std::variant<FunctionType
                           , Scope
                           , TypeRef
                           , TypeValue
                           , BuiltinType<std::uintptr_t>
                           , BuiltinType<std::intptr_t>
                           , BuiltinType<void>
                           , BuiltinType<float>
                           , BuiltinType<double>
                           , BuiltinType<Meter::Tokens::Number>
                           , BuiltinType<Meter::Tokens::Literal>
                           , BuiltinType<Meter::Language::Identifier>
  >;

  using MeterFunction = std::function<Meter::Language::Value(std::set<NamedValue>)>;

  struct NamedValue: Identifier {
    std::variant<TypeRef, Value> value;
  };

  struct MeterState {
    MeterState();
    Scope &getModuleScope() { return states.back().modules; }
  private:
    friend struct ExtraState;
    struct InternalState {
      Scope modules;
    };
    std::deque<InternalState> states;

    struct ExtraState {
      ExtraState(MeterState &met): language{met} {
        language.states.emplace_back(language.states.back());
      }

      ~ExtraState() {
        if (commit) {
          language.states.pop_front();
        } else {
          language.states.pop_back();
        }
      }

      ExtraState(ExtraState const &) = delete;
      ExtraState(ExtraState &&) = default;

      bool commit = false;
    private:
      MeterState &language;
    };
  };

  void Meterialize(MeterState &languageState, Statements statements);
}
