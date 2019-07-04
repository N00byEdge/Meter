#pragma once

#include <array>
#include <string>
#include <string_view>
#include <cstdint>

#include <optional>

namespace Meter::Unicode {
  using chr8 = std::uint8_t;
  using chr16 = std::uint16_t;
  using chr32 = std::uint32_t;

  template<typename T>
  using view = std::basic_string_view<T>;

  template<typename T>
  using str = std::basic_string<T>;

  using view8  = view<chr8>;
  using view16 = view<chr16>;
  using view32 = view<chr32>;
  using str8   = str<chr8>;
  using str16  = str<chr16>;
  using str32  = str<chr32>;

  template<int sz>
  constexpr std::array<chr8, sz> signify(std::int8_t (&arr)[sz]) {
    return { std::begin(arr), std::end(arr) };
  }

  template<int sz>
  constexpr std::array<chr32, sz> signify(std::int32_t (&arr)[sz]) {
    return { std::begin(arr), std::end(arr) };
  }

  namespace Strict {
    enum class ConversionError {
      InvalidContinuation,
      InvalidStart,
      OutOfBounds,
    };

    inline char const *errorName(ConversionError err) {
      switch(err) {
        case ConversionError::InvalidContinuation: return "Invalid continuation";
        case ConversionError::InvalidStart:        return "Invalid starting value";
        case ConversionError::OutOfBounds:         return "Out of bounds value";
        default:                                   return "Unknown error";
      }
    }

    template<typename inputView, typename outputStr>
    struct ConversionResult {
      outputStr output;
      decltype(std::declval<inputView>().begin()) errorLocation;
      decltype(std::declval<inputView>().begin()) parsedUntil;
      std::optional<ConversionError> error;
    };

    ConversionResult<view8,  str32> toCodePoints(view8 c8s);
    ConversionResult<view8,  str16> toUTF16(view8 c8s);

    ConversionResult<view16, str32> toCodePoints(view16 c16s);
    ConversionResult<view16, str8>  toUTF8(view16 c16s);

    ConversionResult<view32, str16> toUTF16(view32 c32s);
    ConversionResult<view32, str8>  toUTF8(view32 c32s);
  }

  namespace LooseyGoosey {
    str32 toCodePoints(view8 c8s);
    str16 toUTF16(view8 c8s);

    str32 toCodePoints(view16 c16s);
    str8  toUTF8(view16 c16s);

    str16 toUTF16(view32 c32s);
    str8  toUTF8(view32 c32s);
  }
}
