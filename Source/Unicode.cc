#include "Meter/Unicode.hh"

namespace Meter::Unicode {
  namespace utf8 {
    constexpr std::uint8_t contMask = 0xc0;
  }

  constexpr bool isContinuation(std::uint8_t val) {
    return (val & utf8::contMask) == 0x80;
  }

  namespace Strict {
    ConversionResult<view8, str32> toCodePoints(view8 bytes) {
      str32 result;
      result.reserve(bytes.size()/4);

      int latestTrailingBytes = 0;
      int trailingRemaining = 0;
      for(auto it = bytes.begin(), anchor = it; it != bytes.end(); ++ it) {
        auto val = *it;
        switch(trailingRemaining) {
        case 0: // No remaining trailing bytes
          anchor = it;
          if(val & 0x80) {
            // Start of new sequence, check sequence length
            // Mask xxxx x000
            switch(val & 0xf8) {
            // 110x'xxxx (110x'x000 masked)
            case 0xc0: case 0xd0: case 0xc8: case 0xd8: // 2 byte length
              latestTrailingBytes = trailingRemaining = 1;
              break;

            // 1110'xxxx (1110'x000 masked)
            case 0xe0: case 0xe1: // 3 byte length
              latestTrailingBytes = trailingRemaining = 2;
              break;

            // 1111'0xxx (1111'0000 masked)
            case 0xf0: // 4 byte length
              latestTrailingBytes = trailingRemaining = 3;
              break;

            default:
              return { std::move(result), it, anchor, ConversionError::InvalidStart };
              break;
            }
          }
          else // ASCII, just put in output
            result += val;
          break;
        case 2: case 3: // Do continuation check but don't finalize multibyte sequence
          if(isContinuation(val)) --trailingRemaining;
          else return { std::move(result), it, anchor, ConversionError::InvalidContinuation };
          break;
        case 1:
          if(isContinuation(val)) { // Valid sequence ended, finalize
            chr32 outValue;
            switch(latestTrailingBytes) {
            case 1: outValue = (static_cast<chr32>(*(it-0) & ~utf8::contMask) <<  0)
                             + (static_cast<chr32>(*(it-1) & 0x1f)            <<  6)
              ;break;
            case 2: outValue = (static_cast<chr32>(*(it-0) & ~utf8::contMask) <<  0)
                             + (static_cast<chr32>(*(it-1) & ~utf8::contMask) <<  6)
                             + (static_cast<chr32>(*(it-2) & 0x0f)            << 12)
              ;break;
            case 3: outValue = (static_cast<chr32>(*(it-0) & ~utf8::contMask) <<  0)
                             + (static_cast<chr32>(*(it-1) & ~utf8::contMask) <<  6)
                             + (static_cast<chr32>(*(it-2) & ~utf8::contMask) << 12)
                             + (static_cast<chr32>(*(it-3) & 0x07)            << 18)
              ;break;
            }
            if(outValue > 0x1ffff) return { std::move(result), it, anchor, ConversionError::OutOfBounds };
            else {
              result += outValue;
              trailingRemaining = 0;
            }
          } else return { std::move(result), it, anchor, ConversionError::InvalidContinuation };
          break;
        }
      }
      return { std::move(result), bytes.end(), bytes.end(), std::nullopt };
    }

    ConversionResult<view32, str8> toUTF8(view32 codepoints) {
      str8 result;
      for(auto it = codepoints.begin(); it != codepoints.end(); ++ it) {
        auto cp = *it;
        if(cp <= 0x7f) { // 1 byte encoding
          result += cp;
        } else if (cp <= 0x7ff) { // 2 byte encoding
          result += 0b1100'0000 | ((cp >>  6) & 0x1f);
          result += 0b1000'0000 | ((cp >>  0) & 0x3f);
        } else if (cp <= 0xffff) { // 3 byte encoding
          result += 0b1110'0000 | ((cp >> 12) & 0x0f);
          result += 0b1000'0000 | ((cp >>  6) & 0x3f);
          result += 0b1000'0000 | ((cp >>  0) & 0x3f);
        } else if (cp <= 0x10ffff) { // 4 byte encoding
          result += 0b1111'0000 | ((cp >> 18) & 0x07);
          result += 0b1000'0000 | ((cp >> 12) & 0x3f);
          result += 0b1000'0000 | ((cp >>  6) & 0x3f);
          result += 0b1000'0000 | ((cp >>  0) & 0x3f);
        } else {
          return { std::move(result), it, it, ConversionError::OutOfBounds };
        }
      }
      return { std::move(result), codepoints.end(), codepoints.end() };
    }
  }
}
