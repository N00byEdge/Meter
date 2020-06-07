#include "Meter/Unicode.hh"

#include "gtest/gtest.h"

namespace Unicode = Meter::Unicode;
namespace Strict = Unicode::Strict;

namespace {
  Unicode::str8 const utf8TestStrings[] {
    reinterpret_cast<Unicode::chr8 const*>(u8"This is a test string with a few emojis 😂😂😂😂😂😂"),
  };
}

TEST(UnicodeStrict, Utf8ToCodePointsAndBack) {
  for(auto &s : utf8TestStrings) {
    auto sview = Unicode::view8{s};
    auto [cps, errit, stopit, err] = Strict::toCodePoints(sview);
    EXPECT_EQ(sview.end(), errit);
    EXPECT_EQ(sview.end(), stopit);
    EXPECT_FALSE(err.has_value());
    auto cpsview = Unicode::view32{cps};
    auto [u8, errit8, stopit8, err8] = Strict::toUTF8(cpsview);
    EXPECT_EQ(cpsview.end(), errit8);
    EXPECT_EQ(cpsview.end(), stopit8);
    EXPECT_FALSE(err8.has_value());
    EXPECT_EQ(u8, sview);
  }
}

namespace {
  std::pair<Unicode::str8, Unicode::chr32> const knownEncodings[] {
    { reinterpret_cast<Unicode::chr8 const*>(u8"\x55"),             static_cast<Unicode::chr32>(U'\x55') },
    { reinterpret_cast<Unicode::chr8 const*>(u8"😂"),                static_cast<Unicode::chr32>(U'😂') },
    { reinterpret_cast<Unicode::chr8 const*>(u8"$"),                static_cast<Unicode::chr32>(U'$') },
    { reinterpret_cast<Unicode::chr8 const*>(u8"\xc2\xa2"),         static_cast<Unicode::chr32>(U'¢') },
    { reinterpret_cast<Unicode::chr8 const*>(u8"\xe0\xa4\xb9"),     static_cast<Unicode::chr32>(U'ह') },
    { reinterpret_cast<Unicode::chr8 const*>(u8"\xe2\x82\xac"),     static_cast<Unicode::chr32>(U'€') },
    { reinterpret_cast<Unicode::chr8 const*>(u8"\xf0\x90\x8d\x88"), static_cast<Unicode::chr32>(U'𐍈') },
  };
}

TEST(UnicodeStrict, KnownEncodings8) {
  for(auto &[enc, cp]: knownEncodings) {
    {
      // UTF-8 to code point
      auto sview = Unicode::view8{enc};
      auto [cps, errit, stopit, err] = Strict::toCodePoints(sview);
      EXPECT_EQ(sview.end(), errit);
      EXPECT_EQ(sview.end(), stopit);
      EXPECT_FALSE(err.has_value());
      EXPECT_EQ(cps.size(), 1);
      if(cps.size() == 1) {
        EXPECT_EQ(cps[0], cp);
      }
    }
    {
      // Code point to UTF-8
      auto str = Unicode::str32{cp};
      auto sview = Unicode::view32{str};
      auto [outEnc, errit, stopit, err] = Strict::toUTF8(sview);
      EXPECT_EQ(sview.end(), errit);
      EXPECT_EQ(sview.end(), stopit);
      EXPECT_FALSE(err.has_value());
      EXPECT_EQ(outEnc.size(), enc.size());
      EXPECT_EQ(enc, outEnc);
    }
  }
}

namespace {
  Unicode::view8 const StartsWithContinuation8{ reinterpret_cast<Unicode::chr8 const*>("\x80") };
  Unicode::view8 const NakedContinuation8     { reinterpret_cast<Unicode::chr8 const*>("\x20\x80") };
  Unicode::view8 const InvalidContinuation2_2 { reinterpret_cast<Unicode::chr8 const*>("\xc2\x20") };
  Unicode::view8 const InvalidContinuation3_2 { reinterpret_cast<Unicode::chr8 const*>("\xe0\x20") };
  Unicode::view8 const InvalidContinuation3_3 { reinterpret_cast<Unicode::chr8 const*>("\xe0\x80\x20") };
  Unicode::view8 const InvalidContinuation4_2 { reinterpret_cast<Unicode::chr8 const*>("\xf0\x20") };
  Unicode::view8 const InvalidContinuation4_3 { reinterpret_cast<Unicode::chr8 const*>("\xf0\x80\x20") };
  Unicode::view8 const InvalidContinuation4_4 { reinterpret_cast<Unicode::chr8 const*>("\xf0\x80\x80\x20") };
  Unicode::view8 const NeedMoreData2_1        { reinterpret_cast<Unicode::chr8 const*>("\xc2") };
  Unicode::view8 const NeedMoreData3_1        { reinterpret_cast<Unicode::chr8 const*>("\xe8") };
  Unicode::view8 const NeedMoreData3_2        { reinterpret_cast<Unicode::chr8 const*>("\xe8\x80") };
  Unicode::view8 const NeedMoreData4_1        { reinterpret_cast<Unicode::chr8 const*>("\xf0") };
  Unicode::view8 const NeedMoreData4_2        { reinterpret_cast<Unicode::chr8 const*>("\xf0\x80") };
  Unicode::view8 const NeedMoreData4_3        { reinterpret_cast<Unicode::chr8 const*>("\xf0\x80\x80") };
  

  std::pair<Unicode::view8, Strict::ConversionResult<Unicode::view8, Unicode::str32>> const expectedResults8toCP[] {
    { StartsWithContinuation8,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        StartsWithContinuation8.begin(),
        StartsWithContinuation8.begin(),
        Strict::ConversionError::InvalidStart,
      }
    },
    { NakedContinuation8,
      {
        reinterpret_cast<Unicode::chr32 const *>(U"\x20"),
        NakedContinuation8.begin() + 1,
        NakedContinuation8.begin() + 1,
        Strict::ConversionError::InvalidStart,
      }
    },
    { InvalidContinuation2_2,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        InvalidContinuation2_2.begin() + 1,
        InvalidContinuation2_2.begin(),
        Strict::ConversionError::InvalidContinuation,
      }
    },
    { InvalidContinuation3_2,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        InvalidContinuation3_2.begin() + 1,
        InvalidContinuation3_2.begin(),
        Strict::ConversionError::InvalidContinuation,
      }
    },
    { InvalidContinuation3_3,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        InvalidContinuation3_3.begin() + 2,
        InvalidContinuation3_3.begin(),
        Strict::ConversionError::InvalidContinuation,
      }
    },
    { InvalidContinuation4_2,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        InvalidContinuation4_2.begin() + 1,
        InvalidContinuation4_2.begin(),
        Strict::ConversionError::InvalidContinuation,
      }
    },
    { InvalidContinuation4_3,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        InvalidContinuation4_3.begin() + 2,
        InvalidContinuation4_3.begin(),
        Strict::ConversionError::InvalidContinuation,
      }
    },
    { InvalidContinuation4_4,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        InvalidContinuation4_4.begin() + 3,
        InvalidContinuation4_4.begin(),
        Strict::ConversionError::InvalidContinuation,
      }
    },
    { NeedMoreData2_1,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        NeedMoreData2_1.end(),
        NeedMoreData2_1.begin(),
        std::nullopt,
      }
    },
    { NeedMoreData3_1,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        NeedMoreData3_1.end(),
        NeedMoreData3_1.begin(),
        std::nullopt,
      }
    },
    { NeedMoreData3_2,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        NeedMoreData3_2.end(),
        NeedMoreData3_2.begin(),
        std::nullopt,
      }
    },
    { NeedMoreData4_1,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        NeedMoreData4_1.end(),
        NeedMoreData4_1.begin(),
        std::nullopt,
      }
    },
    { NeedMoreData4_2,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        NeedMoreData4_2.end(),
        NeedMoreData4_2.begin(),
        std::nullopt,
      }
    },
    { NeedMoreData4_3,
      {
        reinterpret_cast<Unicode::chr32 const *>(U""),
        NeedMoreData4_3.end(),
        NeedMoreData4_3.begin(),
        std::nullopt,
      }
    },
  };
}

TEST(UnicodeStrict, BadEncodings) {
  for(auto &[enc, expectedResult]: expectedResults8toCP) {
    auto [cps, errit, stopit, err] = Strict::toCodePoints(enc);
    EXPECT_EQ(cps, expectedResult.output);
    EXPECT_EQ(errit, expectedResult.errorLocation);
    EXPECT_EQ(stopit, expectedResult.parsedUntil);
    EXPECT_EQ(err.has_value(), expectedResult.error.has_value());
    if(err.has_value() && expectedResult.error.has_value()) {
      EXPECT_EQ(err.value(), expectedResult.error.value());
    }
  }
}
