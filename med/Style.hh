#pragma once

#include <cstdint>

#include "imgui/imgui.h"

struct Style {
  struct Color {
    Color() { }
    Color(float f1, float f2, float f3) { col[0] = f1, col[1] = f2, col[2] = f3; }
    Color(uint8_t c1, uint8_t c2, uint8_t c3): Color{c1/255.f, c2/255.f, c3/255.f} { }
    Color(uint32_t c): Color{(uint8_t)(c >> 16), (uint8_t)(c >> 8), (uint8_t)c} { }
    operator ImVec4() const { return {col[0], col[1], col[2], 1.f}; }
    float col[3];
  };

  struct MaybeColor: Color {
    MaybeColor(): valid{false} { }
    template<typename T, typename ...Ts>
    MaybeColor(T const &v, Ts &&...vs): Color{v, vs...}, valid{true} { };
    bool valid;
    operator bool() const { return valid; }
  };

  void window();
  void init();

  int indentation_width = 2;
  bool curly_braces_on_new_line = false;
  bool space_after_statement = false;
  bool always_trailing_commas = true;
  bool expression_statement_semicolons = true;
  bool space_before_curly_brace = true;
  bool space_around_bops = true;

  Color const &if_color() const { return if_color_override ?: keyword_color; }
  Color const &for_color() const { return for_color_override ?: keyword_color; }
  Color const &do_color() const { return do_color_override ?: keyword_color; }
  Color const &while_color() const { return while_color_override ?: keyword_color; }

  Color text_color;
  Color background_color;
  Color identifier_color;
  Color literal_color;
  Color string_color;

  Color keyword_color;

  MaybeColor if_color_override;
  MaybeColor for_color_override;
  MaybeColor do_color_override;
  MaybeColor while_color_override;

  unsigned display_mode = 0;
};
