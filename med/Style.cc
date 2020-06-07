#include "med/Style.hh"

#include "imgui/imgui.h"

#include <array>
#include <charconv>
#include <functional>
#include <iostream>
#include <cstring>

namespace {
  /*constexpr std::array indentation_levels{1, 2, 4, 8};
  char indentation_buf[16];

  bool indentation_getter(void *data, int idx, char const **out_str) {
    std::to_chars(std::begin(indentation_buf), std::end(indentation_buf), indentation_levels[idx]);
    out_str = indentation_buf;
    return true;
  }*/

  enum StyleMode {
    DarkMode,
    LightMode,
    Classic,
  };

  struct DisplayMode {
    StyleMode mode;
    char const *name;
    void(*changeCallback)(Style &);
  };

  constexpr std::array display_modes {
    DisplayMode {
      StyleMode::DarkMode,
      "Monokai",
      [](Style &s) {
        s.text_color = 0xF8F8F2;
        s.background_color = 0x272822;
        s.keyword_color = 0xF92672;
        s.identifier_color = 0x66D9EF;
        s.literal_color = 0xAE81FF;
        s.string_color = 0xE6DB74;
        ImGui::StyleColorsDark();
      },
    },
    DisplayMode {
      StyleMode::LightMode,
      "Bluloco Light",
      [](Style &s) {
        s.text_color = 0x383A42;
        s.background_color = 0xF9F9F9;
        s.keyword_color = 0x0098DD;
        s.identifier_color = 0x823FF1;
        s.literal_color = 0xCE33C0;
        s.string_color = 0xC5A332;
        ImGui::StyleColorsLight();
      },
    },
  };

  bool display_mode_getter(void*, int idx, char const **out_str) {
    unsigned uidx = idx;
    assert(uidx < display_modes.size());
    *out_str = display_modes[uidx].name;
    return true;
  }

  constexpr ImGuiColorEditFlags color_flags = ImGuiColorEditFlags__OptionsDefault | ImGuiColorEditFlags_NoInputs;

  void show_color(char const *name, Style::Color &col) {
    ImGui::ColorEdit3(name, col.col, color_flags);
  }

  void show_color(char const *name, Style::MaybeColor &col, Style::Color const &initial_value) {
    bool now_valid = col.valid;
    ImGui::Checkbox(name, &now_valid);

    if(!col.valid && now_valid)
      std::memcpy(col.col, initial_value.col, sizeof(col.col));

    col.valid = now_valid;

    if(col.valid) {
      ImGui::SameLine();
      ImGui::ColorEdit3(name, col.col, color_flags | ImGuiColorEditFlags_NoLabel);
    }
  }
}

void Style::init() {
  assert(display_mode < display_modes.size());
  display_modes[display_mode].changeCallback(*this);
}

void Style::window() {
  ImGui::Begin("Style settings");

  if (ImGui::CollapsingHeader("Code style")) {
    ImGui::InputInt("Indentation width", &indentation_width);

    if(indentation_width < 0)
      indentation_width = 0;

    ImGui::Checkbox("Trailing commas", &always_trailing_commas);
    ImGui::Checkbox("Semicolon after expression statement", &expression_statement_semicolons);
  }

  if (ImGui::CollapsingHeader("Spacing")) {
    ImGui::Checkbox("Curly braces on newline", &curly_braces_on_new_line);
    if(!curly_braces_on_new_line)
      ImGui::Checkbox("Space before curly brace", &space_before_curly_brace);
    ImGui::Checkbox("Space around binary operators", &space_around_bops);
    ImGui::Checkbox("Space after statement keyword", &space_after_statement);
  }

  if (ImGui::CollapsingHeader("Colors")) {
    int selected_index = display_mode;
    ImGui::Combo("Display mode", &selected_index, &display_mode_getter, nullptr, display_modes.size());
    assert(selected_index > -1);
    if((unsigned)selected_index != display_mode) {
      display_mode = selected_index;
      display_modes[selected_index].changeCallback(*this);
    }

    show_color("Text", text_color);
    show_color("Background", background_color);
    
    show_color("Keywords", keyword_color);
    show_color("Identifiers", identifier_color);
    show_color("Literals", literal_color);
    show_color("Strings", string_color);

    if (ImGui::TreeNode("Keyword colors")) {
      show_color("Override: if", if_color_override, keyword_color);
      show_color("Override: for", for_color_override, keyword_color);
      show_color("Override: do", do_color_override, keyword_color);
      show_color("Override: while", while_color_override, keyword_color);

      ImGui::TreePop();
    }

  }

  ImGui::End();
}
