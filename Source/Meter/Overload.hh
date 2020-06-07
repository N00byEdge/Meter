#pragma once

namespace Meter {
  template<class... Ts> struct Overload : Ts... { using Ts::operator()...; };
  template<class... Ts> Overload(Ts...) -> Overload<Ts...>;
}