#include <type_traits>

namespace Meter {
  template<typename ...Ts>
  struct Types {};
  template<typename T, typename ...Rest>
  struct Cons { using type = Types<T, Rest...>; };

  template<typename Predicate, typename ...Tokens>
  struct Filter;

  template<typename Predicate>
  struct Filter<Predicate> { using type = Types<>; };

  template<template<typename Tp> typename Predicate, typename Head, typename ...Tail>
  struct Filter<Predicate<Head>, Head, Tail...> {
    using type = typename std::conditional_t<Predicate<Head>::value
                                           , typename Cons<Head, typename Filter<Tail...>::type>::type
                                           , typename Filter<Tail...>::type
    >;
  };
}