//  Copyright (c) 2019 Will Wray https://keybase.io/willwray
//
//  Distributed under the Boost Software License, Version 1.0.
//        http://www.boost.org/LICENSE_1_0.txt
//
//  Repo: https://github.com/willwray/enum_traits

#ifndef LTL_ENUM_TRAITS_HPP
#define LTL_ENUM_TRAITS_HPP

#include <type_traits>

/*
  "enum_traits.hpp": Traits to reflect enum types and enumerator values.
   ^^^^^^^^^^^^^^^
   Targets GCC and Clang with -std=c++17, MSVC with /std:c++17

   ltl::is_scoped_enum<T>;   // Test if type T is a scoped enum.
   ltl::is_scoped_enum_v<T>; //

   ltl::is_fixed_enum<T>;    // Test if type T is a 'fixed' enum, i.e.
   ltl::is_fixed_enum_v<T>;  // an enum  with fixed underlying type.

   ltl::underlying_type<T>;  // c++17 port of c++20's 'SFINAE-friendly'
                             // std::underlying_type; not enum, no 'type'.

   ltl::to_underlying(e);    // Convenience cast to underlying type P1682.

*/

namespace ltl {

// is_scoped_enum<T>: trait to test if type T is a scoped enum
//
template <typename T>
inline constexpr bool is_scoped_enum_v = [] {
    if constexpr ( std::is_enum_v<T> )
        return ! std::is_convertible_v<T, std::underlying_type_t<T>>;
    else
        return false;
}();

template <typename T>
struct is_scoped_enum : std::bool_constant<is_scoped_enum_v<T>> {};

namespace impl {
template <typename T, typename = T>
inline constexpr bool is_fixed_enum_v = false;

template <typename T>
inline constexpr bool is_fixed_enum_v<T, decltype(T{0})> = std::is_enum_v<T>;
} // impl

// is_fixed_enum<T>: trait to test if T is an enum with fixed underlying type
//
template <typename T>
inline constexpr bool is_fixed_enum_v = impl::is_fixed_enum_v<T>;

template <typename T>
struct is_fixed_enum : std::bool_constant<is_fixed_enum_v<T>> {};

namespace impl {
template <typename T, bool = std::is_enum_v<T>>
struct underlying_type {};

template <typename T>
struct underlying_type<T, true>
{
    using type = std::underlying_type_t<T>;
};
} // impl

template <typename T>
struct underlying_type : impl::underlying_type<T> {};

template <typename T>
using underlying_type_t = typename impl::underlying_type<T>::type;

// to_underlying(e): convenience cast from enum value e to its underlying type
//
template <typename E>
constexpr underlying_type_t<E> to_underlying(E e) noexcept
{
    return static_cast<std::underlying_type_t<E>>(e);
}

} // namespace ltl

#endif // LTL_ENUM_TRAITS_HPP
