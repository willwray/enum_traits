//  Copyright (c) 2019 Will Wray https://keybase.io/willwray
//
//  Distributed under the LGPL-3.0-or-later
//        https://www.gnu.org/licenses/lgpl-3.0.txt
//
//  Repo: https://github.com/willwray/enum_traits

#ifndef LTL_ENUMERATOR_TRAITS_HPP
#define LTL_ENUMERATOR_TRAITS_HPP

#include <cstdint>
#include <utility>

#include "enum_traits.hpp"

/*
  "enumerator_traits.hpp": Reflect enumerated values of an enum type
   ^^^^^^^^^^^^^^^^^^^^^
   Targets GCC>=9 and Clang with c++17 std flag (recent MSVC possible*).

   ltl::enumerators_v<E>;  // Array of enumerated values of enum type E.
   ltl::enumerators_t<E>;  // std::integer_sequence of underlying values.
   ltl::enumerators<E>;    // Trait class for enumerated values of E with
                           // array 'value' and typedef 'type' members.

   ltl::is_enumerated_v<e>; // Test if enum value e corresponds to an
                            // enumerated value of its enum type E.
                            // Requires E = decltype(e) is enum type.
  Enumerators
  ===========
   8-bit enum:  Exhaustive compile-time check of all 2^8 values. Fast.

   16-bit enum: Exhaustive compile-time check of all 2^16 values. ~1s.

   32-bit enum: Check 2^16 values of the same-signed 16-bit integer, plus,
                for int32, firstly check 32 values [INT32_MIN, INT16_MIN)
                                   then 360 values (INT16_MAX, INT32_MAX],
                else for uint32, check 392 values (UINT16_MAX,UINT32_MAX].
                The high ranges comprise all values with a contiguous run
                of set bits so that 'flag enums' are partially supported.
  
   64-bit enum: Unsupported. Compile error.

   The array returned by enumerators_v is of an internal type, sufficient
   to index, iterate with a range-for loop or copy into another container.

  Duplicate values
  ================
   *MSVC currently fails to reflect enumerators with duplicate values:
    e.g. enum E { moo, baa=moo }; // moo and baa have duplicate value
         is_enumerated<moo> == false && is_enumerated<baa> == false

   (ltl::enumerators and is_enumerated_v check enumerated *value* only.
    Duplicate-value enumerator ids cannot be distinguished this way.
    E.g. enumerators_v<E> gives only one array-slot 'hit' per value).

  Platform support
  ================
   GCC>=9 is required for a bugfix (earlier versions can be patched).
   C++17 for auto template parameters, constexpr if, inline variables...

   *MSVC currently manages only enum types with 8-bit underlying_type.
   16-bit may be possible; tests managed 14 to 15-bit range (~minutes).

  Disclaimers
  ===========
   The method checks values one-by-one 'brute force' by a back-door; these
   enumerated-value checks use non-standard 'pretty function' preprocessor
   extensions whose output differs between compilers & compiler versions.
   This is not future-proof. Test for your use-case and target platforms.

   Enums with 32-bit underlying type are not exhaustively checked.
   Only a very few 32-bit values outside of 16-bit range are checked.
*/

namespace ltl {

namespace impl {

template <typename T, std::size_t N>
struct array
{
    T data[N];
    static constexpr std::size_t size() noexcept { return N; }
    constexpr T& operator[](int i) { return data[i]; }
    constexpr T const& operator[](int i) const { return data[i]; }
    constexpr T const* begin() const noexcept { return data; }
    constexpr T const* end() const noexcept { return data+N; }
};

template<typename T>
struct array<T,0>
{
    static constexpr T(&&data)[1]{};
    static constexpr std::size_t size() noexcept { return 0; }
    constexpr T const* begin() const noexcept { return data; }
    constexpr T const* end() const noexcept { return data; }
};

template <typename T, std::size_t N>
constexpr std::size_t size(array<T,N> const&) noexcept { return N; }

// cat(a...) Concatenate arrays
//
template <typename T, typename... X>
constexpr auto cat(X const& ... in) noexcept
{
    array<T,(size(X{}) + ...)> acc{};
    constexpr auto copy_cat = [](auto const& src, T* dest) {
        for (auto&& s : src)
            *dest++ = T(s);
        return dest;
    };
    T* out = acc.data;
    (( out = copy_cat(in,out) ),...);
    return acc;
}

// to_seq<g>() convert g array-generator 'functor' to std::integer_sequence
//
template <auto const& g, std::size_t... I>
constexpr auto to_seq( std::index_sequence<I...> = {} )
{
    if constexpr ( sizeof...(I) == 0 )
        return to_seq<g>( std::make_index_sequence<size(decltype(g()){})>{} );
    else {
        constexpr auto a = g();
        return std::integer_sequence<std::decay_t<decltype(a[0])>, a[I]...>{};
    }
}
template <auto const& g> using seq_t = decltype(to_seq<g>());

// Generate negative int32_t values in the range [INT32_MIN,INT16_MIN)
// 32 values, roughly log-distributed, in increasing order.
//
constexpr auto lo16_32s = []
{
    array<int32_t, 32> seq{};
    for (int32_t i{}, m{INT32_MIN}, l{0x20000000};
                 i != 16; ++i, m += 2*l, l >>= 1) {
        seq[2*i] = m;
        seq[2*i + 1] = m + l;
    }
    return seq;
};
using Lo16_int32seq = seq_t<lo16_32s>;

// Generate positive 32-bit values in the range (UINT16_MAX, UINT32_MAX]
// for uint32 or, for int32, values in the range (INT16_MAX, INT32_MAX].
// The sequences contain all values with a contiguous run of set bits.
// 392 values for uint32_t, 360 for int32_t, in increasing order.
//
template <typename Int32>
constexpr auto hi16_32s = []
{
    static_assert( std::is_integral_v<Int32>
                && sizeof(Int32) == 4 );
    constexpr int nb = 16 - std::is_signed_v<Int32>;
    array<Int32, nb*16 + nb*(nb + 1)/2> seq{};
    int k = 0;
    for (int i = 16; i != 16 + nb; ++i)
    {
        Int32 m {Int32{1} << i};
        for (int j = i + 1; j; --j, ++k)
        {
            seq[k] = m;
            m = m | (m >> 1);
        }
    }
    return seq;
};
using Hi16_int32seq = seq_t<hi16_32s<int32_t>>;
using Hi16_uint32seq = seq_t<hi16_32s<uint32_t>>;

constexpr bool not_enumerated_1st_char(char c)
{
#if defined(__clang__)
    return ( c >= '0' && c <= '9' ) || c == '-';
#elif defined(__GNUC__) || defined(__GNUG__)
    return c == '(';
#elif defined(_MSC_VER)
    return c >= '0' && c <= '9' || c == '-';
#else
    return c == '(' || ( c >= '0' && c <= '9' ) || c == '-';
#endif
}

template <auto... v>
constexpr
auto
PTvs()
{
# if defined(__FUNCSIG__)
    return sizeof __FUNCSIG__;
#   define PF (__FUNCSIG__ + PTvs<1>() \
                        - sizeof "1>(void)")
#   define SEP ','
#   define FIN ')'
# else
    return sizeof __PRETTY_FUNCTION__;
#   define PF (__PRETTY_FUNCTION__ + PTvs<1>() \
                                - sizeof "1}]")
#   define SEP ' '
#   define FIN ']'
# endif
}

template <auto... e>
constexpr
bool
PTeI()
{
    return ! not_enumerated_1st_char(*PF);
}

// first_v<v...> is the first value of a similar-type value pack
//
template <auto u, decltype(u)...>
inline constexpr auto first_v = u;

// PTev<v...>() Returns the v's which are enumerators in a array.
//   Requires all v's are values of one enum type,
//   monotonically increasing in value, with no huge leaps (>~x10).
//
template <auto... v>
constexpr
auto
PTev()
{
    using E = std::remove_const_t<decltype(first_v<v...>)>;

    constexpr auto pf = PF;

    constexpr auto count_then_tally = [=](auto size)
    {
        array<E,size()> tally{};

        int counter = 0;
        const char* p = pf;
        
        for (int i = 0, skip = 0; i != sizeof...(v); ++i)
        {
            if ( not_enumerated_1st_char(*p) )
            {
                p += skip;
                if (*p == SEP)
                    --skip;
                else
                    while (*++p != SEP && *p != FIN)
                        ++skip;
                ++p;
            }
            else
            {
                while (*p != SEP && *p != FIN)
                    ++p;
                ++p;
                if constexpr ( tally.size() > 0 )
                {
                    constexpr auto first_u = to_underlying(first_v<v...>);
                    if constexpr ( sizeof(E) == 4 && first_u == 0x10000 )
                        tally[counter] = [i]{ E e[]{v...}; return e[i]; }();
                    else
                        tally[counter] = E(first_u + i);
                }
                ++counter;
            }
        }
        if (*p != '\0')
            throw("ltl::enumerators failed to scan");

        if constexpr (  tally.size() > 0 )
            return tally;
        else
            return counter;
    };
    constexpr auto count = count_then_tally(std::integral_constant<int,0>{});

    if constexpr ( count != 0 )
        return count_then_tally(std::integral_constant<int,count>{});
    else
        return array<int,0>{};
}

template <typename E, std::underlying_type_t<E>... v>
constexpr auto enum_vals(std::integer_sequence<std::underlying_type_t<E>,v...>)
{
    return PTev<E(v)...>();
}

template <typename E, std::underlying_type_t<E> b, std::size_t... n>
constexpr auto enum_valseq_N(std::index_sequence<n...>)
{
    return PTev<E(b + int{n})...>();
}

template <typename E, std::underlying_type_t<E> b, typename Ns,
          std::size_t... m>
constexpr auto enum_valseq_NxM(std::index_sequence<m...>)
{
    return cat<E>( enum_valseq_N<E, b + int{m*Ns::size()}>(Ns{})... );
}

template <typename E>
constexpr auto enumerators()
{
    static_assert( std::is_enum_v<E>, "error: enumerators of non-enum type");

    using V = std::underlying_type_t<E>;
    constexpr bool signed_ = std::is_signed_v<V>;

    using Is6  = std::make_index_sequence<64>;
    using Is8  = std::make_index_sequence<256>;
    using Is10 = std::make_index_sequence<1024>;

    if constexpr ( sizeof(E) == 1 )
        return cat<E>(
               enum_valseq_N<E, signed_ ? -0x80 : 0>(Is8{}) );

    else if constexpr ( sizeof(E) == 2 )
        return enum_valseq_NxM<E, signed_? -0x8000 : 0, Is10>(Is6{});

    else if constexpr ( sizeof(E) == 4 )
    {
      if constexpr ( signed_ )
        return cat<E>(
               enum_vals<E>(Lo16_int32seq{}),
               enum_valseq_NxM<E, -0x8000, Is10>(Is6{}),
               enum_vals<E>(Hi16_int32seq{}) );
      else
        return cat<E>(
               enum_valseq_NxM<E, 0, Is10>(Is6{}),
               enum_vals<E>(Hi16_uint32seq{}) );
    }
}

} // namespace impl

// is_enumerated_v<e>: trait to test if e is an enumerated value.
// Requires e to be a value of enum type; std::is_enum_v<decltype(e)>
//
template <auto e>
inline constexpr bool is_enumerated_v = impl::PTeI<e>();

// enumerators_v<E> : Enumerated values of enum type E, sorted.
//
template <typename E>
inline constexpr auto enumerators_v = impl::enumerators<E>();

template <typename E>
struct enumerators
{
    static constexpr auto const& value = enumerators_v<E>;

    using index_sequence = std::make_index_sequence<value.size()>;
    
    template <std::size_t... I>
    static constexpr auto integer_sequence(std::index_sequence<I...>)
    {
        return std::integer_sequence<std::underlying_type_t<E>,
                                       to_underlying(value[I])...>{};
    }
    using type = decltype(integer_sequence(index_sequence{}));
};

template <typename E>
using enumerators_t = typename enumerators<E>::type;

} // namespace ltl

# undef PF
# undef SEP
# undef FIN

#endif // LTL_ENUMERATOR_TRAITS_HPP
