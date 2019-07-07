#include "enumerator_traits.hpp"

enum class ABC : uint8_t { a, b, c=3 };

constexpr auto ABCs = ltl::enumerators_v<ABC>;

static_assert( ABCs[0] == ABC::a );
static_assert( ABCs[1] == ABC::b );
static_assert( ABCs[2] == ABC::c );

using ABC_t = ltl::enumerators_t<ABC>;

static_assert( std::is_same_v< ABC_t,
               std::integer_sequence<uint8_t, 0, 1, 3>> );

// test constexpr iteration of returned array
//
constexpr auto iterABC = [] {
    int i = 0;
    for (auto&& e : ABCs)
        i += ltl::to_underlying(e);
    return i;
}();
static_assert( iterABC == 4 );

constexpr auto indexABC = [] {
    int i = 0;
    for (std::size_t j = 0; j != ABCs.size(); ++j)
        i += ltl::to_underlying(ABCs[j]);
    return i;
}();
static_assert( indexABC == 4 );

enum O {};
static_assert( ltl::enumerators_v<O>.size() == 0 );
static_assert( ! ltl::is_enumerated_v<O{}> );

enum N { n };
constexpr auto n0 = N();
static_assert( ltl::enumerators_v<N>.size() == 1 );
static_assert( ltl::enumerators_v<N>[0] == n );
static_assert( ltl::is_enumerated_v<n>
            && ltl::is_enumerated_v<n0>
            && ltl::is_enumerated_v<N{}>
            && ! ltl::is_enumerated_v<N(1)>);

enum B : uint8_t { b };
static_assert( ltl::enumerators_v<B>.size() == 1 );
static_assert( ltl::enumerators_v<B>[0] == b );
static_assert( ltl::is_enumerated_v<b> );
static_assert( ltl::is_enumerated_v<B{0}> );
static_assert( ! ltl::is_enumerated_v<B{1}> );

enum class C : uint16_t { a, bade=0xbade, feca=0xFECA};
static_assert( ltl::enumerators_v<C>.size() == 3 );
static_assert( ltl::enumerators_v<C>[0] == C::a
            && ltl::enumerators_v<C>[1] == C::bade
            && ltl::enumerators_v<C>[2] == C::feca);
static_assert( ltl::is_enumerated_v<C::a>
            && ltl::is_enumerated_v<C::bade>
            && ltl::is_enumerated_v<C{0xbade}>
            && ltl::is_enumerated_v<C::feca>
            && ! ltl::is_enumerated_v<C{0xf0cA}> );

int main()
{
 {
  enum class K : int8_t {min8 = INT8_MIN, max8 = INT8_MAX};
  static_assert( ltl::is_enumerated_v<K::min8> );
  static_assert( ltl::is_enumerated_v<K::max8> );
  static_assert( ! ltl::is_enumerated_v<K{}> );
  static_assert( ltl::enumerators_v<K>.size() == 2 );
  static_assert( ltl::enumerators_v<K>[0] == K::min8 );
  static_assert( ltl::enumerators_v<K>[1] == K::max8 );
 }
 {
  enum L : int16_t { min16 = INT16_MIN, max16 = INT16_MAX };
  static_assert( ltl::is_enumerated_v<min16> );
  static_assert( ltl::is_enumerated_v<max16> );
  static_assert( ltl::enumerators_v<L>.size() == 2 );
  static_assert( ltl::enumerators_v<L>[0] == min16 );
  static_assert( ltl::enumerators_v<L>[1] == max16 );
 }
 {
  enum { min32 = INT32_MIN, max32 = INT32_MAX };
  using M = decltype(min32);
  static_assert( ltl::is_enumerated_v<min32> );
  static_assert( ltl::is_enumerated_v<max32> );
  static_assert( ltl::enumerators_v<M>.size() == 2 );
  static_assert( ltl::enumerators_v<M>[0] == min32 );
  static_assert( ltl::enumerators_v<M>[1] == max32 );
 }
}

static_assert( std::is_same_v<

    ltl::impl::Lo16_int32seq,

    std::integer_sequence<int32_t,
        INT32_MIN + 0x00000000, INT32_MIN + 0x20000000,
        INT32_MIN + 0x40000000, INT32_MIN + 0x50000000,
        INT32_MIN + 0x60000000, INT32_MIN + 0x68000000,
        INT32_MIN + 0x70000000, INT32_MIN + 0x74000000,
        INT32_MIN + 0x78000000, INT32_MIN + 0x7A000000,
        INT32_MIN + 0x7C000000, INT32_MIN + 0x7D000000,
        INT32_MIN + 0x7E000000, INT32_MIN + 0x7E800000,
        INT32_MIN + 0x7F000000, INT32_MIN + 0x7F400000,
        INT32_MIN + 0x7F800000, INT32_MIN + 0x7FA00000,
        INT32_MIN + 0x7FC00000, INT32_MIN + 0x7FD00000,
        INT32_MIN + 0x7FE00000, INT32_MIN + 0x7FE80000,
        INT32_MIN + 0x7FF00000, INT32_MIN + 0x7FF40000,
        INT32_MIN + 0x7FF80000, INT32_MIN + 0x7FFA0000,
        INT32_MIN + 0x7FFC0000, INT32_MIN + 0x7FFD0000,
        INT32_MIN + 0x7FFE0000, INT32_MIN + 0x7FFE8000,
        INT32_MIN + 0x7FFF0000, INT32_MIN + 0x7FFF4000>
> );
