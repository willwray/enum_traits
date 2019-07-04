#include "enum_traits.hpp"
#include <cstdint>

enum class ABC : int8_t { a, b, c=3 };

static_assert( ltl::is_scoped_enum_v<ABC> );
static_assert( ltl::is_fixed_enum_v<ABC> );
static_assert( ltl::is_scoped_enum<ABC>::value );
static_assert( ltl::is_fixed_enum<ABC>::value );
static_assert( ltl::to_underlying(ABC::b) == 1 );
static_assert( std::is_same_v<std::underlying_type_t<ABC>, int8_t> );
static_assert( std::is_same_v<decltype(ltl::to_underlying(ABC::b)), int8_t> );

enum { uint64_max = 0xFFFFFFFFFFFFFFFF };
using MAX64 = decltype(uint64_max);
static_assert( ! ltl::is_scoped_enum_v<MAX64> );
static_assert( ! ltl::is_fixed_enum_v<MAX64> );
static_assert( ! ltl::is_scoped_enum<MAX64>::value );
static_assert( ! ltl::is_fixed_enum<MAX64>::value );
static_assert( std::is_same_v<std::underlying_type_t<MAX64>, uint64_t> );
static_assert( std::is_same_v<decltype(ltl::to_underlying( uint64_max)), uint64_t> );

enum O {};
static_assert( ! ltl::is_scoped_enum_v<O> );
static_assert( ! ltl::is_fixed_enum_v<O> );
static_assert( ! ltl::is_scoped_enum<O>::value );
static_assert( ! ltl::is_fixed_enum<O>::value );
static_assert( std::is_same_v<std::underlying_type_t<O>, uint32_t> );
static_assert( std::is_same_v<decltype(ltl::to_underlying(O())), uint32_t> );

enum N { n };
static_assert( ! ltl::is_scoped_enum_v<N> );
static_assert( ! ltl::is_fixed_enum_v<N> );
static_assert( std::is_same_v<std::underlying_type_t<N>, uint32_t> );
static_assert( std::is_same_v<decltype(ltl::to_underlying(N{})), uint32_t> );

enum B : uint8_t { b };
static_assert( ! ltl::is_scoped_enum_v<B> );
static_assert( ltl::is_fixed_enum_v<B> );
static_assert( ltl::is_fixed_enum<B>::value );
static_assert( std::is_same_v<std::underlying_type_t<B>, uint8_t> );
static_assert( std::is_same_v<decltype(ltl::to_underlying(b)), uint8_t> );

enum class C : uint16_t { a, bade=0xbade, feca=0xFECA};
static_assert( ltl::is_scoped_enum_v<C> );
static_assert( ltl::is_fixed_enum_v<C> );
static_assert( std::is_same_v<std::underlying_type_t<C>, uint16_t> );
static_assert( std::is_same_v<decltype(ltl::to_underlying(C::a)), uint16_t> );

// ltl::underlying_type tests
// In C++17 std::underlying_type<non_enum_type> is UB
// In C++20 it is defined as a struct with no 'type' typedef member

static_assert( std::is_same_v<ltl::underlying_type<ABC>::type,
                              std::underlying_type<ABC>::type> );
namespace impl
{
    template <typename, typename = void>
    inline constexpr bool has_type_typedef_member{};

    template <typename T>
    inline constexpr bool has_type_typedef_member<T, std::void_t<
                                                     typename T::type>>{true};
    // test the tester
    static_assert( has_type_typedef_member<std::add_cv<int>> );
    static_assert( ! has_type_typedef_member<int> );
    static_assert( ! has_type_typedef_member<struct X> );
    struct X { using type = int; };
    static_assert( ! has_type_typedef_member<X> );
} // namespace impl

enum Y : int;
static_assert( impl::has_type_typedef_member<ltl::underlying_type<Y>> );
static_assert( ! impl::has_type_typedef_member<ltl::underlying_type<int>> );

int main()
{
  enum { min32 = INT32_MIN, max32 = INT32_MAX };
  using M = decltype(min32);
  static_assert( ! ltl::is_scoped_enum_v<M> );
  static_assert( ! ltl::is_fixed_enum_v<M> );
  static_assert( std::is_same_v<std::underlying_type_t<M>, int32_t> );
  static_assert( ltl::to_underlying(min32) == -0x7fffffff - 1 ); // INT32_MIN

  enum class L : int8_t {};
  static_assert( ltl::is_scoped_enum_v<L> );
  static_assert( ltl::is_fixed_enum_v<L> );
  static_assert( ltl::to_underlying(L{1}) == 1 );
  static_assert( std::is_same_v<std::underlying_type_t<L>, int8_t> );
  static_assert( std::is_same_v<decltype(ltl::to_underlying(L{0})), int8_t> );

}
