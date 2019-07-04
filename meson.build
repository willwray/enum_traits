project('enum_traits', 'cpp',
  default_options : 'cpp_std=c++17',
  version : '0.1'
)

enum_traits_dep = declare_dependency(
  include_directories : include_directories('.')
)

test('test enum traits',
  executable('test_enum_traits', 'test_enum_traits.cpp',
             dependencies: enum_traits_dep)
)

test('test enumerator traits',
  executable('test_enumerator_traits', 'test_enumerator_traits.cpp',
             dependencies: enum_traits_dep)
)
