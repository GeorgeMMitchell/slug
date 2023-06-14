# Copyright (c) 2023, George Mitchell
# See License.txt for license information

include(CTest)

find_package(Catch2 QUIET)
if(NOT Catch2_FOUND)
  FetchContent_Declare(
    Catch2
    GIT_REPOSITORY https://github.com/catchorg/Catch2.git
    GIT_TAG v3.3.2)
  FetchContent_MakeAvailable(Catch2)
endif()

add_executable(slug-test ${SLUG_PROJECT_DIR}/test/src/slug-test.cc)

target_link_libraries(slug-test PRIVATE slug Catch2::Catch2WithMain)

target_compile_options(
  slug-test
  PRIVATE
    $<$<CXX_COMPILER_ID:Clang>:
      -Wall
      -Wextra>

    $<$<CXX_COMPILER_ID:GNU>:
      -Wall
      -Wextra>

    $<$<CXX_COMPILER_ID:MSVC>:
      /W4
      /utf-8>)

add_test(NAME slug-test COMMAND slug-test)
