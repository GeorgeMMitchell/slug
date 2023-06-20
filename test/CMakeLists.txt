# Copyright (c) 2023 - present George Mitchell
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

add_executable(log-test ${SLUG_PROJECT_DIR}/test/src/log-test.cc)

target_link_libraries(log-test PRIVATE slug Catch2::Catch2WithMain)

target_compile_options(
  log-test
  PRIVATE $<$<CXX_COMPILER_ID:Clang>:
          -Wall
          -Wextra>
          $<$<CXX_COMPILER_ID:GNU>:
          -Wall
          -Wextra>
          $<$<CXX_COMPILER_ID:MSVC>:
          /W4
          /utf-8>)

add_executable(log-test-header-only ${SLUG_PROJECT_DIR}/test/src/log-test.cc)

target_link_libraries(log-test-header-only PRIVATE slug-header-only
                                                    Catch2::Catch2WithMain)
