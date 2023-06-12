# Copyright (c) 2023, George Mitchell
# See License.txt for license information

cmake_minimum_required(VERSION 3.12...3.26)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  set(SLUG_ROOT_PROJECT ON)
else()
  set(SLUG_ROOT_PROJECT OFF)
endif()

set(SLUG_PROJECT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SLUG_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

set(SLUG_SOURCES
  ${SLUG_PROJECT_DIR}/src/include/slug/log.h
  ${SLUG_PROJECT_DIR}/src/slug/log.cc)

file(READ ${SLUG_PROJECT_DIR}/src/include/slug/log.h slug_log_h)
if (NOT slug_log_h MATCHES "define SLUG_VERSION_MAJOR[ \t]([0-9]+)")
  message(FATAL_ERROR "Can't find SLUG_VERSION_MAJOR #define macro in ${SLUG_PROJECT_DIR}/src/include/slug/log.h")
endif()
math(EXPR CPACK_PACKAGE_VERSION_MAJOR ${CMAKE_MATCH_1})

if (NOT slug_log_h MATCHES "define SLUG_VERSION_MINOR[ \t]([0-9]+)")
  message(FATAL_ERROR "Can't find SLUG_VERSION_MINOR #define macro in ${SLUG_PROJECT_DIR}/src/include/slug/log.h")
endif()
math(EXPR CPACK_PACKAGE_VERSION_MINOR ${CMAKE_MATCH_1})

if (NOT slug_log_h MATCHES "define SLUG_VERSION_PATCH[ \t]([0-9]+)")
  message(FATAL_ERROR "Can't find SLUG_VERSION_PATCH #define macro in ${SLUG_PROJECT_DIR}/src/include/slug/log.h")
endif()
math(EXPR CPACK_PACKAGE_VERSION_PATCH ${CMAKE_MATCH_1})

set(SLUG_VERSION_MAJOR ${CPACK_PACKAGE_VERSION_MAJOR})
set(SLUG_VERSION_MINOR ${CPACK_PACKAGE_VERSION_MINOR})
set(SLUG_VERSION_PATCH ${CPACK_PACKAGE_VERSION_PATCH})
set(SLUG_VERSION ${SLUG_VERSION_MAJOR}.${SLUG_VERSION_MINOR}.${SLUG_VERSION_PATCH})

set(SLUG_DESCRIPTION "Lightweight C++17 logging library")

project(slug VERSION ${SLUG_VERSION} DESCRIPTION ${SLUG_DESCRIPTION} LANGUAGES CXX)

include(CPack)
include(FetchContent)
include(GNUInstallDirs)

set(CMAKE_EXPORT_PACKAGE_REGISTRY ON)
set(CPACK_PACKAGE_VENDOR slug)
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${SLUG_DESCRIPTION})
set(CPACK_PACKAGE_VERSION_PATCH 0)
set(CPACK_RESOURCE_FILE_LICENSE ${SLUG_PROJECT_DIR}/License.txt)
set(CPACK_RESOURCE_FILE_README ${SLUG_PROJECT_DIR}/ReadMe.adoc)
set(CPACK_SOURCE_GENERATOR TGZ ZIP)
set(CPACK_SOURCE_IGNORE_FILES /.git /.idea /.vscode /.vs /dist /.*build.* /\\\\.DS_Store)

option(SLUG_GLOBAL "Enable Slug's global logging object" ON)
option(SLUG_FMT_HEADERONLY "Consume fmtlib as a header-only library" OFF)
option(SLUG_WIDECHAR_GLOBAL "Enable Slug's global wide character logging object" OFF)
option(SLUG_DOC "Build the documentation" OFF)
set(SLUG_DEBUG_POSTFIX d CACHE STRING "Slug's debug library filename postfix")

if(SLUG_ROOT_PROJECT OR SLUG_DOC)
  find_package(Doxygen QUIET)

  if(Doxygen_FOUND)
    set(DOXYGEN_EXTRACT_ALL YES)
    set(DOXYGEN_BUILTIN_STL_SUPPORT YES)
    set(DOXYGEN_GENERATE_HTML YES)
    set(DOXYGEN_HTML_OUTPUT ${SLUG_BINARY_DIR}/doc)

    doxygen_add_docs(slug-docs ${SLUG_SOURCES}
      ALL USE_STAMP_FILE WORKING_DIRECTORY ${SLUG_PROJECT_DIR}/src)

  else()
    message(STATUS "Doxygen not found; skipping Slug documentation generation")

  endif()
endif()

find_package(fmt QUIET)
if(NOT fmt_FOUND)
  FetchContent_Declare(fmtlib
    GIT_REPOSITORY https://github.com/fmtlib/fmt.git
    GIT_TAG        10.0.0)
    FetchContent_MakeAvailable(fmtlib)
endif()

add_library(slug)
add_library(slug::slug ALIAS slug)
target_sources(slug PRIVATE ${SLUG_SOURCES})
target_compile_features(slug PUBLIC cxx_std_17)

target_include_directories(slug
  PUBLIC
  $<BUILD_INTERFACE:${SLUG_PROJECT_DIR}/src/include>
  $<INSTALL_INTERFACE:include>)

target_compile_definitions(slug PUBLIC
  $<$<CONFIG:Debug,RelWithDebInfo>:SLUG_DEBUG>
  $<$<BOOL:${SLUG_GLOBAL}>:SLUG_GLOBAL>
  $<$<BOOL:${SLUG_WIDECHAR_GLOBAL}>:SLUG_WIDECHAR_GLOBAL>)

if(SLUG_FMT_HEADERONLY)
  target_link_libraries(slug PUBLIC fmt::fmt-header-only)
else()
  target_link_libraries(slug PUBLIC fmt::fmt)
endif()

set_target_properties(slug
  PROPERTIES
  VERSION ${SLUG_VERSION}
  SOVERSION ${SLUG_VERSION_MAJOR}
  PUBLIC_HEADER ${SLUG_PROJECT_DIR}/src/include/slug/log.h
  DEBUG_POSTFIX ${SLUG_DEBUG_POSTFIX})

add_library(slug-header-only INTERFACE)
add_library(slug::slug-header-only ALIAS slug-header-only)
target_sources(slug-header-only INTERFACE ${SLUG_PROJECT_DIR}/src/include/slug/log.h)
target_compile_features(slug-header-only INTERFACE cxx_std_17)
target_link_libraries(slug-header-only INTERFACE fmt::fmt-header-only)
target_include_directories(slug-header-only INTERFACE ${SLUG_PROJECT_DIR}/src/include)
target_compile_definitions(slug-header-only INTERFACE $<$<CONFIG:Debug,RelWithDebInfo>:SLUG_DEBUG>)

if(SLUG_ROOT_PROJECT OR BUILD_TESTING)
  include(CTest)

  find_package(Catch2 QUIET)
  if(NOT Catch2_FOUND)
    FetchContent_Declare(Catch2
    GIT_REPOSITORY https://github.com/catchorg/Catch2.git
    GIT_TAG        v3.3.2)
    FetchContent_MakeAvailable(Catch2)
  endif()

  add_executable(slug-test ${SLUG_PROJECT_DIR}/test/src/slug-test.cc)

  target_link_libraries(slug-test PRIVATE slug slug-header-only Catch2::Catch2WithMain)

  target_compile_options(slug-test PRIVATE
  $<$<CXX_COMPILER_ID:Clang>:
  -Weverything
  -Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-unused-variable

  # Catch2 debra args library or whatever its called
  -Wno-sign-conversion

  # fmt/xchar.h
  -Wno-undefined-func-template
  # fmt/format.h
  -Wno-switch-enum
  # fmt/core.h
  -Wno-documentation-unknown-command -Wno-reserved-identifier

  # fmt/format-inl.h
  -Wno-missing-noreturn -Wno-global-constructors>

  $<$<CXX_COMPILER_ID:GNU>: -Wall -Wextra>
  $<$<CXX_COMPILER_ID:MSVC>: /W4 /utf-8>)

  add_test(NAME slug-test COMMAND slug-test)
endif()

install(TARGETS slug EXPORT slug::slug
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/slug
  FRAMEWORK DESTINATION .
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(TARGETS slug-header-only EXPORT slug::slug-header-only
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/slug)

export(TARGETS slug slug-header-only NAMESPACE slug:: FILE slugTargets.cmake)

export(PACKAGE slug)
