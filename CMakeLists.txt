cmake_minimum_required(VERSION 3.16 FATAL_ERROR)

project("slug" VERSION 1.0.0.0 LANGUAGES CXX)

set(SLUG_ENABLE_GLOBAL ON CACHE BOOL "Builds slug with the default global instance")

set(SLUG_ENABLE_WCHAR_GLOBAL OFF CACHE BOOL "Builds slug with the wide character format global instance")

set(SLUG_ENABLE_TESTING OFF CACHE BOOL "Build and test slug")

add_subdirectory("src")

if(${SLUG_ENABLE_TESTING})
  add_subdirectory("test")
endif()
