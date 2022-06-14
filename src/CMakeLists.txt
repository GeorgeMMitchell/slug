if (${SLUG_ENABLE_GLOBAL} OR ${SLUG_ENABLE_WCHAR_GLOBAL} OR ${SLUG_ENABLE_TESTING})
  add_library("slug")

  target_compile_definitions("slug"
    PUBLIC
      $<$<BOOL:${SLUG_ENABLE_GLOBAL}>:"SLUG_LOG_GLOBAL">
      $<$<BOOL:${SLUG_ENABLE_GLOBAL}>:"SLUG_WLOG_GLOBAL">)

  target_compile_features("slug"
    PUBLIC
      cxx_std_17)

  target_include_directories("slug"
    PUBLIC
      "${CMAKE_CURRENT_SOURCE_DIR}/include")

  target_sources("slug"
    PRIVATE
      "include/slug.hpp"
      "slug.cpp")

else()
  add_library("slug" INTERFACE)

  target_compile_features("slug"
    INTERFACE
      cxx_std_17)

  target_include_directories("slug"
    INTERFACE
      "${CMAKE_CURRENT_SOURCE_DIR}/include")

  target_sources("slug"
    INTERFACE
      "${CMAKE_CURRENT_SOURCE_DIR}/include/slug.hpp")

endif()
