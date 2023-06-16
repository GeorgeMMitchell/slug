find_package(Doxygen QUIET)

if(Doxygen_FOUND)
  set(DOXYGEN_EXTRACT_ALL YES)
  set(DOXYGEN_BUILTIN_STL_SUPPORT YES)
  set(DOXYGEN_GENERATE_HTML YES)
  set(DOXYGEN_HTML_OUTPUT ${SLUG_BINARY_DIR}/doc)

  doxygen_add_docs(slug-docs ${SLUG_SOURCES} ALL USE_STAMP_FILE
                   WORKING_DIRECTORY ${SLUG_PROJECT_DIR}/src)

else()
  message(STATUS "Doxygen not found; skipping Slug documentation generation")

endif()
