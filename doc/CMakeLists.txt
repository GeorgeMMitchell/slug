find_package(Doxygen QUIET)

if(NOT Doxygen_FOUND)
  message(STATUS "Doxygen not found; skipping Slug documentation generation")
  return()
endif()

set(DOXYGEN_EXTRACT_ALL YES)
set(DOXYGEN_BUILTIN_STL_SUPPORT YES)
set(DOXYGEN_GENERATE_XML YES)
set(DOXYGEN_GENERATE_HTML YES)
set(DOXYGEN_XML_OUTPUT ${SLUG_BINARY_DIR}/doc)

doxygen_add_docs(slug-doxygen ${SLUG_PROJECT_DIR}/src ALL
                 WORKING_DIRECTORY ${SLUG_PROJECT_DIR}/src)
