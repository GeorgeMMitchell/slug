include("CTest")

find_package("Boost" 1.77 REQUIRED COMPONENTS)

add_executable("slug_test")

target_link_libraries("slug_test"
  PRIVATE
    "slug"
    "Boost::headers")

target_sources("slug_test"
  PRIVATE
    "slug.test.cpp")
