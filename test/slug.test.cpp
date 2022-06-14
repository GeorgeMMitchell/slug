#include <slug.hpp>

int main() {
  SLUG_LOG_FILE("slug_test.log");
  SLUG_LOG_MIN_LEVEL(slug::all);
  SLUG_LOG_CONSOLE();

  for (std::uint32_t i{}; i < 7; ++i) {
    SLUG_LOG_ERROR("error", " test", " error ", 12, ' ', 4.f, ' ', 4.234523453245);
    SLUG_LOG_WARNING("test warning");
  }

}
