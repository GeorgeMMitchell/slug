// Copyright (c) 2023, George Mitchell
// See License.txt for license information

#include <assert.h>
#include <slug/log.h>

#include <catch2/catch_all.hpp>
#include <iostream>
#include <string_view>

namespace slug::test {

struct config final {
  static constexpr std::string_view out_filename = "slug-test-log.yml";
  static constexpr std::wstring_view wout_filename = L"slug-test-log.yml";

};  // config

}  // namespace slug::test

TEST_CASE("Logger Declaration", "[branching]") {
  [[maybe_unused]] auto &&logger = slug::logger{std::clog};

  logger.set_severity(slug::trace);

  [[maybe_unused]] auto &&allocator = logger.get_allocator();
  [[maybe_unused]] auto &&severity = logger.get_severity();
  [[maybe_unused]] auto &&start_time = logger.get_start_time();

  logger.open_file(slug::test::config::out_filename);
  logger.log(slug::trace, "Writing message to {}", "file");

  logger.open_console(std::clog);
  logger.log(slug::trace, "Writing message to {}", "console");
}

TEST_CASE("Wide Logger Declaration", "[branching]") {
  [[maybe_unused]] auto &&wlogger = slug::wlogger{std::wclog};

  wlogger.set_severity(slug::trace);

  [[maybe_unused]] auto &&allocator = wlogger.get_allocator();
  [[maybe_unused]] auto &&severity = wlogger.get_severity();
  [[maybe_unused]] auto &&start_time = wlogger.get_start_time();

  wlogger.open_file(slug::test::config::wout_filename);
  wlogger.log(slug::trace, L"Writing wide message to file");

  wlogger.open_console(std::wclog);
  wlogger.log(slug::trace, L"Writing wide message to console");
}

TEST_CASE("Native Global Macro", "[globals]") {
  static_assert(SLUG_GLOBAL,
                "Enable slug's global logging instance (-DSLUG_GLOBAL=ON)");

  SLUG_SET_SEVERITY(slug::trace);

  SLUG_LOG(slug::trace, "Test {} message from global logging instance",
           "console");

  SLUG_OPEN_FILE(slug::test::config::out_filename, std::ios::app);

  SLUG_LOG(slug::trace, "Test {} message from global logging instance", "file");

  SLUG_OPEN_CONSOLE(std::clog);
}

TEST_CASE("Wide Global Macro", "[globals]") {
  static_assert(SLUG_W_GLOBAL,
                "Enable slug's wide character global logging instance");

  // SLUG_W_SET_SEVERITY(slug::trace);

  // SLUG_W_LOG(slug::trace, L"Test {} message from global logging instance",
  //            "console");

  // SLUG_W_OPEN_FILE(slug::test::config::out_filename, std::ios::app);

  // SLUG_W_LOG(slug::trace, L"Test {} message from global logging instance",
  //            L"file");

  // SLUG_W_OPEN_CONSOLE(std::wclog);
}
