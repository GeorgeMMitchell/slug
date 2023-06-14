// Copyright (c) 2023, George Mitchell
// See License.txt for license information

#include <slug/log.h>

#include <catch2/catch_all.hpp>

#include <iostream>

TEST_CASE("Logger Branching", "[branching]") {
  auto &&logger = slug::logger{std::clog};

  logger.set_severity(slug::trace);

  auto &&allocator = logger.get_allocator();
  auto &&severity = logger.get_severity();
  auto &&start_time = logger.get_start_time();

  logger.open_file("slug-test.log");
  logger.log(slug::trace, "Writing message to file");

  logger.open_console(std::clog);
  logger.log(slug::trace, "Writing message to console");
}

TEST_CASE("Wide Logger Branching", "[branching]") {
  auto &&wlogger = slug::wlogger{std::wclog};

  wlogger.set_severity(slug::trace);

  auto &&allocator = wlogger.get_allocator();
  auto &&severity = wlogger.get_severity();
  auto &&start_time = wlogger.get_start_time();

  wlogger.open_file("wslug-test.log");
  wlogger.log(slug::trace, L"Writing wide message to file");

  wlogger.open_console(std::wclog);
  wlogger.log(slug::trace, L"Writing wide message to console");
}
