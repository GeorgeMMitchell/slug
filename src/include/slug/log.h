// Copyright (c) 2023 - present, George Mitchell

// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
// LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
// OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
// PERFORMANCE OF THIS SOFTWARE.

#ifndef SLUG_LOG_H_
#define SLUG_LOG_H_

#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <fmt/std.h>
#include <fmt/xchar.h>

#include <chrono>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <memory_resource>
#include <mutex>
#include <ostream>
#include <ratio>
#include <string>
#include <string_view>
#include <thread>
#include <type_traits>
#include <utility>

#ifndef SLUG_HEADER_ONLY
#define SLUG_HEADER_ONLY 0
#endif

#ifndef SLUG_GLOBAL
#define SLUG_GLOBAL 0
#endif

#ifndef SLUG_W_GLOBAL
#define SLUG_W_GLOBAL 0
#endif

#ifndef SLUG_DEBUG
#define SLUG_DEBUG 0
#endif

#define SLUG_VERSION_MAJOR 0
#define SLUG_VERSION_MINOR 0
#define SLUG_VERSION_PATCH 0

namespace slug {

namespace slug_chrono {

using clock_type = std::chrono::system_clock;

using clock_duration = typename clock_type::duration;

using clock_time_point = typename clock_type::time_point;

using rep = float;

using seconds = std::chrono::duration<rep, std::ratio<1>>;

using milliseconds = std::chrono::duration<rep, std::milli>;

using microseconds = std::chrono::duration<rep, std::micro>;

using nanoseconds = std::chrono::duration<rep, std::nano>;

template <typename Duration>
[[nodiscard]] constexpr auto to_seconds(Duration &&dur) noexcept {
  return std::chrono::duration_cast<seconds>(std::forward<Duration>(dur));
}

template <typename Duration>
[[nodiscard]] constexpr auto to_milliseconds(Duration &&dur) noexcept {
  return std::chrono::duration_cast<milliseconds>(std::forward<Duration>(dur));
}

template <typename Duration>
[[nodiscard]] constexpr auto to_microseconds(Duration &&dur) noexcept {
  return std::chrono::duration_cast<microseconds>(std::forward<Duration>(dur));
}

template <typename Duration>
[[nodiscard]] constexpr auto to_nanoseconds(Duration &&dur) noexcept {
  return std::chrono::duration_cast<nanoseconds>(std::forward<Duration>(dur));
}

}  // namespace slug_chrono

namespace slug_fmt {

/// @brief Generic allocator-aware fmt::vformat_to wrapper
/// @tparam Char Character type
/// @tparam CharTraits Character type traits
/// @tparam ...Args fmt::format_args parameter pack
/// @param alloc Allocator instance
/// @param fmt format string
/// @param ...args fmt::format_args function parameter pack
/// @return std::basic_string<Char, CharTraits, Allocator<Char>>
template <typename Char, typename CharTraits,
          template <typename> typename Allocator, typename... Args>
[[nodiscard]] static auto basic_format_to(
    Allocator<Char> const &alloc, std::basic_string_view<Char, CharTraits> fmt,
    Args &&...args) {
  auto &&make_fmtargs = [](auto &&...t_args) {
    if constexpr (std::is_same_v<Char, char>)
      return fmt::make_format_args(t_args...);
    else
      return fmt::make_wformat_args(t_args...);
  };

  auto &&fmtargs = make_fmtargs(args...);
  auto &&membuf =
      fmt::basic_memory_buffer<Char, fmt::inline_buffer_size, Allocator<Char>>{
          alloc};

  fmt::vformat_to(std::back_inserter(membuf), fmt, fmtargs);

  return std::basic_string<Char, CharTraits, Allocator<Char>>{
      membuf.data(), membuf.size(), alloc};
}

}  // namespace slug_fmt

/// @brief Log message severity level
enum struct severity_t { Fatal, Error, Warning, Debug, Trace };

/// @brief Severity string literals

/// @brief Log message contents
/// @tparam Char Character type
/// @tparam CharTraits Character type traits
/// @tparam Allocator Allocator type
template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_message_data final {
  using char_t = Char;
  using char_traits_t = CharTraits;

  using char_allocator_t = Allocator<char_t>;

  using std_string_t =
      std::basic_string<char_t, char_traits_t, char_allocator_t>;

  /// @brief Duration type for program execution time
  using program_execution_duration = slug_chrono::seconds;

  /// @brief Duration type for scope execution time
  using scope_execution_duration = slug_chrono::microseconds;

  explicit basic_message_data()
      : m_string{},
        m_severity{},
        m_start_time{},
        m_program_time{},
        m_thread_id{} {}

  explicit basic_message_data(char_allocator_t const &alloc)
      : m_string{alloc},
        m_severity{},
        m_start_time{},
        m_program_time{},
        m_thread_id{} {}

  template <typename StrT>
  explicit basic_message_data(StrT &&str, severity_t severity,
                              slug_chrono::clock_time_point start,
                              char_allocator_t const &alloc)
      : m_string{std::forward<StrT>(str), alloc},
        m_severity{severity},
        m_start_time{start},
        m_program_time{slug_chrono::clock_type::now()},
        m_thread_id{std::this_thread::get_id()} {}

  [[nodiscard]] constexpr auto program_execution_time() const noexcept
      -> program_execution_duration {
    return m_program_time - m_start_time;
  }

  [[nodiscard]] constexpr auto scope_execution_time() const noexcept
      -> scope_execution_duration {
    return slug_chrono::clock_type::now() - m_program_time;
  }

  [[nodiscard]] constexpr auto &string() const & noexcept { return m_string; }
  [[nodiscard]] constexpr auto &thread_id() const & noexcept {
    return m_thread_id;
  }

  [[nodiscard]] constexpr auto severity() const noexcept { return m_severity; }

 protected:
  std_string_t const m_string;
  severity_t const m_severity;
  slug_chrono::clock_time_point const m_start_time;
  slug_chrono::clock_time_point const m_program_time;
  std::thread::id const m_thread_id;

};  // basic_message_data

/// @brief Message format interface
/// @tparam Char Character type
/// @tparam CharTraits Character type traits
/// @tparam Allocator Allocator type
template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_message_format_base {
  using message_data_t = basic_message_data<Char, CharTraits, Allocator>;

  using char_t = Char;
  using char_traits_t = CharTraits;

  using char_allocator_t = typename message_data_t::char_allocator_t;

  using std_string_view_t = std::basic_string_view<char_t, char_traits_t>;

  using std_string_t = typename message_data_t::std_string_t;

  /// @brief Creates log message string
  /// @param message_data Log message contents to stringify
  /// @returns std::string
  [[nodiscard]] virtual std_string_t create_message(
      message_data_t const &message_data) = 0;

  /// @brief Creates log message header string
  /// @returns std::string
  [[nodiscard]] virtual std_string_t create_header_message() = 0;

  /// @brief Creates log message footer string
  /// @returns std::string
  [[nodiscard]] virtual std_string_t create_footer_message() = 0;

  virtual ~basic_message_format_base() {}

  basic_message_format_base() noexcept : m_char_allocator{} {}

  explicit basic_message_format_base(char_allocator_t const &alloc) noexcept
      : m_char_allocator{alloc} {}

  [[nodiscard]] constexpr auto &get_char_allocator() const & noexcept {
    return m_char_allocator;
  }

  [[nodiscard]] static constexpr std_string_view_t to_string_view(
      severity_t severity) noexcept {
    constexpr Char m_fatal_chars[] = {'f', 'a', 't', 'a', 'l', 0};
    constexpr Char m_error_chars[] = {'e', 'r', 'r', 'o', 'r', 0};
    constexpr Char m_warning_chars[] = {'w', 'a', 'r', 'n', 'i', 'n', 'g', 0};
    constexpr Char m_debug_chars[] = {'d', 'e', 'b', 'u', 'g', 0};
    constexpr Char m_trace_chars[] = {'t', 'r', 'a', 'c', 'e', 0};

    switch (severity) {
      case severity_t::Fatal:
        return {&m_fatal_chars[0], std::size(m_fatal_chars)};
      case severity_t::Error:
        return {&m_error_chars[0], std::size(m_error_chars)};
      case severity_t::Warning:
        return {&m_warning_chars[0], std::size(m_warning_chars)};
      case severity_t::Debug:
        return {&m_debug_chars[0], std::size(m_debug_chars)};
      case severity_t::Trace:
        return {&m_trace_chars[0], std::size(m_trace_chars)};
    }
    return {};
  }

 private:
  char_allocator_t const m_char_allocator;

};  // basic_message_format_base

/// @brief YAML message formatter
template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_yaml_message_format final
    : public basic_message_format_base<Char, CharTraits, Allocator> {
  using char_t = Char;
  using char_traits_t = CharTraits;

  using message_format_base_t =
      basic_message_format_base<Char, CharTraits, Allocator>;

  using message_data_t = typename message_format_base_t::message_data_t;
  using char_allocator_t = typename message_format_base_t::char_allocator_t;
  using std_string_t = typename message_format_base_t::std_string_t;

  explicit basic_yaml_message_format(
      char_allocator_t const &char_allocator) noexcept
      : message_format_base_t{char_allocator} {}

  [[nodiscard]] virtual std_string_t create_message(
      typename message_format_base_t::message_data_t const &message_data)
      override {
    static constexpr auto get_fmt_chars = []() -> char_t const * {
      if constexpr (std::is_same_v<char_t, char>)
        return " - [{}, '{}', '{}', {{'scope_execution_time': {}, "
               "'thread_id': "
               "{}}}]\n";
      else
        return L" - [{}, '{}', '{}', {{'scope_execution_time': {}, "
               L"'thread_id': "
               L"{}}}]\n";
    };

    return slug_fmt::basic_format_to<Char, CharTraits, Allocator>(
        message_format_base_t::get_char_allocator(), get_fmt_chars(),
        message_data.program_execution_time().count(),
        message_format_base_t::to_string_view(message_data.severity()),
        message_data.string(), message_data.scope_execution_time().count(),
        message_data.thread_id());
  }

  [[nodiscard]] std_string_t create_header_message() override {
    static constexpr char_t chars[] = {'-', '-', '-', '\n', 0};
    return std_string_t{&chars[0], message_format_base_t::get_char_allocator()};
  }

  [[nodiscard]] std_string_t create_footer_message() override {
    static constexpr char_t chars[] = {'.', '.', '.', '\n', 0};
    return std_string_t{&chars[0], message_format_base_t::get_char_allocator()};
  }

};  // basic_yaml_message_format

static constexpr auto fatal = severity_t::Fatal;
static constexpr auto error = severity_t::Error;
static constexpr auto warning = severity_t::Warning;
static constexpr auto debug = severity_t::Debug;
static constexpr auto trace = severity_t::Trace;

/// @brief Default log message severity literal
static constexpr auto default_severity =
#if SLUG_DEBUG
    debug;
#else
    warning;
#endif

/// @brief Logger config
/// @tparam Char Character type
/// @tparam CharTraits Character type traits
/// @tparam Allocator Allocator type
template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_logger_config final {
  using char_t = Char;
  using char_traits_t = CharTraits;

  using yaml_message_format_t =
      basic_yaml_message_format<char_t, char_traits_t, Allocator>;

  using message_format_base_t =
      basic_message_format_base<char_t, char_traits_t, Allocator>;

  using std_string_t = typename message_format_base_t::std_string_t;

  using char_allocator_t = typename message_format_base_t::char_allocator_t;

  severity_t severity = default_severity;

  std::shared_ptr<message_format_base_t> message_format =
      std::make_shared<yaml_message_format_t>(char_allocator_t{});

  std::ios::openmode openmode = std::ios::app;

  char_allocator_t char_allocator = char_allocator_t{};

};  // basic_logger_config

/// @brief Sends formatted messages to a sink
/// @tparam Char Character type
/// @tparam CharTraits Character type traits
/// @tparam Allocator Allocator type
template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_logger final {
  static_assert(std::is_same_v<Char, char> || std::is_same_v<Char, wchar_t>,
                "slug::basic_logger requires template parameter Char to be "
                "char or wchar_t");

  using char_t = Char;
  using char_traits_t = CharTraits;

  using logger_config_t = basic_logger_config<char_t, char_traits_t, Allocator>;

  using char_allocator_t = typename logger_config_t::char_allocator_t;

  using message_format_base_t = typename logger_config_t::message_format_base_t;
  using message_data_t = typename message_format_base_t::message_data_t;

  using std_ostream_t = std::basic_ostream<char_t, char_traits_t>;
  using std_string_view_t = std::basic_string_view<char_t, char_traits_t>;

  using std_string_t = typename logger_config_t::std_string_t;

  explicit basic_logger(std_ostream_t &os, logger_config_t cfg = {})
      : m_sink{sink::make_shared_sink(os, cfg.message_format)},
        m_atm_severity{cfg.severity},
        m_char_allocator{cfg.char_allocator} {}

  explicit basic_logger(std::filesystem::path const &path,
                        logger_config_t cfg = {})
      : m_sink{sink::make_shared_sink(path, cfg.openmode, cfg.message_format)},
        m_atm_severity{cfg.severity},
        m_char_allocator{cfg.char_allocator_t} {}

  basic_logger(basic_logger &&l) noexcept
      : m_sink{std::move(l.m_sink)},
        m_atm_severity{std::move(m_atm_severity)},
        m_char_allocator{std::move(l.m_char_allocator)} {}

  ~basic_logger() { m_sink->print_footer_message(); }

  basic_logger(basic_logger const &) = delete;

  basic_logger &operator=(basic_logger const &) = delete;

  basic_logger &operator=(basic_logger &&) = delete;

  template <typename... Args>
  [[maybe_unused]] auto log(severity_t severity, std_string_view_t fmt,
                            Args &&...args) {
    if (m_atm_severity.load() < severity)
      return emitter{message_data_t{m_char_allocator}};

    auto &&msgstr = slug_fmt::basic_format_to<Char, CharTraits, Allocator>(
        m_char_allocator, fmt, args...);

    auto &&msgdata = message_data_t{std::move(msgstr), severity, m_start_time,
                                    m_char_allocator};

    return emitter{std::move(msgdata), m_sink};
  }

  void open_file(std::filesystem::path const &path,
                 std::ios::openmode openmode = std::ios::app) {
    m_sink->set_ostream(path, openmode);
  }

  void open_console(std_ostream_t &os) { m_sink->set_ostream(os); }

  void set_severity(severity_t severity) noexcept {
    m_atm_severity.store(severity);
  }

  [[nodiscard]] constexpr auto &get_allocator() const & noexcept {
    return m_char_allocator;
  }
  [[nodiscard]] constexpr auto get_severity() const noexcept {
    return m_atm_severity.load();
  }
  [[nodiscard]] constexpr auto get_start_time() const noexcept {
    return m_start_time;
  }

  /// @brief Formats message data and prints the data to a given ostream
  struct sink final {
    sink() = default;

    sink(sink const &) = default;

    sink(sink &&) = default;

    sink(std_ostream_t &os, std::shared_ptr<message_format_base_t> const &fmt)
        : m_ostream{ostream::make_shared_ostream(os)}, m_format{fmt} {}

    sink(std::filesystem::path const &path, std::ios::openmode mode,
         std::shared_ptr<message_format_base_t> const &fmt)
        : m_ostream{ostream::make_shared_ostream(path, mode)}, m_format{fmt} {}

    [[nodiscard]] static auto make_shared_sink(
        std_ostream_t &os, std::shared_ptr<message_format_base_t> const &fmt) {
      return std::make_shared<sink>(os, fmt);
    }

    [[nodiscard]] static auto make_shared_sink(
        std::filesystem::path const &path, std::ios::openmode mode,
        std::shared_ptr<message_format_base_t> const &fmt) {
      return std::make_shared<sink>(path, mode, fmt);
    }

    void print_message(message_data_t const &msgdata) {
      print_header_message();

      static_cast<void>(std_scoped_lock{m_sink_mtx});

      if (m_ostream && m_format) {
        m_ostream->print_value(m_format->create_message(msgdata));
      }
    }

    void print_header_message() {
      static_cast<void>(std_scoped_lock{m_sink_mtx});

      if (m_ostream && m_format && !m_first_message_printed) {
        m_ostream->print_value(m_format->create_header_message());
        m_first_message_printed = true;
      }
    }

    void print_footer_message() {
      static_cast<void>(std_scoped_lock{m_sink_mtx});

      if (m_ostream && m_format && m_first_message_printed &&
          !m_last_message_printed) {
        m_ostream->print_value(m_format->create_footer_message());
        m_last_message_printed = true;
      }
    }

    void set_ostream(std_ostream_t &os) {
      print_footer_message();

      static_cast<void>(std_scoped_lock{m_sink_mtx});

      m_ostream = ostream::make_shared_ostream(os);
      m_last_message_printed = false;
      m_first_message_printed = false;
    }

    void set_ostream(std::filesystem::path const &path,
                     std::ios::openmode mode) {
      print_footer_message();

      static_cast<void>(std_scoped_lock{m_sink_mtx});

      m_ostream = ostream::make_shared_ostream(path, mode);
      m_last_message_printed = false;
      m_first_message_printed = false;
    }

    /// @brief Wraps console and file streams
    struct ostream final : public std_ostream_t {
      using std_filebuf = std::basic_filebuf<char_t, char_traits_t>;

      explicit ostream(std_ostream_t &os) : std_ostream_t{os.rdbuf()} {}

      ostream(std::filesystem::path const &path, std::ios::openmode mode)
          : std_ostream_t{&m_filebuf} {
        m_filebuf.open(path, mode | std::ios::out);
      }

      ~ostream() override {
        if (m_filebuf.is_open()) {
          m_filebuf.close();
        }
      }

      [[nodiscard]] static auto make_shared_ostream(std_ostream_t &os) {
        return std::make_shared<ostream>(os);
      }

      [[nodiscard]] static auto make_shared_ostream(
          std::filesystem::path const &path, std::ios::openmode mode) {
        return std::make_shared<ostream>(path, mode);
      }

      template <typename T>
      void print_value(T &&v) {
        static constexpr char_t m_fmt_chars[] = {'{', '}', 0};
        fmt::print(*this, &m_fmt_chars[0], std::forward<T>(v));
      }

     private:
      using std_filebuf = std::basic_filebuf<char_t, char_traits_t>;

      std_filebuf m_filebuf{};

    };  // ostream

   private:
    using std_scoped_lock = std::scoped_lock<std::mutex>;
    using std_shared_ostream = std::shared_ptr<ostream>;

    std_shared_ostream m_ostream{};
    std::shared_ptr<message_format_base_t> m_format{};

    bool m_first_message_printed = false;
    bool m_last_message_printed = false;

    std::mutex mutable m_sink_mtx{};

  };  // sink

 private:
  using std_shared_sink_t = std::shared_ptr<sink>;

  std_shared_sink_t m_sink;
  std::atomic<severity_t> m_atm_severity;
  char_allocator_t m_char_allocator;

  slug_chrono::clock_time_point const m_start_time{
      slug_chrono::clock_type::now()};

  /// @brief Emits a message at the end of its scope
  struct emitter final {
    emitter() = default;

    explicit emitter(message_data_t &&msgdata, std_shared_sink_t const &s = {})
        : m_data{std::move(msgdata)}, m_sink{s} {}

    explicit emitter(message_data_t const &msgdata,
                     std_shared_sink_t const &s = {})
        : m_data{msgdata}, m_sink{s} {}

    ~emitter() { m_sink->print_message(m_data); }

   private:
    message_data_t const m_data{};
    std_shared_sink_t const m_sink{};

  };  // emitter

};    // basic_logger

using logger = basic_logger<char, std::char_traits<char>, std::allocator>;
using wlogger =
    basic_logger<wchar_t, std::char_traits<wchar_t>, std::allocator>;

namespace pmr {

template <typename T>
using allocator = std::pmr::polymorphic_allocator<T>;

using logger = basic_logger<char, std::char_traits<char>, allocator>;
using wlogger = basic_logger<wchar_t, std::char_traits<wchar_t>, allocator>;

}  // namespace pmr

#if SLUG_GLOBAL
namespace detail {
extern logger g_logger;
}
#endif

#if SLUG_W_GLOBAL
namespace detail {
extern wlogger g_wlogger;
}
#endif

}  // namespace slug

#if SLUG_GLOBAL && !SLUG_HEADER_ONLY

#define SLUG_LOG(severity, msg, ...) \
  slug::detail::g_logger.log(severity, msg, __VA_ARGS__)

#define SLUG_SET_SEVERITY(severity) \
  slug::detail::g_logger.set_severity(severity)

#define SLUG_OPEN_FILE(path, mode) slug::detail::g_logger.open_file(path, mode)

#define SLUG_OPEN_CONSOLE(os) slug::detail::g_logger.open_console(os)

#else

#define SLUG_LOG(path, fmt, ...) ((void)0)
#define SLUG_SET_SEVERITY(severity) ((void)0)
#define SLUG_OPEN_FILE(path, mode) ((void)0)
#define SLUG_OPEN_CONSOLE(os) ((void)0)
#define SLUG_OPEN_NEW_CONSOLE(os) ((void)0)

#endif  // SLUG_GLOBAL

#if SLUG_W_GLOBAL && !SLUG_HEADER_ONLY

#define SLUG_W_LOG(severity, msg, ...) \
  slug::detail::g_wlogger.log(severity, msg, __VA_ARGS__)

#define SLUG_W_SET_SEVERITY(severity) \
  slug::detail::g_wlogger.set_severity(severity)

#define SLUG_W_OPEN_FILE(path, mode) \
  slug::detail::g_wlogger.open_file(path, mode)

#define SLUG_W_OPEN_CONSOLE(os) slug::detail::g_wlogger.open_console(os)

#else

#define SLUG_W_LOG(severity, fmt, ...) ((void)0)
#define SLUG_W_SET_SEVERITY(severity) ((void)0)
#define SLUG_W_OPEN_FILE(path, mode) ((void)0)
#define SLUG_W_OPEN_CONSOLE(os) ((void)0)
#define SLUG_W_OPEN_NEW_CONSOLE(os) ((void)0)

#endif  // SLUG_W_GLOBAL

#endif  // SLUG_LOG_H_
