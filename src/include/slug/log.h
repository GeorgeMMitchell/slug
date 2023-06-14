// Copyright (c) 2023, George Mitchell

// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.

// THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
// LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
// OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
// PERFORMANCE OF THIS SOFTWARE.

#ifndef SLUG_H_LOG
#define SLUG_H_LOG

#include <fmt/format.h>
#include <fmt/std.h>
#include <fmt/xchar.h>

#include <chrono>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <memory_resource>
#include <mutex>
#include <ostream>
#include <string>
#include <string_view>
#include <thread>
#include <type_traits>
#include <utility>

#define SLUG_VERSION_MAJOR 0
#define SLUG_VERSION_MINOR 1
#define SLUG_VERSION_PATCH 0

namespace slug {

namespace chrono {

using clock_type = std::chrono::system_clock;
using clock_duration = typename clock_type::duration;
using clock_time_point = typename clock_type::time_point;

using rep = float;

using seconds = std::chrono::duration<rep, std::ratio<1>>;
using milliseconds = std::chrono::duration<rep, std::milli>;
using microseconds = std::chrono::duration<rep, std::micro>;
using nanoseconds = std::chrono::duration<rep, std::nano>;

using program_execution_duration = seconds;
using scope_execution_duration = microseconds;

template <typename Duration>
constexpr auto to_seconds(Duration &&dur) noexcept {
  return std::chrono::duration_cast<seconds>(std::forward<Duration>(dur));
}

template <typename Duration>
constexpr auto to_milliseconds(Duration &&dur) noexcept {
  return std::chrono::duration_cast<milliseconds>(std::forward<Duration>(dur));
}

template <typename Duration>
constexpr auto to_microseconds(Duration &&dur) noexcept {
  return std::chrono::duration_cast<microseconds>(std::forward<Duration>(dur));
}

template <typename Duration>
constexpr auto to_nanoseconds(Duration &&dur) noexcept {
  return std::chrono::duration_cast<nanoseconds>(std::forward<Duration>(dur));
}

}  // namespace chrono

enum struct severity_t { Fatal, Error, Warning, Debug, Trace };

static constexpr auto fatal = severity_t::Fatal, error = severity_t::Error,
                      warning = severity_t::Warning, debug = severity_t::Debug,
                      trace = severity_t::Trace;

static constexpr auto default_severity =
#ifdef SLUG_DEBUG
    debug;
#else
    warning;
#endif

template <typename Char, typename CharTraits,
          template <typename> typename Allocator, typename... Args>
static auto basic_format_to(Allocator<Char> const &alloc,
                            std::basic_string_view<Char, CharTraits> fmt,
                            Args &&...args) {
  using std_string = std::basic_string<Char, CharTraits, Allocator<Char>>;
  using fmt_membuf =
      fmt::basic_memory_buffer<Char, fmt::inline_buffer_size, Allocator<Char>>;

  auto &&fmtargs = [](auto &&...t_args) {
    if constexpr (std::is_same_v<Char, char>) {
      return fmt::make_format_args(t_args...);
    } else {
      return fmt::make_wformat_args(t_args...);
    }
  }(args...);

  return [&alloc, fmt](auto &&t_fmtargs) -> std_string {
    auto &&membuf = fmt_membuf{alloc};

    fmt::vformat_to(std::back_inserter(membuf), fmt, t_fmtargs);

    return std_string{membuf.data(), membuf.size(), alloc};
  }(fmtargs);

}  // basic_format_to

template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_message_data final {
  using char_t = Char;
  using char_traits = CharTraits;
  using char_allocator = Allocator<Char>;

  using std_string = std::basic_string<char_t, char_traits, char_allocator>;

  explicit basic_message_data(char_allocator const &alloc) : m_string{alloc} {}

  template <typename StrT>
  explicit basic_message_data(StrT &&str, severity_t severity,
                              chrono::clock_time_point start,
                              char_allocator const &alloc)
      : m_string{std::forward<StrT>(str), alloc},
        m_severity{severity},
        m_start_time{start},
        m_program_time{chrono::clock_type::now()},
        m_thread_id{std::this_thread::get_id()} {}

  [[nodiscard]] constexpr auto program_execution_time() const noexcept
      -> chrono::program_execution_duration {
    return m_program_time - m_start_time;
  }

  [[nodiscard]] constexpr auto scope_execution_time() const noexcept
      -> chrono::scope_execution_duration {
    return chrono::clock_type::now() - m_program_time;
  }

  [[nodiscard]] constexpr auto &string() const & noexcept { return m_string; }
  [[nodiscard]] constexpr auto &thread_id() const & noexcept {
    return m_thread_id;
  }

  [[nodiscard]] constexpr auto severity() const noexcept { return m_severity; }

 protected:
  std_string const m_string;
  severity_t const m_severity{};
  chrono::clock_time_point const m_start_time{};
  chrono::clock_time_point const m_program_time{};
  std::thread::id const m_thread_id{};

};  // basic_message_data

template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_message_format_base {
  using char_t = Char;
  using char_traits = CharTraits;
  using char_allocator = Allocator<Char>;

  using message_data = basic_message_data<Char, CharTraits, Allocator>;
  using std_string = std::basic_string<char_t, char_traits, char_allocator>;
  using std_string_view = std::basic_string_view<char_t, char_traits>;

  virtual std_string create_message(message_data const &) = 0;
  virtual std_string create_header_message() = 0;
  virtual std_string create_footer_message() = 0;

  virtual ~basic_message_format_base() {}

  basic_message_format_base() noexcept : m_char_allocator{} {}

  explicit basic_message_format_base(char_allocator const &alloc) noexcept
      : m_char_allocator{alloc} {}

  constexpr auto &get_char_allocator() const & noexcept {
    return m_char_allocator;
  }

  constexpr std_string_view get_severity_literal(
      severity_t sev) const noexcept {
    return severity_literals::to_string_view(sev);
  }

 private:
  char_allocator const m_char_allocator;

  struct severity_literals final {
    using char_t = Char;
    using char_traits = CharTraits;

    static constexpr std_string_view to_string_view(
        severity_t severity) noexcept {
      switch (severity) {
        case fatal:
          return &m_fatal_chars[0];
        case error:
          return &m_error_chars[0];
        case warning:
          return &m_warning_chars[0];
        case debug:
          return &m_debug_chars[0];
        case trace:
          return &m_trace_chars[0];
      }
      return {};
    }

   private:
    static constexpr char_t m_fatal_chars[] = {'f', 'a', 't', 'a', 'l', 0};
    static constexpr char_t m_error_chars[] = {'e', 'r', 'r', 'o', 'r', 0};
    static constexpr char_t m_warning_chars[] = {'w', 'a', 'r', 'n',
                                                 'i', 'n', 'g', 0};
    static constexpr char_t m_debug_chars[] = {'d', 'e', 'b', 'u', 'g', 0};
    static constexpr char_t m_trace_chars[] = {'t', 'r', 'a', 'c', 'e', 0};

  };  // severity_literals

};    // basic_message_format_base

template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_yaml_message_format final
    : public basic_message_format_base<Char, CharTraits, Allocator> {
  using char_t = Char;
  using char_traits = CharTraits;
  using char_allocator = Allocator<Char>;
  using message_data = basic_message_data<Char, CharTraits, Allocator>;
  using message_format_base =
      basic_message_format_base<Char, CharTraits, Allocator>;
  using std_string = std::basic_string<char_t, char_traits, char_allocator>;

  explicit basic_yaml_message_format(char_allocator const &alloc) noexcept
      : message_format_base{alloc} {}

  std_string create_message(message_data const &m) override {
    static constexpr auto get_fmt_chars = []() -> char_t const * {
      if constexpr (std::is_same_v<char_t, char>)
        return " - [{}, '{}', '{}', {{'scope_execution_time': {}, 'thread_id': "
               "{}}}]\n";
      else
        return L" - [{}, '{}', '{}', {{'scope_execution_time': {}, "
               L"'thread_id': "
               L"{}}}]\n";
    };

    return basic_format_to<Char, CharTraits, Allocator>(
        message_format_base::get_char_allocator(), get_fmt_chars(),
        m.program_execution_time().count(),
        message_format_base::get_severity_literal(m.severity()), m.string(),
        m.scope_execution_time().count(), m.thread_id());
  }

  std_string create_header_message() override {
    static constexpr char_t chars[] = {'-', '-', '-', '\n', 0};
    return std_string{&chars[0], message_format_base::get_char_allocator()};
  }

  std_string create_footer_message() override {
    static constexpr char_t chars[] = {'.', '.', '.', '\n', 0};
    return std_string{&chars[0], message_format_base::get_char_allocator()};
  }
};  // basic_yaml_message_format

template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_logger_config final {
  using message_format_base =
      basic_message_format_base<Char, CharTraits, Allocator>;
  using yaml_message_format =
      basic_yaml_message_format<Char, CharTraits, Allocator>;
  using std_shared_message_format_base = std::shared_ptr<message_format_base>;

  severity_t severity = default_severity;

  std_shared_message_format_base message_format =
      std::make_shared<yaml_message_format>(Allocator<Char>{});

  std::ios::openmode openmode = std::ios::app;

};  // basic_logger_config

template <typename Char, typename CharTraits,
          template <typename> typename Allocator>
struct basic_logger final {
  static_assert(std::is_same_v<Char, char> || std::is_same_v<Char, wchar_t>,
                "slug::basic_logger requires template parameter Char to be "
                "char or wchar_t");

  using char_t = Char;
  using char_traits = CharTraits;
  using char_allocator = Allocator<Char>;

  using logger_config = basic_logger_config<Char, CharTraits, Allocator>;
  using message_data = basic_message_data<Char, CharTraits, Allocator>;

  using std_ostream = std::basic_ostream<char_t, char_traits>;
  using std_string = std::basic_string<char_t, char_traits, char_allocator>;
  using std_string_view = std::basic_string_view<char_t, char_traits>;
  using std_shared_message_format_base =
      typename logger_config::std_shared_message_format_base;

  explicit basic_logger(std_ostream &os, logger_config cfg = {},
                        char_allocator const &alloc = {})
      : m_sink{sink::make_shared_sink(os, cfg.message_format)},
        m_logger_config{cfg},
        m_char_allocator{alloc} {}

  explicit basic_logger(std::filesystem::path const &path,
                        logger_config cfg = {})
      : m_sink{sink::make_shared_sink(path, cfg.openmode, cfg.message_format)},
        m_logger_config{cfg},
        m_char_allocator{alloc} {}

  basic_logger(basic_logger &&l) noexcept
      : m_sink{std::move(l.m_sink)},
        m_logger_config{std::move(m_logger_config)},
        m_char_allocator{std::move(l.m_char_allocator)} {}

  ~basic_logger() { m_sink->print_footer_message(); }

  basic_logger(basic_logger const &) = delete;

  basic_logger &operator=(basic_logger const &) = delete;

  basic_logger &operator=(basic_logger &&) = delete;

  template <typename... Args>
  auto log(severity_t severity, std_string_view fmt, Args &&...args) {
    if (m_logger_config.severity < severity) {
      return emitter{message_data{m_char_allocator}};
    }

    auto &&msgstr = basic_format_to<Char, CharTraits, Allocator>(
        m_char_allocator, fmt, args...);

    auto &&msgdata = message_data{std::move(msgstr), severity, m_start_time,
                                  m_char_allocator};

    return emitter{std::move(msgdata), m_sink};
  }

  void open_file(std::filesystem::path const &path,
                 std::ios::openmode openmode = std::ios::app) {
    m_sink->set_ostream(path, openmode);
  }

  void open_console(std_ostream &os) { m_sink->set_ostream(os); }

  void set_severity(severity_t severity) noexcept {
    m_logger_config.severity = severity;
  }

  [[nodiscard]] constexpr auto &get_allocator() const & noexcept {
    return m_char_allocator;
  }
  [[nodiscard]] constexpr auto get_severity() const noexcept {
    return m_logger_config.severity;
  }
  [[nodiscard]] constexpr auto get_start_time() const noexcept {
    return m_start_time;
  }

  struct sink final {
    sink() = default;

    sink(sink const &) = default;

    sink(sink &&) = default;

    sink(std_ostream &os, std_shared_message_format_base const &fmt)
        : m_ostream{ostream::make_shared_ostream(os)}, m_format{fmt} {}

    sink(std::filesystem::path const &path, std::ios::openmode mode,
         std_shared_message_format_base const &fmt)
        : m_ostream{ostream::make_shared_ostream(path, mode)}, m_format{fmt} {}

    [[nodiscard]] static auto make_shared_sink(
        std_ostream &os, std_shared_message_format_base const &fmt) {
      return std::make_shared<sink>(os, fmt);
    }

    [[nodiscard]] static auto make_shared_sink(
        std::filesystem::path const &path, std::ios::openmode mode,
        std_shared_message_format_base const &fmt) {
      return std::make_shared<sink>(path, mode, fmt);
    }

    void print_message(message_data const &msgdata) {
      print_header_message();

      static_cast<void>(std_scoped_lock{m_sink_mutex});

      if (m_ostream && m_format) {
        m_ostream->print_value(m_format->create_message(msgdata));
      }
    }

    void print_header_message() {
      static_cast<void>(std_scoped_lock{m_sink_mutex});

      if (m_ostream && m_format && !m_first_message_printed) {
        m_ostream->print_value(m_format->create_header_message());
        m_first_message_printed = true;
      }
    }

    void print_footer_message() {
      static_cast<void>(std_scoped_lock{m_sink_mutex});

      if (m_ostream && m_format && m_first_message_printed &&
          !m_last_message_printed) {
        m_ostream->print_value(m_format->create_footer_message());
        m_last_message_printed = true;
      }
    }

    void set_ostream(std_ostream &os) {
      print_footer_message();

      static_cast<void>(std_scoped_lock{m_sink_mutex});

      m_ostream = ostream::make_shared_ostream(os);
      m_last_message_printed = false;
      m_first_message_printed = false;
    }

    void set_ostream(std::filesystem::path const &path,
                     std::ios::openmode mode) {
      print_footer_message();

      static_cast<void>(std_scoped_lock{m_sink_mutex});

      m_ostream = ostream::make_shared_ostream(path, mode);
      m_last_message_printed = false;
      m_first_message_printed = false;
    }

    struct ostream final : public std_ostream {
      using std_filebuf = std::basic_filebuf<char_t, char_traits>;

      explicit ostream(std_ostream &os) : std_ostream{os.rdbuf()} {}

      ostream(std::filesystem::path const &path, std::ios::openmode mode)
          : std_ostream{&m_filebuf} {
        m_filebuf.open(path, mode | std::ios::out);
      }

      ~ostream() override {
        if (m_filebuf.is_open()) {
          m_filebuf.close();
        }
      }

      [[nodiscard]] static auto make_shared_ostream(std_ostream &os) {
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
      using std_filebuf = std::basic_filebuf<char_t, char_traits>;

      std_filebuf m_filebuf{};

    };  // ostream

   private:
    using std_scoped_lock = std::scoped_lock<std::mutex>;
    using std_shared_ostream = std::shared_ptr<ostream>;

    std_shared_ostream m_ostream{};
    std_shared_message_format_base m_format{};

    bool m_first_message_printed = false;
    bool m_last_message_printed = false;

    std::mutex mutable m_sink_mutex{};

  };  // sink

 private:
  using std_shared_sink = std::shared_ptr<sink>;

  std_shared_sink m_sink;
  logger_config m_logger_config;
  char_allocator m_char_allocator{};

  chrono::clock_time_point const m_start_time{chrono::clock_type::now()};

  struct emitter final {
    emitter() = default;

    explicit emitter(message_data &&msgdata, std_shared_sink const &s = {})
        : m_data{std::move(msgdata)}, m_sink{s} {}

    explicit emitter(message_data const &msgdata, std_shared_sink const &s = {})
        : m_data{msgdata}, m_sink{s} {}

    ~emitter() { m_sink->print_message(m_data); }

   private:
    message_data const m_data{};
    std_shared_sink const m_sink{};

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

namespace detail {

#ifdef SLUG_GLOBAL
extern logger g_logger;
#endif

#ifdef SLUG_WIDECHAR_GLOBAL
extern wlogger g_wlogger;
#endif

}  // namespace detail

}  // namespace slug

#ifdef SLUG_GLOBAL

#define SLUG_LOG_FMT(severity, fmt, ...) \
  slug::detail::g_logger.log(severity, fmt, __VA_ARGS__)

#define SLUG_LOG(severity, msg) slug::detail::g_logger.log(severity, msg)

#define SLUG_SET_SEVERITY(severity) \
  slug::detail::g_logger.set_severity(severity)

#define SLUG_OPEN_FILE(path, mode) slug::detail::g_logger.open_file(path, mode)

#define SLUG_OPEN_CONSOLE() slug::detail::g_logger.open_console()

#define SLUG_OPEN_NEW_CONSOLE(os) slug::detail::g_logger.open_console(os)

#else

#define SLUG_LOG_FMT(severity, fmt, ...) ((void)0)
#define SLUG_LOG(severity, msg) ((void)0)
#define SLUG_SET_SEVERITY(severity) ((void)0)
#define SLUG_SET_ALLOCATOR(alloc) ((void)0)
#define SLUG_OPEN_FILE(path, mode) ((void)0)
#define SLUG_OPEN_CONSOLE() ((void)0)
#define SLUG_OPEN_NEW_CONSOLE(os) ((void)0)

#endif  // SLUG_GLOBAL

#ifdef SLUG_WIDECHAR_GLOBAL

#define SLUG_W_LOG_FMT(severity, fmt, ...) \
  slug::detail::g_wlogger.log(severity, fmt, __VA_ARGS__)

#define SLUG_W_LOG(severity, msg) slug::detail::g_wlogger.log(severity, msg)

#define SLUG_W_SET_SEVERITY(severity) \
  slug::detail::g_wlogger.set_severity(severity)

#define SLUG_W_OPEN_FILE(path, mode) \
  slug::detail::g_wlogger.open_file(path, mode)

#define SLUG_W_OPEN_CONSOLE() slug::detail::g_wlogger.open_console()

#define SLUG_W_OPEN_NEW_CONSOLE(os) slug::detail::g_wlogger.open_console(os)

#else

#define SLUG_W_LOG_FMT(severity, fmt, ...) ((void)0)
#define SLUG_W_LOG(severity, msg) ((void)0)
#define SLUG_W_SET_SEVERITY(severity) ((void)0)
#define SLUG_W_SET_ALLOCATOR(alloc) ((void)0)
#define SLUG_W_OPEN_FILE(path, mode) ((void)0)
#define SLUG_W_OPEN_CONSOLE() ((void)0)
#define SLUG_W_OPEN_NEW_CONSOLE(os) ((void)0)

#endif  // SLUG_WIDECHAR_GLOBAL

#endif  // SLUG_H_LOG
