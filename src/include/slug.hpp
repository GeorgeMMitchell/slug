#ifndef SLUG_HEADER
#define SLUG_HEADER

#if __cplusplus < 201703L
#error C++17 support is required to use slug
#endif

#include <atomic>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <mutex>
#include <sstream>
#include <thread>

#define SLUG_IMPLICIT

namespace slug {

/// \brief std::ostream class for sending output to a file or console
/// \tparam CharT character type
/// \tparam Traits character type traits
template <typename CharT, typename Traits = std::char_traits<CharT>>
class basic_logstream : public std::basic_ostream<CharT, Traits> {
 public:
  using os_type = std::basic_ostream<CharT, Traits>;
  using filebuf_type = std::basic_filebuf<CharT, Traits>;
  using path_type = std::filesystem::path;

 private:
  /// \brief File buffer
  filebuf_type m_filebuf{};

 public:
  /// \brief Initialize basic_logstream for console output
  basic_logstream() : os_type{get_clog().rdbuf()} {}

  /// \brief Initialize basic_logstream for file output
  /// \param filepath Path to output file
  SLUG_IMPLICIT basic_logstream(path_type const& filepath)
      : os_type{get_clog().rdbuf()} {
    open(filepath);
  }

  basic_logstream(basic_logstream const&) = delete;

  basic_logstream(basic_logstream&&) = default;

  basic_logstream& operator=(basic_logstream const&) = delete;

  basic_logstream& operator=(basic_logstream&&) = default;

  virtual ~basic_logstream() { close(); }

  /// \brief Gets console sink
  /// \returns std::clog or std::wclog
  constexpr auto& get_clog() const noexcept {
    if constexpr (std::is_same_v<CharT, char>)
      return std::clog;
    else
      return std::wclog;
  }

  /// \brief Checks if the file buffer's associated file is open
  bool is_open() const { return m_filebuf.is_open(); }

  /// \brief Opens file for output
  /// \param filepath Path to output file
  /// \returns *this
  SLUG_IMPLICIT basic_logstream& open(path_type const& filepath) {
    constexpr auto flags = std::ios::binary | std::ios::out | std::ios::app;

    if (is_open())
      close();

    if (auto&& buf = m_filebuf.open(filepath, flags); buf == nullptr)
      os_type::setstate(std::ios::failbit);

    os_type::flush();
    os_type::rdbuf(&m_filebuf);

    return *this;
  }

  /// \brief Closes the file buffer if open and switches to console output
  /// \returns *this
  basic_logstream& close() {
    os_type::flush();

    if (is_open()) {
      m_filebuf.close();
      os_type::rdbuf(get_clog().rdbuf());
    }

    return *this;
  }

  /// \brief Swap implementation
  void swap(basic_logstream& rhs) {
    if (this != std::addressof(rhs)) {
      os_type::swap(rhs);
      m_filebuf.swap(rhs.m_filebuf);
    }
  }
};  // ^ basic_logstream ^

/// \brief basic_logstream swap specialization
template <typename CharT, typename Traits = std::char_traits<CharT>>
void swap(basic_logstream<CharT, Traits>& lhs,
          basic_logstream<CharT, Traits>& rhs) {
  lhs.swap(rhs);
}

using logstream = basic_logstream<char>;
using wlogstream = basic_logstream<wchar_t>;

enum class log_level : std::uint8_t {
  All,
  Trace,
  Info,
  Warn,
  Error,
  Fatal,
  None
};

/// \brief Main logger class
/// \tparam CharT
/// \tparam Traits
template <typename CharT,
          typename Traits = std::char_traits<CharT>,
          typename StrAllocator = std::allocator<CharT>>
class basic_logger {
 public:
  using string_allocator_type = StrAllocator;
  using logstream_type = basic_logstream<CharT, Traits>;
  using path_type = typename logstream_type::path_type;
  using string_type = std::basic_string<CharT, Traits, StrAllocator>;
  using stringstream_type = std::basic_stringstream<CharT, Traits>;

#ifndef NDEBUG
  static constexpr auto const default_lvl = log_level::Info;
#else
  static constexpr auto const default_lvl = log_level::Error;
#endif

 private:
  /// \brief basic_logger object initialization time relative to epoch
  std::chrono::milliseconds m_start_time{current_time()};

  /// \brief Mutex for basic_logstream object access
  std::mutex mutable m_lstrm_mtx{};

  /// \brief basic_logstream object
  logstream_type mutable m_lstrm;

  /// \brief Default logging level
  std::atomic<log_level> m_min_lvl_atm;

 public:
  /// \brief Initializes basic_logger for console output
  /// \param lvl Sets default logging level
  basic_logger(log_level const lvl = default_lvl)
      : m_lstrm{}, m_min_lvl_atm{lvl} {}

  /// \brief Initializes basic_logger for file output
  /// \param lvl Sets default logging level
  /// \param filepath Path to output file
  SLUG_IMPLICIT basic_logger(log_level const lvl, path_type const& filepath)
      : m_lstrm{filepath}, m_min_lvl_atm{lvl} {}

  /// \brief Initializes basic_logger for file output
  /// \param filepath Path to output file
  /// \param lvl Sets default logging level
  SLUG_IMPLICIT basic_logger(path_type const& filepath,
                             log_level const lvl = default_lvl)
      : m_lstrm{filepath}, m_min_lvl_atm{lvl} {}

  basic_logger(basic_logger const&) = delete;

  basic_logger(basic_logger&&) = default;

  basic_logger& operator=(basic_logger const&) = delete;

  basic_logger& operator=(basic_logger&&) = default;

  virtual ~basic_logger() = default;

  /// \brief Returns basic_logstream object
  constexpr auto& stream() const noexcept { return m_lstrm; }

  /// \brief Locks the basic_logstream mutex in the caller's scope
  /// \returns std::unique_lock<decltype(m_lstrm_mtx)>
  [[nodiscard]] auto lock_stream() const noexcept {
    return std::lock_guard{m_lstrm_mtx};
  }

  /// \brief Sets a new default logging level value
  /// \param lvl New logging level
  /// \returns *this
  constexpr auto& min_log_level(log_level const lvl) noexcept {
    m_min_lvl_atm.store(lvl);
    return *this;
  }

  /// \brief Returns the current minimum logging level
  constexpr auto min_log_level() const noexcept { return m_min_lvl_atm.load(); }

  /// \brief Opens a file for output
  /// \param filepath Path to output file
  /// \returns *this
  auto const& open_file(path_type const& filepath) const {
    auto l{lock_stream()};
    m_lstrm.open(filepath);
    return *this;
  }

  /// \brief Closes the currount output file and switches to console output
  /// \returns *this
  auto const& close_file() const {
    auto l{lock_stream()};
    m_lstrm.close();
    return *this;
  }

  /// \brief Logs fatal messages to the sink
  /// \tparam Ts Template parameter pack of message types
  /// \param msgs Function parameter pack of messages to log
  /// \returns *this
  template <typename... Ts>
  auto const& fatal(Ts&&... msgs) const {
    if (log_level::Fatal >= m_min_lvl_atm.load()) {
      auto l{lock_stream()};
      m_lstrm << msg_prefix() << "fatal: ";
      (m_lstrm << ... << std::forward<Ts>(msgs)) << std::endl;
    }
    return *this;
  }

  /// \brief Logs error messages to the sink
  /// \tparam Ts Template parameter pack of message types
  /// \param msgs Function parameter pack of messages to log
  /// \returns *this
  template <typename... Ts>
  auto const& error(Ts&&... msgs) const {
    if (log_level::Error >= m_min_lvl_atm.load()) {
      auto l{lock_stream()};
      m_lstrm << msg_prefix() << "error: ";
      (m_lstrm << ... << std::forward<Ts>(msgs)) << std::endl;
    }
    return *this;
  }

  /// \brief Logs warning messages to the sink
  /// \tparam Ts Template parameter pack of message types
  /// \param msgs Function parameter pack of messages to log
  /// \returns *this
  template <typename... Ts>
  auto const& warning(Ts&&... msgs) const {
    if (log_level::Warn >= m_min_lvl_atm.load()) {
      auto l{lock_stream()};
      m_lstrm << msg_prefix() << "warn:  ";
      (m_lstrm << ... << std::forward<Ts>(msgs)) << std::endl;
    }
    return *this;
  }

  /// \brief Logs info messages to the sink
  /// \tparam Ts Template parameter pack of message types
  /// \param msgs Function parameter pack of messages to log
  /// \returns *this
  template <typename... Ts>
  auto const& info(Ts&&... msgs) const {
    if (log_level::Info >= m_min_lvl_atm.load()) {
      auto l{lock_stream()};
      m_lstrm << msg_prefix() << "info:  ";
      (m_lstrm << ... << std::forward<Ts>(msgs)) << std::endl;
    }
    return *this;
  }

  /// \brief Logs trace messages to the sink
  /// \tparam Ts Template parameter pack of message types
  /// \param msgs Function parameter pack of messages to log
  /// \returns *this
  template <typename... Ts>
  auto const& trace(Ts&&... msgs) const {
    if (log_level::Trace >= m_min_lvl_atm.load()) {
      auto l{lock_stream()};
      m_lstrm << msg_prefix() << "trace: ";
      (m_lstrm << ... << std::forward<Ts>(msgs)) << std::endl;
    }
    return *this;
  }

  /// \brief Creates the message prefix for a log entry
  /// \returns basic_string<CharT, Traits>
  string_type msg_prefix() const {
    auto sstrm = stringstream_type{std::ios_base::out};

    sstrm << '[' << std::setw(5) << std::this_thread::get_id() << ',' << ' ';
    sstrm << std::fixed << std::setprecision(3)
          << time_cast<double>(elapsed_time()) << "] ";

    return sstrm.str();
  }

  /// \brief Returns the current time since epoch in milliseconds
  std::chrono::milliseconds current_time() const noexcept {
    namespace chr = std::chrono;
    auto const now = chr::steady_clock::now().time_since_epoch();
    return chr::duration_cast<chr::milliseconds>(now);
  }

  /// \brief Returns the time since object initialization in seconds
  std::chrono::milliseconds elapsed_time() const noexcept {
    namespace chr = std::chrono;
    return current_time() - m_start_time;
  }

  /// \brief Casts time duration to another type
  /// \tparam OutRep Type to convert the input time duration to
  /// \tparam OutPeriod Period to convert the input time duration to (defaults to seconds)
  /// \tparam InRep Input time duration type
  /// \tparam InPeriod Input period type
  /// \param in Input time duration to convert from
  template <typename OutRep,
            typename OutPeriod = std::ratio<1>,
            typename InRep,
            typename InPeriod>
  constexpr auto time_cast(
      std::chrono::duration<InRep, InPeriod>&& in) const noexcept {
    using in_duration_type = std::chrono::duration<InRep, InPeriod>;
    using out_duration_type = std::chrono::duration<OutRep, OutPeriod>;

    return std::chrono::duration_cast<out_duration_type>(
               std::forward<in_duration_type>(in))
        .count();
  }

  /// \brief Returns initialization time relative to epoch in milliseconds
  constexpr auto start_time() const noexcept { return m_start_time; }

  void swap(basic_logger&) = delete;
};  // ^ basic_logger ^

template <typename CharT,
          typename Traits = std::char_traits<CharT>,
          typename StrAllocator = std::allocator<CharT>>
void swap(basic_logger<CharT, Traits, StrAllocator>& lhs,
          basic_logger<CharT, Traits, StrAllocator>& rhs) = delete;

using logger = basic_logger<char, std::char_traits<char>, std::allocator<char>>;
using wlogger =
    basic_logger<wchar_t, std::char_traits<wchar_t>, std::allocator<wchar_t>>;

#ifdef SLUG_LOG_GLOBAL
extern logger g_logger;
#endif

#ifdef SLUG_WLOG_GLOBAL
extern wlogger g_wlogger;
#endif

static constexpr auto const none = log_level::None;
static constexpr auto const fatal = log_level::Fatal;
static constexpr auto const error = log_level::Error;
static constexpr auto const warn = log_level::Warn;
static constexpr auto const info = log_level::Info;
static constexpr auto const trace = log_level::Trace;
static constexpr auto const all = log_level::All;

}  // namespace slug

#endif  // SLUG_HEADER
