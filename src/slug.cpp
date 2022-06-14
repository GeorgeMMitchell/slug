#include <slug.hpp>

#ifdef SLUG_LOG_GLOBAL
namespace slug {
  inline logger g_logger{};
}
#endif

#ifdef SLUG_WLOG_GLOBAL
namespace slug {
  inline wlogger g_wlogger{};
}
#endif
