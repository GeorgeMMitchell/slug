#include <slug.hpp>

namespace slug {

#ifdef SLUG_LOG_GLOBAL
inline logger g_logger{};
#endif

#ifdef SLUG_WLOG_GLOBAL
inline wlogger g_wlogger{};
#endif

}  // namespace slug
