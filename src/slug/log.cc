// Copyright (c) 2023, George Mitchell
// Refer to License.txt for license information

#include <slug/log.h>

#include <iostream>

namespace slug::detail {

#ifdef SLUG_GLOBAL
inline logger g_logger{std::clog};
#endif

#ifdef SLUG_WIDECHAR_GLOBAL
inline wlogger g_wlogger{std::wclog};
#endif

}  // namespace slug::detail
