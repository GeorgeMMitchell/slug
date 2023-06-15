// Copyright (c) 2023 - present George Mitchell
// See License.txt for license information

#include <slug/log.h>

#include <iostream>

#if SLUG_GLOBAL
inline auto slug::detail::g_logger = slug::logger{std::clog};
#endif

#if SLUG_W_GLOBAL
inline auto slug::detail::g_wlogger = slug::wlogger{std::wclog};
#endif
