/**
 * @file logger.cc
 *
 * @date   2009-09-30
 * @author jldupont
 */

#include "macros.h"


#ifdef _DEBUG
/**
 * Crude logging function
 */
void doLog(int priority, const char *message, ...) {

	openlog("erlang-dbus-driver", LOG_PID, LOG_LOCAL1);

	char buffer[2048];
	va_list ap;

	va_start(ap, message);
		vsnprintf (buffer, sizeof(buffer), message, ap);
	va_end(ap);

	syslog(priority, buffer, NULL);

	closelog();

}
#endif
