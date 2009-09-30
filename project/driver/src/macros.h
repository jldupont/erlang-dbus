/**
 * @file macros.h
 *
 * @date   2009-09-26
 * @author jldupont
 */

#ifndef EDBUS_MACROS_H_
#define EDBUS_MACROS_H_

#ifdef  __cplusplus
#  define EDBUS_BEGIN_DECLS  extern "C" {
#  define EDBUS_END_DECLS    }
#else
#  define EDBUS_BEGIN_DECLS
#  define EDBUS_END_DECLS
#endif

#ifndef TRUE
#  define TRUE 1
#endif
#ifndef FALSE
#  define FALSE 0
#endif

#ifndef NULL
#  ifdef __cplusplus
#    define NULL        (0L)
#  else /* !__cplusplus */
#    define NULL        ((void*) 0)
#  endif /* !__cplusplus */
#endif


#ifdef _DEBUG
	#include <syslog.h>
	void doLog(int priority, const char *message, ...);
	#define DBGBEGIN
	#define DBGEND
	#define DBGMSG(...) printf(__VA_ARGS__)
	#define DBGLOG(...) doLog(__VA_ARGS__)
	#define DBGLOG_NULL_PTR(ptr, ...) if (NULL==ptr) doLog(__VA_ARGS__)
#else
	#define DBGBEGIN if(0){
	#define DBGEND   }
	#define DBGMSG(...)
	#define DBGLOG(...)
	#define DBGLOG_NULL_PTR(ptr, ...)
#endif

#endif /* MACROS_H_ */
