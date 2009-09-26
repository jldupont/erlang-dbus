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


#endif /* MACROS_H_ */
