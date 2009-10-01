/**
 * @file erlang_dbus_driver.h
 *
 * @date   2009-09-26
 * @author Jean-Lou Dupont
 *
 * @mainpage
 *
 * \section Protocol
 *
 * 	\subsection Types
 *
 * 		\li Type=         atom()
 * 		\li Serial=       int()      % unsigned int
 * 		\li Sender=       string()
 * 		\li Destination=  string()
 * 		\li Path=         string()
 * 		\li Interface=    string()
 * 		\li Member=       string()
 * 		\li Message=      term()
 *
 * 	\subsection METHOD & SIGNAL
 *
 * 		[m|s, Serial, Sender, Destination, Path, Interface, Member, [Message]]
 *
 *	\subsection METHOD RETURN
 *
 * 		[r,   Serial, Sender, Destination, [Message]]
 *
 *	\subsection ERROR
 *
 * 		[e,   Serial, Sender, Destination, Name, [Message]]
 *
 * 	\subsection DBus primitives & Compound Types
 *
 * 		\li STRING      ==> {str, String}
 * 		\li SIGNATURE   ==> {sig, String}
 * 		\li OBJECT PATH ==> {op,  String}
 *		\li INT16       ==> {i16,  Integer}
 *		\li UINT16      ==> {ui16, Integer}
 *		\li INT32       ==> {i32,  Integer}
 *		\li UINT32      ==> {ui32, Integer}
 *		\li INT64       ==> {i64,  Integer}
 *		\li UINT64      ==> {ui64, Integer}
 *		\li DOUBLE      ==> {f,    Float}
 *		\li BYTE        ==> {by,   Integer}
 *		\li BOOLEAN     ==> {bo,   Integer}
 *		\li VARIANT     ==> {var,  term() }
 *		\li ARRAY       ==> {arr,  [term()] }
 *		\li DICT_ENTRY  ==> {dic, term(), term() }
 *		\li STRUCT      ==> {st, [term()] }
 */

#ifndef ERLANG_DBUS_DRIVER_H_
#define ERLANG_DBUS_DRIVER_H_

 #include "macros.h"
 #include <stdlib.h>
#include <stdarg.h>
 #include <pthread.h>
 #include <dbus/dbus.h>
 #include "epapi.h"

	/**
	 * Process Exit Codes
	 */

	#define EDBUS_OK                  0
	#define EDBUS_CONN_ERROR          1
	#define EDBUS_DISCONNECTED        2
	#define EDBUS_ADD_MATCH_ERROR     3
	#define EDBUS_ADD_FILTER_ERROR    4
	#define EDBUS_INIT_MESSAGE        5  // error initializing message back to Erlang
	#define EDBUS_SEND_ERROR          6
	#define EDBUS_UNSUPPORTED_TYPE    7
	#define EDBUS_UNRECOVERABLE_ERROR 8




#endif /* ERLANG_DBUS_DRIVER_H_ */
