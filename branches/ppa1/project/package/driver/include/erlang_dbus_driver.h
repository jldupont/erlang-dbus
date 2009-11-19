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
 * As the driver gets registered on the DBus and acquires a connection, a 'unique-name' is assigned
 * to the connection.  This 'unique-name' (e.g. ":1.1173") is useful for filtering incoming messages.
 * The 'unique-name' is forwarded to the Erlang Client through the following message:
 *
 * 	{uniq_name, string()}
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
 * 		\li STRING      ==> {str,  String}
 * 		\li SIGNATURE   ==> {sig,  String}
 * 		\li OBJECT PATH ==> {op,   String}
 *		\li INT16       ==> {i16,  Integer}
 *		\li UINT16      ==> {ui16, Integer}
 *		\li INT32       ==> {i32,  Integer}
 *		\li UINT32      ==> {ui32, Integer}
 *		\li INT64       ==> {i64,  Integer}
 *		\li UINT64      ==> {ui64, Integer}
 *		\li DOUBLE      ==> {f,    Float}
 *		\li BYTE        ==> {by,   Integer}
 *		\li BOOLEAN     ==> {bo,   Integer}
 *		\li VARIANT     ==> {v,    term() }
 *		\li ARRAY       ==> {a,    [term()] }
 *		\li DICT_ENTRY  ==> {d,    term(), term() }
 *		\li STRUCT      ==> {st,   [term()] }
 */

#ifndef ERLANG_DBUS_DRIVER_H_
#define ERLANG_DBUS_DRIVER_H_

 #include "macros.h"
 #include <stdlib.h>
 #include <stdarg.h>
 #include <string.h>
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
	#define EDBUS_INVALID_UNIQUE_NAME 9
	#define EDBUS_ERROR_SENDING_UNIQ  10
	#define EDBUS_REGISTRATION_FAILED 11
	#define EDBUS_RECEIVE_ERROR       12
	#define EDBUS_DECODE_HEADER_ERROR 13
	#define EDBUS_MALLOC_ERROR        14
	#define EDBUS_CREATE_DBUSMSG_ERROR 15
	#define EDBUS_UNKNOWN_EGRESS_STATE 16
	#define EDBUS_DECODE_ERROR         17






#endif /* ERLANG_DBUS_DRIVER_H_ */
