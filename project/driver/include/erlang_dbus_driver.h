/**
 * @file erlang_dbus_driver.h
 *
 * @date   2009-09-26
 * @author Jean-Lou Dupont
 */

#ifndef ERLANG_DBUS_DRIVER_H_
#define ERLANG_DBUS_DRIVER_H_

 #include "macros.h"
 #include <stdlib.h>
 #include <pthread.h>
 #include <dbus/dbus.h>
 #include "epapi.h"

	/**
	 * Process Exit Codes
	 */

	#define EDBUS_OK               0
	#define EDBUS_CONN_ERROR       1
	#define EDBUS_DISCONNECTED     2
	#define EDBUS_ADD_MATCH_ERROR  3
	#define EDBUS_ADD_FILTER_ERROR 4
	#define EDBUS_INIT_MESSAGE     5  // error initializing message back to Erlang



#endif /* ERLANG_DBUS_DRIVER_H_ */
