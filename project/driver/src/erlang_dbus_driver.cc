/**
 * @file erlang_dbus_driver.cc
 *
 * @date 2009-09-26
 * @author Jean-Lou Dupont
 *
 * \section Overview
 *
 *   \subsection Command-Line
 *
 *    erlang_dbus_driver.so [--system] [--session] [--noingress] [--noegress] Filter(s)
 *
 *   \subsection Architecture
 *
 *    The driver is split in two parts:
 *    1) an ingress direction (DBus   --> Erlang)
 *    2) an egress direction  (Erlang --> DBus)
 *
 *    The 'egress' direction can optionally be disabled: this saves a running thread.
 *
 *
 *  \section Adaptation to Erlang (type mapping)
 *
 *    DBus                Erlang
 *    ====                ======
 *    BYTE                {by,   int()}
 *    BOOLEAN             {bo,   true|false}
 *    INT16               {i16,  int()}
 *    UINT16              {ui16, int()}
 *    INT32               {i32,  int()}
 *    UINT32              {ui32, int()}
 *    INT64               {i64,  int()}
 *    UINT64              {ui64, int()}
 *    DOUBLE              {f,    float()}
 *    STRING              {str,  [int()]}
 *    OBJECT_PATH         {op,   [int()]}   Same as STRING
 *
 *    ARRAY               {a, [term()]}
 *    VARIANT             {v, term()}
 *    STRUCT              {st, [term()]}
 *    DICT                {d, term(), term()}
 *
 *
 */
#include <dbus/dbus.h>
#include <stdio.h>
#include <string.h>
#include "erlang_dbus_driver.h"
#include "ingress.h"
#include "egress.h"


// Prototypes
// ==========
int send_unique_name(const char *uniq_name);



/**
 * Library entry point
 *
 * Arguments:
 *
 *  There must be at least one filter:
 *   --filters Filter1 [Filter2, ..., FilterN]
 *
 *  By default, the 'system' bus is used but can be overidden to the 'session' bus:
 *   --session
 *
 *  The 'egress' (from Erlang to DBus) processing direction can be disabled:
 *   --noegress
 *
 */
int main(int argc, char **argv) {

	DBGLOG(LOG_INFO, "start");

	DBusConnection *connection;
	DBusError error;
	DBusBusType type = DBUS_BUS_SESSION;
	int noegress=FALSE;
	int noingress=FALSE;

	int i = 0;
	for (i = 1; i < argc; i++) {

	  char *arg = argv[i];

	  if (0==strcmp (arg, "--system"))
		  type = DBUS_BUS_SYSTEM;
	  else if (0==strcmp (arg, "--session"))
		  type = DBUS_BUS_SESSION;
	  else if (0==strcmp (arg, "--noegress"))
		  noegress=TRUE;
	  else if (0==strcmp (arg, "--noingress"))
		  noingress=TRUE;
	  else {
		  // add filter
		  DBGLOG(LOG_INFO, "adding filter: %s", arg);
		  ingress_add_filter(arg);
	  }
	}

	  dbus_error_init (&error);

	  connection = dbus_bus_get (type, &error);
	  if (NULL==connection) {
		  dbus_error_free (&error);
		  exit(EDBUS_CONN_ERROR);
	  }

	  DBGLOG(LOG_INFO, "main, conn: %i", connection);

	  dbus_error_init (&error);

	/*
	 * Start 'egress' thread (if required)
	 */
	 if (FALSE==noegress) {
		  egress_init(connection);
	 }


	/*
	 * Start 'ingress' thread
	 */
	if (FALSE==noingress) {
	  ingress_init(connection);
	}


	  /*
	   *   MAIN LOOP  (ingress)
	   *
	   *   There is certainly a better way of handling the main loop
	   *   than what I have done here (just setting a 100ms timeout) but
	   *   since I can't find suitable documentation at the moment,
	   *   I am leaving this as it is until...
	   */
	  while (dbus_connection_read_write_dispatch(connection, 100));

	  exit (EDBUS_OK);
}//


