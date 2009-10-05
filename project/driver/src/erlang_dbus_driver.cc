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
 *    BYTE                {byte, int()}
 *    BOOLEAN             {bool, true|false}
 *    INT16               {int16,  int()}
 *    UINT16              {uint16, int()}
 *    INT32               {int32,  int()}
 *    UINT32              {uint32, int()}
 *    INT64               {int64,  int()}
 *    UINT64              {uint64, int()}
 *    DOUBLE              {double, float()}
 *    STRING              {string, list(int())}
 *    OBJECT_PATH         {object_path, list(int())}   Same as STRING
 *
 *    ARRAY               list()                       Only elements of the same type
 *    VARIANT
 *    STRUCT              list()                       Does not appear to be in use
 *    DICT                [{Key, Value}]
 *
 *   \subsection Term to Erlang Client
 *
 *   {dbus, Type, Serial, Sender, Destination, Path, Interface, Member, Message}
 *
 *	 Type=atom()
 *	 Serial=int()
 *   Sender=Destination=Path=Interface=Member=string()
 *   Message=term()
 *
 *
 */
#include <dbus/dbus.h>
#include <stdio.h>
#include <string.h>
#include "erlang_dbus_driver.h"
#include "ingress.h"
#include "egress.h"
#include "queue.h"


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

	  //int rr=dbus_bus_request_name(connection, "com.jldupont.dbus", 0, &error);
	  //if (-1==rr) {
	//	  DBGLOG(LOG_INFO, "request_name error: %s", error.message);
	//	  exit(EDBUS_REGISTRATION_FAILED);
	 // }

	  // The following breaks big time!

	  //const char *uniq_name = dbus_bus_get_unique_name(connection);
	  //DBGLOG(LOG_INFO, "unique-name: %s", uniq_name);
	  //paranoia
	  //if (NULL==uniq_name) {
	//		exit(EDBUS_INVALID_UNIQUE_NAME);
	  //}




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
	   */
	  while (dbus_connection_read_write_dispatch(connection, -1));

	  exit (EDBUS_OK);
}//


