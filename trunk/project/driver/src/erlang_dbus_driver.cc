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
 *   {msg, Type, Serial, Sender, Destination, Path, Interface, Member, Message}
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

	  DBusConnection *connection;
	  DBusError error;
	  DBusBusType type = DBUS_BUS_SESSION;
	  int noegress=FALSE;
	  int noingress=FALSE;

	  int i = 0;
	  for (i = 1; i < argc; i++) {

		  char *arg = argv[i];

		  if (0==strcmp (arg, "--system"))
			  ingress_set_bus(DBUS_BUS_SYSTEM);
		  else if (0==strcmp (arg, "--session"))
			  ingress_set_bus(DBUS_BUS_SESSION);
		  else if (0==strcmp (arg, "--noegress"))
			  noegress=TRUE;
		  else if (0==strcmp (arg, "--noingress"))
			  noingress=TRUE;
		  else {
			  ingress_add_filter(arg);
		  }
	  }

	  dbus_error_init (&error);

	  connection = dbus_bus_get (type, &error);
	  if (NULL==connection) {
		  exit(EDBUS_CONN_ERROR);
	  }

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
