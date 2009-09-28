/**
 * @file ingress.c
 *
 * @date   2009-09-26
 * @author jldupont
 */
#include <stdlib.h>
#include <pthread.h>
#include "dbus/dbus.h"
#include "ingress.h"
#include "erlang_dbus_driver.h"

char *IFilters[INGRESS_MAX_FILTERS+1];
int IFilterCount=0;
DBusBusType    IBusType;
DBusConnection *IConn=NULL;

static DBusHandlerResult ingress_filter_func (DBusConnection *connection,DBusMessage     *message,void            *user_data);
void handle_message(DBusMessage *message, void *user_data);

void ingress_set_bus(DBusBusType BusType) {
	IBusType=BusType;
}//


void ingress_add_filter(char *filter) {

	if (INGRESS_MAX_FILTERS<=IFilterCount) {
		return;
	}

	IFilters[IFilterCount++]=filter;
}

void ingress_init(DBusConnection *connection) {
	DBusError error;
	IConn=connection;

	/*
	 *  Configure filters
	 */
	int i;
	for (i=0;i<IFilterCount;i++) {
        dbus_bus_add_match (connection, IFilters[i], &error);
        if (dbus_error_is_set (&error)) {
            dbus_error_free (&error);
            exit (EDBUS_ADD_MATCH_ERROR);
        }
	}

	if (!dbus_connection_add_filter (connection, ingress_filter_func, NULL, NULL)) {
	  exit (1);
	}
}//

static DBusHandlerResult
ingress_filter_func (DBusConnection *connection,
					DBusMessage     *message,
					void            *user_data)
{
  handle_message(message, user_data);

  if (dbus_message_is_signal (message,
                              DBUS_INTERFACE_LOCAL,
                              "Disconnected"))
    exit (EDBUS_DISCONNECTED);

  /* Conceptually we want this to be
   * DBUS_HANDLER_RESULT_NOT_YET_HANDLED, but this raises
   * some problems.  See bug 1719.
   */
  return DBUS_HANDLER_RESULT_HANDLED;
}

/**
 *  Handle 'ingress' message coming on DBus
 *
 */
void
handle_message(DBusMessage *message, void *user_data) {

  DBusMessageIter iter;
  EDBusMessage edmsg;

  edmsg.type =   dbus_message_get_type (message);
  edmsg.sender = dbus_message_get_sender (message);
  edmsg.dest =   dbus_message_get_destination (message);

  switch(edmsg.type) {
  case DBUS_MESSAGE_TYPE_METHOD_CALL:
  case DBUS_MESSAGE_TYPE_SIGNAL:

	case DBUS_MESSAGE_TYPE_METHOD_RETURN:
	  printf (" reply_serial=%u\n",
        dbus_message_get_reply_serial (message));
	  break;

	case DBUS_MESSAGE_TYPE_ERROR:
	  printf (" error_name=%s reply_serial=%u\n",
		  dbus_message_get_error_name (message),
        dbus_message_get_reply_serial (message));
	  break;

	default:
	  printf ("\n");
	  break;

  }



}//

