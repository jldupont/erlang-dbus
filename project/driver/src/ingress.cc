/**
 * @file ingress.cc
 *
 * @date   2009-09-26
 * @author jldupont
 *
 * DBus -->  Erlang
 *
 */
#include "erlang_dbus_driver.h"
#include "ingress.h"


char *IFilters[INGRESS_MAX_FILTERS+1];
int IFilterCount=0;
DBusBusType    IBusType;
DBusConnection *IConn=NULL;

//Prototypes
static DBusHandlerResult ingress_filter_func (DBusConnection *connection,DBusMessage     *message,void            *user_data);
void handle_message(DBusMessage *message, void *user_data);
void ingress_do_iter(DBusMessageIter *);



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
            DBGLOG(LOG_ERR, "ingress_init: add_match error, msg: %s", error.message);
            exit (EDBUS_ADD_MATCH_ERROR);
        }
	}

	if (!dbus_connection_add_filter (connection, ingress_filter_func, NULL, NULL)) {
	  exit (EDBUS_ADD_FILTER_ERROR);
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

  return DBUS_HANDLER_RESULT_HANDLED;
}

/**
 *  Handle 'ingress' message coming on DBus
 *
 */
void
handle_message(DBusMessage *message, void *user_data) {

  EDBusMessage edmsg;

  edmsg.type =   dbus_message_get_type (message);
  edmsg.sender = dbus_message_get_sender (message);
  edmsg.dest =   dbus_message_get_destination (message);

  switch(edmsg.type) {
	  case DBUS_MESSAGE_TYPE_METHOD_CALL:
	  case DBUS_MESSAGE_TYPE_SIGNAL:
		  edmsg.serial = dbus_message_get_serial (message);
		  edmsg.Method_Signal.path = dbus_message_get_path (message);
		  edmsg.Method_Signal.interface = dbus_message_get_interface (message);
		  edmsg.Method_Signal.member = dbus_message_get_member (message);
		  break;

	case DBUS_MESSAGE_TYPE_METHOD_RETURN:
		edmsg.serial = dbus_message_get_reply_serial (message);
        break;

	case DBUS_MESSAGE_TYPE_ERROR:
		edmsg.serial = dbus_message_get_reply_serial (message);
		edmsg.Error.name = dbus_message_get_error_name (message);
		break;

	default:
	  break;

  }

  DBusMessageIter iter;
  dbus_message_iter_init (message, &iter);
  ingress_do_iter(&iter);

}//

void
ingress_do_iter(&iter) {

}

