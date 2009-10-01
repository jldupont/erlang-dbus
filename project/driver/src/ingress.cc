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

// Local Constants - do not export
// ===============================

enum {
	I_OK = 0,
	I_UNSUPPORTED_TYPE
};


// "Local" variables
// ==================
char *IFilters[INGRESS_MAX_FILTERS+1];
int IFilterCount=0;
DBusBusType    IBusType;
DBusConnection *IConn=NULL;


// Prototypes
// ==========
static DBusHandlerResult ingress_filter_func (DBusConnection *connection,DBusMessage     *message,void            *user_data);
void handle_message(DBusMessage *message, void *user_data);
int ingress_do_iter(TermHandler *th,EDBusMessage *edmsg, DBusMessageIter *iter);
int ingress_init_message(TermHandler *th, EDBusMessage *edmsg);




// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



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


	 //  Configure filters
	for (int i=0;i<IFilterCount;i++) {
        dbus_bus_add_match (connection, IFilters[i], &error);
        if (dbus_error_is_set (&error)) {
            dbus_error_free (&error);
            DBGLOG(LOG_ERR, "ingress_init: add_match error, msg: %s", error.message);
            exit (EDBUS_ADD_MATCH_ERROR);
        }
	}

	// Configure the filter function
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

	// if these malloc don't go through,
	// there are much bigger problems about the host
	// system looming anywas...
  EDBusMessage   *edmsg=(EDBusMessage *)malloc(sizeof(EDBusMessage));
  DBusMessageIter *iter=(DBusMessageIter *)malloc(sizeof(DBusMessageIter));

  // Start the receive cycle

  edmsg->type =   dbus_message_get_type (message);
  edmsg->sender = dbus_message_get_sender (message);
  edmsg->dest =   dbus_message_get_destination (message);

  switch(edmsg->type) {
	  case DBUS_MESSAGE_TYPE_METHOD_CALL:
	  case DBUS_MESSAGE_TYPE_SIGNAL:
		  edmsg->serial = dbus_message_get_serial (message);
		  edmsg->Method_Signal.path = dbus_message_get_path (message);
		  edmsg->Method_Signal.interface = dbus_message_get_interface (message);
		  edmsg->Method_Signal.member = dbus_message_get_member (message);
		  break;

	case DBUS_MESSAGE_TYPE_METHOD_RETURN:
		edmsg->serial = dbus_message_get_reply_serial (message);
        break;

	case DBUS_MESSAGE_TYPE_ERROR:
		edmsg->serial = dbus_message_get_reply_serial (message);
		edmsg->Error.name = dbus_message_get_error_name (message);
		break;

	default:
	  break;

  }

	// Interface to Erlang
	// ===================
	Pkt         *opkt=new Pkt();
	TermHandler  *oth=new TermHandler();

	oth->init(opkt);

	// Init Message
	if (ingress_init_message(th, edmsg)) {

	}

	dbus_message_iter_init (message, iter);
	int result=ingress_do_iter(edmsg, iter);

	switch(result) {
	case I_OK:
		break;

	case I_UNSUPPORTED_TYPE:
		break;
	}

}//

/**
 * 	Format: {dbus, Type, Serial, Sender, Destination, Path, Interface, Member, Message}
 */
int
ingress_init_message(TermHandler *th, EDBusMessage *edmsg) {
	TermStruct ts;

}

int
ingress_do_iter(TermHandler *th,
				EDBusMessage *edmsg,
				DBusMessageIter *iter) {

	int result=0; // assume everything OK


	do {
	  int type = dbus_message_iter_get_arg_type (iter);

	  if (DBUS_TYPE_INVALID == type)
		  break;


	  switch (type) {

	  case DBUS_TYPE_STRING: {
		char *val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_SIGNATURE: {
		char *val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_OBJECT_PATH: {
		char *val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_INT16: {
		dbus_int16_t val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_UINT16: {
		dbus_uint16_t val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_INT32: {
		dbus_int32_t val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_UINT32: {
		dbus_uint32_t val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_INT64: {
		dbus_int64_t val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_UINT64: {
		dbus_uint64_t val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_DOUBLE: {
		double val;
		dbus_message_iter_get_basic (iter, &val);
		printf ("double %g\n", val);
		break;
	  }

	  case DBUS_TYPE_BYTE: {
		unsigned char val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_BOOLEAN: {
		dbus_bool_t val;
		dbus_message_iter_get_basic (iter, &val);
		break;
	  }

	  case DBUS_TYPE_VARIANT: {
		DBusMessageIter subiter;

		dbus_message_iter_recurse (iter, &subiter);

		printf ("variant ");
		print_iter (&subiter, literal, depth+1);
		break;
	  }
	  case DBUS_TYPE_ARRAY: {
		int current_type;
		DBusMessageIter subiter;

		dbus_message_iter_recurse (iter, &subiter);

		printf("array [\n");
		while ((current_type = dbus_message_iter_get_arg_type (&subiter)) != DBUS_TYPE_INVALID) {
		print_iter (&subiter, literal, depth+1);
		dbus_message_iter_next (&subiter);

		if (dbus_message_iter_get_arg_type (&subiter) != DBUS_TYPE_INVALID)
		  printf (",");
		  }
		indent(depth);
		printf("]\n");
		break;
	  }
	  case DBUS_TYPE_DICT_ENTRY: {
		DBusMessageIter subiter;

		dbus_message_iter_recurse (iter, &subiter);

		printf("dict entry(\n");
		print_iter (&subiter, literal, depth+1);
		dbus_message_iter_next (&subiter);
		print_iter (&subiter, literal, depth+1);
		indent(depth);
		printf(")\n");
		break;
	  }

	  case DBUS_TYPE_STRUCT: {
		int current_type;
		DBusMessageIter subiter;

		dbus_message_iter_recurse (iter, &subiter);

		printf("struct {\n");
		while ((current_type = dbus_message_iter_get_arg_type (&subiter)) != DBUS_TYPE_INVALID)
		  {
		print_iter (&subiter, literal, depth+1);
		dbus_message_iter_next (&subiter);
		if (dbus_message_iter_get_arg_type (&subiter) != DBUS_TYPE_INVALID)
		  printf (",");
		  }
		indent(depth);
		printf("}\n");
		break;
	  }

	  default:
		  result=1;
		  break;

	  }//switch

	} while (dbus_message_iter_next (iter));

	if (result) {
		// format an error term()
	} else {

	}

	// send!

}//

