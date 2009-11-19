/**
 * @file ingress.cc
 *
 * @date   2009-09-26
 * @author jldupont
 *
 * DBus -->  Erlang
 *
 * NOTE: The "fail-fast" paradigm is applied.
 *
 */
#include "erlang_dbus_driver.h"
#include "ingress.h"

// Local Constants - do not export
// ===============================

enum {
	I_OK = 0,
	I_UNSUPPORTED_TYPE,
	I_ENCODE_ERROR
};


// Constants
// =========
static char str_DBUS_MESSAGE_TYPE_METHOD_CALL[]=   "m";
static char str_DBUS_MESSAGE_TYPE_SIGNAL[]=        "s";
static char str_DBUS_MESSAGE_TYPE_METHOD_RETURN[]= "r";
static char str_DBUS_MESSAGE_TYPE_ERROR[]=         "e";



// "Local" variables
// ==================
char *IFilters[INGRESS_MAX_FILTERS+1];
int IFilterCount=0;
DBusConnection *IConn=NULL;
int Iuniq_name_sent=FALSE;


// Prototypes
// ==========
DBusHandlerResult ingress_filter_func (DBusConnection *connection,DBusMessage     *message,void            *user_data);
void ingress_handle_message(DBusMessage *message, void *user_data);
int ingress_do_iter(TermHandler *th, DBusMessageIter *iter);
int ingress_init_message(TermHandler *th, EDBusMessage *edmsg);
char *ingress_translate_type(int);
int ingress_send_unique_name(const char *uniq_name);
int ingress_encode_simple_string(TermHandler *th, char *string);


// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-




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

/**
 * Encodes a STRING as {str, String}
 */
int
ingress_encode_string(TermHandler *th, char *string) {

	//DBGLOG(LOG_INFO, "ingress_encode_string: %s", string);

	TermStruct ts;

	ts.type=TERMTYPE_START_TUPLE;
	ts.size=2;
	th->append(&ts);

	ts.type=TERMTYPE_ATOM;
	ts.Value.string=(void *) "str";
	th->append(&ts);

	ts.type=TERMTYPE_STRING;
	ts.Value.string=(void *) ((NULL==string) ? "":string);
	return th->append(&ts);
}

/**
 * Encodes a STRING as {String}
 */
int
ingress_encode_simple_string(TermHandler *th, char *string) {

	//DBGLOG(LOG_INFO, "ingress_encode_string: %s", string);

	TermStruct ts;

	ts.type=TERMTYPE_START_TUPLE;
	ts.size=1;
	th->append(&ts);

	ts.type=TERMTYPE_STRING;
	ts.Value.string=(void *) ((NULL==string) ? "":string);
	return th->append(&ts);
}

/**
 * Encodes a SIGNATURE primitive as {sig, String}
 */
int
ingress_encode_sig(TermHandler *th, char *string) {

	TermStruct ts;

	ts.type=TERMTYPE_START_TUPLE;
	ts.size=2;
	th->append(&ts);

	ts.type=TERMTYPE_ATOM;
	ts.Value.string=(void *) "sig";
	th->append(&ts);

	ts.type=TERMTYPE_STRING;
	ts.Value.string=(void *) string;
	return th->append(&ts);
}

/**
 * Encodes an OBJECT_PATH primitive as {op, String}
 */
int
ingress_encode_op(TermHandler *th, char *string) {
	TermStruct ts;

	ts.type=TERMTYPE_START_TUPLE;
	ts.size=2;
	th->append(&ts);

	ts.type=TERMTYPE_ATOM;
	ts.Value.string=(void *) "op";
	th->append(&ts);

	ts.type=TERMTYPE_STRING;
	ts.Value.string=(void *) string;
	return th->append(&ts);
}

/**
 * Encodes the _start_ of a tuple as  {Atom,
 * The rest of the tuple must be completed.
 */
int
ingress_encode_tuple_start(TermHandler *th, const char *type_atom) {
	TermStruct ts;

	ts.type=TERMTYPE_START_TUPLE;
	ts.size=2;
	th->append(&ts);

	ts.type=TERMTYPE_ATOM;
	ts.Value.string=(void *) type_atom;
	return th->append(&ts);
}

int
ingress_encode_start_list(TermHandler *th) {
	TermStruct ts;

	ts.type=TERMTYPE_START_LIST;
	ts.size=1;
	return th->append(&ts);
}

int
ingress_init_message(TermHandler *th, EDBusMessage *edmsg) {

	TermStruct ts;

	// [
	ingress_encode_start_list(th);

	// [Type
	char *type=ingress_translate_type(edmsg->type);
	ts.type=TERMTYPE_ATOM;
	ts.Value.string=type;
	th->append(&ts);

	// [Type, Serial
	ingress_encode_start_list(th);
	ts.type=TERMTYPE_LONG;
	ts.Value.uinteger=(unsigned long) edmsg->serial;
	th->append(&ts);

	// [Type, Serial, {Sender}
	ingress_encode_start_list(th);
	ingress_encode_simple_string(th, (char *) edmsg->sender);

	// [Type, Serial, {Sender}, {Destination}
	ingress_encode_start_list(th);
	ingress_encode_simple_string(th, (char *) edmsg->dest);

	switch(edmsg->type) {
	case DBUS_MESSAGE_TYPE_METHOD_CALL:   //1
	case DBUS_MESSAGE_TYPE_SIGNAL:        //4
		// [Type, Serial, {Sender}, {Destination}, {Path}, {Interface}, {Member}
		ingress_encode_start_list(th);
		ingress_encode_simple_string(th, (char *) edmsg->Type.Method_Signal.path);
		ingress_encode_start_list(th);
		ingress_encode_simple_string(th, (char *) edmsg->Type.Method_Signal.interface);
		ingress_encode_start_list(th);
		ingress_encode_simple_string(th, (char *) edmsg->Type.Method_Signal.member);
		break;

	case DBUS_MESSAGE_TYPE_METHOD_RETURN: //2
		break;

	case DBUS_MESSAGE_TYPE_ERROR:         //3
		// ... {Name}...
		ingress_encode_start_list(th);
		ingress_encode_simple_string(th, (char *) edmsg->Type.Error.name);
		break;

	}
	return 0;
}//

/**
 * Translates the 'integer' based identifier for
 * DBus message type to an atom() based one.
 */
char *ingress_translate_type(int type) {
	char *result=NULL;

	switch(type) {
	case DBUS_MESSAGE_TYPE_METHOD_CALL:
		result=str_DBUS_MESSAGE_TYPE_METHOD_CALL;
	  break;
	case DBUS_MESSAGE_TYPE_SIGNAL:
		result=str_DBUS_MESSAGE_TYPE_SIGNAL;
	  break;
	case DBUS_MESSAGE_TYPE_METHOD_RETURN:
		result=str_DBUS_MESSAGE_TYPE_METHOD_RETURN;
	  break;
	case DBUS_MESSAGE_TYPE_ERROR:
		result=str_DBUS_MESSAGE_TYPE_ERROR;
	  break;
	default:
		DBGLOG(LOG_ERR, "ingress_translate_type: unsupported type: %i", type);
	  break;
	}
	  return result;
}

/**
 *  Recursive Message Iteration
 */
int
ingress_do_iter(TermHandler *th,
				DBusMessageIter *iter) {

	DBGBEGIN
		const char DMSG[]= "ingress_do_iter: error: %s";
	DBGEND

	int code=I_OK; // assume everything OK
	int result;
	TermStruct ts;

	do {
	  int type = dbus_message_iter_get_arg_type (iter);

	  code=I_OK;
	  result=0;

	  //hopefully this means we are finished
	  if (DBUS_TYPE_INVALID == type)
		  break;

	  //if something happens, it will most
	  //likely be an encoding error.
	  code=I_ENCODE_ERROR;

	  // start another element in the list
	  ingress_encode_start_list(th);

	  switch (type) {

	  case DBUS_TYPE_STRING: {
		char *val;
		dbus_message_iter_get_basic (iter, &val);
		//DBGLOG(LOG_INFO, "encoding string: %s", val);

		result=ingress_encode_string(th, val);
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding string");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_SIGNATURE: {
		char *val;
		dbus_message_iter_get_basic (iter, &val);
		//DBGLOG(LOG_INFO, "encoding sig: %s", val);

		result=ingress_encode_sig(th, val);
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding signature");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_OBJECT_PATH: {
		char *val;
		dbus_message_iter_get_basic (iter, &val);
		//DBGLOG(LOG_INFO, "encoding object path: %s", val);

		result=ingress_encode_op(th, val);
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding Object Path");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_INT16: {
		dbus_int16_t val;
		dbus_message_iter_get_basic (iter, &val);
		if (!ingress_encode_tuple_start(th, "i16")) {
			ts.type=TERMTYPE_LONG;
			ts.Value.integer= val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding INT16");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_UINT16: {
		dbus_uint16_t val;
		dbus_message_iter_get_basic (iter, &val);
		if (!ingress_encode_tuple_start(th, "ui16")) {
			ts.type=TERMTYPE_ULONG;
			ts.Value.uinteger= val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding UINT16");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_INT32: {
		dbus_int32_t val;
		dbus_message_iter_get_basic (iter, &val);
		if (!ingress_encode_tuple_start(th, "i32")) {
			ts.type=TERMTYPE_LONG;
			ts.Value.integer= val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding INT32");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_UINT32: {
		dbus_uint32_t val;
		dbus_message_iter_get_basic (iter, &val);
		if (!ingress_encode_tuple_start(th, "ui32")) {
			ts.type=TERMTYPE_ULONG;
			ts.Value.uinteger= val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding UINT32");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_INT64: {
		dbus_int64_t val;
		dbus_message_iter_get_basic (iter, &val);
		if (!ingress_encode_tuple_start(th, "i64")) {
			ts.type=TERMTYPE_LONGLONG;
			ts.Value.linteger= val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding INT64");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_UINT64: {
		dbus_uint64_t val;
		dbus_message_iter_get_basic (iter, &val);
		if (!ingress_encode_tuple_start(th, "ui64")) {
			ts.type=TERMTYPE_ULONGLONG;
			ts.Value.luinteger= val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding UINT64");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_DOUBLE: {
		double val;
		dbus_message_iter_get_basic (iter, &val);
		if (!ingress_encode_tuple_start(th, "f")) {
			ts.type=TERMTYPE_DOUBLE;
			ts.Value.afloat= val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding DOUBLE");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_BYTE: {
		unsigned char val;
		dbus_message_iter_get_basic (iter, &val);
		if (!ingress_encode_tuple_start(th, "by")) {
			ts.type=TERMTYPE_ULONG;
			ts.Value.integer= (unsigned long)val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding BYTE");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_BOOLEAN: {
		dbus_bool_t val;
		dbus_message_iter_get_basic (iter, &val);
		//DBGLOG(LOG_INFO, "encoding bool: %i", val);

		if (!ingress_encode_tuple_start(th, "bo")) {
			ts.type=TERMTYPE_ULONG;
			ts.Value.integer= (unsigned long)val;
			result=th->append(&ts);
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding BOOLEAN");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_VARIANT: {
		//DBGLOG(LOG_INFO, "encoding VARIANT");

		ts.type=TERMTYPE_START_TUPLE;
		ts.size=2;
		result=th->append(&ts);
		if (!result) {
			ts.type=TERMTYPE_ATOM;
			ts.Value.string= (void *) "v";
			result=th->append(&ts);
			if (!result) {
				DBusMessageIter subiter;
				dbus_message_iter_recurse (iter, &subiter);
				result=ingress_do_iter(th, &subiter);
			}
		}
		ts.type=TERMTYPE_END_LIST;
		result=th->append(&ts);

		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding VARIANT");
		DBGEND
		break;
	  }
	  case DBUS_TYPE_ARRAY: {
		//DBGLOG(LOG_INFO, "encoding ARRAY");
		int current_type;
		DBusMessageIter subiter;

		ts.type=TERMTYPE_START_TUPLE;
		ts.size=2;
		result=th->append(&ts);
		if (!result) {
			ts.type=TERMTYPE_ATOM;
			ts.Value.string= (void *) "a";
			result=th->append(&ts);
			if (!result) {
					dbus_message_iter_recurse (iter, &subiter);

					while ((current_type = dbus_message_iter_get_arg_type (&subiter)) != DBUS_TYPE_INVALID) {

						result=ingress_do_iter(th, &subiter);
						if (result)
							break;

						dbus_message_iter_next (&subiter);

						if (dbus_message_iter_get_arg_type (&subiter) != DBUS_TYPE_INVALID) {
							if (ingress_encode_start_list(th)) return 1;
						}

					}//while
					if (!result) {
						ts.type=TERMTYPE_END_LIST;
						result=th->append(&ts);
					}
			}
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding ARRAY");
		DBGEND
		break;
	  }
	  case DBUS_TYPE_DICT_ENTRY: {
		//DBGLOG(LOG_INFO, "encoding DICT");
		DBusMessageIter subiter;

		dbus_message_iter_recurse (iter, &subiter);
		ts.type=TERMTYPE_START_TUPLE;
		ts.size=3;
		result=th->append(&ts);
		if (!result) {

			ts.type=TERMTYPE_ATOM;
			ts.Value.string= (void *) "d";
			result=th->append(&ts);
			if (!result) {

				result=ingress_do_iter(th, &subiter);
				if (!result) {

					ts.type=TERMTYPE_END_LIST;
					th->append(&ts);

					dbus_message_iter_next (&subiter);
					result=ingress_do_iter(th, &subiter);

					ts.type=TERMTYPE_END_LIST;
					th->append(&ts);
				}
			}
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding DICT");
		DBGEND
		break;
	  }

	  case DBUS_TYPE_STRUCT: {
		//DBGLOG(LOG_INFO, "encoding STRUCT");

		int current_type;
		DBusMessageIter subiter;

		dbus_message_iter_recurse (iter, &subiter);

		ts.type=TERMTYPE_START_TUPLE;
		ts.size=2;
		result=th->append(&ts);
		if (!result) {

			ts.type=TERMTYPE_ATOM;
			ts.Value.string= (void *) "st";
			result=th->append(&ts);
			if (!result) {

				while ((current_type = dbus_message_iter_get_arg_type (&subiter)) != DBUS_TYPE_INVALID) {

					result=ingress_do_iter(th, &subiter);
					if (!result)
						break;

					dbus_message_iter_next (&subiter);
					if (dbus_message_iter_get_arg_type (&subiter) != DBUS_TYPE_INVALID) {
						if (ingress_encode_start_list(th)) return 1;
					}

				}//while
				ts.type=TERMTYPE_END_LIST;
				result=th->append(&ts);
			}
		}
		DBGBEGIN
		if (result) DBGLOG(LOG_ERR, DMSG, "encoding STRUCT");
		DBGEND
		break;
	  }

	  default:
		  DBGLOG(LOG_ERR, "ingress_do_iter: unknown type: %i", type);
		  code=I_UNSUPPORTED_TYPE;
		  result=1;
		  break;

	  }//switch

	} while (dbus_message_iter_next (iter) && (!result));

	if (result)
		return code;

	return 0;
}//


/**
 * Filter Function registered with DBus
 */
DBusHandlerResult
ingress_filter_func (DBusConnection *connection,
					DBusMessage     *message,
					void            *user_data)
{
	//DBGLOG(LOG_INFO, "ingress_filter_func, conn: %i  message: %i", connection, message);


	// we need to inform the Erlang Client of our 'unique-name'
	// associated with the connection.
	if (Iuniq_name_sent == FALSE) {
	  const char *uniq_name = dbus_bus_get_unique_name(connection);
	  DBGLOG(LOG_INFO, "unique-name: %s", uniq_name);

	  //paranoia
	  if (NULL==uniq_name) {
			exit(EDBUS_INVALID_UNIQUE_NAME);
	  }
	  if (ingress_send_unique_name(uniq_name)) {
		  exit(EDBUS_ERROR_SENDING_UNIQ);
	  }
	  Iuniq_name_sent=TRUE;
	}

	// main message handling starts here
	ingress_handle_message(message, user_data);

	if (dbus_message_is_signal (message,
							  DBUS_INTERFACE_LOCAL,
							  "Disconnected"))
	exit (EDBUS_DISCONNECTED);

	// We are passing the message along to the Erlang Client...
	return DBUS_HANDLER_RESULT_HANDLED;
}


char *
ingress_copy_string(const char *istr) {

	if (NULL==istr) return NULL;

	size_t len=strlen(istr);

	char *copy=(char *)malloc( len+1 );

	return strcpy( copy, (char *)istr );
}//

void
ingress_clean_edbus_message(EDBusMessage *edmsg) {

	//be forgetful ;-)
	if (NULL==edmsg) return;

	if (NULL != edmsg->sender) { free((void *)edmsg->sender); edmsg->sender=NULL; }
	if (NULL != edmsg->dest)   { free((void *)edmsg->dest);   edmsg->dest=NULL; }
	if (NULL != edmsg->Type.Error.name) { free((void *)edmsg->Type.Error.name); edmsg->Type.Error.name=NULL; }
	if (NULL != edmsg->Type.Method_Signal.path) { free((void *)edmsg->Type.Method_Signal.path); edmsg->Type.Method_Signal.path=NULL; }
	if (NULL != edmsg->Type.Method_Signal.interface) { free((void *)edmsg->Type.Method_Signal.interface); edmsg->Type.Method_Signal.interface=NULL; }
	if (NULL != edmsg->Type.Method_Signal.member) { free((void *)edmsg->Type.Method_Signal.member); edmsg->Type.Method_Signal.member=NULL; }
}//

void
ingress_init_edbus_message(EDBusMessage *edmsg) {

	edmsg->sender=NULL;
	edmsg->dest=NULL;
	edmsg->Type.Error.name=NULL;
	edmsg->Type.Method_Signal.interface=NULL;
	edmsg->Type.Method_Signal.path=NULL;
	edmsg->Type.Method_Signal.member=NULL;

}//

/**
 *  Handle 'ingress' message coming on DBus
 *
 */
void
ingress_handle_message(DBusMessage *message, void *user_data) {

	// if these malloc don't go through,
	// there are much bigger problems about the host
	// system looming anywas...
	EDBusMessage   *edmsg=(EDBusMessage *)malloc(sizeof(EDBusMessage));
	DBusMessageIter *iter=(DBusMessageIter *)malloc(sizeof(DBusMessageIter));

	ingress_init_edbus_message(edmsg);

	const char *tmp;

	// Start the receive cycle
	edmsg->type =   dbus_message_get_type (message);

	tmp = dbus_message_get_sender (message);
	edmsg->sender = ingress_copy_string( tmp );

	tmp = dbus_message_get_destination (message);
	edmsg->dest = ingress_copy_string( tmp );

	//DBGLOG(LOG_INFO, "ingress_handle_message, message type: %i", edmsg->type);

	switch(edmsg->type) {
	  case DBUS_MESSAGE_TYPE_METHOD_CALL:
	  case DBUS_MESSAGE_TYPE_SIGNAL:
		  edmsg->serial = dbus_message_get_serial (message);
		  tmp=dbus_message_get_path (message);
		  edmsg->Type.Method_Signal.path = ingress_copy_string(tmp);

		  tmp=dbus_message_get_interface (message);
		  edmsg->Type.Method_Signal.interface = ingress_copy_string(tmp);

		  tmp=dbus_message_get_member (message);
		  edmsg->Type.Method_Signal.member = ingress_copy_string(tmp);
		  break;

	case DBUS_MESSAGE_TYPE_METHOD_RETURN:
		edmsg->serial = dbus_message_get_reply_serial (message);
		break;

	case DBUS_MESSAGE_TYPE_ERROR:
		edmsg->serial = dbus_message_get_reply_serial (message);

		tmp=dbus_message_get_error_name (message);
		edmsg->Type.Error.name = ingress_copy_string(tmp);
		break;

	default:
		DBGLOG(LOG_ERR, "ingress_handle_message: unknown type: %i", edmsg->type);
		break;

	}

	// Interface to Erlang
	//  using EPAPI library
	// ===================
	Pkt         *opkt=new Pkt();
	TermHandler  *oth=new TermHandler();

	oth->init(opkt);

	// Init Message
	if (ingress_init_message(oth, edmsg)) {

		//not much left to do if we can't even communicate back to the Erlang Client...
		DBGLOG(LOG_ERR, "error initializing message");
		exit(EDBUS_INIT_MESSAGE);
	}

	// Iterate over the DBus message
	dbus_message_iter_init (message, iter);
	int result=ingress_do_iter(oth, iter);

	// We need to properly close the Erlang term()
	TermStruct ts;
	ts.type=TERMTYPE_END_LIST;
	oth->append(&ts);

	//regardless of what happens, we do not need
	//these objects anymore... we can't recycle object
	//instances because I do not know if the filter-func needs
	//to be re-entrant... I don't really need to bother with this,
	//at least not now.
	delete oth;
	ingress_clean_edbus_message( edmsg );
	free( edmsg );
	free( iter );

	PktHandler *ph;

	switch(result) {
	case I_OK:
		ph=new PktHandler();
		result=ph->tx(opkt);
		delete opkt;
		delete ph;
		if (result)
			exit(EDBUS_SEND_ERROR);
		break;

	case I_UNSUPPORTED_TYPE:
		delete opkt;
		exit(EDBUS_UNSUPPORTED_TYPE);

	default:
		// @TODO maybe try and send back an error term() ?
		delete opkt;
		exit(EDBUS_UNRECOVERABLE_ERROR);
	}

}//


int
ingress_send_unique_name(const char *uniq_name) {

	Pkt         *p =new Pkt();
	PktHandler  *ph=new PktHandler();
	TermHandler *th=new TermHandler();

	th->init(p);

	TermStruct ts;
	ts.type=TERMTYPE_START_TUPLE;
	ts.size=2;
	th->append(&ts);

	ts.type=TERMTYPE_ATOM;
	ts.Value.string=(void *) "unique_name";
	th->append(&ts);

	ts.type=TERMTYPE_STRING;
	ts.Value.string=(void *) uniq_name;
	th->append(&ts);

	int result=ph->tx(p);

	delete p;
	delete ph;
	delete th;

	return result;
}//
