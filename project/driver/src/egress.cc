/**
 * @file egress.cc
 *
 * @date   2009-09-26
 * @author jldupont
 *
 * \section Overview
 * This module processes the Erlang Client to DBus messages.
 *
 * \section Protocol
 *
 * From the DBus documentation,
 *
 *   [MsgType, ...]
 *
 *   MsgType=atom()
 *   \li m:  Method_Call
 *   \li r:  Method_Return
 *   \li e:  Error
 *   \li s:  Signal
 *
 * For the ARRAY & VARIANT types, the 'element-type' must be specified when building the "container".
 * Thus, some look-ahead must be performed.
 *
 * The signature for the types DICT and STRUCT are built by the DBus library when appending elements to the container.
 *
 *
 * \subsection Message Type "Method_Call"
 *
 * 	[m, Serial, Sender, Destination, Path, Interface, Member, [Message]]
 *
 * \subsection Message Type "Signal"
 *
 *  [s, Serial, Sender, Destination, Path, Interface, Member, [Message]]
 *
 * \subsection Message Type "Method_Return"
 *
 *  [r, Serial, Sender, Destination, [Message]]
 *
 * \subsection Message Type "Error"
 *
 *  [e, Serial, Sender, Destination, Name, [Message]]
 *
 */
#include <stdlib.h>
#include <pthread.h>
#include "dbus/dbus.h"
#include "egress.h"
#include "erlang_dbus_driver.h"


// "Local" variables
pthread_t _egressThread;

#define ETYPE_METHOD_CALL      "m"
#define ETYPE_METHOD_RETURN    "r"
#define ETYPE_SIGNAL           "s"
#define ETYPE_ERROR            "e"


#define ETYPE_STRING  "str"
#define ETYPE_SIG     "sig"
#define ETYPE_OP      "op"
#define ETYPE_INT16   "i16"
#define ETYPE_UINT16  "ui16"
#define ETYPE_INT32   "i32"
#define ETYPE_UINT32  "ui32"
#define ETYPE_INT64   "i64"
#define ETYPE_UINT64  "ui64"
#define ETYPE_DOUBLE  "f"
#define ETYPE_BYTE    "by"
#define ETYPE_BOOL    "bo"
#define ETYPE_VARIANT "v"
#define ETYPE_ARRAY   "a"
#define ETYPE_DICT    "d"
#define ETYPE_STRUCT  "st"




typedef struct _MessageHeader {

	int type;

	// MsgType "Method_Call", "Method_Return", "Signal" & "Error"
	int serial;
	const char *sender;
	const char *dest;

	// MsgType "Method_Call" & "Signal"
	const char *path;
	const char *interface;
	const char *member;

	// MsgType "Error"
	const char *name;

} MessageHeader;


enum _TIState {
	 TIS_START
	,TIS_WAIT_PRIM
	,TIS_WAIT_LIST
} TIState;


// To iterate over Terms of an Erlang Client message
typedef struct _TermIterator {
	int state;
} TermIterator;





// Prototypes
// ===========
void *__egress_thread_function(void *conn);
void egress_handle_code(int code);
int  egress_translate_type(const char *type);
int egress_decode_header(TermHandler *th, MessageHeader *mh);
int egress_iter(TermIterator *ti, TermHandler *th, DBusMessage *dm);
DBusMessage *egress_init_dbus_message(MessageHeader *mh);
int egress_translate_etype(const char *tt);

// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



/**
 * Egress thread initialization
 */
void egress_init(DBusConnection *conn) {

	pthread_create(&_egressThread, NULL, &__egress_thread_function, (void *)conn);
}//


void *
__egress_thread_function(void *conn) {

	Pkt         *p  = new Pkt();
	PktHandler  *ph = new PktHandler();
	TermHandler *th = new TermHandler();

	MessageHeader mh;
	DBusMessage   *dm;

	do {

		// Blocking
		int r = ph->rx(&p);
		if (r) {
			DBGLOG(LOG_ERR, "egress thread: error, msg: %s", ph->strerror());
			exit(EDBUS_RECEIVE_ERROR);
		}

		// before *any* iteration can be done
		th->init(p);

		r=egress_decode_header(th, &mh);
		if (r) {
			DBGLOG(LOG_ERR, "egress thread: can't decode header");
			exit(EDBUS_DECODE_HEADER_ERROR);
		}

		// Init message to DBus
		dm=egress_init_dbus_message(&mh);
		if (NULL==dm) {
			DBGLOG(LOG_ERR, "egress thread: can't create DBus message");
			exit(EDBUS_CREATE_DBUSMSG_ERROR);
		}
		TermIterator ti;
		ti.state=TIS_START;

		// start iterating & translating
		r=egress_iter(&ti, th, dm);
		egress_handle_code(r);    //this will exit if required

		//recycle the packet
		p->clean();

		// FINALLY, send on the DBus
		//r=egress_send(mh,)


	} while(TRUE);

	return NULL;
}//

/**
 * Translate the message type from the Erlang Client
 * to the native DBus type.
 */
int
egress_translate_type(const char *type) {

	if (NULL==type) {
		return DBUS_MESSAGE_TYPE_INVALID;
	}

	if (strcmp(type, ETYPE_METHOD_CALL)==0) {
		return DBUS_MESSAGE_TYPE_METHOD_CALL;
	}

	if (strcmp(type, ETYPE_METHOD_RETURN)==0) {
		return DBUS_MESSAGE_TYPE_METHOD_RETURN;
	}

	if (strcmp(type, ETYPE_SIGNAL)==0) {
		return DBUS_MESSAGE_TYPE_SIGNAL;
	}

	if (strcmp(type, ETYPE_ERROR)==0) {
		return DBUS_MESSAGE_TYPE_ERROR;
	}

	return DBUS_MESSAGE_TYPE_INVALID;
}//



void
egress_handle_code(int code) {

	DBGLOG(LOG_ERR, "egress thread: error, msg: %s");
}//


int
egress_decode_header(TermHandler *th, MessageHeader *mh) {

	TermStruct ts;

	// First, we should be getting a "start list"
	th->clean(&ts);
	int r=th->iter(&ts);
	if (r) {
		DBGLOG(LOG_ERR, "egress_decode_header: expecting 'start list'");
		return r;
	}
	if (TERMTYPE_START_LIST != ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_header: missing 'start list'");
		return r;
	}

	// Next, we need the MsgType as an atom()
	th->clean(&ts);
	r=th->iter(&ts);
	if (r) {
		DBGLOG(LOG_ERR, "egress_decode_header: expecting 'msg_type'");
		return r;
	}
	if (TERMTYPE_ATOM != ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_header: missing atom(msg_type)");
		return r;
	}
	mh->type=egress_translate_type((const char *) ts.Value.string);


	// Next, we need the Serial info as an integer()
	th->clean(&ts);
	r=th->iter(&ts);
	if (r) {
		DBGLOG(LOG_ERR, "egress_decode_header: expecting 'serial'");
		return r;
	}
	if (TERMTYPE_LONG != ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_header: missing integer(serial)");
		return r;
	}

	mh->serial = ts.Value.integer;

	// Next, we need the Sender info as an string()
	th->clean(&ts);
	r=th->iter(&ts);
	if (r) {
		DBGLOG(LOG_ERR, "egress_decode_header: expecting 'sender'");
		return r;
	}
	if (TERMTYPE_STRING != ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_header: missing string(sender)");
		return r;
	}
	mh->sender = (const char *) ts.Value.string;

	// we don't want to 'clean' this term struct because we have
	// transferred ownership of the string onto the MessageHeader structure.
	ts.Value.string=NULL;

	// Next, we need the Destination info as an string()
	r=th->iter(&ts);
	if (r) {
		DBGLOG(LOG_ERR, "egress_decode_header: expecting 'destination'");
		return r;
	}
	if (TERMTYPE_STRING != ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_header: missing string(destination)");
		return r;
	}
	mh->dest = (const char *) ts.Value.string;  // this can be NULL e.g. Signals
	ts.Value.string=NULL;

	// ----------------------------------------------------------------------------
	// COMMON PART END
	// ----------------------------------------------------------------------------

	switch(mh->type) {

	// we need Path, Interface & Member elements
	case DBUS_MESSAGE_TYPE_SIGNAL:
	case DBUS_MESSAGE_TYPE_METHOD_CALL:
		r=th->iter(&ts);
		if (r) {
			DBGLOG(LOG_ERR, "egress_decode_header: expecting 'path'");
			return r;
		}
		if (TERMTYPE_STRING != ts.type) {
			DBGLOG(LOG_ERR, "egress_decode_header: missing string(path)");
			return r;
		}
		mh->path = (const char *) ts.Value.string;
		ts.Value.string = NULL;

		r=th->iter(&ts);
		if (r) {
			DBGLOG(LOG_ERR, "egress_decode_header: expecting 'interface'");
			return r;
		}
		if (TERMTYPE_STRING != ts.type) {
			DBGLOG(LOG_ERR, "egress_decode_header: missing string(interface)");
			return r;
		}
		mh->interface = (const char *) ts.Value.string;
		ts.Value.string = NULL;

		r=th->iter(&ts);
		if (r) {
			DBGLOG(LOG_ERR, "egress_decode_header: expecting 'member'");
			return r;
		}
		if (TERMTYPE_STRING != ts.type) {
			DBGLOG(LOG_ERR, "egress_decode_header: missing string(member)");
			return r;
		}
		mh->member = (const char *) ts.Value.string;
		ts.Value.string = NULL;
		break;


	// complete header already
	case DBUS_MESSAGE_TYPE_METHOD_RETURN:
		break;


	// we still need the "Name" element
	case DBUS_MESSAGE_TYPE_ERROR:
		r=th->iter(&ts);
		if (r) {
			DBGLOG(LOG_ERR, "egress_decode_header: expecting 'name'");
			return r;
		}
		if (TERMTYPE_STRING != ts.type) {
			DBGLOG(LOG_ERR, "egress_decode_header: missing string(name)");
			return r;
		}
		mh->name = (const char *) ts.Value.string;
		ts.Value.string = NULL;
		break;


	// fail fast!
	default:
	case DBUS_MESSAGE_TYPE_INVALID:
		DBGLOG(LOG_ERR, "egress thread: invalid message type");
		exit(EDBUS_UNSUPPORTED_TYPE);
		break;

	}//switch


	// all went well!
	return 0;
}//


/**
 * Only valid message types should end-up here
 *
 */
DBusMessage *
egress_init_dbus_message(MessageHeader *mh) {

	DBusMessage *dm=dbus_message_new(mh->type);
	if (NULL==dm)
		return NULL;

	switch(mh->type) {

	// we need Path, Interface & Member elements
	case DBUS_MESSAGE_TYPE_SIGNAL:
	case DBUS_MESSAGE_TYPE_METHOD_CALL:
		dbus_message_set_interface(dm, mh->interface);
		dbus_message_set_path(dm, mh->path);
		dbus_message_set_member(dm, mh->member);
		break;

	// common part only needed
	case DBUS_MESSAGE_TYPE_METHOD_RETURN:
		dbus_message_set_reply_serial(dm, mh->serial);
		break;

	// we still need the "Name" element
	case DBUS_MESSAGE_TYPE_ERROR:
		dbus_message_set_error_name(dm, mh->name);
		break;


	// fail fast!
	default:
	case DBUS_MESSAGE_TYPE_INVALID:
		DBGLOG(LOG_ERR, "egress thread: invalid message type");
		exit(EDBUS_UNSUPPORTED_TYPE);
		break;

	}//switch

	// Common part
	dbus_message_set_sender(dm,       mh->sender);
	dbus_message_set_destination(dm,  mh->dest);


	return dm;
}//

/**
 * @param tt DBUS_TYPE_xyz
 */
void
egress_append_prim(TermHandler *th, DBusMessage *dm, int tt) {

	TermStruct ts;
	switch(tt) {
	case DBUS_TYPE_BYTE:
		byte = strtoul (value, NULL, 0);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_BYTE, &byte);
		break;

	case DBUS_TYPE_DOUBLE:
		d = strtod (value, NULL);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_DOUBLE, &d);
		break;

	case DBUS_TYPE_INT16:
		int16 = strtol (value, NULL, 0);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_INT16, &int16);
		break;

	case DBUS_TYPE_UINT16:
		uint16 = strtoul (value, NULL, 0);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT16, &uint16);
		break;

	case DBUS_TYPE_INT32:
		int32 = strtol (value, NULL, 0);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_INT32, &int32);
		break;

	case DBUS_TYPE_UINT32:
		uint32 = strtoul (value, NULL, 0);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT32, &uint32);
		break;

	case DBUS_TYPE_INT64:
		int64 = strtoll (value, NULL, 0);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_INT64, &int64);
		break;

	case DBUS_TYPE_UINT64:
		uint64 = strtoull (value, NULL, 0);
		dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT64, &uint64);
		break;

	case DBUS_TYPE_STRING:
		dbus_message_iter_append_basic (iter, DBUS_TYPE_STRING, &value);
		break;

	case DBUS_TYPE_OBJECT_PATH:
		dbus_message_iter_append_basic (iter, DBUS_TYPE_OBJECT_PATH, &value);
		break;

	case DBUS_TYPE_BOOLEAN:
	}//switch


}//


/**
 * If we are at the start of the message part
 * of the packet received, we would get at least
 * one other list element.
 *
 */
int
egress_iter(TermIterator *ti, TermHandler *th, DBusMessage *dm) {

	TermStruct ts;
	int tt;

	switch(ti->state) {
		case TIS_START:
			tt=egress_decode_prim(th);
			egress_append_prim(th, dm, tt);
			break;

		case TIS_WAIT_PRIM:
		case TIS_WAIT_LIST:

		default:
			// this shouldn't happen since we are designing the state-machine!
			DBGLOG(LOG_ERR, "egress_iter: unknown state");
			exit(EDBUS_UNKNOWN_EGRESS_STATE);
	}//switch




	int r=th->iter(&ts);
	if (r) return 1;

	switch(ts.type) {

	case TERMTYPE_END:
		break;

	case TERMTYPE_START_LIST:

	case TERMTYPE_END_LIST:
	case TERMTYPE_START_TUPLE:
	case TERMTYPE_ATOM:
	case TERMTYPE_STRING:
	case TERMTYPE_DOUBLE:
	case TERMTYPE_LONG:
	case TERMTYPE_ULONG:
	case TERMTYPE_LONGLONG:
	case TERMTYPE_ULONGLONG:
	case TERMTYPE_BINARY:
	case TERMTYPE_NIL:

	//fail fast
	case TERMTYPE_UNSUPPORTED:
	case TERMTYPE_INVALID:
	default:
		DBGLOG(LOG_ERR, "egress_iter: invalid/unsupported type: %i", ts.type);
		exit(EDBUS_UNSUPPORTED_TYPE);
		break;
	}//switch


	return 0;
}//

/**
 * Expects to decode a tuple() consisting
 * of a DBus primitive as coded by the Erlang Client
 *
 *  {atom(),
 *
 *  @return DBUS_TYPE_xyz
 */
int
egress_decode_prim(TermHandler *th) {

	TermStruct ts;

	int r=th->iter(&ts);
	if (r) return 1;
	if (TERMTYPE_START_TUPLE!=ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_prim: expecting 'start_tuple'");
		return DBUS_TYPE_INVALID;
	}

	r=th->iter(&ts);
	if (r) return 1;
	if (TERMTYPE_ATOM!=ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_prim: expecting 'atom()'");
		return DBUS_TYPE_INVALID;
	}

	r=egress_translate_etype((const char *)ts.Value.string);
	th->clean(&ts);

	return r;
}//


int
egress_translate_etype(const char *tt) {

	if (strcmp(tt, ETYPE_STRING)==0) return DBUS_TYPE_STRING;
	if (strcmp(tt, ETYPE_SIG)==0)    return DBUS_TYPE_SIGNATURE;
	if (strcmp(tt, ETYPE_OP)==0)     return DBUS_TYPE_OBJECT_PATH;
	if (strcmp(tt, ETYPE_INT16)==0)  return DBUS_TYPE_INT16;
	if (strcmp(tt, ETYPE_UINT16)==0) return DBUS_TYPE_UINT16;
	if (strcmp(tt, ETYPE_INT32)==0)  return DBUS_TYPE_INT32;
	if (strcmp(tt, ETYPE_UINT32)==0) return DBUS_TYPE_UINT32;
	if (strcmp(tt, ETYPE_INT64)==0)  return DBUS_TYPE_INT64;
	if (strcmp(tt, ETYPE_UINT64)==0) return DBUS_TYPE_UINT64;
	if (strcmp(tt, ETYPE_DOUBLE)==0) return DBUS_TYPE_DOUBLE;
	if (strcmp(tt, ETYPE_BYTE)==0)   return DBUS_TYPE_BYTE;
	if (strcmp(tt, ETYPE_BOOL)==0)   return DBUS_TYPE_BOOLEAN;
	if (strcmp(tt, ETYPE_VARIANT)==0)return DBUS_TYPE_VARIANT;
	if (strcmp(tt, ETYPE_ARRAY)==0)  return DBUS_TYPE_ARRAY;
	if (strcmp(tt, ETYPE_DICT)==0)   return DBUS_TYPE_DICT_ENTRY;
	if (strcmp(tt, ETYPE_STRUCT)==0) return DBUS_TYPE_STRUCT;

	return DBUS_TYPE_INVALID;
}
