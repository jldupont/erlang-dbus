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
 * 	[m, Serial, {Sender}, {Destination}, {Path}, {Interface}, {Member}, [Message]]
 *
 * \subsection Message Type "Signal"
 *
 *  [s, Serial, {Sender}, {Destination}, {Path}, {Interface}, {Member}, [Message]]
 *
 * \subsection Message Type "Method_Return"
 *
 *  [r, Serial, {Sender}, {Destination}, [Message]]
 *
 * \subsection Message Type "Error"
 *
 *  [e, Serial, {Sender}, {Destination}, {Name}, [Message]]
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
	 TIS_START_PRIM
	,TIS_START_VARIANT
	,TIS_START_STRUCT
	,TIS_START_ARRAY
	,TIS_START_DICT
	,TIS_FILL_VALUE
	,TIS_STOP
} TIState;


// To iterate over Terms of an Erlang Client message
typedef struct _TermIterator {
	int state;
	int tt; // current term-type
	int ct; // container element-type
} TermIterator;


typedef struct _DecodePrim {
	int end_list;
	int end_packet;
	int dt; //DBus type
} DecodePrim;


// Prototypes
// ===========
void *__egress_thread_function(void *conn);
void egress_handle_code(int code);
int  egress_translate_type(const char *type);
void egress_decode_header(TermHandler *th, MessageHeader *mh);
int egress_iter(TermIterator *ti, TermHandler *th, DBusMessageIter *iter);
DBusMessage *egress_init_dbus_message(MessageHeader *mh);
int egress_translate_etype(const char *tt);
void egress_append_prim(TermHandler *th, DBusMessageIter *iter, int tt);
int egress_decode_prim(TermHandler *th, DecodePrim *dp);
void egress_message_header_init(MessageHeader *mh);
void egress_message_header_clean(MessageHeader *mh);
void egress_decode_tuple_string(TermHandler *th, const char **result);

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
	TermIterator ti;
	DBusMessageIter iter;

	egress_message_header_init(&mh);

	DBGLOG(LOG_INFO, "egress thread: starting");

	do {
		//DBGLOG(LOG_INFO, "egress thread: starting loop");

		// Blocking
		int r = ph->rx(&p);
		if (r) {
			DBGLOG(LOG_ERR, "egress thread: error, msg: %s", ph->strerror());
			exit(EDBUS_RECEIVE_ERROR);
		}

		DBGLOG(LOG_INFO, "egress thread: received packet");

		// before *any* iteration can be done
		th->init(p);

		// fail fast if something wrong
		egress_decode_header(th, &mh);

		// Init message to DBus
		dm=egress_init_dbus_message(&mh);
		if (NULL==dm) {
			DBGLOG(LOG_ERR, "egress thread: can't create DBus message");
			exit(EDBUS_CREATE_DBUSMSG_ERROR);
		}

		egress_message_header_clean(&mh);

		// start iterating & translating
		dbus_message_iter_init_append (dm, &iter);

		ti.state=TIS_START_PRIM;
		egress_iter(&ti, th, &iter);

		//recycle the packet
		p->clean();

		// FINALLY, send on the DBus
		DBGLOG(LOG_INFO, "egress thread: about to send on DBus");

		if (!dbus_connection_send ((DBusConnection *)conn, dm, NULL)) {
			DBGLOG(LOG_ERR, "egress: error sending message");
			exit(EDBUS_SEND_ERROR);
		}
		DBGLOG(LOG_INFO, "egress thread: after send on DBus");

		//dbus_message_unref (dm);

		//DBGLOG(LOG_INFO, "egress thread: loop end");

	} while(TRUE);

	DBGLOG(LOG_INFO, "egress thread: ending");

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
egress_message_header_init(MessageHeader *mh) {

	mh->dest=NULL;
	mh->sender=NULL;
	mh->path=NULL;
	mh->interface=NULL;
	mh->member=NULL;
	mh->name=NULL;
}

void egress_message_header_clean(MessageHeader *mh) {

	if (NULL!=mh->dest)      { free((void*) mh->dest);      mh->dest=NULL; };
	if (NULL!=mh->sender)    { free((void*) mh->sender);    mh->sender=NULL; };
	if (NULL!=mh->path)      { free((void*) mh->path);      mh->path=NULL; };
	if (NULL!=mh->interface) { free((void*) mh->interface); mh->interface=NULL; };
	if (NULL!=mh->member)    { free((void*) mh->member);    mh->member=NULL; };
	if (NULL!=mh->name)      { free((void*) mh->name);      mh->name=NULL; };
}

void
egress_decode_header(TermHandler *th, MessageHeader *mh) {

	TermStruct ts;

	// First, we should be getting a "start list"
	th->clean(&ts);
	int r=th->iter(&ts);
	if (r || (!r && (TERMTYPE_START_LIST != ts.type))) {
		DBGLOG(LOG_ERR, "egress_decode_header: expecting 'start list'");
		exit(EDBUS_DECODE_ERROR);
	}

	// Next, we need the MsgType as an atom()
	r=th->iter(&ts);
	if (r || (!r && (TERMTYPE_ATOM != ts.type))) {
		DBGLOG(LOG_ERR, "egress_decode_header: expecting 'msg_type'");
		exit(EDBUS_DECODE_ERROR);
	}
	mh->type=egress_translate_type((const char *) ts.Value.string);

	DBGLOG(LOG_INFO, "egress decode header: type: %s  <%i>", ts.Value.string, mh->type);

	// Next, we need the Serial info as an integer()
	th->clean(&ts);
	r=th->iter(&ts);
	if (r || (!r && (TERMTYPE_LONG!=ts.type))) {
		DBGLOG(LOG_ERR, "egress_decode_header: expecting integer 'serial'");
		exit(EDBUS_DECODE_ERROR);
	}

	mh->serial = ts.Value.integer;

	DBGLOG(LOG_INFO, "egress decode header: Serial: %i", ts.Value.integer);

	//{Sender}
	egress_decode_tuple_string(th, &(mh->sender));
	DBGLOG(LOG_INFO, "egress decode header: Sender: %s", mh->sender);

	//{Destination}
	egress_decode_tuple_string(th, &(mh->dest));

	// ----------------------------------------------------------------------------
	// COMMON PART END
	// ----------------------------------------------------------------------------

	switch(mh->type) {

	// we need Path, Interface & Member elements
	case DBUS_MESSAGE_TYPE_SIGNAL:
	case DBUS_MESSAGE_TYPE_METHOD_CALL:

		//{Path}
		egress_decode_tuple_string(th, &(mh->path));

		// {Interface}
		egress_decode_tuple_string(th, &(mh->interface));

		// {Member}
		egress_decode_tuple_string(th, &(mh->member));
		break;

	// complete header already
	case DBUS_MESSAGE_TYPE_METHOD_RETURN:
		break;

	// we still need the "Name" element
	case DBUS_MESSAGE_TYPE_ERROR:

		// {Name}
		egress_decode_tuple_string(th, &(mh->name));
		break;

	// fail fast!
	default:
	case DBUS_MESSAGE_TYPE_INVALID:
		DBGLOG(LOG_ERR, "egress thread: invalid message type");
		exit(EDBUS_UNSUPPORTED_TYPE);
		break;

	}//switch
}//

/**
 * Expects to decode a string() inside a tuple()
 *
 * Fail fast
 */
void
egress_decode_tuple_string(TermHandler *th, const char **result) {

	TermStruct ts;

	// {
	int r=th->iter(&ts);
	if (r || (!r && (TERMTYPE_START_TUPLE!=ts.type))) {
		DBGLOG(LOG_ERR, "egress_decode_tuple_string: expecting start tuple");
		exit(EDBUS_DECODE_ERROR);
	}

	// {string()}
	th->clean(&ts);
	r=th->iter(&ts);
	if (r || (!r && (TERMTYPE_STRING!=ts.type))) {
		DBGLOG(LOG_ERR, "egress_decode_tuple_string: expecting string");
		exit(EDBUS_DECODE_ERROR);
	}
	*result = (const char *) ts.Value.string;

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
		DBGLOG(LOG_INFO, "egress_init_dbus_message: interface<%s> path<%s> member<%s>", mh->interface, mh->path, mh->member);

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
egress_append_prim(TermHandler *th, DBusMessageIter *iter, int tt) {

	TermStruct ts;
	int r=th->iter(&ts);
	if (r) {
		DBGLOG(LOG_ERR, "egress_append_prim: expecting a term()");
		exit(EDBUS_DECODE_ERROR);
	}//=============================

	switch(tt) {
	case DBUS_TYPE_BYTE: {
		if (TERMTYPE_LONG==ts.type) {
			unsigned char b = (unsigned char) ts.Value.integer;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_BYTE, &b);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'BYTE'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_DOUBLE: {
		if (TERMTYPE_DOUBLE==ts.type) {
			double d = ts.Value.afloat;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_DOUBLE, &d);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'DOUBLE'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_INT16: {
		if (TERMTYPE_LONG==ts.type) {
			dbus_int16_t int16 = (dbus_int16_t) ts.Value.integer;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_INT16, &int16);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'INT16'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_UINT16: {
		if (TERMTYPE_ULONG==ts.type) {
			dbus_uint16_t uint16 = (dbus_uint16_t) ts.Value.uinteger;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT16, &uint16);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'UINT16'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_INT32: {
		if (TERMTYPE_LONG==ts.type) {
			dbus_int32_t int32 = (dbus_int32_t) ts.Value.integer;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_INT32, &int32);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'INT32'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_UINT32: {
		if (TERMTYPE_ULONG==ts.type) {
			dbus_uint32_t int32 = (dbus_uint32_t) ts.Value.uinteger;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT32, &int32);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'INT32'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_INT64: {
		if (TERMTYPE_LONGLONG==ts.type) {
			dbus_int64_t uint64 = (dbus_int64_t) ts.Value.linteger;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_INT64, &uint64);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'INT64'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_UINT64: {
		if (TERMTYPE_ULONGLONG==ts.type) {
			dbus_uint64_t int64 = (dbus_uint64_t) ts.Value.luinteger;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT64, &int64);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'UINT64'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_STRING: {
		if (TERMTYPE_STRING==ts.type) {
			dbus_message_iter_append_basic (iter, DBUS_TYPE_STRING, &ts.Value.string);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'STRING'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;

	}
	case DBUS_TYPE_OBJECT_PATH: {
		if (TERMTYPE_STRING==ts.type) {
			dbus_message_iter_append_basic (iter, DBUS_TYPE_OBJECT_PATH, &ts.Value.string);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'STRING'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	case DBUS_TYPE_BOOLEAN: {
		if (TERMTYPE_LONG==ts.type) {
			dbus_bool_t b = (dbus_bool_t) ts.Value.integer;
			dbus_message_iter_append_basic (iter, DBUS_TYPE_BOOLEAN, &b);
		} else {
			DBGLOG(LOG_ERR, "egress_append_prim: expecting 'BOOLEAN'");
			exit(EDBUS_DECODE_ERROR);
		}
		break;
	}
	}//switch

	th->clean(&ts);

}//


int
egress_next_state(int tt) {

	int ns;

	switch(tt) {
	case DBUS_TYPE_ARRAY:      ns=TIS_START_ARRAY;   break;
	case DBUS_TYPE_VARIANT:    ns=TIS_START_VARIANT; break;
	case DBUS_TYPE_STRUCT:	   ns=TIS_START_STRUCT;  break;
	case DBUS_TYPE_DICT_ENTRY: ns=TIS_START_DICT;    break;

	// if we do not have to deal with a compound,
	// make sure we complete the primitive then
	default:
		ns=TIS_FILL_VALUE;
		break;
	}//switch

	return ns;
}//


void
process_struct_container(TermHandler *th, DBusMessageIter *iter) {

	TermStruct ts;
	DBusMessageIter citer;
	TermIterator cti;

	DBGLOG(LOG_INFO, "egress_iter: start STRUCT");
	int r=th->iter(&ts);
	if (r || (!r && (TERMTYPE_START_LIST!=ts.type))) {
		DBGLOG(LOG_ERR, "egress_iter: expecting 'start array'");
		exit(EDBUS_DECODE_ERROR);
	}
	dbus_message_iter_open_container (iter,
									DBUS_TYPE_STRUCT,
									NULL,
									&citer);
	cti.state=TIS_START_PRIM;
	egress_iter(&cti, th, &citer);
	dbus_message_iter_close_container(iter, &citer);
}//

void
process_array_container(TermHandler *th, DBusMessageIter *iter) {

	TermStruct ts;
	DBusMessageIter citer;
	TermIterator cti;
	DecodePrim dp;
	char sig[2];

	DBGLOG(LOG_INFO, "egress_iter: start ARRAY");
	int r=th->iter(&ts);
	if (r || (!r && (TERMTYPE_START_LIST!=ts.type))) {
		DBGLOG(LOG_ERR, "egress_iter: expecting 'start of list'");
		exit(EDBUS_DECODE_ERROR);
	}
	egress_decode_prim(th, &dp);
	if ((TRUE==dp.end_packet) || (TRUE==dp.end_list)) {
		DBGLOG(LOG_ERR, "process_array_container: expecting a complete primitive");
		exit(EDBUS_DECODE_ERROR);
	}
	sig[0]=dp.dt;
	sig[1]='\0';
	dbus_message_iter_open_container (iter,
									DBUS_TYPE_ARRAY,
									sig,
									&citer);
	cti.state=TIS_START_PRIM;
	egress_iter(&cti, th, &citer);
	dbus_message_iter_close_container(iter, &citer);
}

void
process_variant_container(TermHandler *th, DBusMessageIter *iter) {

	TermStruct ts;
	DBusMessageIter citer;
	TermIterator cti;
	DecodePrim dp;
	char sig[2];

	DBGLOG(LOG_INFO, "egress_iter: start VARIANT");
	int r=th->iter(&ts);
	if (r || (!r && (TERMTYPE_START_LIST!=ts.type))) {
		DBGLOG(LOG_ERR, "egress_iter: expecting 'start array'");
		exit(EDBUS_DECODE_ERROR);
	}
	egress_decode_prim(th, &dp);
	if ((TRUE==dp.end_packet) || (TRUE==dp.end_list)) {
		DBGLOG(LOG_ERR, "egress_iter: (VARIANT) expecting a complete primitive");
		exit(EDBUS_DECODE_ERROR);
	}
	sig[0]=dp.dt;
	sig[1]='\0';
	dbus_message_iter_open_container (iter,
									DBUS_TYPE_ARRAY,
									sig,
									&citer);
	cti.state=TIS_START_PRIM;
	egress_iter(&cti, th, &citer);
	dbus_message_iter_close_container(iter, &citer);

}//

/**
 * If we are at the start of the message part
 * of the packet received, we would get at least
 * one other list element.
 *
 * From the documentation:
 * =======================
 * " Container types are for example struct, variant, and array.
 *   For variants, the contained_signature should be the type of the single value inside the variant.
 *   For structs and dict entries, contained_signature should be NULL; it will be set to whatever types you write into the struct.
 *   For arrays, contained_signature should be the type of the array elements.
 * "
 *
 * DICT_ENTRY:
 *   A DICT_ENTRY works exactly like a struct, but rather than parentheses it uses curly braces, and it has more restrictions.
 *   The restrictions are: it occurs only as an array element type; it has exactly two single complete types inside the curly braces;
 *   the first single complete type (the "key") must be a basic type rather than a container type.
 *   Implementations:
 *    - must not accept dict entries outside of arrays,
 *    - must not accept dict entries with zero, one, or more than two fields, and
 *    - must not accept dict entries with non-basic-typed keys.
 *   A dict entry is always a key-value pair.
 *
 *
 */
int
egress_iter(TermIterator *ti, TermHandler *th, DBusMessageIter *iter) {

	DBusMessageIter citer;
	TermIterator cti;
	DecodePrim dp;


	do {
		switch(ti->state) {
			case TIS_START_PRIM:
				egress_decode_prim(th, &dp);

				//not entirely correct... but good anyways:
				//the Erlang Client is assumed to be handing us
				//properly formatted data, no? ;-)
				if ((TRUE==dp.end_packet) || (TRUE==dp.end_list))
					return 0;

				ti->state=egress_next_state(dp.dt);
				break;

			case TIS_START_DICT: {
				DBGLOG(LOG_INFO, "egress_iter: start DICT");
				dbus_message_iter_open_container (iter,
												DBUS_TYPE_DICT_ENTRY,
												NULL,
												&citer);
				// @TODO check to make sure we are only dealing
				//       with a non-compound type for the 'key'
				//		 part of the dict_entry
				cti.state=TIS_START_PRIM;
				egress_iter(&cti, th, &citer);
				cti.state=TIS_START_PRIM;
				egress_iter(&cti, th, &citer);

				dbus_message_iter_close_container(iter, &citer);
				break;
			}
			case TIS_START_STRUCT: {
				process_struct_container(th, iter);
				ti->state=TIS_START_PRIM;
				break;
			}
				// we just need the type that follows
				// in order to start building the container
			case TIS_START_ARRAY: {
				process_array_container(th, iter);
				ti->state=TIS_START_PRIM;
				break;
			}

			case TIS_START_VARIANT: {
				process_variant_container(th, iter);
				ti->state=TIS_START_PRIM;
				break;
			}

			case TIS_FILL_VALUE:
				egress_append_prim(th, iter, ti->tt);
				ti->state=TIS_START_PRIM;
				break;

			default:
				// this shouldn't happen since we are designing the state-machine!
				DBGLOG(LOG_ERR, "egress_iter: unknown state");
				exit(EDBUS_UNKNOWN_EGRESS_STATE);
		}//switch

	} while(ti->state != TIS_STOP);

	return 0;
}//


/**
 * Expects to decode a tuple() consisting
 * of a DBus primitive as coded by the Erlang Client
 *
 *  {atom(),
 *
 *	@return 0 SUCCESS
 *  @return 1 FAILURE
 */
int
egress_decode_prim(TermHandler *th, DecodePrim *dp) {

	TermStruct ts;

	int r=th->iter(&ts);
	if (r) return 1;

	// if we hit an end of list OR packet,
	// return quickly
	switch(ts.type) {
	case TERMTYPE_END_LIST:
		dp->end_list=TRUE;
		dp->end_packet=FALSE;
		return 0;
	case TERMTYPE_NIL:
		dp->end_list=TRUE;
		dp->end_packet=TRUE;
		return 0;

	case TERMTYPE_INVALID:
	case TERMTYPE_UNSUPPORTED:
		DBGLOG(LOG_ERR, "egress_decode_prim: expecting 'end list' or 'end packet' or 'start_tuple'");
		exit(EDBUS_DECODE_ERROR);

	default:
		// proceed further.
		break;
	}

	//... else, we might get lucky in
	//decoding a primitive type

	if (TERMTYPE_START_TUPLE!=ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_prim: expecting 'start_tuple'");
		exit(EDBUS_DECODE_ERROR);
	}

	r=th->iter(&ts);
	if (r) return 1;
	if (TERMTYPE_ATOM!=ts.type) {
		DBGLOG(LOG_ERR, "egress_decode_prim: expecting 'atom()'");
		exit(EDBUS_DECODE_ERROR);
	}

	dp->end_list=FALSE;
	dp->end_packet=FALSE;
	dp->dt=egress_translate_etype((const char *)ts.Value.string);
	th->clean(&ts);

	return 0;
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
}//
