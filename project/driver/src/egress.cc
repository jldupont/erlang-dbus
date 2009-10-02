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



// Prototypes
// ===========
void *__egress_thread_function(void *conn);
void egress_handle_code(int code);
int  egress_translate_type(const char *type);
int egress_decode_header(TermHandler *th, MessageHeader *mh);
int egress_iter(TermHandler *th);


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

		r=egress_iter(th);
		egress_handle_code(r);    //this will exit if required

		//recycle the packet
		p->clean();


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

	// all went well!
	return 0;
}//



int
egress_iter(TermHandler *th) {

	TermStruct ts;

}//


