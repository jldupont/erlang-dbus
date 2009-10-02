/**
 * @file egress.cc
 *
 * @date   2009-09-26
 * @author jldupont
 *
 * \section Overview
 *
 * This module implements the communication from the Erlang Client towards DBus.
 *
 * \section Protocol
 *
 *	[MsgType,
 *
 * \section Protocol from Erlang Client
 *
 *   {pkt, Signature, Data}
 *
 *  Signature:
 *
 */
#include <stdlib.h>
#include <pthread.h>
#include "dbus/dbus.h"
#include "egress.h"
#include "erlang_dbus_driver.h"


DBusConnection *EConn;
pthread_t _egressThread;

void *__egress_thread_function(void *params);

void egress_init(DBusConnection *conn) {

	EConn=conn;
	pthread_create(&_egressThread, NULL, &__egress_thread_function, NULL);

}//


void *
__egress_thread_function(void *params) {

	return NULL;
}//
