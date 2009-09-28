/**
 * @file egress.c
 *
 * @date   2009-09-26
 * @author jldupont
 *
 * \section Overview
 *
 * Erlang Client --> DBus
 *
 * - Receive Protocol Packet from Erlang Client
 * - Perform protocol adaptation
 * - Send on DBus
 *
 * \section Protocol from Erlang Client
 *
 *   {pkt, Signature, Data}
 *
 *  Signature:
 *
 */
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
