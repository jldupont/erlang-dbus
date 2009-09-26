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
#include "egress.h"
#include "queue.h"


// Input Queue
queue *_egressQueue;

// Switch Thread
pthread_t _egressThread;


void
egress_init() {

	_egressQueue = queue_create(-1);
	pthread_create(&_egressThread, NULL, &__egress_thread_function, NULL);

}//


void *
__egress_thread_function(void *params) {



}//
