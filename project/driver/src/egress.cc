/**
 * @file egress.cc
 *
 * @date   2009-09-26
 * @author jldupont
 *
 */
#include <stdlib.h>
#include <pthread.h>
#include "dbus/dbus.h"
#include "egress.h"
#include "erlang_dbus_driver.h"


// "Local" variables
pthread_t _egressThread;



// Prototypes
// ===========
void *__egress_thread_function(void *conn);
void handle_code(int code);




// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-




void egress_init(DBusConnection *conn) {

	pthread_create(&_egressThread, NULL, &__egress_thread_function, (void *)conn);

}//


void *
__egress_thread_function(void *conn) {

	Pkt         *p  = new Pkt();
	PktHandler  *ph = new PktHandler();
	TermHandler *th = new TermHandler();

	do {

		// Blocking
		int r = ph->rx(&p);
		if (r) {
			last_error = ph->last_error;
			DBGLOG(LOG_ERR, "egress thread: error, msg: %s", ph->strerror());
			exit(EDBUS_RECEIVE_ERROR);
		}

		// before *any* iteration can be done
		th->init(p);

		r=egress_iter(th);
		handle_code(r);    //this will exit if required

		//recycle the packet
		p->clean();


	} while(TRUE);

	return NULL;
}//

void
handle_code(int code) {

}

int
egress_iter(TermHandler *th) {

	TermStruct ts;

}//


