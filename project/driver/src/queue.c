/**
 * @file queue.c
 *
 * @date 2009-04-24
 * @author: Jean-Lou Dupont
 *
 *
 * The _queue_ module isn't aware of its elements
 * and thus it is the responsibility of the clients
 * to properly dispose of the _nodes_ once retrieved.
 *
 * The term ``node`` is used generically to refer
 * to a node element inside a queue.
 *
 */
#include <time.h>
#include <math.h>
#include <pthread.h>
#include <errno.h>

#include "queue.h"

// PRIVATE
// =======
void *__queue_get_safe(queue *q);
int   queue_put_head_safe( queue *q, void *node );
int queue_put_safe( queue *q, void *node );



/**
 * Creates a queue
 */
queue *queue_create(int id) {

	pthread_cond_t *cond = (pthread_cond_t *) malloc( sizeof (pthread_cond_t) );
	if (NULL == cond) {
		return NULL;
	}

	// if this malloc fails,
	//  there are much bigger problems that loom
	pthread_mutex_t *mutex = (pthread_mutex_t *) malloc( sizeof(pthread_mutex_t) );
	queue *q = (queue *) malloc( sizeof(queue) );

	if ((NULL != q) && (NULL != mutex)){

		q->head  = NULL;
		q->tail  = NULL;
		q->num   = 0;
		q->id    = id;
		q->total_in  = 0;
		q->total_out = 0;

		pthread_mutex_init( mutex, NULL );
		pthread_cond_init( cond, NULL );

		q->mutex      = mutex;
		q->cond       = cond;

	} else {

		if (NULL!=q)
			free(q);

		if (NULL!=mutex)
			pthread_mutex_destroy(mutex);

	}

	return q;
}// init

/**
 * Destroys a queue
 *
 * This function is **not** aware of the
 *  message(s) potentially inside the queue,
 *  thus, the queue must be drained **before**
 *  using this function.
 *
 * The queue can be drained by:
 * - stopping the thread that ``puts``
 * - ``getting`` until NULL is returned
 *
 */
void queue_destroy(queue *q) {

	if (NULL==q) {
		return;
	}
	pthread_mutex_t *mutex = q->mutex;
	pthread_cond_t  *cond  = q->cond;

	pthread_mutex_lock( mutex );
		free(q);
		q=NULL;
	pthread_mutex_unlock( mutex );

	pthread_mutex_destroy(mutex);
	pthread_cond_destroy(cond);

}//



/**
 * Queues a node (blocking)
 *
 * @return 1 => success
 * @return 0 => error
 *
  */
int queue_put(queue *q, void *node) {

	if ((NULL==q) || (NULL==node)) {
		return 0;
	}

	pthread_mutex_lock( q->mutex );

		int code = queue_put_safe( q, node );
		if (code)
			pthread_cond_signal( q->cond );

	pthread_mutex_unlock( q->mutex );

	return code;
}//[/queue_put]



/**
 * Queues a node (non-blocking)
 *
 * @return 1  => success
 * @return 0  => error
 * @return -1 => busy
 *
 */
int queue_put_nb(queue *q, void *node) {

	if ((NULL==q) || (NULL==node)) {
		return 0;
	}

	if (EBUSY == pthread_mutex_trylock( q->mutex ))
		return -1;

		int code = queue_put_safe( q, node );
		if (code)
			pthread_cond_signal( q->cond );

	pthread_mutex_unlock( q->mutex );

	return code;
}//


/**
 * Queue Put Wait
 *
 * @return 0  ERROR
 * @return 1  SUCCESS
 *
 */
	int
queue_put_wait(queue *q, void *node) {

	if ((NULL==q) || (NULL==node)) {
		return 0;
	}

	int code;

	while(1) {

		// quick try... hopefully we get lucky
		if (EBUSY != pthread_mutex_trylock( q->mutex )) {
			code = queue_put_safe( q, node );
			if (code)
				pthread_cond_signal( q->cond );

			pthread_mutex_unlock( q->mutex );

			break;

		} else {
			pthread_mutex_lock( q->mutex );

				int rc = pthread_cond_wait( q->cond, q->mutex );
				if (ETIMEDOUT==rc) {
					code = 1;//not an error to have timed-out really
					break;
				} else {
					code = 0;
				}

			pthread_mutex_unlock( q->mutex );
		}

	}

	return code;
}//

/**
 * Queue_put_safe
 *
 * Lock is not handled here - the caller must take
 * care of this.
 *
 * @return 0 => error
 * @return 1 => success
 *
 */
	int
queue_put_safe( queue *q, void *node ) {

	int code = 1;
	queue_node *new_node=NULL;

	// if this malloc fails,
	//  there are much bigger problems that loom
	new_node = (queue_node *) malloc(sizeof(queue_node));
	if (NULL!=new_node) {

		// new node...
		new_node->node = node;
		new_node->next = NULL;

		// there is a tail... put at the end
		if (NULL!=q->tail)
			(q->tail)->next=new_node;

		// point tail to the new element
		q->tail = new_node;

		// adjust head
		if (NULL==q->head)
			q->head=new_node;

		q->total_in++;
		q->num++;

	} else {

		code = 0;
	}

	return code;
}//


/**
 * Retrieves the next node from a queue
 *
 * @return NULL if none.
 *
 */
void *queue_get(queue *q) {

	if (NULL==q) {
		return NULL;
	}

	pthread_mutex_lock( q->mutex );

		void *node=NULL;
		node = __queue_get_safe(q);

	pthread_mutex_unlock( q->mutex );

	return node;
}//[/queue_get]

/**
 * Retrieves the next node from a queue (non-blocking)
 *
 * @return NULL => No node OR BUSY
 *
 */
void *queue_get_nb(queue *q) {

	if (NULL==q) {
		return NULL;
	}

	if (EBUSY==pthread_mutex_trylock( q->mutex )) {
		return NULL;
	}

		void *node=NULL;
		node = __queue_get_safe(q);

	pthread_mutex_unlock( q->mutex );

	return node;
}//[/queue_get]


/**
 * Waits for a node in the queue
 *
 * @return 0 SUCCESS
 * @return 1 FAILURE
 *
 */
int queue_wait(queue *q) {

	if (NULL==q) {
		return 1;
	}

	//DEBUG_LOG(LOG_DEBUG,"queue_wait: BEFORE LOCK on q[%x][%i]",q,q->id);
	pthread_mutex_lock( q->mutex );

		// it seems we need to wait...
		//DEBUG_LOG(LOG_DEBUG,"queue_wait: BEFORE COND_WAIT on q[%x][%i]",q,q->id);
		int rc = pthread_cond_wait( q->cond, q->mutex );

		//int result2 = pthread_mutex_trylock( q->mutex );
		//DEBUG_LOG(LOG_DEBUG,"queue_get_wait: TRYLOCK q[%x][%i] result[%i] ",q,q->id,result2==EBUSY);

		if ((ETIMEDOUT==rc) || (0==rc)){
			rc=0;
		} else {
			rc=1;
		}

	pthread_mutex_unlock( q->mutex );
	//DEBUG_LOG(LOG_DEBUG,"queue_wait: AFTER LOCK on q[%x][%i]",q,q->id);

	return rc;
}//

/**
 * Wait but with timeout
 *
 * @return 0 SUCCESS
 * @return 1 FAILURE
 */
int queue_wait_timer(queue *q, int usec_timer) {

	if (NULL==q) {
		return 1;
	}
	/*
		struct timeval {
			long    tv_sec;
			long    tv_usec;
		};
	 */
	struct timespec now;

	/*
		struct timespec
		  {
			__time_t tv_sec;
			long int tv_nsec;
		  };
	 */
	struct timespec timeout;

	// __STD_TYPE __TIME_T_TYPE __time_t
	//__time_t t;

	// __TIME_T_TYPE ==> long int
	//__TIME_T_TYPE t;


	//DEBUG_LOG(LOG_DEBUG,"queue_wait: BEFORE LOCK on q[%x][%i]",q,q->id);
	pthread_mutex_lock( q->mutex );

		//gettimeofday(&now, NULL);
		clock_gettime(CLOCK_REALTIME, &now);

		timeout.tv_sec  = now.tv_sec;
		timeout.tv_nsec = now.tv_nsec + (usec_timer*1000);

		if (timeout.tv_nsec >= (long int)1000000000) {
			timeout.tv_nsec -= (long int)1000000000;
			timeout.tv_sec ++;
		}

		// it seems we need to wait...
		int rc = pthread_cond_timedwait( q->cond, q->mutex, &timeout );
		if ((ETIMEDOUT==rc) || (0==rc)){
			rc=0;
		} else {
			//clocks are not precise
			//... the user might also have messed up,
			//so at least provide a log
			if (rc==EINVAL){
				rc=0;
			} else {
				//something else is wrong then...
				rc=1;
			}
		}
	pthread_mutex_unlock( q->mutex );

	return rc;
}//

void *__queue_get_safe(queue *q) {

	queue_node *tmp = NULL;
	void *node=NULL;

	tmp = q->head;
	if (tmp!=NULL) {

		// the queue contained at least one node
		node = tmp->node;

		// adjust tail: case if tail==head
		//  ie. only one element present
		if (q->head == q->tail) {
			q->tail = NULL;
			q->head = NULL;
		} else {
			// adjust head : next pointer is already set
			//  to NULL in queue_put
			q->head = (q->head)->next;
		}

		//DEBUG_LOG(LOG_DEBUG,"queue_get: MESSAGE PRESENT, freeing queue_node[%x]", tmp);
		free(tmp);

		q->total_out++;
		q->num--;

		//{
		int count=0, in=q->total_in, out=q->total_out;
		tmp = q->head;
		while(tmp) {
			count++;
			tmp = tmp->next;
		}
		if ((in-out) != count) {
			// debug...
		}
		//}

	}

	return node;
}//

/**
 * Verifies if a message is present
 *
 * @return 1 if at least 1 message is present,
 * @return 0  if NONE
 * @return -1 on ERROR
 *
 */
int queue_peek(queue *q) {

	if (NULL==q) {
		return -1;
	}

	queue_node *tmp = NULL;
	int result = 0;

	pthread_mutex_lock( q->mutex );

		tmp = q->head;
		result = (tmp!=NULL);

	pthread_mutex_unlock( q->mutex );

	return result;
} // queue_peek





void queue_signal(queue *q) {

	pthread_mutex_lock( q->mutex );

		int rc = pthread_cond_signal( q->cond );
		//if (rc)


	pthread_mutex_unlock( q->mutex );
}




/**
 * Queues a node at the HEAD (non-blocking)
 *
 * @return 1  => success
 * @return 0  => error
 * @return -1 => busy
 *
 */
	int
queue_put_head_nb(queue *q, void *node) {

	if ((NULL==q) || (NULL==node)) {
		return 0;
	}

	if (EBUSY == pthread_mutex_trylock( q->mutex ))
		return -1;

		int code = queue_put_head_safe( q, node );
		if (code)
			pthread_cond_signal( q->cond );

	pthread_mutex_unlock( q->mutex );

	return code;
}//[/queue_put]

/**
 * Puts a node at the HEAD of the queue
 *
 * This function is meant to support _high priority_ messages.
 *
 * @param q      queue reference
 * @param node   message reference
 *
 * @return 1 => success
 * @return 0 => error
 *
 */
int   queue_put_head(queue *q, void *node) {

	if ((NULL==q) || (NULL==node)) {
		return 0;
	}

	pthread_mutex_lock( q->mutex );

		int code = queue_put_head_safe( q, node );
		if (code)
			pthread_cond_signal( q->cond );

	pthread_mutex_unlock( q->mutex );

	//DEBUG_LOG(LOG_DEBUG,"queue_put: END");

	return code;

}//

/**
 * Queue Put Head Wait
 *
 * @return 0 ERROR
 * @return 1 SUCCESS
 *
 */
	int
queue_put_head_wait(queue *q, void *node) {

	if ((NULL==q) || (NULL==node)) {
		return 0;
	}

	int code;

	while(1) {

		// quick try... hopefully we get lucky
		if (EBUSY != pthread_mutex_trylock( q->mutex )) {

			code = queue_put_head_safe( q, node );
			if (code)
				pthread_cond_signal( q->cond );

			pthread_mutex_unlock( q->mutex );

			break;

		} else {

			pthread_mutex_lock( q->mutex );

				int rc = pthread_cond_wait( q->cond, q->mutex );
				if (ETIMEDOUT==rc) {
					rc=1;
				} else {
					rc=0;
				}

			pthread_mutex_unlock( q->mutex );
		}

	}

	return code;

}//


/**
 * Puts a node a the HEAD of the queue
 * without regards to thread-safety
 *
 * @return 1  SUCCESS
 * @return 0  ERROR
 *
 */
	int
queue_put_head_safe( queue *q, void *msg ) {

	int code = 1;
	queue_node *tmp=NULL;

	// if this malloc fails,
	//  there are much bigger problems that loom
	tmp = (queue_node *) malloc(sizeof(queue_node));
	if (NULL!=tmp) {

		tmp->node = msg;
		tmp->next = NULL;

		// there is a head... put at the front
		if (NULL!=q->head) {
			tmp->next = q->head;
		}

		// adjust head
		q->head = tmp;

		// adjust tail
		if (NULL==q->tail)
			q->tail=tmp;

		q->total_in++;
		q->num++;

	} else {

		code = 0;
	}

	return code;
}//
