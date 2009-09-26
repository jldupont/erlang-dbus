/**
 * @file   queue.h
 *
 * @date   2009-04-23
 * @author Jean-Lou Dupont
 *
 *
 */

#ifndef QUEUE_H_
#define QUEUE_H_

#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

		/**
		 * Queue node - entry in a queue
		 *
		 * @param msg:  pointer to the message
		 * @param next: pointer to the ``next`` queue_node entry
		 */
		typedef struct _queue_node {
			void *node;
			struct _queue_node *next;
		} queue_node;

		/**
		 * Queue - thread-safe
		 *
		 * @param mutex: mutex
		 * @param cond:  the condition variable
		 * @param head:  pointer to ``head``
		 * @param tail:  pointer to ``tail``
		 */
		typedef struct {
			pthread_cond_t  *cond;
			pthread_mutex_t *mutex;
			struct _queue_node *head, *tail;
			int num;
			int id;
			int total_in;
			int total_out;
		} queue;


	// Prototypes
	// ==========
	queue *queue_create(int id);

	void  queue_destroy(queue *queue);

	int   queue_put_nb(queue *q, void *msg);

	/**
	 * @return 1 => success
	 * @return 0 => failure
	 */
	int   queue_put(queue *q, void *msg);
	int	  queue_put_wait(queue *q, void *node);

	int   queue_put_head_nb(queue *q, void *node);
	int   queue_put_head(queue *q, void *msg);
	int   queue_put_head_wait(queue *q, void *node);


	void *queue_get(queue *q);
	void *queue_get_nb(queue *q);
	int   queue_wait(queue *q);
	int   queue_wait_timer(queue *q, int usec_timer);

	int   queue_peek(queue *q);
	void  queue_signal(queue *q);


#endif /* QUEUE_H_ */
