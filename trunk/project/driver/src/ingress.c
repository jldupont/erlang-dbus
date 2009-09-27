/**
 * @file ingress.c
 *
 * @date   2009-09-26
 * @author jldupont
 */

#include <pthread.h>
#include "ingress.h"

char *IFilters[INGRESS_MAX_FILTERS+1];
int IFilterCount=0;
DBusBusType    IBusType=NULL;
DBusConnection *IConn=NULL;

void ingress_set_bus(DBusBusType BusType) {
	IBusType=BusType;
}//


void ingress_add_filter(char *filter) {

	if (INGRESS_MAX_FILTERS<=IFilterCount) {
		return;
	}

	IFilters[IFilterCount++]=filter;
}

void ingress_init(DBusConnection *conn) {
	DBusError error;
	IConn=conn;


}//
