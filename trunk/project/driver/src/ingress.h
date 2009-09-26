/**
 * @file ingress.h
 *
 * @date   2009-09-26
 * @author jldupont
 */


#ifndef INGRESS_H_
#define INGRESS_H_

 #include "macros.h"
 #include "dbus/dbus.h"

 typedef IngressFilters (char *IFilters[]);

 EDBUS_BEGIN_DECLS

   void ingress_init(DBusBusType BusType, IngressFilters *);

 EDBUS_END_DECLS



#endif /* INGRESS_H_ */
