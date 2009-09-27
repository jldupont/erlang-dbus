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

 #define INGRESS_MAX_FILTERS 255

 EDBUS_BEGIN_DECLS

	 void ingress_add_filter(char *filter);
	 void ingress_set_bus(DBusBusType BusType);
	 void ingress_init(DBusConnection conn);

 EDBUS_END_DECLS



#endif /* INGRESS_H_ */
