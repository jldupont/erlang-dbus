/**
 * @file ingress.h
 *
 * @date   2009-09-26
 * @author jldupont
 */


#ifndef INGRESS_H_
#define INGRESS_H_

 #include "macros.h"
 #include <dbus/dbus.h>

 #define INGRESS_MAX_FILTERS 255

 void ingress_add_filter(char *filter);
  void ingress_init(DBusConnection *conn);

 typedef union _EDBusMessage {
		 int type;
		 const char *sender;
		 const char *dest;
		 dbus_uint32_t serial;

		 struct _Method_Signal {
			 const char *path;
			 const char *interface;
			 const char *member;
		 } Method_Signal;

		 struct _Reply {} Reply;
		 struct _Error {
			 const char *name;
		 } Error;
	 } EDBusMessage;



#endif /* INGRESS_H_ */
