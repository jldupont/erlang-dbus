/**
 * @file erlang_dbus_driver.c
 *
 * @date 2009-09-26
 * @author Jean-Lou Dupont
 *
 * \section Overview
 *
 * The driver is split in two parts:
 * 1) an ingress direction (DBus   --> Erlang)
 * 2) an egress direction  (Erlang --> DBus)
 *
 * The 'egress' direction can optionally be disabled: this saves a running thread.
 *
 * \section DBus primitive types
 *
 *  BYTE
 *  BOOLEAN
 *  INT16
 *  UINT16
 *  INT32
 *  UINT32
 *  INT64
 *  UINT64
 *  DOUBLE
 *  STRING
 *  OBJECT_PATH
 *  SIGNATURE
 *
 * \section DBus compound types
 *
 *  ARRAY
 *  VARIANT
 *  STRUCT
 *  DICT
 *
 *  \section Adaptation to Erlang (type mapping)
 *
 *    DBus                Erlang
 *    ====                ======
 *    BYTE                {byte, int()}
 *    BOOLEAN             {bool, true|false}
 *    INT16               {int16,  int()}
 *    UINT16              {uint16, int()}
 *    INT32               {int32,  int()}
 *    UINT32              {uint32, int()}
 *    INT64               {int64,  int()}
 *    UINT64              {uint64, int()}
 *    DOUBLE              {double, float()}
 *    STRING              {string, list(int())}
 *    OBJECT_PATH         {object_path, list(int())}   Same as STRING
 *
 *    ARRAY               list()                       Only elements of the same type
 *    VARIANT
 *    STRUCT              list()                       Does not appear to be in use
 *    DICT                [{Key, Value}]
 *
 */
#include "erlang_dbus_driver.h"
#include <stdio.h>
#include <string.h>
#include <dbus/dbus.h>
#include "queue.h"


/**
 * Library entry point
 *
 * Arguments:
 *
 *  There must be at least one filter:
 *   --filters Filter1 [Filter2, ..., FilterN]
 *
 *  By default, the 'system' bus is used but can be overidden to the 'session' bus:
 *   --session
 *
 *  The 'egress' (from Erlang to DBus) processing direction can be disabled:
 *   --noegress
 *
 */
int main(int argc, char **argv) {

	/*
	 * Start 'ingress' thread
	 */



	/*
	 * Start 'egress' thread (if required)
	 */

}//
