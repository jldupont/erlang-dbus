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
 * \section DBus primitives
 *
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


}//
