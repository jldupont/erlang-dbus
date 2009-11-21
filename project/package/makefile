#
# @author Jean-Lou Dupont
# @title  Erlang-DBus
#
PRJ=erlang-dbus

all:
	@make -C client
	@make -C client-test        
	@make -C driver   
	@make -C driver-test  
	@make -C monitor

clean:
	@make -C client       clean
	@make -C client-test  clean      
	@make -C driver       clean
	@make -C driver-test  clean
	@make -C monitor      clean
	
install:
	@install -D $(CURDIR)/monitor/erlang-dbus-monitor                $(DESTDIR)/usr/bin/erlang-dbus-monitor
	@install -D $(CURDIR)/driver/Release/erlang-dbus-driver          $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/driver/erlang-dbus-driver
	
	@install -D $(CURDIR)/client/src/erlang-dbus-client.erl          $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/src/erlang-dbus-client.erl
	@install -D $(CURDIR)/client/src/erlang-dbus-client-server.erl   $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/src/erlang-dbus-client-server.erl
	@install -D $(CURDIR)/client/src/erlang-dbus-client-tools.erl    $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/src/erlang-dbus-client-tools.erl
	
	@install -D $(CURDIR)/monitor/src/erlang_dbus_monitor.erl        $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/src/erlang_dbus_monitor.erl
	@install -D $(CURDIR)/monitor/ebin/erlang_dbus_monitor.beam      $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/ebin/erlang_dbus_monitor.beam
	
	@install -D $(CURDIR)/client/ebin/erlang-dbus-client.beam        $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/ebin/erlang-dbus-client.beam
	@install -D $(CURDIR)/client/ebin/erlang-dbus-client-server.beam $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/ebin/erlang-dbus-client-server.beam
	@install -D $(CURDIR)/client/ebin/erlang-dbus-client-tools.beam  $(DESTDIR)/usr/lib/erlang/lib/$(PRJ)/ebin/erlang-dbus-client-tools.beam
	

.PHONY: all clean install
