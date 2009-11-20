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
	@cp monitor/erlang-dbus-monitor                /usr/bin/
	@cp driver/Release/erlang-dbus-driver          /usr/lib/erlang/lib/$(PRJ)/driver
	
	@cp client/src/erlang-dbus-client.erl          /usr/lib/erlang/lib/$(PRJ)/src
	@cp client/src/erlang-dbus-client-server.erl   /usr/lib/erlang/lib/$(PRJ)/src
	@cp client/src/erlang-dbus-client-tools.erl    /usr/lib/erlang/lib/$(PRJ)/src
	
	@cp monitor/src/*.erl                          /usr/lib/erlang/lib/$(PRJ)/src
	@cp monitor/ebin/*.beam                        /usr/lib/erlang/lib/$(PRJ)/ebin	
	
	@cp client/ebin/erlang-dbus-client.beam        /usr/lib/erlang/lib/$(PRJ)/ebin
	@cp client/ebin/erlang-dbus-client-server.beam /usr/lib/erlang/lib/$(PRJ)/ebin
	@cp client/ebin/erlang-dbus-client-tools.beam  /usr/lib/erlang/lib/$(PRJ)/ebin
	

.PHONY: all clean install
