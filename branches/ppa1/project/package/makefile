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
	

.PHONY: all clean
