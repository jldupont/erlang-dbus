# @author Jean-Lou Dupont
#

ERLC=erlc -o ebin -I include
SRCPATH=./src/
SOURCES=erlang-dbus-client.erl erlang-dbus-client-tools.erl
EDOC=escript docs

all: code doc

doc:
	$(EDOC)


code:
	@for s in $(SOURCES) ; do \
		$(ERLC) $(SRCPATH)$$s ; \
		done

clean:
	@rm -f ebin/*.beam


.PHONY: doc clean
