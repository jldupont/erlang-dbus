# @author Jean-Lou Dupont
#

ERLC=erlc -o ebin -I include
SOURCES=src/test.erl

all:
	@for s in $(SOURCES) ; do \
		$(ERLC) $$s ; \
		done

clean:
	@rm -f ebin/*.beam

