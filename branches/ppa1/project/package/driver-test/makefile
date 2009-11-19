# @author Jean-Lou Dupont
#

ERLC=erlc -v -o ebin -I include
SOURCES=$(wildcard src/*.erl)

all:
	@for s in $(SOURCES) ; do \
		echo "Compiling $$s"; \
		$(ERLC) $$s ; \
		done

clean:
	@rm -f ebin/*.beam

.PHONY: all clean
