.PHONY: all compile clean doc docs lib test

all: compile

compile:
	@erl -make

clean:
	rm -f *.beam ebin/*.beam
	rm -f erl_crash.dump
	rm -f `find doc/* -prune -type f`
	make -C test clean

distclean: clean
	make -C lib distclean
	make -C test clean

doc:
	cp doc/priv/* doc
	./docgen.erl doc src/*.erl

docs: doc

install: soft_install

soft_install:
	ln -sf `pwd`/ssacli.erl /usr/local/bin/ssacli

lib:
	make -C lib

test:
	make -C test
