.PHONY: all compile clean doc docs

all: compile

compile:
	@erl -make

clean:
	rm -f *.beam ebin/*.beam
	rm -f erl_crash.dump
	rm -f `find doc/* -prune -type f`
	
doc:
	cp doc/priv/* doc
	./docgen.erl doc src/*.erl

docs: doc

install: soft_install

soft_install:
	ln -sf `pwd`/ssacli.erl /usr/local/bin/ssacli

