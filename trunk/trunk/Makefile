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
	./docgen.erl doc src/swf*.erl

docs: doc
