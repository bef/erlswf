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

stat:
	@all_erls="src/*erl ssacli.erl test/*.erl include/*.hrl" ;\
	echo "lines of code (w/o comments):" ;\
	cat `echo $$all_erls` |grep -v '^[[:space:]]*$$' |grep -v "^%" |wc -l ;\
	echo "functions:" ;\
	cat `echo $$all_erls` |grep  '^[a-z].*(.*) ->' |wc -l ;\
	echo "inline functions (fun()s)" ;\
	cat `echo $$all_erls` |grep  'fun(' |wc -l ;\
	echo "total comments:" ;\
	cat `echo $$all_erls` |grep  '%% ' |wc -l ;\
	echo "pure comments:" ;\
	cat `echo $$all_erls` |grep  '^[[:space:]]*%% ' |wc -l ;\
	echo "@doc comments:" ;\
	cat `echo $$all_erls` |grep  '^%% @doc' |wc -l ;\
	echo "lines including binaries (<<>>):" ;\
	cat `echo $$all_erls` |grep  '<<' |wc -l ;\
	echo
