all: getjsonlib compilejsonlib

getjsonlib:
	if [ -d "erlang-rfc4627" ]; then \
		cd erlang-rfc4627 ; hg update ;\
	else \
		hg clone http://hg.opensource.lshift.net/erlang-rfc4627/ ;\
	fi

compilejsonlib:
	mkdir -p erlang-rfc4627/ebin
	make -C erlang-rfc4627

clean:
	make -C erlang-rfc4627 clean

distclean:
	rm -rf erlang-rfc4627
