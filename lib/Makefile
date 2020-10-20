all: getjsonlib compilejsonlib

getjsonlib:
	if [ -d "erlang-rfc4627" ]; then \
		cd erlang-rfc4627 ; git pull ;\
	else \
		git clone https://github.com/tonyg/erlang-rfc4627.git ;\
	fi

compilejsonlib:
	mkdir -p erlang-rfc4627/ebin
	make -C erlang-rfc4627

clean:
	make -C erlang-rfc4627 clean

distclean:
	rm -rf erlang-rfc4627
