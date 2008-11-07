test:
	mkdir -p run
	cd run ; erl -sname ct -noshell -pa `pwd`/../../ebin -pa `pwd`/.. -s ct_run script_start -s erlang halt -dir ..

clean:
	rm -rf run
	rm -f *.beam

