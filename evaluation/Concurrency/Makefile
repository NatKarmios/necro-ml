all: eval

eval: concurrent_ss.ml monads.ml test_interpreter.ml
	ocamlc monads.ml concurrent_ss.ml test_interpreter.ml -o eval

test: eval
	./eval

concurrent_ss.ml: concurrent_ss.sk ../../necroml
	../../necroml concurrent_ss.sk -o concurrent_ss.ml

monads.ml:
	ln -s ../../necromonads.ml monads.ml

../../necroml:
	make -C ../..

clean:
	rm -rf concurrent_ss.ml *.cm[io] eval
