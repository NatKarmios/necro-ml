all: eval

eval: while.ml monads.ml test_interpreter_while.ml
	ocamlc monads.ml while.ml test_interpreter_while.ml -o eval

test: eval
	./eval

while.ml: while.sk ../../necroml
	../../necroml while.sk -o while.ml

monads.ml:
	ln -s ../../necromonads.ml monads.ml

../../necroml:
	make -C ../..

clean:
	rm -rf while.ml *.cm[io] eval
