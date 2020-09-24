.PHONY: all test

all: solve

sudoku.ml: ../../necroml
	../../necroml sudoku.sk -o sudoku.ml

../../necroml:
	make -C ../..

solve: interpreter_sudoku.ml sudoku.ml monads.ml
	ocamlc sudoku.ml monads.ml interpreter_sudoku.ml -o solve

monads.ml:
	ln -s ../monads.ml

test: solve
	./solve easy.txt

clean:
	rm -f monads.ml solve sudoku.ml *.cm[io]
