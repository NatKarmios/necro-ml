.PHONY: all test

all: solve

boolean.ml: ../../necroml
	../../necroml -d boolean.sk -o boolean.ml
list.ml: ../../necroml
	../../necroml -d list.sk -o list.ml
nat.ml: ../../necroml
	../../necroml -d nat.sk -o nat.ml
sudoku.ml: ../../necroml
	../../necroml -d sudoku.sk -o sudoku.ml

../../necroml:
	make -C ../..

solve: interpreter_sudoku.ml boolean.ml nat.ml list.ml sudoku.ml monads.ml
	ocamlc monads.ml boolean.ml nat.ml list.ml sudoku.ml \
		interpreter_boolean.ml interpreter_nat.ml interpreter_list.ml \
		interpreter_sudoku.ml -o solve

monads.ml:
	ln -s ../../necromonads.ml monads.ml

test: solve
	./solve easy.txt

clean:
	rm -f monads.ml solve nat.ml list.ml boolean.ml sudoku.ml *.cm[io]
