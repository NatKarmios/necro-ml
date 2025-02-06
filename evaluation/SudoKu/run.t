  $ necroml boolean.sk -o boolean.ml
  $ necroml -d nat.sk -o nat.ml
  $ necroml -d list.sk -o list.ml
  $ necroml -d sudoku.sk -o sudoku.ml
  $ ocamlc boolean.ml nat.ml list.ml sudoku.ml interpreter_boolean.ml interpreter_nat.ml interpreter_list.ml interpreter_sudoku.ml -o solve
  $ ./solve easy.txt
