  $ necroml boolean.sk -o boolean.ml
  $ necroml -d nat.sk -o nat.ml
  $ necroml -d list.sk -o list.ml
  $ necroml -d sudoku.sk -o sudoku.ml
  $ ocamlc -I ../../monads ../../monads/necromonads.ml boolean.ml nat.ml list.ml sudoku.ml interpreter_boolean.ml interpreter_nat.ml interpreter_list.ml interpreter_sudoku.ml -o solve
  $ ./solve easy.txt
  265|198|734
  891|437|625
  473|562|189
  -----------
  948|351|267
  126|784|593
  357|926|418
  -----------
  584|219|376
  632|875|941
  719|643|852
