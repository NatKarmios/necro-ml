SHELL=bash
red=`tput setaf 1``tput setab 7`
reset=`tput sgr0`
TESTS_RULES = $(shell ls necro-test/*.sk)
TESTS = $(addprefix test/,$(shell ls test))
.PHONY: all test clean

all: necroml

necroml: generator.ml necroml.ml
	ocamlfind ocamlc -package necro -linkpkg necro.cma generator.ml necroml.ml -o necroml

clean:
	rm -f *.cm{i,o}
	rm -f necroml
	rm -f test/*.cm{i,o}
	rm -f *.native
	rm -f test/{arith{,_list},brainfuck,concurrent,implicits,lambda,lists{,_list},miniml,nat,options,pcf,while{,_amb}}.ml

test: necroml Makefile $(TESTS)
	@echo "Testing necroml on arithmetic.sk"
	@set -o pipefail
	@./necroml necro-test/arithmetic.sk -o test/arith.ml
	@ocamlc -I test test/monads.ml test/arith.ml test/test_interpreter_arith.ml -o test_interpreter_arith.native
	@./test_interpreter_arith.native || echo "$(red)Error in file arithmetic.sk testing necroml.$(reset)"

	@echo "Testing necroml on brainfuck.sk"
	@set -o pipefail
	@./necroml necro-test/brainfuck.sk -o test/brainfuck.ml
	@ocamlc -I test test/monads.ml test/brainfuck.ml test/test_interpreter_bf.ml -o test_interpreter_bf.native
	@./test_interpreter_bf.native || echo "$(red)Error in file brainfuck.sk testing necroml.$(reset)"

	@echo "Testing necroml on implicits.sk"
	@set -o pipefail
	@./necroml necro-test/implicits.sk -o test/implicits.ml
	@ocamlc -I test test/monads.ml test/implicits.ml test/test_interpreter_implicits.ml -o test_interpreter_implicits.native
	@./test_interpreter_implicits.native || echo "$(red)Error in file implicits.sk testing necroml.$(reset)"

	@echo "Testing necroml on lambda.sk"
	@set -o pipefail
	@./necroml necro-test/lambda.sk -o test/lambda.ml
	@ocamlc -I test test/monads.ml test/lambda.ml test/test_interpreter_lambda.ml -o test_interpreter_lambda.native
	@./test_interpreter_lambda.native || echo "$(red)Error in file lambda.sk testing necroml.$(reset)"

	@echo "Testing necroml on lists.sk"
	@set -o pipefail
	@./necroml necro-test/lists.sk -o test/lists.ml
	@ocamlc -I test test/monads.ml test/lists.ml test/test_interpreter_lists.ml -o test_interpreter_lists.native
	@./test_interpreter_lists.native || echo "$(red)Error in file lists.sk testing necroml.$(reset)"

	@echo "Testing necroml on miniml.sk"
	@set -o pipefail
	@./necroml necro-test/miniml.sk -o test/miniml.ml
	@ocamlc -I test test/monads.ml test/miniml.ml test/test_interpreter_miniml.ml -o test_interpreter_miniml.native
	@./test_interpreter_miniml.native || echo "$(red)Error in file miniml.sk testing necroml.$(reset)"

	@echo "Testing necroml on nat.sk"
	@set -o pipefail
	@./necroml necro-test/nat.sk -o test/nat.ml
	@ocamlc -I test test/monads.ml test/nat.ml test/test_interpreter_nat.ml -o test_interpreter_nat.native
	@./test_interpreter_nat.native || echo "$(red)Error in file nat.sk testing necroml.$(reset)"

	@echo "Testing necroml on options_monad.sk"
	@set -o pipefail
	@./necroml necro-test/options_monad.sk -o test/options.ml
	@ocamlc -I test test/monads.ml test/options.ml test/test_interpreter_options.ml -o test_interpreter_options.native
	@./test_interpreter_options.native || echo "$(red)Error in file options_monad.sk testing necroml.$(reset)"

	@echo "Testing necroml on pcf.sk"
	@set -o pipefail
	@./necroml necro-test/pcf.sk -o test/pcf.ml
	@ocamlc -I test test/monads.ml test/pcf.ml test/test_interpreter_pcf.ml -o test_interpreter_pcf.native
	@./test_interpreter_pcf.native || echo "$(red)Error in file pcf.sk testing necroml.$(reset)"

	@echo "Testing necroml on while_monad.sk"
	@set -o pipefail
	@./necroml necro-test/while_monad.sk -o test/while.ml
	@ocamlc -I test test/monads.ml test/while.ml test/test_interpreter_while.ml -o test_interpreter_while.native
	@./test_interpreter_while.native || echo "$(red)Error in file while_monad.sk testing necroml.$(reset)"

	@echo "Testing necroml on while_amb.sk"
	@set -o pipefail
	@./necroml necro-test/while_amb.sk -o test/while_amb.ml
	@ocamlc -I test test/monads.ml test/while_amb.ml test/test_interpreter_while_amb.ml -o test_interpreter_while_amb.native
	@./test_interpreter_while_amb.native || echo "$(red)Error in file while_amb.sk testing necroml --collect.$(reset)"

	@echo "Testing necroml on concurrent.sk"
	@set -o pipefail
	@./necroml necro-test/concurrent.sk -o test/concurrent.ml
	@ocamlc -I test test/monads.ml test/concurrent.ml test/test_interpreter_concurrent.ml -o test_interpreter_concurrent.native
	@./test_interpreter_concurrent.native || echo "$(red)Error in file concurrent.sk testing necroml --collect.$(reset)"
