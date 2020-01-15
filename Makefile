CMO = DJINNlexer.cmo DJINNparser.cmo DJINNinterp.cmo x86_64.cmo DJINNcompile.cmo DJINNmain.cmo
GENERATED = DJINNlexer.ml DJINNparser.ml DJINNparser.mli
FLAGS = -annot -g

all: djinn
	./djinn --interpreter-only test.nx
	./djinn --compiler-only test2.nx
	gcc -no-pie -g test.s -o a.out
	./a.out

interp: djinn
	./djinn --interpreter-only test.nx

compiler: djinn
	./djinn --compiler-only test2.nx
	gcc -no-pie -g test.s -o a.out
	./a.out

.PHONY: tests
tests: djinn
	@echo "Teste de prints"
	./djinn --interpreter-only  test_prints.nx
	@echo "Teste de if-else"
	./djinn --interpreter-only  test_ifs.nx
	@echo "Teste de for"
	./djinn --interpreter-only  test_for.nx
	@echo "Teste de operações booleanas"
	./djinn --interpreter-only  test_boolops.nx
	@echo "Teste de operações com variáveis"
	./djinn --interpreter-only  test_vars.nx
	@echo "Teste de operações com arrays"
	./djinn --interpreter-only  test_array.nx

djinn: $(CMO)
	ocamlc $(FLAGS) -o $@ nums.cma $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

.mly.mli:
	menhir -v $<

clean:
	rm -f *.cm[io] *.o *.annot *~ djinn $(GENERATED)
	rm -f parser.output parser.automaton

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend