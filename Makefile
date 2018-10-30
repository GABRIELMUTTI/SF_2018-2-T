all: syntax.ml results.ml evaluator.ml
	ocamlfind ocamlopt -o Evaluator syntax.ml results.ml evaluator.ml  

run: Evaluator 
	./Evaluator
