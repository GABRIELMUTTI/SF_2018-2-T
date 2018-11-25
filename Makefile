all: syntax.ml results.ml support.ml functions.ml evaluator.ml typeInfer.ml main.ml
	ocamlfind ocamlopt -o Evaluator syntax.ml results.ml support.ml functions.ml evaluator.ml typeInfer.ml main.ml

run: Evaluator 
	./Evaluator

brun: all run

clean:
	rm *.cmi
	rm *.cmx
	rm *.o
