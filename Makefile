all: syntax.ml results.ml support.ml functions.ml evaluator.ml typeInfer.ml
	ocamlfind ocamlopt -o Evaluator syntax.ml results.ml support.ml functions.ml evaluator.ml typeInfer.ml

run: Evaluator 
	./Evaluator

brun: all run

clean:
	rm *.cmi
	rm *.cmx
	rm *.o
