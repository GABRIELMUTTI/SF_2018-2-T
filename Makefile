all: syntax.ml results.ml functions.ml evaluator.ml typeInfer.ml support.ml main.ml
	ocamlfind ocamlopt -o Program_T_SF syntax.ml results.ml functions.ml typeInfer.ml support.ml evaluator.ml main.ml

run: Program_T_SF 
	./Program_T_SF

brun: all run

clean:
	rm *.cmi
	rm *.cmx
	rm *.o
