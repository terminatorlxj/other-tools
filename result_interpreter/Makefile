
binary:
	ocamllex lexer.mll       
	ocamlyacc parser.mly   
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o interpreter.exe lexer.cmo parser.cmo main.cmo

