dve.lex.sml: dve.lex
	ml-lex dve.lex

dve.grm.sml: dve.grm
	ml-yacc dve.grm

all: dve.lex.sml dve.grm.sml
	sml -m dve.cm

clean:
	rm -rf .cm dve.lex.sml dve.grm.sml *~
