OS=$(shell uname -s)
CPU=$(shell uname -p)
HEADER=bdd.h
H=BDD.libh
D=bdd-sml
HF=../bdd-h.sml
CF=bdd.cm
LIB=bdd

$(D)/$(CF): $(HEADER) $(OS).$(CPU)
	ml-nlffigen -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $(HEADER)

$(D)\$(CF): $(D)/$(CF)

Darwin.i386: $(LIB).x86-darwin

Darwin.powerpc: $(LIB).ppc-darwin
	
Linux.i686: $(LIB).x86-linux

clean:
	rm -Rf $(D) $(LIB)

