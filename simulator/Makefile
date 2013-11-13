ARCH=$(shell uname)

ifeq ($(ARCH), HP-UX)
	MLARCH = hppa-hpux
endif
ifeq ($(ARCH), SunOS)
	MLARCH = sparc-solaris
endif
ifeq ($(ARCH), IRIX64)
	MLARCH = mipseb-irix
endif
ifeq ($(ARCH), Linux)
	MLARCH = x86-linux
endif
ifeq ($(findstring CYGWIN_NT, $(ARCH)), CYGWIN_NT)
	MLARCH = x86-cygwin
endif
ifeq ($(ARCH), Darwin)
	MLARCH = x86-darwin
endif

all: cpn.ML.$(MLARCH)

clean:
	(cd ../;\
	find . -name .cm -print0 | xargs -0 rm -Rf)

test: clean all start

install:
	(cp sim/cpn.ML.* ../simulator/runtime/;\
	rm sim/cpn.ML.*)


start:
	(mv sim/cpn.ML.$(MLARCH) runtime;\
	cd runtime;\
	chmod a+x *.*;\
	PORT=$$RANDOM;\
	echo "Starting daemon on port $$PORT";\
	./cpnmld.$(MLARCH) $$PORT ./run.$(MLARCH))
	

cpn.ML.$(MLARCH): sml-full.$(MLARCH)
	(cd com/;\
	rm CpnML.dmo.*;\
	make;\
	cd ../sim;\
	rm cpn.ML.*;\
	make)

sml-full.$(MLARCH):
	(echo "SMLofNJ.exportML \"sml-full\";" | sml -m image.cm;\
	mv sml-full.* runtime/heap/)

.SILENT:
