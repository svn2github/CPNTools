ARCH = $(shell uname)

ifeq ($(ARCH), HP-UX)
	MLARCH = hppa-hpux
endif
ifeq ($(ARCH), SunOS)
	MLARCH = sparc-solaris
endif
ifeq ($(ARCH), IRIX)
	MLARCH = mipseb-irix
endif
ifeq ($(ARCH), Linux)
	MLARCH = x86-linux
	SML = ../runtime/run.x86-linux @SMLload=../runtime/heap/sml-full.x86-linux
endif
ifeq ($(findstring CYGWIN_NT, $(ARCH)), CYGWIN_NT)
	MLARCH = x86-win32
	SML = ../runtime/run.x86-win32.exe @SMLload=../runtime/heap/sml-full.x86-win32
	MLARCH = x86-cygwin
	SML = ../runtime/run.x86-cygwin @SMLload=../runtime/heap/sml-full.x86-cygwin
endif
ifeq ($(ARCH), Darwin)
	ifeq ($(shell uname -p), i386)
		MLARCH = x86-darwin
		SML = ../runtime/run.x86-darwin @SMLload=../runtime/heap/sml-full.x86-darwin
	else
		MLARCH = ppc-darwin
		SML = ../runtime/run.ppc-darwin @SMLload=../runtime/heap/sml-full.ppc-darwin
	endif
endif

DMOIMAGE = CpnML.dmo.$(MLARCH)
ifndef SML
	SML = /users/cpn/SML/release/bin/sml-full-topcompile
endif

MLFILES = $(wildcard *.sml)

$(DMOIMAGE): $(MLFILES)
	echo 'use "link.sml"; CpnML.Toploop.build "CpnML.dmo";' | $(SML);

clean:
	rm -f CpnML.dmo.*
