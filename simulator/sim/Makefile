ARCH = $(shell uname)

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
	SML = ../runtime/run.x86-linux
endif
ifeq ($(findstring CYGWIN_NT, $(ARCH)), CYGWIN_NT)
	MLARCH = x86-win32
	SML = ../runtime/run.x86-win32.exe
	MLARCH = x86-cygwin
	SML = ../runtime/run.x86-cygwin
endif
ifeq ($(ARCH), Darwin)
	MLARCH = x86-darwin
	SML = ../runtime/run.x86-darwin
endif

CPNIMAGE = cpn.ML.$(MLARCH)
DMOIMAGE = CpnML.dmo.$(MLARCH) 
DMODIR = ../com
ifndef SML
	SML = /users/cpn/SML/release/bin/sml
endif

MLFILES = $(wildcard *.sml) $(wildcard external/*.sml) $(wildcard ReportStuff/*.sml) $(wildcard perf/*.sml) $(wildcard ../commscpn/*.sml)

$(CPNIMAGE): $(MLFILES) $(DMODIR)/$(DMOIMAGE)
	echo 'use "link.sml"; CpnML.Toploop.build "cpn.ML";' | $(SML) @SMLload=$(DMODIR)/$(DMOIMAGE);

clean:
	rm -f cpn.ML.*
