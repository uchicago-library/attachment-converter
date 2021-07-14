# BSD make / GNU make interroperability for a GNU makefile
# regardless of which make reads this file, gmake is invoked on GNUmakefile
# so don't change this, GNUmakefile is your real makefile
all .DEFAULT!
	gmake $@ $(MAKEFLAGS)
