GHCFLAGS=-Wall

clean:
	rm -f *.hi
	rm -f *.o
	rm -f NetworkTest.exe

NetworkTest:
	ghc $(GHCFLAGS) --make $@

IrkNetTest:
	ghc $(GHCFLAGS) --make $@

IrkParseTest:
	ghc $(GHCFLAGS) --make $@
