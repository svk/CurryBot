GHCFLAGS=-Wall

clean:
	rm -f *.hi
	rm -f *.o
	rm -f NetworkTest.exe
	rm -f IrkNetTest.exe
	rm -f IrkParseTest.exe
	rm -f CurryBot.exe

NetworkTest:
	ghc $(GHCFLAGS) --make $@

CurryBot:
	ghc $(GHCFLAGS) --make $@

IrkParseTest:
	ghc $(GHCFLAGS) --make $@
