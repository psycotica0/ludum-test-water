.PHONY: all clean

all: water

water: water.hs
	ghc water.hs

clean:
	$(RM) *.o
	$(RM) *.hi
	$(RM) water
