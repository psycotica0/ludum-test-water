.PHONY: all clean

all: water

water: water.hs
	ghc water.hs

clean:
	$(RM) *.o
	$(RM) Game/Water/*.o
	$(RM) *.hi
	$(RM) Game/Water/*.hi
	$(RM) water
