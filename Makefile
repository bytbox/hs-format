all: main

main: main.hs
	ghc --make $@

clean:
	${RM} main *.o *.hi

