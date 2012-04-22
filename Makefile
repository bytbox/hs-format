all: main

main: main.hs Text/Format.lhs
	ghc --make $@

clean:
	${RM} main *.o *.hi

