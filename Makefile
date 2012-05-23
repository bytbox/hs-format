all: main

main: main.hs Text/Format.hs
	ghc -W --make $@

clean:
	${RM} main *.o *.hi */*.o */*.hi

