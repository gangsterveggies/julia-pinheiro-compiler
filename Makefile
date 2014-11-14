default:
	rm -f *.hs
	alex Lexer.x
	happy julia.y
	ghc -o julia Lexer.hs julia.hs

clean:
	rm -f *.hs
	rm -f *.hi
	rm -f *.o
	rm -f julia
