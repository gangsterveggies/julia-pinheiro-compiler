default:
	rm -f Lexer.hs
	rm -f Parser.hs
	alex Lexer.x
	happy Parser.y
	ghc -o julia Lexer.hs Parser.hs TCompile.hs julia.hs

clean:
	rm -f Lexer.hs
	rm -f Parser.hs
	rm -f *.hi
	rm -f *.o
	rm -f julia
