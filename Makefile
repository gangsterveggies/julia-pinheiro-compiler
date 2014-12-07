default:
	if [ ! -f Lexer.hs ]; then alex Lexer.x; fi
	if [ ! -f Parser.hs ]; then happy Parser.y; fi
	if [ Lexer.x -nt Lexer.hs ]; then alex Lexer.x; fi
	if [ Parser.y -nt Parser.hs ]; then happy Parser.y; fi
	ghc -o julia Lexer.hs Parser.hs TCompile.hs StaticAnalysis.hs Stack.hs Scope.hs julia.hs

clean:
	rm -f Lexer.hs
	rm -f Parser.hs
	rm -f *.hi
	rm -f *.o
	rm -f julia
