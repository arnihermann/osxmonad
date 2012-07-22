all:
	gcc -g -c utils.c
	hsc2hs Window.hsc
	ghc --make -framework Cocoa Main.hs Window.hs utils.o
