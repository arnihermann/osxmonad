all:
	gcc -g -c utils.m
	hsc2hs Window.hsc
	ghc -o osxmonad --make -framework Cocoa Main.hs Window.hs utils.o
