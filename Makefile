all:
	ghc -O3 Main.hs -o whilei

clean:
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete

clear:
	make clean
	rm whilei
