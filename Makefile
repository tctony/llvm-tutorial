.PHONY: clean

a.out: toy.cpp
	clang++ -g -O3 toy.cpp

clean:
	rm -rf a.out*
