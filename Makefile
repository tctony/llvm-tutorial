.PHONY: clean

a.out: toy.cpp
	clang++ -g -O3 toy.cpp `/usr/local/opt/llvm/bin/llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native`

fib.o: a.out fib.ks
	./a.out < fib.ks 2>&1 | /usr/local/opt/llvm/bin/llc -filetype=obj > fib.o
fib: fib.o print.o
	gcc fib.o print.o -o fib

clean:
	rm -rf a.out* fib fib.o
