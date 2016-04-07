.PHONY: clean

a.out: toy.cpp
	clang++ -g -O3 toy.cpp `/usr/local/opt/llvm/bin/llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native`

clean:
	rm -rf a.out*
