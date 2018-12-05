.PHONY: all;
all: build/lexer;

build/lexer.d/:
	mkdir -p build/lexer.d/

build/lexer.d/%.cc.re: %.y build/lexer.d/;
	bison -t -Slalr1.cc --report=all $< -o $@

build/lexer.d/%.cc: build/lexer.d/%.cc.re;
	re2c $< -o $@
	
build/lexer: build/lexer.d/lexer.cc;
	clang++-8 -std=c++17 $< -g -o $@ -DYYDEBUG=1 -Wall -O3 -fsanitize=address,undefined

clean:
	rm -rf build/

