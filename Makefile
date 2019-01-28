.PHONY: all;
all: build/lexer;

build/lexer.d/:
	mkdir -p build/lexer.d/

build/lexer.d/%.o: Source/%.cc Source/Meter/*.hh build/lexer.d/ Makefile;
	g++-7 -std=gnu++17 -c $< -ISource/ -o $@ -Wall -Os

build/lexer: build/lexer.d/main.o build/lexer.d/Tokenizer.o build/lexer.d/ASTizer.o;
	g++-7 -std=gnu++17 $^ -o $@ -Wall -Os

clean:
	rm -rf build/
