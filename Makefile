.PHONY: all;
all: build/lexer;

_OBJECTS =                                \
					 ASTizer                        \
           Codegenizer                    \
					 main                           \
					 Meterializer                   \
					 Tokenizer                      \
					 Unicode                        \

OBJECTS = $(patsubst %,build/lexer.d/%.o,$(_OBJECTS))

build/lexer.d/%.o: Source/%.cc;
	mkdir -p build/lexer.d/
	g++-7 -std=gnu++17 -c $< -ISource/ -o $@ -Wall -Os

build/lexer: $(OBJECTS);
	g++-7 -std=gnu++17 $^ -o $@ -Wall -Os

clean:
	rm -rfv build/lexer.d/*.o
