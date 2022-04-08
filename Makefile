CXX = c++ -std=c++17
CXXC = $(CXX) -c -I include -I gen
RE2C = re2c
BISON = bison

build/snowglobe: build/main.o build/input.o build/lexer.o build/parser.o build/diagnostic.o | build
	$(CXX) $^ -o $@

build/main.o: src/main.cpp include/input.hpp include/diagnostic.hpp gen/parser.cpp | build
	$(CXXC) $< -o $@

build/input.o: src/input.cpp include/input.hpp | build
	$(CXXC) $< -o $@

build/lexer.o: gen/lexer.cpp include/input.hpp include/diagnostic.hpp gen/parser.cpp | build
	$(CXXC) $< -o $@

build/parser.o: gen/parser.cpp include/input.hpp include/diagnostic.hpp | build
	$(CXXC) $< -o $@

build/diagnostic.o: src/diagnostic.cpp include/diagnostic.hpp gen/parser.cpp | build
	$(CXXC) $< -o $@

gen/lexer.cpp: src/lexer.re | gen
	$(RE2C) $< -o $@

gen/parser.cpp: src/parser.y | gen
	$(BISON) $< -o $@

gen:
	mkdir gen

build:
	mkdir build

clean:
	rm -r gen
	rm -r build

.PHONY: clean
