CXX = c++ -std=c++17
CXXC = $(CXX) -c -I $I -I $G
RE2C = re2c
BISON = bison

S = src
I = include
B = build
G = gen

$B/snowglobe: $B/main.o $B/input.o $B/lexer.o $B/parser.o $B/diagnostics.o $B/compiler.o | $B
	$(CXX) $^ -o $@

$B/main.o: $S/main.cpp $I/input.hpp $I/location.hpp $I/diagnostics.hpp $I/ast.hpp $I/compiler.hpp $I/program.hpp $G/parser.cpp | $B
	$(CXXC) $< -o $@

$B/input.o: $S/input.cpp $I/input.hpp | $B
	$(CXXC) $< -o $@

$B/lexer.o: $G/lexer.cpp $I/input.hpp $I/location.hpp $I/diagnostics.hpp $I/ast.hpp $G/parser.cpp | $B
	$(CXXC) $< -o $@

$B/parser.o: $G/parser.cpp $I/input.hpp $I/location.hpp $I/diagnostics.hpp $I/ast.hpp $I/utils.hpp | $B
	$(CXXC) $< -o $@

$B/diagnostics.o: $S/diagnostics.cpp $I/location.hpp $I/diagnostics.hpp | $B
	$(CXXC) $< -o $@

$B/compiler.o: $S/compiler.cpp $I/compiler.hpp $I/ast.hpp $I/program.hpp $I/location.hpp $I/diagnostics.hpp $I/utils.hpp  | $B
	$(CXXC) $< -o $@

$G/lexer.cpp: $S/lexer.re | $G
	$(RE2C) $< -o $@

$G/parser.cpp: $S/parser.y | $G
	$(BISON) $< -o $@

$G:
	mkdir $G

$B:
	mkdir $B

clean:
	rm -rf $G
	rm -rf $B

.PHONY: clean
