CXX = c++ -std=c++17 -c -Wall -Wextra -I $I -I $G
LINKER = c++ -std=c++17
RE2C = re2c
BISON = bison

S = src
I = include
B = build
C = build/compiler
G = gen

location_hpp = $I/location.hpp
input_hpp = $I/input.hpp $(location_hpp)
ast_hpp = $I/ast.hpp $(location_hpp)
program_hpp = $I/program.hpp
utils_hpp = $I/utils.hpp
diagcol_hpp = $I/diagcol.hpp $(location_hpp)
compiler_hpp = $I/compiler.hpp $(ast_hpp) $(program_hpp) $(diagcol_hpp)
parser_hpp = $G/parser.cpp $(location_hpp) $(input_hpp) $(diagcol_hpp) $(ast_hpp)
diags_hpp = $I/diags.hpp $(diagcol_hpp) $(ast_hpp) $(program_hpp) $(compiler_hpp)

compiler = $C/compiler.o $C/globals.o $C/functions.o $C/statements.o $C/expressions.o $C/constants.o $C/types.o $C/conversions.o

$B/snowglobe: $B/input.o $B/lexer.o $B/parser.o $B/program.o $(compiler) $B/diagcol.o $B/diags.o $B/main.o | $B $C
	$(LINKER) $^ -o $@

$B/main.o: $S/main.cpp $(input_hpp) $(diagcol_hpp) $(ast_hpp) $(parser_hpp) $(compiler_hpp) $(program_hpp) | $B
	$(CXX) $< -o $@

$B/input.o: $S/input.cpp $(input_hpp) | $B
	$(CXX) $< -o $@

$B/lexer.o: $G/lexer.cpp $(input_hpp) $(diagcol_hpp) $(diags_hpp) $(parser_hpp) $(ast_hpp) | $B
	$(CXX) $< -o $@

$B/parser.o: $G/parser.cpp $(parser_hpp) $(utils_hpp) $(diags_hpp) | $B
	$(CXX) $< -o $@

$B/program.o: $S/program.cpp $(program_hpp) $(utils_hpp) | $B
	$(CXX) $< -o $@

$(compiler): $C/%.o: $S/compiler/%.cpp $(compiler_hpp) $(ast_hpp) $(program_hpp) $(diags_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$B/diagcol.o: $S/diagcol.cpp $(diagcol_hpp) | $B
	$(CXX) $< -o $@

$B/diags.o: $S/diags.cpp $(diags_hpp) | $B
	$(CXX) $< -o $@

$G/lexer.cpp: $S/lexer.re | $G
	$(RE2C) $< -o $@

$G/parser.cpp: $S/parser.y | $G
	$(BISON) $< -o $@

$G:
	mkdir $G

$B:
	mkdir $B

$C: | $B
	mkdir $C

clean:
	rm -rf $G
	rm -rf $B

.PHONY: clean
