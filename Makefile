CXX = c++ -std=c++17
CXXC = $(CXX) -c -I $I -I $G
RE2C = re2c
BISON = bison

S = src
I = include
B = build
G = gen

location_hpp = $I/location.hpp
input_hpp = $I/input.hpp $(location_hpp)
ast_hpp = $I/ast.hpp $(location_hpp)
program_hpp = $I/program.hpp
utils_hpp = $I/utils.hpp
diagnostics_hpp = $I/diagnostics.hpp $(location_hpp)
compiler_hpp = $I/compiler.hpp $(ast_hpp) $(program_hpp) $(diagnostics_hpp)
parser_hpp = $G/parser.cpp $(location_hpp) $(input_hpp) $(diagnostics_hpp) $(ast_hpp)

compiler = $B/compiler.o $B/variables.o $B/functions.o $B/structs.o $B/enums.o $B/constants.o $B/types.o $B/subtyping.o

$B/snowglobe: $B/main.o $B/input.o $B/lexer.o $B/parser.o $B/diagnostics.o $B/program.o $(compiler) | $B
	$(CXX) $^ -o $@

$B/main.o: $S/main.cpp $(input_hpp) $(diagnostics_hpp) $(ast_hpp) $(parser_hpp) $(compiler_hpp) $(program_hpp) | $B
	$(CXXC) $< -o $@

$B/input.o: $S/input.cpp $(input_hpp) | $B
	$(CXXC) $< -o $@

$B/lexer.o: $G/lexer.cpp $(input_hpp) $(diagnostics_hpp) $(parser_hpp) $(ast_hpp) | $B
	$(CXXC) $< -o $@

$B/parser.o: $G/parser.cpp $(parser_hpp) $(utils_hpp) $(location_hpp) | $B
	$(CXXC) $< -o $@

$B/diagnostics.o: $S/diagnostics.cpp $(diagnostics_hpp) | $B
	$(CXXC) $< -o $@

$B/program.o: $S/program.cpp $(program_hpp) $(utils_hpp) | $B
	$(CXXC) $< -o $@

$(compiler): $B/%.o: $S/compiler/%.cpp $(compiler_hpp) $(ast_hpp) $(program_hpp) $(diagnostics_hpp) $(utils_hpp) | $B
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
