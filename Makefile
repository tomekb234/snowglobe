CXX = c++ -std=c++17 -c -Os -Wall -Wextra -I $I -I $G
LINKER = c++ -std=c++17 -s
LLVM_LINKER_FLAGS = $(shell llvm-config --libs)
LLVM_CXX_FLAGS = -isystem $(shell llvm-config --includedir)
RE2C = re2c
BISON = bison

S = src
I = include
B = build
C = build/compiler
CG = build/codegen
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
codegen_hpp = $I/codegen.hpp $(program_hpp) $(diagcol_hpp)

compiler = $C/compiler.o $C/globals.o $C/functions.o $C/statements.o $C/expressions.o $C/constants.o $C/types.o $C/conversions.o $C/copying.o $C/deleting.o
codegen = $(CG)/codegen.o $(CG)/types.o

$B/snowglobe: $B/input.o $B/lexer.o $B/parser.o $B/program.o $(compiler) $B/diagcol.o $B/diags.o $(codegen) $B/main.o | $B $C
	$(LINKER) $^ $(LLVM_LINKER_FLAGS) -o $@

$B/main.o: $S/main.cpp $(input_hpp) $(diagcol_hpp) $(ast_hpp) $(parser_hpp) $(compiler_hpp) $(program_hpp) $(codegen_hpp) | $B
	$(CXX) $(LLVM_CXX_FLAGS) $< -o $@

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

$(codegen): $(CG)/%.o: $S/codegen/%.cpp $(codegen_hpp) $(diags_hpp) $(utils_hpp) | $(CG)
	$(CXX) $(LLVM_CXX_FLAGS) $< -o $@

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

$(CG): | $B
	mkdir $(CG)

clean:
	rm -rf $G
	rm -rf $B

.PHONY: clean
