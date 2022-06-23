CXX = c++ -std=c++17 -c -Os -Wall -Wextra -I $I -I $G
LINKER = c++ -std=c++17 -s
LLVM_LINKER_FLAGS = $(shell llvm-config --libs)
LLVM_CXX_FLAGS = -I $(shell llvm-config --includedir)
RE2C = re2c
BISON = bison

S = src
T = src/compiler
I = include
J = include/compiler
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
parser_hpp = $G/parser.cpp $(location_hpp) $(input_hpp) $(diagcol_hpp) $(ast_hpp)

compiler_hpp = $J/compiler.hpp $(ast_hpp) $(program_hpp) $(diagcol_hpp)
compiler_utils_hpp = $J/utils.hpp $(compiler_hpp)
types_hpp = $J/types.hpp $(compiler_hpp)
constants_hpp = $J/constants.hpp $(compiler_hpp)
functions_hpp = $J/functions.hpp $(compiler_hpp)
conversions_hpp = $J/conversions.hpp $(functions_hpp)
copying_hpp = $J/copying.hpp $(functions_hpp)
deletion_hpp = $J/deletion.hpp $(functions_hpp)
assignment_hpp = $J/assignment.hpp $(functions_hpp)
statements_hpp = $J/statements.hpp $(functions_hpp)
expressions_hpp = $J/expressions.hpp $(functions_hpp)

diags_hpp = $I/diags.hpp $(diagcol_hpp) $(ast_hpp) $(program_hpp) $(compiler_hpp)
codegen_hpp = $I/codegen.hpp $(program_hpp) $(diagcol_hpp)

compiler = $C/compiler.o $C/utils.o $C/types.o $C/constants.o $C/functions.o $C/conversions.o $C/copying.o $C/deletion.o $C/assignment.o $C/statements.o $C/expressions.o
codegen = $(CG)/codegen.o $(CG)/types.o $(CG)/external.o $(CG)/builtin.o

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

$C/compiler.o: $T/compiler.cpp $(compiler_hpp) $(types_hpp) $(constants_hpp) $(functions_hpp) $(conversions_hpp) $(compiler_utils_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/utils.o: $T/utils.cpp $(compiler_utils_hpp) $(functions_hpp) $(conversions_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/types.o: $T/types.cpp $(types_hpp) $(constants_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/constants.o: $T/constants.cpp $(constants_hpp) $(expressions_hpp) $(compiler_utils_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/functions.o: $T/functions.cpp $(functions_hpp) $(statements_hpp) $(expressions_hpp) $(copying_hpp) $(deletion_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/conversions.o: $T/conversions.cpp $(conversions_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/copying.o: $T/copying.cpp $(copying_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/deletion.o: $T/deletion.cpp $(deletion_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/assignment.o: $T/assignment.cpp $(assignment_hpp) $(conversions_hpp) $(deletion_hpp) $(compiler_utils_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/statements.o: $T/statements.cpp $(statements_hpp) $(expressions_hpp) $(conversions_hpp) $(assignment_hpp) $(deletion_hpp) $(compiler_utils_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

$C/expressions.o: $T/expressions.cpp $(expressions_hpp) $(types_hpp) $(constants_hpp) $(constants_hpp) $(conversions_hpp) $(copying_hpp) $(deletion_hpp) $(compiler_utils_hpp) $(diags_hpp) $(program_hpp) $(utils_hpp) | $C
	$(CXX) $< -o $@

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
