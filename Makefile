build/snowglobe: src/main.cpp src/input.hpp src/input.cpp gen/lexer.cpp gen/parser.cpp
	mkdir -p build
	c++ -std=c++17 src/main.cpp src/input.cpp gen/lexer.cpp gen/parser.cpp -I src -I gen -o build/snowglobe

gen/lexer.cpp: src/lexer.re
	mkdir -p gen
	re2c src/lexer.re -o gen/lexer.cpp

gen/parser.cpp: src/parser.y
	mkdir -p gen
	bison src/parser.y -o gen/parser.cpp -H
