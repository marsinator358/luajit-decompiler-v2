all:
	g++ -funsigned-char -D_CHAR_UNSIGNED -std=c++20 main.cpp lua/lua.cpp ast/ast.cpp bytecode/bytecode.cpp bytecode/prototype.cpp -o decompile

clean:
	rm decompile
