#include "..\main.h"

Ast::Ast(const Bytecode& bytecode) : bytecode(bytecode) {}

void Ast::build() {
	SimpleAst simpleAst(bytecode);
	simpleAst.build();
}
