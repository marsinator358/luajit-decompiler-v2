class Ast {
public:

	struct BinaryOperation;
	struct Expression;
	struct Field;
	struct Function;
	struct FunctionCall;
	struct PrefixExpression;
	struct Statement;
	struct Table;
	struct UnaryOperation;
	struct Variable;
	#include "building_blocks.h"

	Ast(const Bytecode& bytecode);

	void build();

	Block chunk;

private:

	class SimpleAst;
	#include "simple_ast.h"

	const Bytecode& bytecode;
};
