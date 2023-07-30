class Ast {
private:

	static constexpr uint32_t INVALID_ID = -1;

	enum ConstantType {
		INVALID_CONSTANT,
		NIL_CONSTANT,
		BOOL_CONSTANT,
		NUMBER_CONSTANT
	};

	struct Expression;
	struct Constant;
	struct Variable;
	struct FunctionCall;
	struct Table;
	struct BinaryOperation;
	struct UnaryOperation;
	struct Statement;
	struct Local;
	struct SlotScope;
	struct Function;
	#include "building_blocks.h"

public:

	Ast(const Bytecode& bytecode);
	~Ast();

	void operator()();

	Function* chunk = nullptr;

private:
	
	Function*& new_function(const Bytecode::Prototype& prototype);
	Statement*& new_statement(const AST_STATEMENT& type);
	Expression*& new_expression(const AST_EXPRESSION& type);
	void build_functions(Function& function);
	void build_instructions(Function& function);
	void assign_debug_info(Function& function);
	void group_jumps(Function& function);
	void build_loops(Function& function);
	void build_local_scopes(Function& function, std::vector<Statement*>& block);
	void build_expressions(Function& function, std::vector<Statement*>& block);
	void collect_slot_scopes(Function& function, std::vector<Statement*>& block);
	void eliminate_slots(Function& function, std::vector<Statement*>& block);
	void build_conditions(Function& function, std::vector<Statement*>& block);
	Expression* new_slot(const uint8_t& slot);
	Expression* new_literal(const uint8_t& literal);
	Expression* new_signed_literal(const uint16_t& signedLiteral);
	Expression* new_primitive(const uint16_t& primitive);
	Expression* new_number(const Function& function, const uint16_t& index);
	Expression* new_string(const Function& function, const uint16_t& index);
	Expression* new_table(const Function& function, const uint16_t& index);
	Expression* new_cdata(const Function& function, const uint16_t& index);

	static uint32_t get_block_index_from_id(const std::vector<Statement*>& block, const uint32_t& id);
	static uint32_t get_extended_id_from_statement(Statement* const& statement);
	static void check_valid_name(Constant* const& constant);
	void check_special_number(Expression* const& expression, const bool& convertInfinity);
	static ConstantType get_constant_type(Expression* const& expression);

	const Bytecode& bytecode;
	bool isFR2Enabled = false;
	std::vector<Statement*> statements;
	std::vector<Function*> functions;
	std::vector<Expression*> expressions;
	uint32_t functionsComplete = 0;
};
