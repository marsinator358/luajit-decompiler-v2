class Ast {
public:

	static constexpr size_t INVALID_ID = -1;

	enum CONSTANT_TYPE : uint8_t {
		INVALID_CONSTANT,
		NIL_CONSTANT,
		BOOL_CONSTANT,
		NUMBER_CONSTANT
	};

	class Function;
	class Block;
	class IdReference;

	struct Local;
	struct Upvalue;
	struct SlotInfo;
	struct SlotScope;
	struct Label;

	struct Statement;
	struct Expression;
	struct Constant;
	struct Variable;
	struct FunctionCall;
	struct Table;
	struct BinaryOperation;
	struct UnaryOperation;
	struct Ternary;

	#include "block.h"
	#include "id_reference.h"
	#include "function.h"
	#include "building_blocks.h"
	#include "condition_decompiler.h"

	Ast(const Bytecode& bytecode);
	~Ast();

	void operator()();

	Function chunk;

private:

	Function*& new_function(const Bytecode::Prototype& prototype);
	Statement*& new_statement(const AST_STATEMENT& type, Function& function);
	Expression*& new_expression(const AST_EXPRESSION& type);
	void build_function(Function& function, uint32_t& functionCounter, const uint32_t& level);
	void copy_instructions(Function& function);
	void prepare_debug_info(Function& function);
	void register_jumps(Function& function);
	void build_loops(Function& function);
	size_t build_local_scopes(Function& function, Block& block, const size_t& initialLocalIndex = INVALID_ID);
	void build_expressions(Function& function, Block& block);
	void build_slot_scopes(Function& function, Block& block, const size_t& begin = 0, const size_t& end = INVALID_ID);
	void eliminate_slots(Function& function, Block& block);
	Expression* new_slot(const uint8_t& slot);
	Expression* new_literal(const uint8_t& literal);
	Expression* new_signed_literal(const uint16_t& signedLiteral);
	Expression* new_primitive(const uint8_t& primitive);
	Expression* new_number(const Function& function, const uint16_t& index);
	Expression* new_string(const Function& function, const uint16_t& index);
	Expression* new_table(const Function& function, const uint16_t& index);
	Expression* new_cdata(const Function& function, const uint16_t& index);

	static size_t get_extended_id_from_statement(Statement& statement);
	static void check_valid_name(Expression* const& expression);
	void check_special_number(Expression*& expression);
	static CONSTANT_TYPE get_constant_type(Expression* const& expression);

	const Bytecode& bytecode;
	std::vector<Function*> functions;
	std::vector<Statement*> statements;
	std::vector<Expression*> expressions;
};
