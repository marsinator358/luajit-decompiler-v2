class Ast {
private:

	static constexpr uint32_t INVALID_ID = -1;

	enum CONSTANT_TYPE {
		INVALID_CONSTANT,
		NIL_CONSTANT,
		BOOL_CONSTANT,
		NUMBER_CONSTANT
	};

public:
	struct Local;
	struct SlotScope;
	struct Expression;
	struct Constant;
	struct Variable;
	struct FunctionCall;
	struct Table;
	struct BinaryOperation;
	struct UnaryOperation;
	struct Statement;
	struct Function;
	#include "building_blocks.h"
	#include "function.h"

	Ast(const Bytecode& bytecode, const bool& ignoreDebugInfo, const bool& minimizeDiffs);
	~Ast();

	void operator()();

	Function* chunk = nullptr;

private:

	struct ConditionBuilder;
	#include "conditionBuilder.h"

	struct BlockInfo {
		uint32_t index = INVALID_ID;
		std::vector<Statement*>& block;
		BlockInfo* const previousBlock;
	};

	Function*& new_function(const Bytecode::Prototype& prototype, const uint32_t& level);
	Statement*& new_statement(const AST_STATEMENT& type);
	Expression*& new_expression(const AST_EXPRESSION& type);
	void build_functions(Function& function, uint32_t& functionCounter);
	void build_instructions(Function& function);
	void assign_debug_info(Function& function);
	void group_jumps(Function& function);
	void build_loops(Function& function);
	void build_local_scopes(Function& function, std::vector<Statement*>& block);
	void build_expressions(Function& function, std::vector<Statement*>& block);
	void build_slot_scopes(Function& function, std::vector<Statement*>& block, BlockInfo* const& previousBlock);
	void eliminate_slots(Function& function, std::vector<Statement*>& block, BlockInfo* const& previousBlock);
	void eliminate_conditions(Function& function, std::vector<Statement*>& block, BlockInfo* const& previousBlock);
	void build_multi_assignment(Function& function, std::vector<Statement*>& block);
	void build_if_statements_from_map(Function& function, std::vector<Statement*>& block, BlockInfo* const& previousBlock, std::unordered_map<Statement*, uint32_t>& offsetMap);
	void build_if_statements(Function& function, std::vector<Statement*>& block, BlockInfo* const& previousBlock);
	void clean_up(Function& function);
	void clean_up_block(Function& function, std::vector<Statement*>& block, uint32_t& variableCounter, uint32_t& iteratorCounter, BlockInfo* const& previousBlock);
	Expression* new_slot(const uint8_t& slot);
	Expression* new_literal(const uint8_t& literal);
	Expression* new_signed_literal(const uint16_t& signedLiteral);
	Expression* new_primitive(const uint8_t& primitive);
	Expression* new_number(const Function& function, const uint16_t& index);
	Expression* new_string(const Function& function, const uint16_t& index);
	Expression* new_table(const Function& function, const uint16_t& index);
	Expression* new_cdata(const Function& function, const uint16_t& index);

	static uint32_t get_block_index_from_id(const std::vector<Statement*>& block, const uint32_t& id);
	static uint32_t get_extended_id_from_statement(Statement* const& statement);
	static uint32_t get_label_from_next_statement(Function& function, const BlockInfo& blockInfo, const bool& returnExtendedLabel, const bool& excludeDeclaration);
	static bool is_valid_block(Function& function, const BlockInfo& blockInfo, const uint32_t& blockBegin);
	static void check_valid_name(Constant* const& constant);
	void check_special_number(Expression* const& expression, const bool& isCdata = false);
	static CONSTANT_TYPE get_constant_type(Expression* const& expression);

	const Bytecode& bytecode;
	const bool ignoreDebugInfo;
	const bool minimizeDiffs;
	bool isFR2Enabled = false;
	std::vector<Statement*> statements;
	std::vector<Function*> functions;
	std::vector<Expression*> expressions;
	uint64_t prototypeDataLeft = 0;
};
