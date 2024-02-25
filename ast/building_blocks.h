enum AST_EXPRESSION {
	AST_EXPRESSION_CONSTANT,
	AST_EXPRESSION_VARARG,
	AST_EXPRESSION_FUNCTION,
	AST_EXPRESSION_VARIABLE,
	AST_EXPRESSION_FUNCTION_CALL,
	AST_EXPRESSION_TABLE,
	AST_EXPRESSION_BINARY_OPERATION,
	AST_EXPRESSION_UNARY_OPERATION
};

struct Ast::Expression {
	Expression(const AST_EXPRESSION& type) {
		set_type(type);
	}

	~Expression() {
		delete_type();
	}

	void set_type(const AST_EXPRESSION& type) {
		delete_type();
		this->type = type;

		switch (type) {
		case AST_EXPRESSION_CONSTANT:
			constant = new Constant;
			break;
		case AST_EXPRESSION_VARARG:
			returnCount = 0;
			break;
		case AST_EXPRESSION_VARIABLE:
			variable = new Variable;
			break;
		case AST_EXPRESSION_FUNCTION_CALL:
			functionCall = new FunctionCall;
			break;
		case AST_EXPRESSION_TABLE:
			table = new Table;
			break;
		case AST_EXPRESSION_BINARY_OPERATION:
			binaryOperation = new BinaryOperation;
			break;
		case AST_EXPRESSION_UNARY_OPERATION:
			unaryOperation = new UnaryOperation;
			break;
		}
	}

	void delete_type() {
		switch (type) {
		case AST_EXPRESSION_CONSTANT:
			delete constant;
			constant = nullptr;
			break;
		case AST_EXPRESSION_FUNCTION:
			function = nullptr;
			break;
		case AST_EXPRESSION_VARIABLE:
			delete variable;
			variable = nullptr;
			break;
		case AST_EXPRESSION_FUNCTION_CALL:
			delete functionCall;
			functionCall = nullptr;
			break;
		case AST_EXPRESSION_TABLE:
			delete table;
			table = nullptr;
			break;
		case AST_EXPRESSION_BINARY_OPERATION:
			delete binaryOperation;
			binaryOperation = nullptr;
			break;
		case AST_EXPRESSION_UNARY_OPERATION:
			delete unaryOperation;
			unaryOperation = nullptr;
			break;
		}
	}

	AST_EXPRESSION type;

	union {
		Constant* constant = nullptr;
		Function* function;
		Variable* variable;
		FunctionCall* functionCall;
		Table* table;
		BinaryOperation* binaryOperation;
		UnaryOperation* unaryOperation;
		uint8_t returnCount;
	};
};

enum AST_CONSTANT {
	AST_CONSTANT_NIL,
	AST_CONSTANT_FALSE,
	AST_CONSTANT_TRUE,
	AST_CONSTANT_NUMBER,
	AST_CONSTANT_CDATA_SIGNED,
	AST_CONSTANT_CDATA_UNSIGNED,
	AST_CONSTANT_CDATA_IMAGINARY,
	AST_CONSTANT_STRING
};

struct Ast::Constant {
	AST_CONSTANT type;

	union {
		double number;
		int64_t signed_integer;
		uint64_t unsigned_integer = 0;
	};

	std::string string;
	bool isName = false;
};

enum AST_VARIABLE {
	AST_VARIABLE_SLOT,
	AST_VARIABLE_UPVALUE,
	AST_VARIABLE_GLOBAL,
	AST_VARIABLE_TABLE_INDEX
};

struct Ast::Variable {
	AST_VARIABLE type;
	uint8_t slot = 0;
	SlotScope** slotScope = nullptr;
	std::string name;
	Expression* table = nullptr;
	Expression* tableIndex = nullptr;
	bool isMultres = false;
	uint32_t multresIndex = 0;
};

struct Ast::FunctionCall {
	Expression* function = nullptr;
	std::vector<Expression*> arguments;
	Expression* multresArgument = nullptr;
	bool isMethod = false;
	uint8_t returnCount = 0;
};

struct Ast::Table {
	struct Field {
		Expression* key = nullptr;
		Expression* value = nullptr;
	};

	struct {
		std::vector<Expression*> list;
		std::vector<Field> fields;
	} constants;

	std::vector<Field> fields;
	uint32_t multresIndex = 0;
	Expression* multresField = nullptr;
};

enum AST_BINARY_OPERATION {
	AST_BINARY_ADDITION,
	AST_BINARY_SUBTRACTION,
	AST_BINARY_MULTIPLICATION,
	AST_BINARY_DIVISION,
	AST_BINARY_EXPONENTATION,
	AST_BINARY_MODULO,
	AST_BINARY_CONCATENATION,
	AST_BINARY_LESS_THAN,
	AST_BINARY_LESS_EQUAL,
	AST_BINARY_GREATER_THEN,
	AST_BINARY_GREATER_EQUAL,
	AST_BINARY_EQUAL,
	AST_BINARY_NOT_EQUAL,
	AST_BINARY_AND,
	AST_BINARY_OR
};

struct Ast::BinaryOperation {
	AST_BINARY_OPERATION type;
	Expression* leftOperand = nullptr;
	Expression* rightOperand = nullptr;
};

enum AST_UNARY_OPERATION {
	AST_UNARY_MINUS,
	AST_UNARY_NOT,
	AST_UNARY_LENGTH
};

struct Ast::UnaryOperation {
	AST_UNARY_OPERATION type;
	Expression* operand = nullptr;
};

enum AST_STATEMENT {
	AST_STATEMENT_EMPTY,
	AST_STATEMENT_INSTRUCTION,
	AST_STATEMENT_RETURN,
	AST_STATEMENT_CONDITION,
	AST_STATEMENT_GOTO,
	AST_STATEMENT_NUMERIC_FOR,
	AST_STATEMENT_GENERIC_FOR,
	AST_STATEMENT_LOOP,
	AST_STATEMENT_BREAK,
	AST_STATEMENT_DECLARATION,
	AST_STATEMENT_ASSIGNMENT,
	AST_STATEMENT_FUNCTION_CALL,
	AST_STATEMENT_IF,
	AST_STATEMENT_ELSE,
	AST_STATEMENT_WHILE,
	AST_STATEMENT_REPEAT,
	AST_STATEMENT_DO,
	AST_STATEMENT_LABEL
};

struct Ast::Statement {
	Statement(const AST_STATEMENT& type) : type(type) {}

	AST_STATEMENT type;

	struct {
		Bytecode::BC_OP type = Bytecode::BC_OP_INVALID;
		uint8_t a = 0;
		uint8_t b = 0;
		uint8_t c = 0;
		uint16_t d = 0;
		uint32_t id = INVALID_ID;
		uint32_t target = INVALID_ID;
		uint32_t label = INVALID_ID;
	} instruction;

	Function* function = nullptr;
	std::vector<Statement*> block;
	Local* locals = nullptr;

	struct {
		bool allowSlotSwap = false;
		bool swapped = false;
	} condition;

	struct {
		void register_slots(Expression*& expression) {
			openSlots.emplace_back(&expression);
		}

		template <typename... Expressions>
		void register_slots(Expression*& expression, Expressions*&... expressions) {
			openSlots.emplace_back(&expression);
			return register_slots(expressions...);
		}

		bool isPotentialMethod = false;
		bool isTableConstructor = false;
		bool forwardDeclaration = false;
		CONSTANT_TYPE allowedConstantType = NUMBER_CONSTANT;
		std::vector<Variable> variables;
		std::vector<Expression*> expressions;
		std::vector<Expression**> openSlots;
		Expression* multresReturn = nullptr;
	} assignment;
};
