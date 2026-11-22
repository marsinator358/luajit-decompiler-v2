enum AST_EXPRESSION : uint8_t {
	AST_EXPRESSION_CONSTANT,
	AST_EXPRESSION_VARARG,
	AST_EXPRESSION_FUNCTION,
	AST_EXPRESSION_VARIABLE,
	AST_EXPRESSION_FUNCTION_CALL,
	AST_EXPRESSION_TABLE,
	AST_EXPRESSION_BINARY_OPERATION,
	AST_EXPRESSION_UNARY_OPERATION,
	AST_EXPRESSION_TERNARY
};

struct Expression {
	Expression(const AST_EXPRESSION& type) : type(type) {
		switch (type) {
		case AST_EXPRESSION_CONSTANT:
			constant = new Constant;
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
		case AST_EXPRESSION_TERNARY:
			ternary = new Ternary;
			break;
		}
	}

	~Expression() {
		switch (type) {
		case AST_EXPRESSION_CONSTANT:
			delete constant;
			break;
		case AST_EXPRESSION_VARIABLE:
			delete variable;
			break;
		case AST_EXPRESSION_FUNCTION_CALL:
			delete functionCall;
			break;
		case AST_EXPRESSION_TABLE:
			delete table;
			break;
		case AST_EXPRESSION_BINARY_OPERATION:
			delete binaryOperation;
			break;
		case AST_EXPRESSION_UNARY_OPERATION:
			delete unaryOperation;
			break;
		case AST_EXPRESSION_TERNARY:
			delete ternary;
			break;
		}
	}

	AST_EXPRESSION type;

	union {
		Constant* constant;
		Function* function;
		Variable* variable;
		FunctionCall* functionCall;
		Table* table;
		BinaryOperation* binaryOperation;
		UnaryOperation* unaryOperation;
		Ternary* ternary;
	};

	bool isMultres = false;
};

enum AST_CONSTANT : uint8_t {
	AST_CONSTANT_NIL,
	AST_CONSTANT_FALSE,
	AST_CONSTANT_TRUE,
	AST_CONSTANT_NUMBER,
	AST_CONSTANT_CDATA_SIGNED,
	AST_CONSTANT_CDATA_UNSIGNED,
	AST_CONSTANT_CDATA_IMAGINARY,
	AST_CONSTANT_STRING
};

struct Constant {
	AST_CONSTANT type;

	union {
		double number;
		int64_t signed_integer;
		uint64_t unsigned_integer;

		struct {
			const std::string* string;
			bool isName;
		};
	};
};

enum AST_VARIABLE : uint8_t {
	AST_VARIABLE_SLOT,
	AST_VARIABLE_SLOT_MULTRES,
	AST_VARIABLE_UPVALUE,
	AST_VARIABLE_GLOBAL,
	AST_VARIABLE_TABLE_INDEX,
	AST_VARIABLE_TABLE_MULTRES,
};

struct Variable {
	AST_VARIABLE type;

	union {
		struct {
			uint8_t slot;
			SlotScope** slotScope;
		};

		const std::string* name;

		struct {
			Expression* table;
			Expression* tableIndex;
			bool safeNavigation;
			bool isEndOfSafeNavChain;
		};
	};
};

struct FunctionCall {
	Expression* function;
	std::vector<Expression*> arguments;
	bool isMethod = false;
	bool safeNavigation = false;
	bool isEndOfSafeNavChain = false;
};

struct Table {
	struct Field {
		Expression* key;
		Expression* value;
	};

	struct {
		std::vector<Expression*> list;
		std::vector<Field> fields;
	} constants;

	std::vector<Field> fields;
};

enum AST_BINARY_OPERATION : uint8_t {
	AST_BINARY_ADDITION,
	AST_BINARY_SUBTRACTION,
	AST_BINARY_MULTIPLICATION,
	AST_BINARY_DIVISION,
	AST_BINARY_EXPONENTATION,
	AST_BINARY_MODULO,
	AST_BINARY_CONCATENATION,
	AST_BINARY_BIT_AND,
	AST_BINARY_BIT_OR,
	AST_BINARY_BIT_EXCLUSIVE_OR,
	AST_BINARY_BIT_LEFT_SHIFT,
	AST_BINARY_BIT_RIGHT_SHIFT,
	AST_BINARY_BIT_ARITHMETIC_RIGHT_SHIFT,
	AST_BINARY_LESS_THAN,
	AST_BINARY_LESS_EQUAL,
	AST_BINARY_GREATER_THEN,
	AST_BINARY_GREATER_EQUAL,
	AST_BINARY_EQUAL,
	AST_BINARY_NOT_EQUAL,
	AST_BINARY_AND,
	AST_BINARY_OR,
	AST_BINARY_NIL_COALESCING
};

struct BinaryOperation {
	AST_BINARY_OPERATION type;
	Expression* leftOperand;
	Expression* rightOperand;
};

enum AST_UNARY_OPERATION : uint8_t {
	AST_UNARY_MINUS,
	AST_UNARY_NOT,
	AST_UNARY_LENGTH,
	AST_UNARY_BIT_NOT
};

struct UnaryOperation {
	AST_UNARY_OPERATION type;
	Expression* operand;
};

struct Ternary {
	Expression* condition;
	Expression* leftOperand;
	Expression* rightOperand;
};

enum AST_STATEMENT : uint8_t {
	AST_STATEMENT_EMPTY,
	AST_STATEMENT_INSTRUCTION,
	AST_STATEMENT_RETURN,
	AST_STATEMENT_CONDITION,
	AST_STATEMENT_GOTO,
	AST_STATEMENT_NUMERIC_FOR,
	AST_STATEMENT_GENERIC_FOR,
	AST_STATEMENT_LOOP,
	AST_STATEMENT_LOCAL,
	AST_STATEMENT_ASSIGNMENT,
	AST_STATEMENT_FUNCTION_CALL,
	AST_STATEMENT_VARARG,
	AST_STATEMENT_IF,
	AST_STATEMENT_ELSE,
	AST_STATEMENT_WHILE,
	AST_STATEMENT_REPEAT,
	AST_STATEMENT_BREAK,
	AST_STATEMENT_CONTINUE,
	AST_STATEMENT_DO,
	AST_STATEMENT_LABEL
};

struct Statement {
	Statement(const AST_STATEMENT& type, Function& function) : type(type), block(function, this) {}
	~Statement() {}

	AST_STATEMENT type;
	size_t id = INVALID_ID;
	const Bytecode::Instruction* instruction = nullptr;
	Label* label = nullptr;
	IdReference* target = nullptr;
	Block block;
	Statement* reference = this;

	struct {
		template <typename... Expressions>
		void register_slots(Expressions*&... expressions) {
			(openSlots.emplace_back(&expressions), ...);
		}

		union {
			Local* locals = nullptr;

			struct {
				bool allowSlotSwap;
				bool swapped;
			};
		};

		CONSTANT_TYPE allowedConstantType = NUMBER_CONSTANT;
		std::vector<Variable> variables;
		std::vector<Expression*> expressions;
		std::vector<Expression**> openSlots;
		std::unordered_set<uint8_t> usedLocals;
	} assignment;
};
