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

typedef std::string Name;
typedef std::vector<Ast::Name> NameList;
typedef std::vector<Ast::Field> FieldList;
typedef std::vector<Ast::Variable> VariableList;
typedef std::vector<Ast::Expression> ExpressionList;
typedef std::vector<Ast::Statement> Block;

enum AST_VARIABLE {
	AST_VARIABLE_NAME,
	AST_VARIABLE_TABLE_INDEX
};

struct Ast::Variable {
	~Variable() {
		delete_type();
	}

	void set_type(const AST_VARIABLE& type) {
		delete_type();
		this->type = type;

		switch (type) {
		case AST_VARIABLE_NAME:
			name = new Name;
			break;
		case AST_VARIABLE_TABLE_INDEX:
			table = new PrefixExpression;
			tableIndex = new Expression;
			break;
		}
	}

	void delete_type() {
		switch (type) {
		case AST_VARIABLE_NAME:
			delete name;
			name = nullptr;
			break;
		case AST_VARIABLE_TABLE_INDEX:
			delete table;
			table = nullptr;
			delete tableIndex;
			tableIndex = nullptr;
			break;
		}
	}

	AST_VARIABLE type = (AST_VARIABLE)-1;

	Name* name = nullptr;
	PrefixExpression* table = nullptr;
	Expression* tableIndex = nullptr;
};

enum AST_PREFIX_EXPRESSION {
	AST_PREFIX_EXPRESSION_VARIABLE,
	AST_PREFIX_EXPRESSION_FUNCTION_CALL,
	AST_PREFIX_EXPRESSION_EXPRESSION
};

struct Ast::PrefixExpression {
	~PrefixExpression() {
		delete_type();
	}

	void set_type(const AST_PREFIX_EXPRESSION& type) {
		delete_type();
		this->type = type;

		switch (type) {
		case AST_PREFIX_EXPRESSION_VARIABLE:
			variable = new Variable;
			break;
		case AST_PREFIX_EXPRESSION_FUNCTION_CALL:
			functionCall = new FunctionCall;
			break;
		case AST_PREFIX_EXPRESSION_EXPRESSION:
			expression = new Expression;
			break;
		}
	}

	void delete_type() {
		switch (type) {
		case AST_PREFIX_EXPRESSION_VARIABLE:
			delete variable;
			variable = nullptr;
			break;
		case AST_PREFIX_EXPRESSION_FUNCTION_CALL:
			delete functionCall;
			functionCall = nullptr;
			break;
		case AST_PREFIX_EXPRESSION_EXPRESSION:
			delete expression;
			expression = nullptr;
			break;
		}
	}

	AST_PREFIX_EXPRESSION type = (AST_PREFIX_EXPRESSION)-1;

	Variable* variable = nullptr;
	FunctionCall* functionCall = nullptr;
	Expression* expression = nullptr;
};

struct Ast::FunctionCall {
	PrefixExpression function;
	ExpressionList arguments;
};

enum AST_EXPRESSION {
	AST_EXPRESSION_NIL,
	AST_EXPRESSION_FALSE,
	AST_EXPRESSION_TRUE,
	AST_EXPRESSION_INTEGER,
	AST_EXPRESSION_NUMBER,
	AST_EXPRESSION_STRING,
	AST_EXPRESSION_VARARG,
	AST_EXPRESSION_FUNCTION,
	AST_EXPRESSION_PREFIX_EXPRESSION,
	AST_EXPRESSION_TABLE,
	AST_EXPRESSION_BINARY_OPERATION,
	AST_EXPRESSION_UNARY_OPERATION
};

struct Ast::Expression {
	~Expression() {
		delete_type();
	}

	void set_type(const AST_EXPRESSION& type) {
		delete_type();
		this->type = type;

		switch (type) {
		case AST_EXPRESSION_INTEGER:
			integer = new int32_t;
			break;
		case AST_EXPRESSION_NUMBER:
			number = new double;
			break;
		case AST_EXPRESSION_STRING:
			string = new std::string;
			break;
		case AST_EXPRESSION_FUNCTION:
			function = new Function;
			break;
		case AST_EXPRESSION_PREFIX_EXPRESSION:
			prefixExpression = new PrefixExpression;
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
		case AST_EXPRESSION_INTEGER:
			delete integer;
			integer = nullptr;
			break;
		case AST_EXPRESSION_NUMBER:
			delete number;
			number = nullptr;
			break;
		case AST_EXPRESSION_STRING:
			delete string;
			string = nullptr;
			break;
		case AST_EXPRESSION_FUNCTION:
			delete function;
			function = nullptr;
			break;
		case AST_EXPRESSION_PREFIX_EXPRESSION:
			delete prefixExpression;
			prefixExpression = nullptr;
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

	AST_EXPRESSION type = (AST_EXPRESSION)-1;

	int32_t* integer = nullptr;
	double* number = nullptr;
	std::string* string = nullptr;
	Function* function = nullptr;
	PrefixExpression* prefixExpression = nullptr;
	Table* table = nullptr;
	BinaryOperation* binaryOperation = nullptr;
	UnaryOperation* unaryOperation = nullptr;
};

struct Ast::Function {
	NameList parameters;
	bool hasVararg = false;
	Block functionStatements;
};

struct Ast::Field {
	bool hasKey = false;
	Expression key;
	Expression value;
};

struct Ast::Table {
	FieldList fields;
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
	AST_BINARY_OPERATION type = (AST_BINARY_OPERATION)-1;
	Expression leftOperand;
	Expression rightOperand;
};

enum AST_UNARY_OPERATION {
	AST_UNARY_MINUS,
	AST_UNARY_NOT,
	AST_UNARY_LENGTH
};

struct Ast::UnaryOperation {
	AST_UNARY_OPERATION type = (AST_UNARY_OPERATION)-1;
	Expression operand;
};

enum AST_STATEMENT {
	AST_STATEMENT_DECLARATION,
	AST_STATEMENT_ASSIGNMENT,
	AST_STATEMENT_FUNCTION_CALL,
	AST_STATEMENT_DO,
	AST_STATEMENT_IF,
	AST_STATEMENT_WHILE,
	AST_STATEMENT_REPEAT,
	AST_STATEMENT_NUMERIC_FOR,
	AST_STATEMENT_GENERIC_FOR,
	AST_STATEMENT_BREAK,
	AST_STATEMENT_RETURN,
	AST_STATEMENT_GOTO,
	AST_STATEMENT_LABEL,
	AST_STATEMENT_LIST
};

struct Ast::Statement {
	struct Declaration {
		NameList variableNames;
		ExpressionList assignments;
	};

	struct Assignment {
		VariableList variables;
		ExpressionList assignments;
	};

	struct FunctionCall {
		PrefixExpression function;
		ExpressionList arguments;
	};

	struct Do {
		Block doStatements;
	};

	struct If {
		Expression ifCondition;
		Block ifStatements;
		std::vector<Expression> elseifConditions;
		std::vector<Block> elseifStatements;
		bool hasElse = false;
		Block elseStatements;
	};

	struct While {
		Expression whileCondition;
		Block whileStatements;
	};

	struct Repeat {
		Block repeatStatements;
		Expression repeatCondition;
	};

	struct NumericFor {
		Name indexName;
		Expression indexExpression, limitExpression, stepExpression;
		Block forStatements;
	};

	struct GenericFor {
		NameList indexNames;
		ExpressionList forExpressions;
		Block forStatements;
	};

	struct Return {
		ExpressionList returnExpressions;
	};

	struct Goto {
		Name labelName;
	};

	struct Label {
		Name name;
	};

	struct List {
		std::vector<Statement> statements;
	};

	~Statement() {
		delete_type();
	}

	void set_type(const AST_STATEMENT& type) {
		delete_type();
		this->type = type;

		switch (type) {
		case AST_STATEMENT_DECLARATION:
			declaration = new Declaration;
			break;
		case AST_STATEMENT_ASSIGNMENT:
			assignment = new Assignment;
			break;
		case AST_STATEMENT_FUNCTION_CALL:
			functionCall = new FunctionCall;
			break;
		case AST_STATEMENT_DO:
			doBlock = new Do;
			break;
		case AST_STATEMENT_IF:
			ifStatement = new If;
			break;
		case AST_STATEMENT_WHILE:
			whileLoop = new While;
			break;
		case AST_STATEMENT_REPEAT:
			repeatLoop = new Repeat;
			break;
		case AST_STATEMENT_NUMERIC_FOR:
			numericFor = new NumericFor;
			break;
		case AST_STATEMENT_GENERIC_FOR:
			genericFor = new GenericFor;
			break;
		case AST_STATEMENT_RETURN:
			returnStatement = new Return;
			break;
		case AST_STATEMENT_GOTO:
			gotoStatement = new Goto;
			break;
		case AST_STATEMENT_LABEL:
			label = new Label;
			break;
		case AST_STATEMENT_LIST:
			statementList = new List;
			break;
		}
	}

	void delete_type() {
		switch (type) {
		case AST_STATEMENT_DECLARATION:
			delete declaration;
			declaration = nullptr;
			break;
		case AST_STATEMENT_ASSIGNMENT:
			delete assignment;
			assignment = nullptr;
			break;
		case AST_STATEMENT_FUNCTION_CALL:
			delete functionCall;
			functionCall = nullptr;
			break;
		case AST_STATEMENT_DO:
			delete doBlock;
			doBlock = nullptr;
			break;
		case AST_STATEMENT_IF:
			delete ifStatement;
			ifStatement = nullptr;
			break;
		case AST_STATEMENT_WHILE:
			delete whileLoop;
			whileLoop = nullptr;
			break;
		case AST_STATEMENT_REPEAT:
			delete repeatLoop;
			repeatLoop = nullptr;
			break;
		case AST_STATEMENT_NUMERIC_FOR:
			delete numericFor;
			numericFor = nullptr;
			break;
		case AST_STATEMENT_GENERIC_FOR:
			delete genericFor;
			genericFor = nullptr;
			break;
		case AST_STATEMENT_RETURN:
			delete returnStatement;
			returnStatement = nullptr;
			break;
		case AST_STATEMENT_GOTO:
			delete gotoStatement;
			gotoStatement = nullptr;
			break;
		case AST_STATEMENT_LABEL:
			delete label;
			label = nullptr;
			break;
		case AST_STATEMENT_LIST:
			delete statementList;
			statementList = nullptr;
			break;
		}
	}

	AST_STATEMENT type = (AST_STATEMENT)-1;
	uint32_t lineOffset = 0;

	Declaration* declaration = nullptr;
	Assignment* assignment = nullptr;
	FunctionCall* functionCall = nullptr;
	Do* doBlock = nullptr;
	If* ifStatement = nullptr;
	While* whileLoop = nullptr;
	Repeat* repeatLoop = nullptr;
	NumericFor* numericFor = nullptr;
	GenericFor* genericFor = nullptr;
	Return* returnStatement = nullptr;
	Goto* gotoStatement = nullptr;
	Label* label = nullptr;
	List* statementList = nullptr;
};
