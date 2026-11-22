class ConditionDecompiler {
public:

	struct Node {
		enum TYPE : uint8_t {
			LESS_THAN,
			LESS_EQUAL,
			GREATER_THEN,
			GREATER_EQUAL,
			NOT_LESS_THAN,
			NOT_LESS_EQUAL,
			NOT_GREATER_THEN,
			NOT_GREATER_EQUAL,
			EQUAL,
			NOT_EQUAL,
			TRUTHY_TEST,
			FALSY_TEST,
			BOOL_TRUTHY_TEST,
			BOOL_FALSY_TEST,
			UNCONDITIONAL_TRUE,
			UNCONDITIONAL_FALSE,
			AND,
			OR,
			NOT_AND,
			NOT_OR,
			END_TARGET,
			TRUE_TARGET,
			FALSE_TARGET,
		} type;

		static constexpr uint8_t TYPE_PREFERENCES[END_TARGET][2] = {
			{ 3, 1 },
			{ 3, 1 },
			{ 3, 1 },
			{ 3, 1 },
			{ 1, 3 },
			{ 1, 3 },
			{ 1, 3 },
			{ 1, 3 },
			{ 3, 3 },
			{ 3, 3 },
			{ 3, 2 },
			{ 2, 3 },
			{},
			{},
			{ 3, 3 },
			{ 3, 3 },
			{ 3, 0 },
			{ 3, 0 },
			{ 0, 3 },
			{ 0, 3 }
		};

		Node(const TYPE& type) : type(type) {}

		bool inverted = false;
		Label* label = nullptr;
		Label* targetLabel = nullptr;
		Node* targetNode = nullptr;
		bool hasAlternateTarget = false;
		uint32_t incomingNodes = 0;
		const std::vector<Expression*>* expressions = nullptr;
		Node* leftNode = nullptr;
		Node* rightNode = nullptr;
	};

	ConditionDecompiler(Ast* ast, Label* const& endTargetLabel, Label* const& trueTargetLabel, Label* const& falseTargetLabel, Variable* const& targetSlot = nullptr);
	ConditionDecompiler(Ast* ast, Label* const& trueTargetLabel, Label* const& falseTargetLabel);
	~ConditionDecompiler();

	Expression* operator()();
	void add_node(const Node::TYPE& type, Label* const& label, Label* const& targetLabel, const std::vector<Expression*>& expressions, const bool& hasAlternateTarget = false);

	static Node::TYPE get_node_type(const Statement& statement);

	std::vector<Node*> conditionNodes;

private:

	enum MODE : uint8_t {
		ASSIGNMENT,
		STATEMENT
	} const mode;

	Node*& new_node(const Node::TYPE& type);
	bool link_nodes();
	void fix_return_nodes();
	bool decompile_condition();
	void merge_nodes(Node* const& leftNode, Node* const& rightNode);
	Node* copy_node(Node* const& node);
	Expression* build_expression(Node* const& node);
	Expression* build_not(Expression* const& operand);
	Expression* build_binary(const Node::TYPE& type, Expression* const& leftOperand, Expression* const& rightOperand);

	static void invert_node(Node* const& node);
	static bool invert_any_node(Node* const& leftNode, Node* const& rightNode);

	Ast& ast;
	Variable* const targetSlot; //TODO
	Node endTarget;
	Node trueTarget;
	Node falseTarget;
	Node* endAssignment;
	std::vector<Node*> nodes;
};
