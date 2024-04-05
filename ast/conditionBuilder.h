struct Ast::ConditionBuilder {
	enum TYPE {
		ASSIGNMENT,
		STATEMENT
	} const type;

	struct Node {
		enum TYPE {
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
			UNCONDITIONAL,
			AND,
			OR,
			NOT_AND,
			NOT_OR,
			END_TARGET,
			TRUE_TARGET,
			FALSE_TARGET,
		} type;

		static constexpr uint8_t TYPE_PREFERENCE[TYPE::END_TARGET][2] = {
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
			{ 0, 0 },
			{ 0, 0 },
			{ 3, 3 },
			{ 3, 0 },
			{ 3, 0 },
			{ 0, 3 },
			{ 0, 3 }
		};

		Node(const TYPE& type) : type(type) {}

		uint32_t nodeLabel = INVALID_ID;
		uint32_t targetLabel = INVALID_ID;
		Node* targetNode = nullptr;
		uint32_t incomingNodes = 0;
		bool inverted = false;
		std::vector<Expression*>* expressions = nullptr;
		Node* leftNode = nullptr;
		Node* rightNode = nullptr;
	};

	ConditionBuilder(const TYPE& type, Ast& ast, const uint32_t& endTargetLabel, const uint32_t& trueTargetLabel, const uint32_t& falseTargetLabel) : ast(ast), type(type) {
		if (type == ASSIGNMENT) {
			endTarget = new_node(Node::END_TARGET);
			endTarget->nodeLabel = endTargetLabel;
		}

		trueTarget = new_node(Node::TRUE_TARGET);
		trueTarget->nodeLabel = trueTargetLabel;
		falseTarget = new_node(Node::FALSE_TARGET);
		falseTarget->nodeLabel = falseTargetLabel;
	}

	~ConditionBuilder() {
		for (uint32_t i = nodes.size(); i--;) {
			delete nodes[i];
		}
	}

	Node*& new_node(const Node::TYPE& type) {
		return nodes.emplace_back(new Node(type));
	}

	static Node::TYPE get_node_type(const Bytecode::BC_OP& instruction, const bool& swapped) {
		switch (instruction) {
		case Bytecode::BC_OP_ISLT:
			return swapped ? Node::GREATER_THEN : Node::LESS_THAN;
		case Bytecode::BC_OP_ISGE:
			return swapped ? Node::NOT_GREATER_THEN : Node::NOT_LESS_THAN;
		case Bytecode::BC_OP_ISLE:
			return swapped ? Node::GREATER_EQUAL : Node::LESS_EQUAL;
		case Bytecode::BC_OP_ISGT:
			return swapped ? Node::NOT_GREATER_EQUAL : Node::NOT_LESS_EQUAL;
		case Bytecode::BC_OP_ISEQV:
		case Bytecode::BC_OP_ISEQS:
		case Bytecode::BC_OP_ISEQN:
		case Bytecode::BC_OP_ISEQP:
			return Node::EQUAL;
		case Bytecode::BC_OP_ISNEV:
		case Bytecode::BC_OP_ISNES:
		case Bytecode::BC_OP_ISNEN:
		case Bytecode::BC_OP_ISNEP:
			return Node::NOT_EQUAL;
		case Bytecode::BC_OP_ISTC:
		case Bytecode::BC_OP_IST:
			return Node::TRUTHY_TEST;
		case Bytecode::BC_OP_ISFC:
		case Bytecode::BC_OP_ISF:
			return Node::FALSY_TEST;
		case Bytecode::BC_OP_JMP:
			return Node::UNCONDITIONAL;
		default:
			throw nullptr;
		}
	}

	void add_node(const Node::TYPE& type, const uint32_t& nodeLabel, const uint32_t& targetLabel, std::vector<Expression*>* const& expressions) {
		conditionNodes.emplace_back(new_node(type));
		conditionNodes.back()->nodeLabel = nodeLabel;
		conditionNodes.back()->targetLabel = targetLabel;
		conditionNodes.back()->expressions = expressions;
	}

	bool link_nodes() {
		switch (type) {
		case ASSIGNMENT:
			conditionNodes.emplace_back(falseTarget);
			conditionNodes.emplace_back(trueTarget);
			conditionNodes.emplace_back(endTarget);
			break;
		case STATEMENT:
			conditionNodes.emplace_back(trueTarget);
			conditionNodes.emplace_back(falseTarget);
			break;
		}

		for (uint32_t i = conditionNodes.size(); i--;) {
			if (conditionNodes[i]->targetLabel == INVALID_ID) continue;

			for (uint32_t j = conditionNodes.size(); j--;) {
				if (conditionNodes[i]->targetLabel != conditionNodes[j]->nodeLabel) continue;
				conditionNodes[i]->targetNode = conditionNodes[j];
				conditionNodes[j]->incomingNodes++;
				break;
			}

			if (!conditionNodes[i]->targetNode) return false;
		}

		conditionNodes.pop_back();
		if (type == ASSIGNMENT) conditionNodes.pop_back();
		return true;
	}

	void fix_return_nodes() {
		switch (type) {
		case ASSIGNMENT:
			for (uint32_t i = conditionNodes.size() - 1; i--;) {
				switch (conditionNodes[i]->targetNode->type) {
				case Node::END_TARGET:
					conditionNodes[i]->targetNode = conditionNodes[i]->type == Node::TRUTHY_TEST ? trueTarget : falseTarget;
					conditionNodes[i]->targetNode->incomingNodes++;
					conditionNodes[i]->inverted = conditionNodes[i]->type == Node::FALSY_TEST;
					continue;
				case Node::TRUE_TARGET:
					if (conditionNodes[i]->type == Node::TRUTHY_TEST) conditionNodes[i]->type = Node::BOOL_TRUTHY_TEST;
					continue;
				case Node::FALSE_TARGET:
					if (conditionNodes[i]->type == Node::FALSY_TEST) conditionNodes[i]->type = Node::BOOL_FALSY_TEST;
					conditionNodes[i]->inverted = true;
					continue;
				}
			}

			break;
		case STATEMENT:
			for (uint32_t i = conditionNodes.size() - 1; i--;) {
				if (conditionNodes[i]->targetNode->type == Node::FALSE_TARGET) conditionNodes[i]->inverted = true;
			}

			if (conditionNodes[conditionNodes.size() - 2]->inverted) break;
			conditionNodes[conditionNodes.size() - 2]->leftNode = copy_node(conditionNodes[conditionNodes.size() - 2]);
			conditionNodes[conditionNodes.size() - 2]->rightNode = new_node(Node::UNCONDITIONAL);
			conditionNodes[conditionNodes.size() - 2]->targetNode->incomingNodes--;
			conditionNodes[conditionNodes.size() - 2]->targetNode = falseTarget;
			conditionNodes[conditionNodes.size() - 2]->targetNode->incomingNodes++;
			conditionNodes[conditionNodes.size() - 2]->type = Node::NOT_OR;
			conditionNodes[conditionNodes.size() - 2]->inverted = true;
			break;
		}
	}

	bool build_boolean_logic() {
		for (uint32_t i = conditionNodes.size() - 1; --i;) {
			if (conditionNodes[i - 1]->targetNode == conditionNodes[i] && conditionNodes[i]->incomingNodes == 1) {
				if (Node::TYPE_PREFERENCE[conditionNodes[i - 1]->type][conditionNodes[i - 1]->inverted] != 3) invert_node(conditionNodes[i - 1]);
				conditionNodes[i - 1]->leftNode = copy_node(conditionNodes[i - 1]);
				conditionNodes[i - 1]->rightNode = new_node(Node::UNCONDITIONAL);
				conditionNodes[i - 1]->rightNode->inverted = conditionNodes[i - 1]->inverted;
				conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::AND : Node::OR;
				merge_nodes(conditionNodes[i - 1], conditionNodes[i]);
				conditionNodes[i - 1]->type = conditionNodes[i - 1]->leftNode->inverted ? (conditionNodes[i - 1]->inverted ? Node::NOT_OR : Node::OR) : (conditionNodes[i - 1]->inverted ? Node::NOT_AND : Node::AND);
				conditionNodes[i - 1]->leftNode->inverted = false;
				conditionNodes.erase(conditionNodes.begin() + i);
				i = conditionNodes.size() - 1;
			} else if (!conditionNodes[i]->incomingNodes) {
				if (conditionNodes[i - 1]->targetNode == conditionNodes[i]->targetNode) {
					if (conditionNodes[i - 1]->inverted != conditionNodes[i]->inverted && !invert_any_node(conditionNodes[i - 1], conditionNodes[i])) return false;
					merge_nodes(conditionNodes[i - 1], conditionNodes[i]);
					conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::NOT_AND : Node::OR;
					conditionNodes.erase(conditionNodes.begin() + i);
					i = conditionNodes.size() - 1;
				} else if (conditionNodes[i - 1]->targetNode == conditionNodes[i + 1]) {
					if (conditionNodes[i - 1]->inverted == conditionNodes[i]->inverted && !invert_any_node(conditionNodes[i - 1], conditionNodes[i])) return false;
					merge_nodes(conditionNodes[i - 1], conditionNodes[i]);
					conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::NOT_OR : Node::AND;
					conditionNodes.erase(conditionNodes.begin() + i);
					i = conditionNodes.size() - 1;
				}
			}
		}

		conditionNodes.pop_back();
		return conditionNodes.size() == 1;
	}

	static bool invert_any_node(Node* const& leftNode, Node* const& rightNode) {
		if (leftNode->targetNode->type < Node::END_TARGET) {
			invert_node(rightNode->targetNode->type > Node::END_TARGET || Node::TYPE_PREFERENCE[leftNode->type][!leftNode->inverted] >= Node::TYPE_PREFERENCE[rightNode->type][!rightNode->inverted] ? leftNode : rightNode);
		} else {
			if (rightNode->targetNode->type > Node::END_TARGET) return false;
			invert_node(rightNode);
		}

		return true;
	}

	static void invert_node(Node* const& node) {
		node->inverted = !node->inverted;
		if (Node::TYPE_PREFERENCE[node->type][node->inverted]) return;
		invert_node(node->leftNode);
		invert_node(node->rightNode);
		node->type = node->inverted ? (node->type == Node::AND ? Node::NOT_OR : Node::NOT_AND) : (node->type == Node::NOT_AND ? Node::OR : Node::AND);
	}

	Node* copy_node(Node* const& node) {
		Node* const copy = new_node(node->type);
		copy->inverted = node->inverted;
		copy->expressions = node->expressions;
		copy->leftNode = node->leftNode;
		copy->rightNode = node->rightNode;
		return copy;
	}

	void merge_nodes(Node* const& node, Node* const& targetNode) {
		node->leftNode = copy_node(node);
		node->rightNode = targetNode;
		node->targetNode->incomingNodes--;
		node->targetNode = targetNode->targetNode;
		node->inverted = targetNode->inverted;
	}

	Expression* build_expression(Node* const& node) {
		switch (node->type) {
		case Node::LESS_THAN:
		case Node::LESS_EQUAL:
		case Node::GREATER_THEN:
		case Node::GREATER_EQUAL:
			return node->inverted ? build_not(build_binary(node->type, (*node->expressions)[0], (*node->expressions)[1])) : build_binary(node->type, (*node->expressions)[0], (*node->expressions)[1]);
		case Node::NOT_LESS_THAN:
		case Node::NOT_LESS_EQUAL:
		case Node::NOT_GREATER_THEN:
		case Node::NOT_GREATER_EQUAL:
			return node->inverted ? build_binary(node->type, (*node->expressions)[0], (*node->expressions)[1]) : build_not(build_binary(node->type, (*node->expressions)[0], (*node->expressions)[1]));
		case Node::EQUAL:
			return build_binary(node->inverted ? Node::NOT_EQUAL : Node::EQUAL, (*node->expressions)[0], (*node->expressions)[1]);
		case Node::NOT_EQUAL:
			return build_binary(node->inverted ? Node::EQUAL : Node::NOT_EQUAL, (*node->expressions)[0], (*node->expressions)[1]);
		case Node::TRUTHY_TEST:
			return node->inverted ? build_not((*node->expressions).back()) : (*node->expressions).back();
		case Node::FALSY_TEST:
			return node->inverted ? (*node->expressions).back() : build_not((*node->expressions).back());
		case Node::BOOL_TRUTHY_TEST:
			return node->inverted ? build_not((*node->expressions).back()) : build_not(build_not((*node->expressions).back()));
		case Node::BOOL_FALSY_TEST:
			return node->inverted ? build_not(build_not((*node->expressions).back())) : build_not((*node->expressions).back());
		case Node::UNCONDITIONAL:
			return ast.new_primitive(node->inverted ? 1 : 2);
		case Node::AND:
		case Node::OR:
			return node->inverted ? build_not(build_binary(node->type, build_expression(node->leftNode), build_expression(node->rightNode)))
				: build_binary(node->type, build_expression(node->leftNode), build_expression(node->rightNode));
		case Node::NOT_AND:
		case Node::NOT_OR:
			return node->inverted ? build_binary(node->type, build_expression(node->leftNode), build_expression(node->rightNode))
				: build_not(build_binary(node->type, build_expression(node->leftNode), build_expression(node->rightNode)));
		default:
			throw nullptr;
		}
	}

	Expression* build_not(Expression* const& operand) {
		Expression* const expression = ast.new_expression(AST_EXPRESSION_UNARY_OPERATION);
		expression->unaryOperation->type = AST_UNARY_NOT;
		expression->unaryOperation->operand = operand;
		return expression;
	}

	Expression* build_binary(const Node::TYPE& type, Expression* const& leftOperand, Expression* const& rightOperand) {
		Expression* const expression = ast.new_expression(AST_EXPRESSION_BINARY_OPERATION);

		switch (type) {
		case Node::LESS_THAN:
		case Node::NOT_LESS_THAN:
			expression->binaryOperation->type = AST_BINARY_LESS_THAN;
			break;
		case Node::LESS_EQUAL:
		case Node::NOT_LESS_EQUAL:
			expression->binaryOperation->type = AST_BINARY_LESS_EQUAL;
			break;
		case Node::GREATER_THEN:
		case Node::NOT_GREATER_THEN:
			expression->binaryOperation->type = AST_BINARY_GREATER_THEN;
			break;
		case Node::GREATER_EQUAL:
		case Node::NOT_GREATER_EQUAL:
			expression->binaryOperation->type = AST_BINARY_GREATER_EQUAL;
			break;
		case Node::EQUAL:
			expression->binaryOperation->type = AST_BINARY_EQUAL;
			break;
		case Node::NOT_EQUAL:
			expression->binaryOperation->type = AST_BINARY_NOT_EQUAL;
			break;
		case Node::AND:
		case Node::NOT_AND:
			expression->binaryOperation->type = AST_BINARY_AND;
			break;
		case Node::OR:
		case Node::NOT_OR:
			expression->binaryOperation->type = AST_BINARY_OR;
			break;
		default:
			throw nullptr;
		}

		expression->binaryOperation->leftOperand = leftOperand;
		expression->binaryOperation->rightOperand = rightOperand;
		return expression;
	}

	Expression* build_condition() {
		if (link_nodes()) {
			fix_return_nodes();
			if (build_boolean_logic()) return build_expression(conditionNodes.back());
		}

		return nullptr;
	}

	Ast& ast;
	std::vector<Node*> nodes;
	std::vector<Node*> conditionNodes;
	Node* endTarget = nullptr;
	Node* trueTarget = nullptr;
	Node* falseTarget = nullptr;
};
