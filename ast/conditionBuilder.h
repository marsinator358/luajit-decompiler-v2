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
			UNCONDITIONAL,
			AND,
			OR,
			NOT_OR,
			NOT_AND,
			END_TARGET,
			TRUE_TARGET,
			FALSE_TARGET,
		} type;

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
			endTarget = new_node();
			endTarget->type = Node::END_TARGET;
			endTarget->nodeLabel = endTargetLabel;
		}

		trueTarget = new_node();
		trueTarget->type = Node::TRUE_TARGET;
		trueTarget->nodeLabel = trueTargetLabel;
		falseTarget = new_node();
		falseTarget->type = Node::FALSE_TARGET;
		falseTarget->nodeLabel = falseTargetLabel;
	}

	~ConditionBuilder() {
		for (uint32_t i = nodes.size(); i--;) {
			delete nodes[i];
		}
	}

	Node*& new_node() {
		nodes.emplace_back(new Node);
		return nodes.back();
	}

	Node::TYPE get_node_type(const Bytecode::BC_OP& instruction, const bool& swapped) {
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
		}
	}

	void add_node(const Node::TYPE& type, const uint32_t& nodeLabel, const uint32_t& targetLabel, std::vector<Expression*>* const& expressions) {
		conditionNodes.emplace_back(new_node());
		conditionNodes.back()->type = type;
		conditionNodes.back()->nodeLabel = nodeLabel;
		conditionNodes.back()->targetLabel = targetLabel;
		conditionNodes.back()->expressions = expressions;
	}

	void link_nodes() {
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
		}

		conditionNodes.pop_back();
		if (type == ASSIGNMENT) conditionNodes.pop_back();
	}

	void fix_return_nodes() {
		switch (type) {
		case ASSIGNMENT:
			for (uint32_t i = conditionNodes.size() - 1; i--;) {
				switch (conditionNodes[i]->targetNode->type) {
				case Node::END_TARGET:
					if (conditionNodes[i]->type == Node::TRUTHY_TEST) {
						conditionNodes[i]->targetNode = trueTarget;
						trueTarget->incomingNodes++;
						continue;
					}

					conditionNodes[i]->targetNode = falseTarget;
					falseTarget->incomingNodes++;
				case Node::FALSE_TARGET:
					conditionNodes[i]->inverted = true;
				}
			}

			break;
		case STATEMENT:
			for (uint32_t i = conditionNodes.size() - 1; i--;) {
				if (conditionNodes[i]->targetNode->type == Node::FALSE_TARGET) conditionNodes[i]->inverted = true;
			}

			if (!conditionNodes[conditionNodes.size() - 2]->inverted) {
				conditionNodes[conditionNodes.size() - 2]->leftNode = copy_node(conditionNodes[conditionNodes.size() - 2]);
				conditionNodes[conditionNodes.size() - 2]->rightNode = new_node();
				conditionNodes[conditionNodes.size() - 2]->rightNode->type = Node::UNCONDITIONAL;
				trueTarget->incomingNodes--;
				conditionNodes[conditionNodes.size() - 2]->targetNode = falseTarget;
				falseTarget->incomingNodes++;
				conditionNodes[conditionNodes.size() - 2]->type = Node::NOT_OR;
				conditionNodes[conditionNodes.size() - 2]->inverted = true;
			}

			break;
		}
	}

	bool build_boolean_logic() {
		for (uint32_t i = conditionNodes.size() - 1; --i;) {
			if (conditionNodes[i - 1]->targetNode == conditionNodes[i] && conditionNodes[i]->incomingNodes == 1) {
				//TODO
				conditionNodes[i - 1]->inverted = false;
				conditionNodes[i - 1]->leftNode = copy_node(conditionNodes[i - 1]);
				conditionNodes[i - 1]->rightNode = new_node();
				conditionNodes[i - 1]->rightNode->type = Node::UNCONDITIONAL;
				conditionNodes[i - 1]->type = Node::OR;
				merge_nodes(conditionNodes[i - 1], conditionNodes[i]);
				conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::NOT_AND : Node::AND;
				conditionNodes.erase(conditionNodes.begin() + i);
				i = conditionNodes.size() - 1;
			} else if (!conditionNodes[i]->incomingNodes) {
				if (conditionNodes[i - 1]->targetNode == conditionNodes[i]->targetNode) {
					if (conditionNodes[i - 1]->inverted != conditionNodes[i]->inverted && !invert_node(conditionNodes[i - 1], conditionNodes[i])) return false;
					merge_nodes(conditionNodes[i - 1], conditionNodes[i]);
					conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::NOT_AND : Node::OR;
					conditionNodes.erase(conditionNodes.begin() + i);
					i = conditionNodes.size() - 1;
				} else if (conditionNodes[i - 1]->targetNode == conditionNodes[i + 1]) {
					if (conditionNodes[i - 1]->inverted == conditionNodes[i]->inverted && !invert_node(conditionNodes[i - 1], conditionNodes[i])) return false;
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

	bool invert_node(Node* const& leftNode, Node* const& rightNode) {
		if (leftNode->targetNode->type < Node::END_TARGET) {
			if (rightNode->targetNode->type > Node::END_TARGET) {
				leftNode->inverted = !leftNode->inverted;
			} else {
				//TODO
				if (rightNode->inverted) {
					rightNode->inverted = false;
				} else {
					leftNode->inverted = !leftNode->inverted;
				}
			}
		} else {
			if (rightNode->targetNode->type > Node::END_TARGET) return false;
			rightNode->inverted = !rightNode->inverted;
		}

		return true;
	}

	Node* copy_node(Node* const& node) {
		Node* const copy = new_node();
		copy->type = node->type;
		copy->inverted = node->inverted;
		copy->expressions = node->expressions;
		copy->leftNode = node->leftNode;
		copy->rightNode = node->rightNode;
		return copy;
	}

	void merge_nodes(Node* const& leftNode, Node* const& rightNode) {
		leftNode->leftNode = copy_node(leftNode);
		leftNode->rightNode = rightNode;
		leftNode->targetNode->incomingNodes--;
		leftNode->targetNode = rightNode->targetNode;
		leftNode->inverted = rightNode->inverted;
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
		}

		expression->binaryOperation->leftOperand = leftOperand;
		expression->binaryOperation->rightOperand = rightOperand;
		return expression;
	}

	Expression* build_condition() {
		link_nodes();
		fix_return_nodes();
		return build_boolean_logic() ? build_expression(conditionNodes.back()) : nullptr;
	}

	Ast& ast;
	std::vector<Node*> nodes;
	std::vector<Node*> conditionNodes;
	Node* endTarget = nullptr;
	Node* trueTarget = nullptr;
	Node* falseTarget = nullptr;
};
