#include "..\main.h"

Ast::ConditionDecompiler::ConditionDecompiler(Ast* ast, Label* const& endTargetLabel, Label* const& trueTargetLabel, Label* const& falseTargetLabel, Variable* const& targetSlot)
	: mode(ASSIGNMENT), ast(*ast), targetSlot(targetSlot), endTarget(Node::END_TARGET), trueTarget(Node::TRUE_TARGET), falseTarget(Node::FALSE_TARGET) {
	endTarget.label = endTargetLabel;
	trueTarget.label = trueTargetLabel;
	falseTarget.label = falseTargetLabel;
}

Ast::ConditionDecompiler::ConditionDecompiler(Ast* ast, Label* const& trueTargetLabel, Label* const& falseTargetLabel)
	: mode(STATEMENT), ast(*ast), targetSlot(nullptr), endTarget(Node::END_TARGET), trueTarget(Node::TRUE_TARGET), falseTarget(Node::FALSE_TARGET) {
	trueTarget.label = trueTargetLabel;
	falseTarget.label = falseTargetLabel;
}

Ast::ConditionDecompiler::~ConditionDecompiler() {
	for (size_t i = nodes.size(); i--;) {
		delete nodes[i];
	}
}

Ast::ConditionDecompiler::Node*& Ast::ConditionDecompiler::new_node(const Node::TYPE& type) {
	return nodes.emplace_back(new Node(type));
}

Ast::Expression* Ast::ConditionDecompiler::operator()() {
	if (link_nodes()) {
		fix_return_nodes();
		if (decompile_condition()) return build_expression(conditionNodes.back());
	}

	return nullptr;
}

bool Ast::ConditionDecompiler::link_nodes() {
	switch (mode) {
	case ASSIGNMENT:
		endAssignment = conditionNodes.back();
		conditionNodes.emplace_back(&falseTarget);
		conditionNodes.emplace_back(&trueTarget);
		conditionNodes.emplace_back(&endTarget);
		break;
	case STATEMENT:
		conditionNodes.emplace_back(&trueTarget);
		conditionNodes.emplace_back(&falseTarget);
		break;
	}

	for (size_t i = conditionNodes.size(); i--;) {
		if (!conditionNodes[i]->targetLabel) continue;

		for (size_t j = conditionNodes.size(); j--;) {
			if (conditionNodes[i]->targetLabel != conditionNodes[j]->label) continue;
			conditionNodes[i]->targetNode = conditionNodes[j];
			conditionNodes[j]->incomingNodes++;
			break;
		}

		if (!conditionNodes[i]->targetNode) return false;
	}

	conditionNodes.pop_back();
	if (mode == ASSIGNMENT) conditionNodes.pop_back();
	return true;
}

void Ast::ConditionDecompiler::fix_return_nodes() {
	for (size_t i = conditionNodes.size() - 1; i--;) {
		switch (conditionNodes[i]->targetNode->type) {
		case Node::END_TARGET:
			conditionNodes[i]->targetNode = conditionNodes[i]->type == Node::TRUTHY_TEST ? &trueTarget : &falseTarget;
			conditionNodes[i]->targetNode->incomingNodes++;
			conditionNodes[i]->inverted = conditionNodes[i]->type == Node::FALSY_TEST;
			continue;
		case Node::FALSE_TARGET:
			conditionNodes[i]->inverted = true;
		case Node::TRUE_TARGET:
			if (mode == STATEMENT) continue;

			switch (conditionNodes[i]->type) {
			case Node::TRUTHY_TEST:
				conditionNodes[i]->type = Node::BOOL_TRUTHY_TEST;
				continue;
			case Node::FALSY_TEST:
				conditionNodes[i]->type = Node::BOOL_FALSY_TEST;
				continue;
			}

			continue;
		default:
			if (Node::TYPE_PREFERENCES[conditionNodes[i]->type][conditionNodes[i]->inverted] != 3) conditionNodes[i]->inverted = true;
			continue;
		}
	}

	if (mode != STATEMENT || conditionNodes[conditionNodes.size() - 2]->inverted) return;
	conditionNodes[conditionNodes.size() - 2]->leftNode = copy_node(conditionNodes[conditionNodes.size() - 2]);
	conditionNodes[conditionNodes.size() - 2]->rightNode = new_node(Node::UNCONDITIONAL_FALSE);
	conditionNodes[conditionNodes.size() - 2]->rightNode->inverted = true;
	conditionNodes[conditionNodes.size() - 2]->targetNode->incomingNodes--;
	conditionNodes[conditionNodes.size() - 2]->targetNode = &falseTarget;
	conditionNodes[conditionNodes.size() - 2]->targetNode->incomingNodes++;
	conditionNodes[conditionNodes.size() - 2]->type = Node::NOT_OR;
	conditionNodes[conditionNodes.size() - 2]->inverted = true;
}

bool Ast::ConditionDecompiler::decompile_condition() {
	for (size_t i = conditionNodes.size() - 1; --i;) {
		if (!conditionNodes[i]->incomingNodes) {
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
			} else if (conditionNodes[i]->targetNode == conditionNodes[i + 1]) {
				if (conditionNodes[i - 1]->inverted == conditionNodes[i]->inverted) invert_node(conditionNodes[i]);
				conditionNodes[i]->leftNode = copy_node(conditionNodes[i]);
				conditionNodes[i]->rightNode = new_node(Node::UNCONDITIONAL_FALSE);
				conditionNodes[i]->inverted = !conditionNodes[i]->inverted;
				conditionNodes[i]->rightNode->inverted = conditionNodes[i]->inverted;
				conditionNodes[i]->type = conditionNodes[i]->inverted ? Node::NOT_OR : Node::AND;
				std::swap<Node*>(conditionNodes[i]->targetNode, conditionNodes[i - 1]->targetNode);
				conditionNodes[i]->hasAlternateTarget = conditionNodes[i - 1]->hasAlternateTarget;
				merge_nodes(conditionNodes[i - 1], conditionNodes[i]);
				conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::NOT_AND : Node::OR;
				conditionNodes.erase(conditionNodes.begin() + i);
				i = conditionNodes.size() - 1;
			} else if (conditionNodes[i]->hasAlternateTarget
				&& (conditionNodes[i - 1]->targetNode == endAssignment
					|| conditionNodes[i + 1] == endAssignment)) {
				conditionNodes[i]->targetNode->incomingNodes--;
				conditionNodes[i]->targetNode = endAssignment;
				endAssignment->incomingNodes++;
				i++;
			} else if (conditionNodes[i]->targetNode == endAssignment && conditionNodes[i - 1]->hasAlternateTarget) {
				conditionNodes[i - 1]->targetNode->incomingNodes--;
				conditionNodes[i - 1]->targetNode = endAssignment;
				endAssignment->incomingNodes++;
				i++;
			}
		} else if (conditionNodes[i]->incomingNodes == 1 && conditionNodes[i - 1]->targetNode == conditionNodes[i]) {
			conditionNodes[i - 1]->leftNode = copy_node(conditionNodes[i - 1]);
			conditionNodes[i - 1]->rightNode = new_node(Node::UNCONDITIONAL_FALSE);
			conditionNodes[i - 1]->inverted = !conditionNodes[i - 1]->inverted;
			conditionNodes[i - 1]->rightNode->inverted = conditionNodes[i - 1]->inverted;
			conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::NOT_OR : Node::AND;
			conditionNodes[i - 1]->hasAlternateTarget = conditionNodes[i]->hasAlternateTarget;

			if (conditionNodes[i - 1]->inverted == conditionNodes[i]->inverted) {
				merge_nodes(conditionNodes[i - 1], conditionNodes[i]);
				conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::NOT_AND : Node::OR;
			} else {
				merge_nodes(conditionNodes[i - 1], conditionNodes[i]);
				conditionNodes[i - 1]->type = conditionNodes[i - 1]->inverted ? Node::NOT_OR : Node::AND;
			}

			conditionNodes.erase(conditionNodes.begin() + i);
			i = conditionNodes.size() - 1;
		}
	}

	conditionNodes.pop_back();
	return conditionNodes.size() == 1;
}

Ast::ConditionDecompiler::Node::TYPE Ast::ConditionDecompiler::get_node_type(const Statement& statement) {
	if (statement.type == AST_STATEMENT_CONDITION) {
		switch (statement.instruction->type) {
		case Bytecode::BC_OP_ISLT:
			return statement.assignment.swapped ? Node::GREATER_THEN : Node::LESS_THAN;
		case Bytecode::BC_OP_ISGE:
			return statement.assignment.swapped ? Node::NOT_GREATER_THEN : Node::NOT_LESS_THAN;
		case Bytecode::BC_OP_ISLE:
			return statement.assignment.swapped ? Node::GREATER_EQUAL : Node::LESS_EQUAL;
		case Bytecode::BC_OP_ISGT:
			return statement.assignment.swapped ? Node::NOT_GREATER_EQUAL : Node::NOT_LESS_EQUAL;
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
			return Node::UNCONDITIONAL_TRUE;
		}
	}

	switch (statement.assignment.expressions.back()->constant->type) {
	case AST_CONSTANT_NIL:
	case AST_CONSTANT_FALSE:
		return Node::FALSY_TEST;
	case AST_CONSTANT_TRUE:
	case AST_CONSTANT_NUMBER:
	case AST_CONSTANT_STRING:
		return Node::TRUTHY_TEST;
	}
}

void Ast::ConditionDecompiler::add_node(const Node::TYPE& type, Label* const& label, Label* const& targetLabel, const std::vector<Expression*>& expressions, const bool& hasAlternateTarget) {
	conditionNodes.emplace_back(new_node(type));
	conditionNodes.back()->label = label;
	conditionNodes.back()->targetLabel = targetLabel;
	conditionNodes.back()->expressions = &expressions;
	conditionNodes.back()->hasAlternateTarget = hasAlternateTarget;
}

void Ast::ConditionDecompiler::invert_node(Node* const& node) {
	node->inverted = !node->inverted;
	if (Node::TYPE_PREFERENCES[node->type][node->inverted]) return;
	invert_node(node->leftNode);
	invert_node(node->rightNode);
	node->type = node->inverted ? (node->type == Node::AND ? Node::NOT_OR : Node::NOT_AND) : (node->type == Node::NOT_AND ? Node::OR : Node::AND);
}

bool Ast::ConditionDecompiler::invert_any_node(Node* const& leftNode, Node* const& rightNode) {
	if (leftNode->targetNode->type < Node::END_TARGET) {
		if (rightNode->targetNode->type < Node::END_TARGET) {
			auto calculate_score = [](const auto& self, Node* const& node)->int32_t {
				//TODO is this even necessary?
				switch (node->type) {
				case Node::AND:
				case Node::OR:
				case Node::NOT_AND:
				case Node::NOT_OR:
					return self(self, node->leftNode) + self(self, node->rightNode);
				}

				return Node::TYPE_PREFERENCES[node->type][!node->inverted] - Node::TYPE_PREFERENCES[node->type][node->inverted];
			};

			int32_t leftScore = calculate_score(calculate_score, leftNode);
			int32_t rightScore = calculate_score(calculate_score, rightNode);
			invert_node((leftScore > 0 ? leftScore >= rightScore : leftScore > rightScore) ? leftNode : rightNode);
		} else {
			invert_node(leftNode);
		}
	} else {
		if (rightNode->targetNode->type > Node::END_TARGET) return false;
		invert_node(rightNode);
	}

	return true;
}

void Ast::ConditionDecompiler::merge_nodes(Node* const& leftNode, Node* const& rightNode) {
	leftNode->leftNode = copy_node(leftNode);
	leftNode->rightNode = rightNode;
	leftNode->targetNode->incomingNodes--;
	leftNode->targetNode = rightNode->targetNode;
	if (!rightNode->hasAlternateTarget) leftNode->hasAlternateTarget = false;
	leftNode->inverted = rightNode->inverted;
}

Ast::ConditionDecompiler::Node* Ast::ConditionDecompiler::copy_node(Node* const& node) {
	Node* copy = new_node(node->type);
	copy->inverted = node->inverted;
	copy->expressions = node->expressions;
	copy->leftNode = node->leftNode;
	copy->rightNode = node->rightNode;
	return copy;
}

Ast::Expression* Ast::ConditionDecompiler::build_expression(Node* const& node) {
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
	case Node::UNCONDITIONAL_TRUE:
		return ast.new_primitive(node->inverted ? 1 : 2);
	case Node::UNCONDITIONAL_FALSE:
		return ast.new_primitive(node->inverted ? 2 : 1);
	case Node::AND:
	case Node::OR:
	case Node::NOT_AND:
	case Node::NOT_OR:
		return build_binary(node->type, build_expression(node->leftNode), build_expression(node->rightNode));
	}
}

Ast::Expression* Ast::ConditionDecompiler::build_not(Expression* const& operand) {
	Expression* expression = ast.new_expression(AST_EXPRESSION_UNARY_OPERATION);
	expression->unaryOperation->type = AST_UNARY_NOT;
	expression->unaryOperation->operand = operand;
	return expression;
}

Ast::Expression* Ast::ConditionDecompiler::build_binary(const Node::TYPE& type, Expression* const& leftOperand, Expression* const& rightOperand) {
	Expression* expression = ast.new_expression(AST_EXPRESSION_BINARY_OPERATION);

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
