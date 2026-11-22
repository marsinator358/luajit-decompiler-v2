#include "..\main.h"

Ast::Ast(const Bytecode& bytecode) : bytecode(bytecode), chunk(*bytecode.main) {}

Ast::~Ast() {
	for (size_t i = functions.size(); i--;) {
		delete functions[i];
	}

	for (size_t i = statements.size(); i--;) {
		delete statements[i];
	}

	for (size_t i = expressions.size(); i--;) {
		delete expressions[i];
	}
}

Ast::Function*& Ast::new_function(const Bytecode::Prototype& prototype) {
	return functions.emplace_back(new Function(prototype));
}

Ast::Statement*& Ast::new_statement(const AST_STATEMENT& type, Function& function) {
	return statements.emplace_back(new Statement(type, function));
}

Ast::Expression*& Ast::new_expression(const AST_EXPRESSION& type) {
	return expressions.emplace_back(new Expression(type));
}

void Ast::operator()() {
	print_progress_bar();
	uint32_t functionCounter = 0;
	build_function(chunk, functionCounter, 0);
	erase_progress_bar();
}

void Ast::build_function(Function& function, uint32_t& counter, const uint32_t& level) {
	function.id = counter;
	counter++;
	function.level = level;
	copy_instructions(function);
	prepare_debug_info(function);
	register_jumps(function);
	build_loops(function);
	assert(!build_local_scopes(function, function.block), L"Failed to build local scopes from debug info", DEBUG_INFO);
	build_expressions(function, function.block);
	build_slot_scopes(function, function.block);
	eliminate_slots(function, function.block);
	function.slotScopeCollector.verify_scopes_closed();
	function.verify_upvalue_scopes();

	for (size_t i = function.childFunctions.size(); i--;) {
		build_function(*function.childFunctions[i], counter, level + 1);
	}
}

void Ast::copy_instructions(Function& function) {
	for (size_t i = 0; i < function.prototype.instructions.size(); i++) {
		function.block.emplace(i, new_statement(AST_STATEMENT_INSTRUCTION, function));
		function.block[i].instruction = &function.prototype.instructions[i];

		switch (function.block[i].instruction->type) {
		case Bytecode::BC_OP_CALLMT:
		case Bytecode::BC_OP_CALLT:
		case Bytecode::BC_OP_RETM:
		case Bytecode::BC_OP_RET:
		case Bytecode::BC_OP_RET0:
		case Bytecode::BC_OP_RET1:
			function.block[i].type = AST_STATEMENT_RETURN;
		}
	}

	assert(function.block.back().type == AST_STATEMENT_RETURN, L"Missing return at function end", DEBUG_INFO);
}

void Ast::prepare_debug_info(Function& function) {
	if (!function.prototype.header.debugInfo || arguments.ignoreDebugInfo) return;
	std::vector<size_t> activeLocalScopes(function.prototype.header.parameters, INVALID_ID);

	for (size_t i = activeLocalScopes.size(); i < function.prototype.debugInfo.size(); i++) {
		assert(!activeLocalScopes.size()
			|| function.prototype.debugInfo[i].scopeBegin >= activeLocalScopes.back()
			|| function.prototype.debugInfo[i].scopeEnd <= activeLocalScopes.back(),
			L"Illegal debug info local scope overlap", DEBUG_INFO);

		while (activeLocalScopes.size() && activeLocalScopes.back() < function.prototype.debugInfo[i].scopeEnd) {
			activeLocalScopes.pop_back();
		}

		if (function.prototype.debugInfo[i].type == Bytecode::BC_VAR_STR) {
			if (function.locals.size()
				&& function.prototype.debugInfo[i].scopeBegin == function.prototype.debugInfo[i].scopeEnd
				&& function.prototype.debugInfo[i].scopeBegin == function.locals.back().scopeEnd) {
				Statement& statement = function.get_statement_from_id(function.prototype.debugInfo[i].scopeBegin);

				switch (statement.instruction->type) {
				case Bytecode::BC_OP_KPRI:
					if ((uint8_t)statement.instruction->d) break;
				case Bytecode::BC_OP_KNIL:
					if ((statement.instruction->type == Bytecode::BC_OP_KPRI ? statement.instruction->a : (uint8_t)statement.instruction->d) < activeLocalScopes.size()) {
						while (activeLocalScopes.size() != function.locals.back().baseSlot) {
							assert(activeLocalScopes.back() == function.prototype.debugInfo[i].scopeBegin, L"Failed to build variable scope", DEBUG_INFO);
							activeLocalScopes.pop_back();
						}

						function.locals.emplace_back();
						function.locals.back().baseSlot = activeLocalScopes.size();
						function.locals.back().scopeBegin = function.block[function.prototype.debugInfo[i].scopeBegin];
						function.locals.back().scopeEnd = function.block[function.prototype.debugInfo[i].scopeEnd];
						function.locals.back().excludeFromScope = function.locals[function.locals.size() - 2].scopeBegin
							== function.locals[function.locals.size() - 2].scopeEnd ? function.locals[function.locals.size() - 2].excludeFromScope : true;
					}
				}
			}

			if (!function.locals.size()
				|| function.prototype.debugInfo[i].scopeBegin != function.locals.back().scopeBegin
				|| function.prototype.debugInfo[i].scopeEnd != function.locals.back().scopeEnd) {
				function.locals.emplace_back();
				function.locals.back().baseSlot = activeLocalScopes.size();
				function.locals.back().scopeBegin = function.block[function.prototype.debugInfo[i].scopeBegin];
				function.locals.back().scopeEnd = function.block[function.prototype.debugInfo[i].scopeEnd];
			}

			function.locals.back().names.emplace_back(&function.prototype.debugInfo[i].name);
		}

		assert(activeLocalScopes.size() < 256, L"Debug info exceeds maximum number of slots", DEBUG_INFO);
		activeLocalScopes.emplace_back(function.prototype.debugInfo[i].scopeEnd);
	}
}

void Ast::register_jumps(Function& function) {
	for (size_t i = function.block.size(); i--;) {
		switch (function.block[i].instruction->type) {
		case Bytecode::BC_OP_ISLT:
		case Bytecode::BC_OP_ISGE:
		case Bytecode::BC_OP_ISLE:
		case Bytecode::BC_OP_ISGT:
		case Bytecode::BC_OP_ISEQV:
		case Bytecode::BC_OP_ISNEV:
		case Bytecode::BC_OP_ISEQS:
		case Bytecode::BC_OP_ISNES:
		case Bytecode::BC_OP_ISEQN:
		case Bytecode::BC_OP_ISNEN:
		case Bytecode::BC_OP_ISEQP:
		case Bytecode::BC_OP_ISNEP:
		case Bytecode::BC_OP_ISTC:
		case Bytecode::BC_OP_ISFC:
		case Bytecode::BC_OP_IST:
		case Bytecode::BC_OP_ISF:
			function.block[i].type = AST_STATEMENT_CONDITION;
			function.block[i].block.move(0, function.block, i + 1, i + 2);
			continue;
		case Bytecode::BC_OP_UCLO:
		case Bytecode::BC_OP_JMP:
			function.block[i].type = AST_STATEMENT_GOTO;
		case Bytecode::BC_OP_ISNEXT:
		case Bytecode::BC_OP_FORI:
		case Bytecode::BC_OP_FORL:
		case Bytecode::BC_OP_ITERL:
		case Bytecode::BC_OP_LOOP:
			function.register_jump(function.block[i], function.block[i].id + (function.block[i].instruction->d - Bytecode::BC_OP_JMP_BIAS + 1));
			continue;
		}
	}

	function.apply_labels();
	std::unordered_set<Statement*> localScopeBorders;

	for (size_t i = function.locals.size(); i--;) {
		localScopeBorders.emplace(&function.get_statement_from_id(function.locals[i].scopeBegin));
		localScopeBorders.emplace(&function.get_statement_from_id(function.locals[i].scopeEnd));
	}

	for (size_t i = function.block.size(); i--;) {
		switch (function.block[i].type) {
		case AST_STATEMENT_INSTRUCTION:
			if (function.block[i].instruction->type == Bytecode::BC_OP_ITERC && function.is_valid_label(function.block[i].label)) {
				size_t index = function.block.get_index_from_id(function.block[i].label->jumps.front());
				if (index != INVALID_ID && function.block[index].instruction->type == Bytecode::BC_OP_JMP) function.block[index].type = AST_STATEMENT_INSTRUCTION;
			}

			continue;
		case AST_STATEMENT_CONDITION:
			assert(function.block[i].block.back().type == AST_STATEMENT_GOTO
				&& function.block[i].block.back().instruction->type == Bytecode::BC_OP_JMP
				&& !function.is_valid_label(function.block[i].block.back().label),
				L"Invalid condition", DEBUG_INFO);
			continue;
		case AST_STATEMENT_GOTO:
			if (function.block[i].instruction->type == Bytecode::BC_OP_UCLO) {
				if (*function.block[i].target != function.block[i].id + 1 || *function.block[i].target != get_extended_id_from_statement(function.block[i + 1])) continue;
				function.remove_jump(function.block[i]);
				function.block[i].type = AST_STATEMENT_EMPTY;
				continue;
			}

			if (*function.block[i].target != function.block[i].id + 1
				|| function.block[i + 1].type != AST_STATEMENT_GOTO
				|| function.block[i + 1].instruction->type != Bytecode::BC_OP_JMP
				|| function.block[i + 1].label->jumps.size() > 1
				|| localScopeBorders.contains(&function.block[i]))
				continue;
			function.block[i].type = AST_STATEMENT_CONDITION;
			function.block[i].block.move(0, function.block, i + 1, i + 2);
			continue;
		}
	}

	for (size_t i = function.block.size(); i--;) {
		if (i
			&& function.block[i - 1].type == AST_STATEMENT_RETURN
			&& function.is_valid_label(function.block[i].label)
			&& function.block[i].label->jumps.size() == 1
			&& !localScopeBorders.contains(&function.block[i])) {
			size_t index = function.block.get_index_from_id(function.block[i].label->jumps.back());

			if (index != INVALID_ID
				&& function.block[index].type == AST_STATEMENT_GOTO
				&& function.block[index].instruction->type == Bytecode::BC_OP_UCLO) {
				function.remove_jump(function.block[index]);
				function.block.emplace(index, new_statement(AST_STATEMENT_EMPTY, function));
				i++;
				function.block[index].instruction = function.block[index + 1].instruction;
				function.block[index + 1].type = AST_STATEMENT_RETURN;
				function.block[index + 1].instruction = function.block[i].instruction;
				function.block[i].type = AST_STATEMENT_EMPTY;
				if (!function.is_valid_label(function.block[index + 1].label)) continue;
				function.block[index].label = function.block[index + 1].label;
				function.block[index + 1].label = nullptr;
				function.block[index].label->target = function.block[index];
				continue;
			}
		}

		//if (function.block[i].instruction->type == Bytecode::BC_OP_RET0) function.block[i].type = AST_STATEMENT_EMPTY;
		break;
	}
}

void Ast::build_loops(Function& function) {
	for (size_t i = function.block.size(); i--;) {
		if (function.block[i].type != AST_STATEMENT_INSTRUCTION) continue;
		size_t targetIndex;
		
		switch (function.block[i].instruction->type) {
		case Bytecode::BC_OP_FORI:
			targetIndex = function.block.get_index_from_id(*function.block[i].target - 1);
			assert(targetIndex != INVALID_ID
				&& targetIndex > i
				&& function.block[targetIndex].instruction->type == Bytecode::BC_OP_FORL
				&& *function.block[targetIndex].target == function.block[i].id + 1,
				L"Invalid numeric for loop", DEBUG_INFO);
			function.remove_jump(function.block[i]);
			function.block[i].type = AST_STATEMENT_NUMERIC_FOR;
			function.remove_jump(function.block[targetIndex]);
			function.block[targetIndex].type = AST_STATEMENT_EMPTY;
			break;
		case Bytecode::BC_OP_ISNEXT:
		case Bytecode::BC_OP_JMP:
			targetIndex = function.block.get_index_from_id(*function.block[i].target + 1);
			assert(targetIndex != INVALID_ID
				&& targetIndex > i + 1
				&& function.block[targetIndex].instruction->type == Bytecode::BC_OP_ITERL
				&& ((function.block[i].instruction->type == Bytecode::BC_OP_JMP
					&& function.block[targetIndex - 1].instruction->type == Bytecode::BC_OP_ITERC)
					|| (function.block[i].instruction->type == Bytecode::BC_OP_ISNEXT
						&& function.block[targetIndex - 1].instruction->type == Bytecode::BC_OP_ITERN))
				&& *function.block[targetIndex].target == function.block[i].id + 1
				&& !function.is_valid_label(function.block[targetIndex].label),
				L"Invalid generic for loop", DEBUG_INFO);
			function.remove_jump(function.block[i]);
			function.block[i].type = AST_STATEMENT_GENERIC_FOR;
			function.remove_jump(function.block[targetIndex]);
			function.block[targetIndex].type = AST_STATEMENT_EMPTY;
			function.block[targetIndex - 1].type = AST_STATEMENT_EMPTY;
			function.block[i].instruction = function.block[targetIndex - 1].instruction;
			break;
		case Bytecode::BC_OP_LOOP:
			targetIndex = function.block.get_index_from_id(*function.block[i].target);
			assert(targetIndex != INVALID_ID && targetIndex >= i, L"Invalid loop instruction", DEBUG_INFO);
			function.remove_jump(function.block[i]);

			if (targetIndex == i) {
				assert(function.block[i + 1].type == AST_STATEMENT_GOTO
					&& *function.block[i + 1].target <= function.block[i].id
					&& !function.is_valid_label(function.block[i + 1].label),
					L"Invalid goto loop", DEBUG_INFO);
				function.block[i].type = AST_STATEMENT_EMPTY;
				if (function.block[i + 1].instruction->type == Bytecode::BC_OP_JMP) function.block[i + 1].instruction = function.block[i].instruction;
				continue;
			}

			function.block[i].type = AST_STATEMENT_LOOP;
			targetIndex--;
			break;
		default:
			continue;
		}

		function.block[i].block.move(0, function.block, i + 1, targetIndex + 1);
		function.loopEnds[&function.get_statement_from_id(function.block[i].block.get_end_id())] = function.block[i];
		/*
		size_t breakTarget = get_extended_id_from_statement(function.block[i + 1]);

		for (size_t j = function.block[i].block.size(); j--;) {
			switch (function.block[i].block[j].type) {
			case AST_STATEMENT_CONDITION:
				if (function.block[i].block[j].instruction->type == Bytecode::BC_OP_JMP && *function.block[i].block[j].block.back().target == breakTarget)
					function.block[i].block[j].block.back().type = AST_STATEMENT_BREAK;
				continue;
			case AST_STATEMENT_GOTO:
				if (*function.block[i].block[j].target == breakTarget) function.block[i].block[j].type = AST_STATEMENT_BREAK;
				continue;
			}
		}
		*/
	}
}

size_t Ast::build_local_scopes(Function& function, Block& block, const size_t& initialLocalIndex) {
	size_t localIndex = initialLocalIndex == INVALID_ID ? function.locals.size() : initialLocalIndex;

	for (size_t i = block.size(); localIndex && i--;) {
		if (function.locals[localIndex - 1].scopeBegin < block[i].id) continue;

		if (function.locals[localIndex - 1].scopeBegin > block[i].id) {
			switch (block[i].type) {
			case AST_STATEMENT_CONDITION:
				break;
			case AST_STATEMENT_NUMERIC_FOR:
			case AST_STATEMENT_GENERIC_FOR:
			case AST_STATEMENT_LOOP:
				localIndex = build_local_scopes(function, block[i].block, localIndex);

				if (localIndex && function.locals[localIndex - 1].scopeBegin <= block[i].id) {
					i++;
					continue;
				}
			default:
				return localIndex;
			}
		}

		switch (block[i].type) {
		case AST_STATEMENT_CONDITION:
			if (function.locals[localIndex - 1].scopeBegin != block[i].block.back().id) return localIndex;
			break;
		case AST_STATEMENT_NUMERIC_FOR:
		case AST_STATEMENT_GENERIC_FOR:
			if (function.locals[localIndex - 1].scopeEnd >= block[i].block.get_end_id()) return localIndex;
			localIndex--;
			block[i].assignment.locals = &function.locals[localIndex];
			continue;
		}

		localIndex--;
		i++;
		block.emplace(i, new_statement(AST_STATEMENT_LOCAL, function));
		block[i].instruction = block[i - 1].instruction;
		block[i].assignment.locals = &function.locals[localIndex];
		if (block[i].assignment.locals->scopeBegin == block[i].assignment.locals->scopeEnd) continue;
		size_t scopeEndIndex = i + 1;

		while (scopeEndIndex < block.size()
			&& (block[scopeEndIndex].block.get_end_id() - 1 <= block[i].assignment.locals->scopeEnd
				|| (block[scopeEndIndex].type == AST_STATEMENT_LOCAL
					&& block[scopeEndIndex].assignment.locals->scopeEnd == block[i].assignment.locals->scopeEnd))) {
			scopeEndIndex++;
		}

		while (block[scopeEndIndex - 1].type == AST_STATEMENT_LOCAL && block[scopeEndIndex - 1].assignment.locals->excludeFromScope) {
			scopeEndIndex--;
		}

		if (block[i].assignment.locals->scopeEnd != block[scopeEndIndex - 1].block.get_end_id() - 1
			&& (block[scopeEndIndex - 1].type != AST_STATEMENT_LOCAL
				|| block[scopeEndIndex - 1].assignment.locals->scopeEnd != block[i].assignment.locals->scopeEnd)) {
			localIndex++;
			return localIndex;
		}

		block[i].block.move(0, block, i + 1, scopeEndIndex);
		/*
		size_t scopeEndIndex = block.get_index_from_id(function.locals[localIndex - 1].scopeEnd + 1);

		if (scopeEndIndex == INVALID_ID) {
			if (function.locals[localIndex - 1].scopeEnd != block.get_end_id() - 1) return localIndex;
			scopeEndIndex = block.size();
		}

		while (block[scopeEndIndex - 1].type == AST_STATEMENT_LOCAL && block[scopeEndIndex - 1].assignment.locals->excludeFromScope) {
			scopeEndIndex--;
		}

		localIndex--;
		i++;
		scopeEndIndex++;
		block.emplace(i, new_statement(AST_STATEMENT_LOCAL, function));
		block[i].instruction = block[i - 1].instruction;
		block[i].assignment.locals = &function.locals[localIndex];
		if (scopeEndIndex < block.size()
			&& block[scopeEndIndex].type == AST_STATEMENT_LOCAL
			&& !block[scopeEndIndex].block.size())
			scopeEndIndex++;
		block[i].block.move(0, block, i + 1, scopeEndIndex);
		*/
	}

	return localIndex;
}

void Ast::build_expressions(Function& function, Block& block) {
	auto set_slot = [](Variable& variable, const uint8_t& slot)->void {
		variable.type = AST_VARIABLE_SLOT;
		variable.slot = slot;
	};

	for (size_t i = block.size(); i--;) {
		switch (block[i].type) {
		case AST_STATEMENT_INSTRUCTION:
			block[i].type = AST_STATEMENT_ASSIGNMENT;
			block[i].assignment.expressions.emplace_back();

			switch (block[i].instruction->type) {
			case Bytecode::BC_OP_MOV:
				block[i].assignment.expressions.back() = new_slot(block[i].instruction->d);
				block[i].assignment.register_slots(block[i].assignment.expressions.back());
				break;
			case Bytecode::BC_OP_NOT:
			case Bytecode::BC_OP_UNM:
			case Bytecode::BC_OP_LEN:
			case Bytecode::BC_OP_BNOT:
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_UNARY_OPERATION);

				switch (block[i].instruction->type) {
				case Bytecode::BC_OP_NOT:
					block[i].assignment.expressions.back()->unaryOperation->type = AST_UNARY_NOT;
					block[i].assignment.allowedConstantType = INVALID_CONSTANT;
					break;
				case Bytecode::BC_OP_UNM:
					block[i].assignment.expressions.back()->unaryOperation->type = AST_UNARY_MINUS;
					block[i].assignment.allowedConstantType = BOOL_CONSTANT;
					break;
				case Bytecode::BC_OP_LEN:
					block[i].assignment.expressions.back()->unaryOperation->type = AST_UNARY_LENGTH;
					break;
				case Bytecode::BC_OP_BNOT:
					block[i].assignment.expressions.back()->unaryOperation->type = AST_UNARY_BIT_NOT;
					block[i].assignment.allowedConstantType = BOOL_CONSTANT;
					break;
				}

				block[i].assignment.expressions.back()->unaryOperation->operand = new_slot(block[i].instruction->d);
				block[i].assignment.register_slots(block[i].assignment.expressions.back()->unaryOperation->operand);
				break;
			case Bytecode::BC_OP_ADDVN:
			case Bytecode::BC_OP_SUBVN:
			case Bytecode::BC_OP_MULVN:
			case Bytecode::BC_OP_DIVVN:
			case Bytecode::BC_OP_MODVN:
			case Bytecode::BC_OP_ADDNV:
			case Bytecode::BC_OP_SUBNV:
			case Bytecode::BC_OP_MULNV:
			case Bytecode::BC_OP_DIVNV:
			case Bytecode::BC_OP_MODNV:
			case Bytecode::BC_OP_ADDVV:
			case Bytecode::BC_OP_SUBVV:
			case Bytecode::BC_OP_MULVV:
			case Bytecode::BC_OP_DIVVV:
			case Bytecode::BC_OP_MODVV:
			case Bytecode::BC_OP_POW:
			case Bytecode::BC_OP_BAND:
			case Bytecode::BC_OP_BOR:
			case Bytecode::BC_OP_BXOR:
			case Bytecode::BC_OP_BSHL:
			case Bytecode::BC_OP_BSHR:
			case Bytecode::BC_OP_BSAR:
				block[i].assignment.allowedConstantType = BOOL_CONSTANT;
			case Bytecode::BC_OP_CAT:
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_BINARY_OPERATION);

				switch (block[i].instruction->type) {
				case Bytecode::BC_OP_ADDVN:
				case Bytecode::BC_OP_ADDNV:
				case Bytecode::BC_OP_ADDVV:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_ADDITION;
					break;
				case Bytecode::BC_OP_SUBVN:
				case Bytecode::BC_OP_SUBNV:
				case Bytecode::BC_OP_SUBVV:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_SUBTRACTION;
					break;
				case Bytecode::BC_OP_MULVN:
				case Bytecode::BC_OP_MULNV:
				case Bytecode::BC_OP_MULVV:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_MULTIPLICATION;
					break;
				case Bytecode::BC_OP_DIVVN:
				case Bytecode::BC_OP_DIVNV:
				case Bytecode::BC_OP_DIVVV:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_DIVISION;
					break;
				case Bytecode::BC_OP_MODVN:
				case Bytecode::BC_OP_MODNV:
				case Bytecode::BC_OP_MODVV:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_MODULO;
					break;
				case Bytecode::BC_OP_POW:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_EXPONENTATION;
					break;
				case Bytecode::BC_OP_CAT:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_CONCATENATION;
					break;
				case Bytecode::BC_OP_BAND:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_BIT_AND;
					break;
				case Bytecode::BC_OP_BOR:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_BIT_OR;
					break;
				case Bytecode::BC_OP_BXOR:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_BIT_EXCLUSIVE_OR;
					break;
				case Bytecode::BC_OP_BSHL:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_BIT_LEFT_SHIFT;
					break;
				case Bytecode::BC_OP_BSHR:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_BIT_RIGHT_SHIFT;
					break;
				case Bytecode::BC_OP_BSAR:
					block[i].assignment.expressions.back()->binaryOperation->type = AST_BINARY_BIT_ARITHMETIC_RIGHT_SHIFT;
					break;
				}

				switch (block[i].instruction->type) {
				case Bytecode::BC_OP_ADDVN:
				case Bytecode::BC_OP_SUBVN:
				case Bytecode::BC_OP_MULVN:
				case Bytecode::BC_OP_DIVVN:
				case Bytecode::BC_OP_MODVN:
					block[i].assignment.expressions.back()->binaryOperation->leftOperand = new_slot(block[i].instruction->b);
					block[i].assignment.register_slots(block[i].assignment.expressions.back()->binaryOperation->leftOperand);
					block[i].assignment.expressions.back()->binaryOperation->rightOperand = new_number(function, block[i].instruction->c);
					break;
				case Bytecode::BC_OP_ADDNV:
				case Bytecode::BC_OP_SUBNV:
				case Bytecode::BC_OP_MULNV:
				case Bytecode::BC_OP_DIVNV:
				case Bytecode::BC_OP_MODNV:
					block[i].assignment.expressions.back()->binaryOperation->leftOperand = new_number(function, block[i].instruction->c);
					block[i].assignment.expressions.back()->binaryOperation->rightOperand = new_slot(block[i].instruction->b);
					block[i].assignment.register_slots(block[i].assignment.expressions.back()->binaryOperation->rightOperand);
					break;
				case Bytecode::BC_OP_ADDVV:
				case Bytecode::BC_OP_SUBVV:
				case Bytecode::BC_OP_MULVV:
				case Bytecode::BC_OP_DIVVV:
				case Bytecode::BC_OP_MODVV:
				case Bytecode::BC_OP_POW:
				case Bytecode::BC_OP_BAND:
				case Bytecode::BC_OP_BOR:
				case Bytecode::BC_OP_BXOR:
				case Bytecode::BC_OP_BSHL:
				case Bytecode::BC_OP_BSHR:
				case Bytecode::BC_OP_BSAR:
					block[i].assignment.expressions.back()->binaryOperation->leftOperand = new_slot(block[i].instruction->b);
					block[i].assignment.expressions.back()->binaryOperation->rightOperand = new_slot(block[i].instruction->c);
					block[i].assignment.register_slots(block[i].assignment.expressions.back()->binaryOperation->leftOperand, block[i].assignment.expressions.back()->binaryOperation->rightOperand);
					break;
				case Bytecode::BC_OP_CAT:
					assert(block[i].instruction->c > block[i].instruction->b, L"Invalid CAT instruction", DEBUG_INFO);
					block[i].assignment.expressions.back()->binaryOperation->leftOperand = new_slot(block[i].instruction->b);

					for (Expression* expression = block[i].assignment.expressions.back(); true; expression = expression->binaryOperation->rightOperand) {
						block[i].assignment.register_slots(expression->binaryOperation->leftOperand);

						if (expression->binaryOperation->leftOperand->variable->slot + 1 == block[i].instruction->c) {
							expression->binaryOperation->rightOperand = new_slot(block[i].instruction->c);
							block[i].assignment.register_slots(expression->binaryOperation->rightOperand);
							break;
						}

						expression->binaryOperation->rightOperand = new_expression(AST_EXPRESSION_BINARY_OPERATION);
						expression->binaryOperation->rightOperand->binaryOperation->type = AST_BINARY_CONCATENATION;
						expression->binaryOperation->rightOperand->binaryOperation->leftOperand = new_slot(expression->binaryOperation->leftOperand->variable->slot + 1);
					}

					break;
				}

				break;
			case Bytecode::BC_OP_KSTR:
				block[i].assignment.expressions.back() = new_string(function, block[i].instruction->d);
				check_valid_name(block[i].assignment.expressions.back());
				break;
			case Bytecode::BC_OP_KCDATA:
				block[i].assignment.expressions.back() = new_cdata(function, block[i].instruction->d);
				break;
			case Bytecode::BC_OP_KSHORT:
				block[i].assignment.expressions.back() = new_signed_literal(block[i].instruction->d);
				break;
			case Bytecode::BC_OP_KNUM:
				block[i].assignment.expressions.back() = new_number(function, block[i].instruction->d);
				break;
			case Bytecode::BC_OP_KPRI:
				block[i].assignment.expressions.back() = new_primitive(block[i].instruction->d);
				break;
			case Bytecode::BC_OP_KNIL:
				assert((uint8_t)block[i].instruction->d > block[i].instruction->a, L"Invalid KNIL instruction", DEBUG_INFO);
				block[i].assignment.expressions.back() = new_primitive(0);

				for (uint8_t j = block[i].instruction->d; j > block[i].instruction->a; j--) {
					block.emplace(i + 1, new_statement(AST_STATEMENT_ASSIGNMENT, function));
					block[i + 1].instruction = block[i].instruction;
					block[i + 1].assignment.variables.emplace_back();
					set_slot(block[i + 1].assignment.variables.back(), j);
					block[i + 1].assignment.expressions.emplace_back(new_primitive(0));
				}

				break;
			case Bytecode::BC_OP_UGET:
				assert((uint8_t)block[i].instruction->d < function.upvalues.size(), L"Invalid UGET instruction", DEBUG_INFO);
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_VARIABLE);
				block[i].assignment.expressions.back()->variable->type = AST_VARIABLE_UPVALUE;
				block[i].assignment.expressions.back()->variable->slotScope = function.upvalues[(uint8_t)block[i].instruction->d].slotScope;
				break;
			case Bytecode::BC_OP_USETV:
			case Bytecode::BC_OP_USETS:
			case Bytecode::BC_OP_USETN:
			case Bytecode::BC_OP_USETP:
				assert(block[i].instruction->a < function.upvalues.size(), L"Invalid USET instruction", DEBUG_INFO);
				block[i].assignment.variables.emplace_back();
				block[i].assignment.variables.back().type = AST_VARIABLE_UPVALUE;
				block[i].assignment.variables.back().slotScope = function.upvalues[block[i].instruction->a].slotScope;

				switch (block[i].instruction->type) {
				case Bytecode::BC_OP_USETV:
					block[i].assignment.expressions.back() = new_slot(block[i].instruction->d);
					block[i].assignment.register_slots(block[i].assignment.expressions.back());
					break;
				case Bytecode::BC_OP_USETS:
					block[i].assignment.expressions.back() = new_string(function, block[i].instruction->d);
					break;
				case Bytecode::BC_OP_USETN:
					block[i].assignment.expressions.back() = new_number(function, block[i].instruction->d);
					break;
				case Bytecode::BC_OP_USETP:
					block[i].assignment.expressions.back() = new_primitive(block[i].instruction->d);
					break;
				}

				continue;
			case Bytecode::BC_OP_FNEW:
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_FUNCTION);
				assert(function.get_constant(block[i].instruction->d).type == Bytecode::BC_KGC_CHILD, L"Constant is not a prototype", DEBUG_INFO);
				block[i].assignment.expressions.back()->function = new_function(*function.get_constant(block[i].instruction->d).prototype);
				function.childFunctions.emplace_back(block[i].assignment.expressions.back()->function);
				block[i].assignment.expressions.back()->function->upvalues.resize(block[i].assignment.expressions.back()->function->prototype.upvalues.size());

				for (size_t j = block[i].assignment.expressions.back()->function->upvalues.size(); j--;) {
					block[i].assignment.expressions.back()->function->upvalues[j].upvalue = &block[i].assignment.expressions.back()->function->prototype.upvalues[j];
					if (block[i].assignment.expressions.back()->function->upvalues[j].upvalue->local) continue;
					assert(block[i].assignment.expressions.back()->function->upvalues[j].upvalue->slot < function.upvalues.size(), L"Invalid non-local upvalue", DEBUG_INFO);
					block[i].assignment.expressions.back()->function->upvalues[j].slotScope = function.upvalues[block[i].assignment.expressions.back()->function->upvalues[j].upvalue->slot].slotScope;
				}

				break;
			case Bytecode::BC_OP_TNEW:
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_TABLE);
				break;
			case Bytecode::BC_OP_TDUP:
				block[i].assignment.expressions.back() = new_table(function, block[i].instruction->d);
				break;
			case Bytecode::BC_OP_GGET:
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_VARIABLE);
				block[i].assignment.expressions.back()->variable->type = AST_VARIABLE_GLOBAL;
				assert(function.get_constant(block[i].instruction->d).type == Bytecode::BC_KGC_STR, L"Constant is not a string", DEBUG_INFO);
				block[i].assignment.expressions.back()->variable->name = &function.get_constant(block[i].instruction->d).string;
				function.usedNames.emplace(*block[i].assignment.expressions.back()->variable->name);
				break;
			case Bytecode::BC_OP_GSET:
				block[i].assignment.variables.emplace_back();
				block[i].assignment.variables.back().type = AST_VARIABLE_GLOBAL;
				assert(function.get_constant(block[i].instruction->d).type == Bytecode::BC_KGC_STR, L"Constant is not a string", DEBUG_INFO);
				block[i].assignment.variables.back().name = &function.get_constant(block[i].instruction->d).string;
				function.usedNames.emplace(*block[i].assignment.variables.back().name);
				block[i].assignment.expressions.back() = new_slot(block[i].instruction->a);
				block[i].assignment.register_slots(block[i].assignment.expressions.back());
				continue;
			case Bytecode::BC_OP_TGETV:
			case Bytecode::BC_OP_TGETS:
			case Bytecode::BC_OP_TGETB:
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_VARIABLE);
				block[i].assignment.expressions.back()->variable->type = AST_VARIABLE_TABLE_INDEX;
				block[i].assignment.expressions.back()->variable->table = new_slot(block[i].instruction->b);
				block[i].assignment.register_slots(block[i].assignment.expressions.back()->variable->table);
				block[i].assignment.expressions.back()->variable->safeNavigation = false;
				block[i].assignment.expressions.back()->variable->isEndOfSafeNavChain = false;

				switch (block[i].instruction->type) {
				case Bytecode::BC_OP_TGETV:
					block[i].assignment.expressions.back()->variable->tableIndex = new_slot(block[i].instruction->c);
					block[i].assignment.register_slots(block[i].assignment.expressions.back()->variable->tableIndex);
					break;
				case Bytecode::BC_OP_TGETS:
					block[i].assignment.expressions.back()->variable->tableIndex = new_string(function, block[i].instruction->c);
					check_valid_name(block[i].assignment.expressions.back()->variable->tableIndex);
					break;
				case Bytecode::BC_OP_TGETB:
					block[i].assignment.expressions.back()->variable->tableIndex = new_literal(block[i].instruction->c);
					break;
				}

				break;
			case Bytecode::BC_OP_TSETV:
			case Bytecode::BC_OP_TSETS:
			case Bytecode::BC_OP_TSETB:
				block[i].assignment.variables.emplace_back();
				block[i].assignment.variables.back().type = AST_VARIABLE_TABLE_INDEX;
				block[i].assignment.variables.back().table = new_slot(block[i].instruction->b);
				block[i].assignment.variables.back().safeNavigation = false;
				block[i].assignment.variables.back().isEndOfSafeNavChain = false;

				switch (block[i].instruction->type) {
				case Bytecode::BC_OP_TSETV:
					block[i].assignment.variables.back().tableIndex = new_slot(block[i].instruction->c);
					block[i].assignment.register_slots(block[i].assignment.variables.back().tableIndex);
					break;
				case Bytecode::BC_OP_TSETS:
					block[i].assignment.variables.back().tableIndex = new_string(function, block[i].instruction->c);
					check_valid_name(block[i].assignment.variables.back().tableIndex);
					break;
				case Bytecode::BC_OP_TSETB:
					block[i].assignment.variables.back().tableIndex = new_literal(block[i].instruction->c);
					break;
				}

				block[i].assignment.expressions.back() = new_slot(block[i].instruction->a);
				block[i].assignment.register_slots(block[i].assignment.expressions.back());
				continue;
			case Bytecode::BC_OP_TSETM:
				block[i].assignment.variables.emplace_back();
				block[i].assignment.variables.back().type = AST_VARIABLE_TABLE_MULTRES;
				block[i].assignment.variables.back().table = new_slot(block[i].instruction->a - 1);
				assert(function.get_number_constant(block[i].instruction->d).type == Bytecode::BC_KNUM_NUM && (uint32_t)function.get_number_constant(block[i].instruction->d).number,
					L"Constant number is not a valid multres table index", DEBUG_INFO);
				block[i].assignment.variables.back().tableIndex = new_expression(AST_EXPRESSION_CONSTANT);
				block[i].assignment.variables.back().tableIndex->constant->type = AST_CONSTANT_NUMBER;
				block[i].assignment.variables.back().tableIndex->constant->number = (uint32_t)function.get_number_constant(block[i].instruction->d).number;
				block[i].assignment.expressions.back() = new_slot(block[i].instruction->a);
				block[i].assignment.expressions.back()->variable->type = AST_VARIABLE_SLOT_MULTRES;
				block[i].assignment.register_slots(block[i].assignment.expressions.back());
				continue;
			case Bytecode::BC_OP_CALLM:
			case Bytecode::BC_OP_CALL:
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_FUNCTION_CALL);
				
				if (block[i].instruction->b) {
					if (block[i].instruction->b == 1) {
						block[i].type = AST_STATEMENT_FUNCTION_CALL;
					} else {
						block[i].assignment.variables.resize((uint8_t)(block[i].instruction->b - 1));

						for (size_t j = block[i].assignment.variables.size(); j--;) {
							block[i].assignment.variables[j].type = AST_VARIABLE_SLOT;
							block[i].assignment.variables[j].slot = block[i].instruction->a + j;
						}
					}
				} else {
					block[i].assignment.variables.emplace_back();
					block[i].assignment.variables.back().type = AST_VARIABLE_SLOT_MULTRES;
					block[i].assignment.variables.back().slot = block[i].instruction->a;
				}

				if (block[i].instruction->b != 2) block[i].assignment.expressions.back()->isMultres = true;
				block[i].assignment.expressions.back()->functionCall->function = new_slot(block[i].instruction->a);
				block[i].assignment.register_slots(block[i].assignment.expressions.back()->functionCall->function);
				block[i].assignment.expressions.back()->functionCall->arguments.resize((uint8_t)(block[i].instruction->c + (block[i].instruction->type == Bytecode::BC_OP_CALLM ? 1 : -1)));

				for (size_t j = 0; j < block[i].assignment.expressions.back()->functionCall->arguments.size(); j++) {
					block[i].assignment.expressions.back()->functionCall->arguments[j] = new_slot(block[i].instruction->a + (bytecode.header.fr2 ? 2 : 1) + j);
					block[i].assignment.register_slots(block[i].assignment.expressions.back()->functionCall->arguments[j]);
				}

				if (block[i].instruction->type == Bytecode::BC_OP_CALLM) block[i].assignment.expressions.back()->functionCall->arguments.back()->variable->type = AST_VARIABLE_SLOT_MULTRES;
				continue;
			case Bytecode::BC_OP_VARG:
				block[i].assignment.expressions.back() = new_expression(AST_EXPRESSION_VARARG);

				if (block[i].instruction->b) {
					if (block[i].instruction->b == 1) {
						block[i].type = AST_STATEMENT_VARARG;
					} else {
						block[i].assignment.variables.resize((uint8_t)(block[i].instruction->b - 1));

						for (size_t j = block[i].assignment.variables.size(); j--;) {
							block[i].assignment.variables[j].type = AST_VARIABLE_SLOT;
							block[i].assignment.variables[j].slot = block[i].instruction->a + j;
						}
					}
				} else {
					block[i].assignment.variables.emplace_back();
					block[i].assignment.variables.back().type = AST_VARIABLE_SLOT_MULTRES;
					block[i].assignment.variables.back().slot = block[i].instruction->a;
				}

				if (block[i].instruction->b != 2) block[i].assignment.expressions.back()->isMultres = true;
				continue;
			case Bytecode::BC_OP_ITERC:
			case Bytecode::BC_OP_ITERN:
			case Bytecode::BC_OP_FORL:
			case Bytecode::BC_OP_ITERL:
				assert(false, L"Invalid for loop", DEBUG_INFO);
			}

			block[i].assignment.variables.emplace_back();
			set_slot(block[i].assignment.variables.back(), block[i].instruction->a);
			continue;
		case AST_STATEMENT_RETURN:
			switch (block[i].instruction->type) {
			case Bytecode::BC_OP_CALLMT:
			case Bytecode::BC_OP_CALLT:
				block[i].assignment.expressions.emplace_back(new_expression(AST_EXPRESSION_FUNCTION_CALL));
				block[i].assignment.expressions.back()->isMultres = true;
				block[i].assignment.expressions.back()->functionCall->function = new_slot(block[i].instruction->a);
				block[i].assignment.register_slots(block[i].assignment.expressions.back()->functionCall->function);
				block[i].assignment.expressions.back()->functionCall->arguments.resize((uint8_t)(block[i].instruction->d + (block[i].instruction->type == Bytecode::BC_OP_CALLMT ? 1 : -1)));

				for (size_t j = 0; j < block[i].assignment.expressions.back()->functionCall->arguments.size(); j++) {
					block[i].assignment.expressions.back()->functionCall->arguments[j] = new_slot(block[i].instruction->a + (bytecode.header.fr2 ? 2 : 1) + j);
					block[i].assignment.register_slots(block[i].assignment.expressions.back()->functionCall->arguments[j]);
				}

				if (block[i].instruction->type == Bytecode::BC_OP_CALLMT) block[i].assignment.expressions.back()->functionCall->arguments.back()->variable->type = AST_VARIABLE_SLOT_MULTRES;
				break;
			case Bytecode::BC_OP_RET:
				assert((uint8_t)block[i].instruction->d >= 3, L"Invalid RET instruction", DEBUG_INFO);
			case Bytecode::BC_OP_RETM:
			case Bytecode::BC_OP_RET1:
				block[i].assignment.expressions.resize(block[i].instruction->type == Bytecode::BC_OP_RET1 ? 1 : (uint8_t)(block[i].instruction->d + (block[i].instruction->type == Bytecode::BC_OP_RETM ? 1 : -1)));

				for (size_t j = 0; j < block[i].assignment.expressions.size(); j++) {
					block[i].assignment.expressions[j] = new_slot(block[i].instruction->a + j);
					block[i].assignment.register_slots(block[i].assignment.expressions[j]);
				}

				if (block[i].instruction->type == Bytecode::BC_OP_RETM) block[i].assignment.expressions.back()->variable->type = AST_VARIABLE_SLOT_MULTRES;
				break;
			}

			continue;
		case AST_STATEMENT_CONDITION:
			switch (block[i].instruction->type) {
			case Bytecode::BC_OP_ISLT:
			case Bytecode::BC_OP_ISGE:
			case Bytecode::BC_OP_ISLE:
			case Bytecode::BC_OP_ISGT:
			case Bytecode::BC_OP_ISEQV:
			case Bytecode::BC_OP_ISNEV:
			case Bytecode::BC_OP_ISEQS:
			case Bytecode::BC_OP_ISNES:
			case Bytecode::BC_OP_ISEQN:
			case Bytecode::BC_OP_ISNEN:
			case Bytecode::BC_OP_ISEQP:
			case Bytecode::BC_OP_ISNEP:
				block[i].assignment.expressions.resize(2);
				block[i].assignment.expressions[0] = new_slot(block[i].instruction->a);
				block[i].assignment.register_slots(block[i].assignment.expressions[0]);

				switch (block[i].instruction->type) {
				case Bytecode::BC_OP_ISLT:
				case Bytecode::BC_OP_ISGE:
				case Bytecode::BC_OP_ISLE:
				case Bytecode::BC_OP_ISGT:
					block[i].assignment.allowSlotSwap = true;
				case Bytecode::BC_OP_ISEQV:
				case Bytecode::BC_OP_ISNEV:
					block[i].assignment.expressions[1] = new_slot(block[i].instruction->d);
					block[i].assignment.register_slots(block[i].assignment.expressions[1]);
					break;
				case Bytecode::BC_OP_ISEQS:
				case Bytecode::BC_OP_ISNES:
					block[i].assignment.expressions[1] = new_string(function, block[i].instruction->d);
					break;
				case Bytecode::BC_OP_ISEQN:
				case Bytecode::BC_OP_ISNEN:
					block[i].assignment.expressions[1] = new_number(function, block[i].instruction->d);
					break;
				case Bytecode::BC_OP_ISEQP:
				case Bytecode::BC_OP_ISNEP:
					block[i].assignment.expressions[1] = new_primitive(block[i].instruction->d);
					break;
				}

				break;
			case Bytecode::BC_OP_ISTC:
			case Bytecode::BC_OP_ISFC:
				assert(block[i].instruction->a != (uint8_t)block[i].instruction->d, L"Invalid test-copy instruction", DEBUG_INFO);
				block[i].block.emplace(0, new_statement(AST_STATEMENT_ASSIGNMENT, function));
				block[i].block[0].instruction = block[i].instruction;
				block[i].block[0].assignment.variables.emplace_back();
				set_slot(block[i].block[0].assignment.variables.back(), block[i].instruction->a);
			case Bytecode::BC_OP_IST:
			case Bytecode::BC_OP_ISF:
				block[i].assignment.expressions.emplace_back(new_slot(block[i].instruction->d));
				if (block[i].block.size() == 2) block[i].block[0].assignment.expressions.emplace_back(block[i].assignment.expressions.back());
				block[i].assignment.register_slots(block[i].assignment.expressions.back());
				block[i].assignment.allowedConstantType = INVALID_CONSTANT;
				break;
			}

			continue;
		case AST_STATEMENT_NUMERIC_FOR:
			build_expressions(function, block[i].block);
			block[i].assignment.variables.emplace_back();
			set_slot(block[i].assignment.variables.back(), block[i].instruction->a + 3);
			assert(!function.prototype.header.debugInfo
				|| arguments.ignoreDebugInfo
				|| (block[i].assignment.locals
					&& block[i].assignment.locals->names.size() == 1
					&& block[i].assignment.variables.back().slot == block[i].assignment.locals->baseSlot),
				L"Numeric for loop variable does not match with debug info", DEBUG_INFO);
			block[i].assignment.expressions.resize(3);
			block[i].assignment.expressions[0] = new_slot(block[i].instruction->a);
			block[i].assignment.expressions[1] = new_slot(block[i].instruction->a + 1);
			block[i].assignment.expressions[2] = new_slot(block[i].instruction->a + 2);
			block[i].assignment.register_slots(block[i].assignment.expressions[0], block[i].assignment.expressions[1], block[i].assignment.expressions[2]);
			continue;
		case AST_STATEMENT_GENERIC_FOR:
			build_expressions(function, block[i].block);
			block[i].assignment.variables.resize((uint8_t)(block[i].instruction->b - 1));
			assert(block[i].assignment.variables.size(), L"Generic for loop has no variables", DEBUG_INFO);

			for (size_t j = block[i].assignment.variables.size(); j--;) {
				set_slot(block[i].assignment.variables[j], block[i].instruction->a + j);
			}

			assert(!function.prototype.header.debugInfo
				|| arguments.ignoreDebugInfo
				|| (block[i].assignment.locals
					&& block[i].assignment.variables.front().slot == block[i].assignment.locals->baseSlot
					&& block[i].assignment.variables.size() == block[i].assignment.locals->names.size()),
				L"Generic for loop variables do not match with debug info", DEBUG_INFO);
			block[i].assignment.expressions.resize(3);
			block[i].assignment.expressions[0] = new_slot(block[i].instruction->a - 3);
			block[i].assignment.expressions[1] = new_slot(block[i].instruction->a - 2);
			block[i].assignment.expressions[2] = new_slot(block[i].instruction->a - 1);
			block[i].assignment.register_slots(block[i].assignment.expressions[0], block[i].assignment.expressions[1], block[i].assignment.expressions[2]);
			continue;
		case AST_STATEMENT_LOOP:
			build_expressions(function, block[i].block);
			continue;
		case AST_STATEMENT_LOCAL:
			build_expressions(function, block[i].block);
			block[i].assignment.variables.resize(block[i].assignment.locals->names.size());
			block[i].assignment.expressions.resize(block[i].assignment.variables.size());

			for (size_t j = 0; j < block[i].assignment.variables.size(); j++) {
				set_slot(block[i].assignment.variables[j], block[i].assignment.locals->baseSlot + j);
				block[i].assignment.expressions[j] = new_slot(block[i].assignment.variables[j].slot);
				block[i].assignment.register_slots(block[i].assignment.expressions[j]);
			}

			continue;
		}
	}
}

void Ast::build_slot_scopes(Function& function, Block& block, const size_t& begin, const size_t& end) {
	auto build_nil_assignment = [this, &function](Block& block, const size_t& index, const Statement& next, const uint8_t& slot)->void {
		assert(function.slotScopeCollector.slotInfos[slot].activeSlotScope
			&& function.slotScopeCollector.slotInfos[slot].minScopeBegin > next.id
			&& ((next.instruction->type == Bytecode::BC_OP_KPRI
					&& !(uint8_t)next.instruction->d
					&& next.instruction->a == slot)
				|| (next.instruction->type == Bytecode::BC_OP_KNIL
					&& (uint8_t)next.instruction->d <= slot
					&& next.instruction->a >= slot)),
			L"Slot scope does not match with debug info", DEBUG_INFO);
		block.emplace(index, new_statement(AST_STATEMENT_ASSIGNMENT, function));
		block[index].instruction = next.instruction;
		block[index].assignment.expressions.emplace_back(new_primitive(0));
		block[index].assignment.variables.emplace_back();
		block[index].assignment.variables.back().type = AST_VARIABLE_SLOT;
		block[index].assignment.variables.back().slot = slot;
		function.slotScopeCollector.end_scope(block[index], slot, block[index].assignment.variables.back().slotScope);
	};

	auto get_next_assignment = [&block](bool& isPossibleCondition, uint8_t& targetSlot, std::vector<size_t>& conditionBlockBorders, std::vector<size_t>& assignmentIndexes, const bool& setTargetSlot = false)->void {
		isPossibleCondition = false;
		size_t index = conditionBlockBorders.back() - 1;

		if (block[index].type == AST_STATEMENT_ASSIGNMENT && block[index].assignment.variables.back().type == AST_VARIABLE_SLOT) {
			if (setTargetSlot) {
				isPossibleCondition = true;
				targetSlot = block[index].assignment.variables.back().slot;
			} else if (block[index].assignment.variables.back().slot == targetSlot) {
				isPossibleCondition = true;
			}
		} else if ((block[index].type == AST_STATEMENT_ASSIGNMENT
				&& (block[index].assignment.variables.back().type == AST_VARIABLE_TABLE_INDEX
					|| block[index].assignment.variables.back().type == AST_VARIABLE_TABLE_MULTRES))
			|| (block[index].type == AST_STATEMENT_CONDITION
				&& block[index].block.size() == 1)) {
			while (index--) {
				if (block[index].type != AST_STATEMENT_ASSIGNMENT
					|| block[index].assignment.variables.back().type != AST_VARIABLE_SLOT
					|| block[index].assignment.expressions.back()->type != AST_EXPRESSION_TABLE)
					continue;
				/*if (block[index].type == AST_STATEMENT_ASSIGNMENT
					&& block[index].assignment.variables.back().table->variable->slot != block[i].assignment.variables.back().slot)
					break;*/

				if (setTargetSlot) {
					isPossibleCondition = true;
					targetSlot = block[index].assignment.variables.back().slot;
					break;
				}
				
				if (block[index].assignment.variables.back().slot == targetSlot) {
					isPossibleCondition = true;
				}

				break;
			}
		}

		assignmentIndexes.emplace_back(index);
	};

	auto find_condition_blocks = [&function, &block, &get_next_assignment](const auto& self, Label* const& targetLabel, Label* const& extendedTargetLabel,
		bool& isPossibleCondition, uint8_t& targetSlot, std::vector<size_t>& conditionBlockBorders, std::vector<size_t>& assignmentIndexes) {
		for (size_t i = conditionBlockBorders.back(); isPossibleCondition && i--;) {
			if (conditionBlockBorders.size() != 1) get_next_assignment(isPossibleCondition, targetSlot, conditionBlockBorders, assignmentIndexes);
			i = assignmentIndexes.back();

			if (!isPossibleCondition || block[i].id < function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin) {
				conditionBlockBorders.emplace_back(i);
				return;
			}

			isPossibleCondition = false;

			while (i--) {
				switch (block[i].type) {
				case AST_STATEMENT_CONDITION:
					if ((*block[i].block.back().target != extendedTargetLabel->target
							|| block[i].instruction->type != Bytecode::BC_OP_ISNEP)
						&& (!targetLabel
							|| *block[i].block.back().target != targetLabel->target))
						continue;
					if (block[i].assignment.expressions.size() != 1 || block[i].instruction->type != Bytecode::BC_OP_ISNEP) return;
					conditionBlockBorders.emplace_back(i + 1);

					if (block[i].block.size() == 2) {
						if (block[i].block[0].assignment.variables.back().slot != targetSlot) return;
						isPossibleCondition = true;
						conditionBlockBorders.emplace_back(i);
						assignmentIndexes.emplace_back(i);
						if (block[i].id < function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin) return;
						continue;
					}

					if (block[i].assignment.expressions[0]->variable->slot != targetSlot || !i) return;
					conditionBlockBorders.emplace_back(i);
					get_next_assignment(isPossibleCondition, targetSlot, conditionBlockBorders, assignmentIndexes);
					if (!isPossibleCondition) return;

					if (!function.is_valid_label(block[i].label) || block[i].label->jumps.front() >= block[assignmentIndexes.back()].id) {
						conditionBlockBorders.pop_back();
						assignmentIndexes.pop_back();
						break;
					}

					if (block[i].label->jumps.back() >= block[i].id) {
						isPossibleCondition = false;
						return;
					}

					assignmentIndexes.pop_back();
					assignmentIndexes.emplace_back(i);
					self(self, block[i].label, block[i].label, isPossibleCondition, targetSlot, conditionBlockBorders, assignmentIndexes);
					i = assignmentIndexes.back();
					if (!isPossibleCondition || block[i].id < function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin) return;
					continue;
				case AST_STATEMENT_GOTO:
					if (*block[i].target != extendedTargetLabel->target
						&& (!targetLabel
							|| *block[i].target != targetLabel->target))
						continue;
					if (!i
						|| (*block[i].target != extendedTargetLabel->target
							&& (function.is_valid_label(block[i].label)
								|| block[i - 1].type != AST_STATEMENT_ASSIGNMENT
								|| !get_constant_type(block[i - 1].assignment.expressions.back()))))
						return;
					conditionBlockBorders.emplace_back(i + 1);

					if (!function.is_valid_label(block[i].label)) {
						isPossibleCondition = true;
						break;
					}

					if (*block[i].target != extendedTargetLabel->target || block[i].label->jumps.back() >= block[i].id) return;
					conditionBlockBorders.emplace_back(i);
					assignmentIndexes.emplace_back(i);
					isPossibleCondition = true;
					self(self, block[i].label, extendedTargetLabel, isPossibleCondition, targetSlot, conditionBlockBorders, assignmentIndexes);
					i = assignmentIndexes.back();
					if (!isPossibleCondition || block[i].id < function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin) return;
				default:
					continue;
				}

				break;
			}
		}
	};

	for (size_t i = end == INVALID_ID ? block.size() : end; i-- && i >= begin;) {
		switch (block[i].type) {
		case AST_STATEMENT_NUMERIC_FOR:
		case AST_STATEMENT_GENERIC_FOR:
		case AST_STATEMENT_LOOP:
			function.slotScopeCollector.extend_scopes(block[i]);
			build_slot_scopes(function, block[i].block);

			for (size_t j = block[i].assignment.variables.size(); j--;) {
				function.slotScopeCollector.begin_scope(block[i], block[i].assignment.variables[j].slot);
			}

			function.slotScopeCollector.merge_scopes(function.get_statement_from_id(block[i].block.get_end_id() - (block[i].type == AST_STATEMENT_LOOP ? 1 : (block[i].type == AST_STATEMENT_NUMERIC_FOR ? 2 : 3))));
			//function.slotScopeCollector.merge_scopes(function.get_statement_from_id(block[i].block.get_end_id() - 1));
			break;
		case AST_STATEMENT_LOCAL:
			for (size_t j = function.slotScopeCollector.slotInfos.size(); j-- && j >= block[i].assignment.variables.front().slot;) {
				if (!function.slotScopeCollector.slotInfos[j].activeSlotScope) continue;
				Statement& next = function.get_statement_from_id(block[i].assignment.locals->scopeEnd);

				for (size_t k = j + 1; k-- && k >= block[i].assignment.variables.front().slot;) {
					build_nil_assignment(block, i + 1, next, k);
				}

				break;
			}

			build_slot_scopes(function, block[i].block);

			for (size_t j = function.slotScopeCollector.slotInfos.size(); j-- && j > block[i].assignment.variables.back().slot;) {
				if (!function.slotScopeCollector.slotInfos[j].activeSlotScope) continue;

				for (size_t k = j + 1; k-- && k > block[i].assignment.variables.back().slot;) {
					build_nil_assignment(block[i].block, 0, block[i], k);
				}

				break;
			}

			for (size_t j = block[i].assignment.variables.size(); j--;) {
				function.slotScopeCollector.begin_scope(block[i], block[i].assignment.variables[j].slot);
			}

			function.slotScopeCollector.merge_scopes(function.get_statement_from_id(block[i].block.get_end_id() - 1));
			break;
		case AST_STATEMENT_CONDITION:
			if (block[i].block.size() == 2) {
				function.slotScopeCollector.extend_scopes(block[i]);
				build_slot_scopes(function, block[i].block);
				break;
			}
		case AST_STATEMENT_ASSIGNMENT:
			if (block[i].assignment.variables.size() > 1) break;
			Label* targetLabel = function.get_label_from_id(block[i].block.get_end_id(), false, true);
			Label* extendedTargetLabel = function.get_label_from_id(block[i].block.get_end_id(), true, true);
			if (!extendedTargetLabel) break;
			bool isPossibleCondition;
			uint8_t targetSlot;
			std::vector<size_t> conditionBlockBorders = { i + 1 };
			std::vector<size_t> assignmentIndexes;
			get_next_assignment(isPossibleCondition, targetSlot, conditionBlockBorders, assignmentIndexes, true);
			if (!isPossibleCondition
				|| function.slotScopeCollector.slotInfos[targetSlot].isParameter
				|| !function.slotScopeCollector.slotInfos[targetSlot].activeSlotScope
				|| function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin >= block[assignmentIndexes.back()].id
				|| function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin <= block[begin].id
				|| (extendedTargetLabel->target <= block[i].id
					&& extendedTargetLabel->target >= function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin)
				|| (targetLabel
					? (targetLabel == extendedTargetLabel
						? function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin != targetLabel->jumps.front()
						: function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin != targetLabel->jumps.front()
						&& (function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin != extendedTargetLabel->jumps.front()
							|| extendedTargetLabel->jumps.front() > targetLabel->jumps.front()))
					: function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin != extendedTargetLabel->jumps.front()))
				break;
			find_condition_blocks(find_condition_blocks, targetLabel, extendedTargetLabel, isPossibleCondition, targetSlot, conditionBlockBorders, assignmentIndexes);
			if (!isPossibleCondition || conditionBlockBorders.back() < begin) break;

			for (size_t j = conditionBlockBorders.size() - 1; isPossibleCondition && j--;) {
				for (size_t k = conditionBlockBorders[j + 1]; k < conditionBlockBorders[j]; k++) {
					if (!function.is_valid_label(block[k].label)
						|| ((block[k].label->jumps.back() < block[k].id
								|| k == conditionBlockBorders.back())
							&& (k <= assignmentIndexes[j]
								|| block[k].label->jumps.front() > block[assignmentIndexes[j]].id))) {
						switch (block[k].type) {
						case AST_STATEMENT_CONDITION:
							if (k < assignmentIndexes[j]
								? *block[k].block.back().target > block[k].id
									&& (*block[k].block.back().target <= block[assignmentIndexes[j]].id
										|| (block[k].block.size() == 1
											&& *block[k].block.back().target > block[conditionBlockBorders[j] - 1].id
											&& *block[k].block.back().target <= block[assignmentIndexes.back()].id))
								: ((!block[k].assignment.openSlots.size()
											|| (block[k].assignment.expressions[0]->variable->slot != targetSlot
												&& (block[k].assignment.openSlots.size() == 1
													|| block[k].assignment.expressions[1]->variable->slot != targetSlot)
												&& (block[k].block.size() == 1
													|| block[k].block[0].assignment.variables.back().slot != targetSlot))
											|| k == conditionBlockBorders[j] - 1)
										&& *block[k].block.back().target > block[k].id
										&& (*block[k].block.back().target < block[conditionBlockBorders[j] - 1].id
											|| (*block[k].block.back().target == block[conditionBlockBorders[j] - 1].id
												&& conditionBlockBorders[j] - 1 == i)
											|| k == conditionBlockBorders[j] - 1))
									|| (*block[k].block.back().target == extendedTargetLabel->target
										&& k == conditionBlockBorders[j] - 1))
								continue;
							break;
						case AST_STATEMENT_GOTO:
							if (block[k].instruction->type == Bytecode::BC_OP_JMP
								&& ((*block[k].target > block[k].id
										&& (k < assignmentIndexes[j]
											? *block[k].target <= block[assignmentIndexes[j]].id
											: *block[k].target < block[conditionBlockBorders[j] - 1].id
												|| (*block[k].target == block[conditionBlockBorders[j] - 1].id
													&& conditionBlockBorders[j] - 1 == i)
												|| k == conditionBlockBorders[j] - 1))
									|| (*block[k].target == extendedTargetLabel->target
										&& k == conditionBlockBorders[j] - 1)))
								continue;
							break;
						case AST_STATEMENT_ASSIGNMENT:
							if (block[k].assignment.variables.size() != 1) break;
							switch (block[k].assignment.variables.back().type) {
							case AST_VARIABLE_SLOT:
								if (block[k].assignment.expressions.back()->type == AST_EXPRESSION_FUNCTION && block[k].assignment.usedLocals.contains(targetSlot)) break;
							case AST_VARIABLE_SLOT_MULTRES:
							case AST_VARIABLE_TABLE_INDEX:
							case AST_VARIABLE_TABLE_MULTRES:
								if (k <= assignmentIndexes[j]) continue;

								for (size_t l = block[k].assignment.openSlots.size(); l--;) {
									if ((*block[k].assignment.openSlots[l])->variable->slot != targetSlot) continue;
									isPossibleCondition = false;
									break;
								}

								if (isPossibleCondition) continue;
							}

							break;
						}
					}

					isPossibleCondition = false;
					break;
				}
			}

			if (!isPossibleCondition) break;
			SlotScope* targetSlotScope = function.slotScopeCollector.slotInfos[targetSlot].activeSlotScope;
			size_t slotScopeIndex = function.slotScopeCollector.slotInfos[targetSlot].slotScopes.size() - 1;
			IdReference minScopeBegin = function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin;
			function.slotScopeCollector.slotInfos[targetSlot].minScopeBegin = block[i];
			i += 1;

			for (size_t j = 1; j < conditionBlockBorders.size(); j++) {
				if (!function.slotScopeCollector.slotInfos[targetSlot].activeSlotScope) {
					targetSlotScope->usages++;
					function.slotScopeCollector.slotInfos[targetSlot].activeSlotScope = targetSlotScope;
					function.slotScopeCollector.slotInfos[targetSlot].slotScopes.erase(function.slotScopeCollector.slotInfos[targetSlot].slotScopes.begin() + slotScopeIndex);
					function.slotScopeCollector.slotInfos[targetSlot].slotScopes.emplace_back(targetSlotScope);
					slotScopeIndex = function.slotScopeCollector.slotInfos[targetSlot].slotScopes.size() - 1;
				}

				size_t end = i;
				i = conditionBlockBorders[j];

				if (block[end - 1].type == AST_STATEMENT_CONDITION && block[end - 1].block.size() == 2) {
					build_slot_scopes(function, block[end - 1].block);
					function.slotScopeCollector.attach_scope(block[end - 1], (*block[end - 1].assignment.openSlots.back())->variable->slot, (*block[end - 1].assignment.openSlots.back())->variable->slotScope);
					build_slot_scopes(function, block, i, end - 1);
				} else {
					build_slot_scopes(function, block, i, end);
				}

				if (!function.slotScopeCollector.slotInfos[targetSlot].activeSlotScope
					|| (i - end == 1
						&& (block[i].type == AST_STATEMENT_CONDITION
							|| block[i].type == AST_STATEMENT_GOTO)
						&& function.is_valid_label(block[i].label))
					|| j == conditionBlockBorders.size() - 1)
					continue;
				function.slotScopeCollector.merge_scope(targetSlot, block[conditionBlockBorders.front() - 1]);
				function.slotScopeCollector.extend_scope(targetSlot, minScopeBegin);

				for (size_t j = conditionBlockBorders.front(); j-- > i;) {
					if (!function.is_valid_label(block[j].label)) continue;
					function.slotScopeCollector.extend_scopes(block[j].label->jumps.front());
				}

				break;
			}

			continue;
		}

		if (block[i].type == AST_STATEMENT_ASSIGNMENT && block[i].assignment.expressions.back()->type == AST_EXPRESSION_FUNCTION) {
			for (size_t j = block[i].assignment.expressions.back()->function->upvalues.size(); j--;) {
				if (!block[i].assignment.expressions.back()->function->upvalues[j].upvalue->local) continue;
				function.slotScopeCollector.attach_scope(block[i], block[i].assignment.expressions.back()->function->upvalues[j].upvalue->slot, block[i].assignment.expressions.back()->function->upvalues[j].slotScope);
				block[i].assignment.usedLocals.emplace(block[i].assignment.expressions.back()->function->upvalues[j].upvalue->slot);
				function.build_upvalue_scope(block[i].id, block[i].assignment.expressions.back()->function->upvalues[j].upvalue->slot);
			}
		}

		for (size_t j = block[i].assignment.variables.size(); j--;) {
			IdReference minScopeBegin;

			switch (block[i].assignment.variables[j].type) {
			case AST_VARIABLE_SLOT:
				if (block[i].type == AST_STATEMENT_LOCAL) {
					minScopeBegin = function.slotScopeCollector.slotInfos[block[i].assignment.variables[j].slot].minScopeBegin;
					function.slotScopeCollector.slotInfos[block[i].assignment.variables[j].slot].minScopeBegin = block[i];
				}
			case AST_VARIABLE_SLOT_MULTRES:
				function.slotScopeCollector.end_scope(block[i], block[i].assignment.variables[j].slot, block[i].assignment.variables[j].slotScope);
				if (block[i].type == AST_STATEMENT_LOCAL) function.slotScopeCollector.slotInfos[block[i].assignment.variables[j].slot].minScopeBegin = minScopeBegin;
				continue;
			case AST_VARIABLE_TABLE_INDEX:
			case AST_VARIABLE_TABLE_MULTRES:
				function.slotScopeCollector.attach_scope(block[i], block[i].assignment.variables[j].table->variable->slot, block[i].assignment.variables[j].table->variable->slotScope);
				continue;
			}
		}

		for (size_t j = block[i].assignment.openSlots.size(); j--;) {
			function.slotScopeCollector.attach_scope(block[i], (*block[i].assignment.openSlots[j])->variable->slot, (*block[i].assignment.openSlots[j])->variable->slotScope);
		}

		if (!function.is_valid_label(block[i].label)) continue;
		IdReference scopeEnd = function.get_scope_end_from_label(block[i].label);
		function.slotScopeCollector.merge_scopes(scopeEnd);
		function.slotScopeCollector.extend_scopes(function.get_scope_begin_from_label(block[i].label, scopeEnd));
	}
}

void Ast::eliminate_slots(Function& function, Block& block) {
	// for tables use table.constants.array[0]
	// has_self_reference -> usedSlots/usedLocals

	auto get_next_assignment = [&](const size_t& index, const bool& ignoreIndex = false)->size_t {
		if (!index) return INVALID_ID;
		Label* targetLabel = function.get_label_from_id(block[index - 1].block.get_end_id(), true, ignoreIndex);
		if (!targetLabel) return INVALID_ID;
		size_t next = index;

		while (--next && block[next].type == AST_STATEMENT_CONDITION) {
			if (block[next].block.size() == 2
				|| ((*block[next].block.back().target <= block[next].id
						|| *block[next].block.back().target > block[index - 1].id)
					&& *block[next].block.back().target != targetLabel->target))
				return INVALID_ID;
		}

		if (block[next].type != AST_STATEMENT_ASSIGNMENT
			|| block[next].assignment.variables.size() != 1
			|| ((!ignoreIndex
					|| next + 1 != index)
				&& function.is_valid_label(block[next + 1].label)))
			return INVALID_ID;

		for (size_t i = next + 2; i <= index; i++) {
			if (i == index && ignoreIndex) break;
			if (function.is_valid_label(block[i].label)
				&& (block[i].label->jumps.front() <= block[next].id
					|| block[i].label->jumps.back() >= block[i].id))
				return INVALID_ID;
		}

		return next;
	};

	auto try_eliminate_condition = [&](const size_t& upperIndex, size_t& lowerIndex, Expression** const& currentOpenSlot = nullptr)->bool {
		if (upperIndex + 1 == lowerIndex) return true;
		Expression** expression = nullptr;
		bool mergeDownwards = true;

		if (currentOpenSlot) {
			switch (block[lowerIndex].type) {
			case AST_STATEMENT_ASSIGNMENT:
				if (block[lowerIndex].assignment.variables.size() == 1 && block[lowerIndex].assignment.variables.back().type == AST_VARIABLE_TABLE_INDEX) {
					if (currentOpenSlot == &block[lowerIndex].assignment.variables.back().table) {
						expression = &block[lowerIndex].assignment.variables.back().tableIndex;
						break;
					}
					
					if (currentOpenSlot == &block[lowerIndex].assignment.variables.back().tableIndex) expression = &block[lowerIndex].assignment.expressions.back();
					break;
				}
			case AST_STATEMENT_RETURN:
			case AST_STATEMENT_FUNCTION_CALL:
				if (block[lowerIndex].type != AST_STATEMENT_RETURN || block[lowerIndex].assignment.expressions.size() == 1) {
					switch (block[lowerIndex].assignment.expressions.back()->type) {
					case AST_EXPRESSION_FUNCTION_CALL:
						if (block[lowerIndex].assignment.expressions.back()->functionCall->function->variable->type != AST_VARIABLE_SLOT) break;

						if (currentOpenSlot == &block[lowerIndex].assignment.expressions.back()->functionCall->function) {
							if (block[lowerIndex].assignment.expressions.back()->functionCall->arguments.size() && !block[lowerIndex].assignment.expressions.back()->functionCall->arguments.front()->isMultres)
								expression = &block[lowerIndex].assignment.expressions.back()->functionCall->arguments.front();
							break;
						}

						for (size_t i = 0; i < block[lowerIndex].assignment.expressions.back()->functionCall->arguments.size(); i++) {
							if (currentOpenSlot != &block[lowerIndex].assignment.expressions.back()->functionCall->arguments[i]) continue;
							if (i + 1 < block[lowerIndex].assignment.expressions.back()->functionCall->arguments.size() && !block[lowerIndex].assignment.expressions.back()->functionCall->arguments[i + 1]->isMultres)
								expression = &block[lowerIndex].assignment.expressions.back()->functionCall->arguments[i + 1];
							break;
						}

						break;
					case AST_EXPRESSION_BINARY_OPERATION:
						if (block[lowerIndex].assignment.expressions.back()->binaryOperation->type != AST_BINARY_CONCATENATION) {
							if (currentOpenSlot == &block[lowerIndex].assignment.expressions.back()->binaryOperation->leftOperand)
								expression = &block[lowerIndex].assignment.expressions.back()->binaryOperation->rightOperand;
							break;
						}

						for (BinaryOperation* concatenation = block[lowerIndex].assignment.expressions.back()->binaryOperation;
							concatenation->type == AST_BINARY_CONCATENATION;
							concatenation = concatenation->rightOperand->binaryOperation) {
							if (currentOpenSlot != &concatenation->leftOperand) continue;
							expression = concatenation->rightOperand->type == AST_EXPRESSION_BINARY_OPERATION && concatenation->rightOperand->binaryOperation->type == AST_BINARY_CONCATENATION
								? &concatenation->rightOperand->binaryOperation->leftOperand : &concatenation->rightOperand;
							break;
						}

						break;
					}

					break;
				}
			case AST_STATEMENT_CONDITION:
			case AST_STATEMENT_NUMERIC_FOR:
			case AST_STATEMENT_GENERIC_FOR:
			case AST_STATEMENT_LOCAL:
				for (size_t i = 0; i < block[lowerIndex].assignment.expressions.size(); i++) {
					if (currentOpenSlot != &block[lowerIndex].assignment.expressions[i]) continue;
					if (i + 1 < block[lowerIndex].assignment.expressions.size() && !block[lowerIndex].assignment.expressions[i + 1]->isMultres) expression = &block[lowerIndex].assignment.expressions[i + 1];
					break;
				}

				break;
			}
		}

		if (!expression
			&& block[upperIndex].assignment.expressions.back()->type == AST_EXPRESSION_TABLE
			&& (!block[upperIndex].assignment.expressions.back()->table->fields.size()
				|| !block[upperIndex].assignment.expressions.back()->table->fields.back().value->isMultres)) {
			if (block[upperIndex].assignment.variables.back().type == AST_VARIABLE_SLOT) {
				for (size_t i = upperIndex + 1; i < lowerIndex; i++) {
					if (block[i].assignment.usedLocals.contains(block[upperIndex].assignment.variables.back().slot)) return false;
				}
			}

			mergeDownwards = false;
			block[upperIndex].assignment.expressions.back()->table->fields.emplace_back(Table::Field { .key = new_expression(AST_EXPRESSION_CONSTANT),
				.value = block[upperIndex].assignment.expressions.back()->table->constants.list.size() ? block[upperIndex].assignment.expressions.back()->table->constants.list[0] : new_primitive(0)});
			block[upperIndex].assignment.expressions.back()->table->fields.back().key->constant->type = AST_CONSTANT_NUMBER;
			block[upperIndex].assignment.expressions.back()->table->fields.back().key->constant->number = 0;
			expression = &block[upperIndex].assignment.expressions.back()->table->fields.back().value;
		}

		if (!expression) return false;
		Label* endTarget = (Label*)&endTarget;
		ConditionDecompiler conditionDecompiler(this, endTarget, nullptr, nullptr);

		for (size_t i = upperIndex + 1; i < lowerIndex; i++) {
			conditionDecompiler.add_node(conditionDecompiler.get_node_type(block[i]), block[i].label, function.get_label_from_id(*block[i].target, false, false), block[i].assignment.expressions);
		}

		conditionDecompiler.add_node(ConditionDecompiler::Node::TRUTHY_TEST, conditionDecompiler.conditionNodes.back()->targetLabel, endTarget, std::vector<Expression*> { *expression });
		*expression = conditionDecompiler();
		assert(*expression, L"Failed to decompile condition", DEBUG_INFO);

		while (--lowerIndex > upperIndex) {
			function.remove_jump(block[lowerIndex].block.back());
			if (block[lowerIndex].instruction->type == Bytecode::BC_OP_JMP) function.remove_jump(block[lowerIndex]);
			block.merge(lowerIndex, mergeDownwards);
		}

		return true;
	};

	for (size_t i = 0; i < block.size(); i++) {
		switch (block[i].type) {
		case AST_STATEMENT_RETURN:
			if (!i
				|| function.is_valid_label(block[i].label)
				|| block[i - 1].type != AST_STATEMENT_EMPTY
				|| block[i - 1].instruction->type != Bytecode::BC_OP_UCLO)
				break;
			block.merge(i);
			i--;
			break;
		case AST_STATEMENT_LOCAL:
			if (block[i].assignment.openSlots.size() == 1
				&& (*(*block[i].assignment.openSlots.back())->variable->slotScope)->usages >= 3
				&& *(*block[i].assignment.openSlots.back())->variable->slotScope == *block[i].assignment.variables.back().slotScope
				&& (*block[i].assignment.variables.back().slotScope)->isUpvalue
				&& (*block[i].assignment.variables.back().slotScope)->scopeBegin == block[i].id - 1
				&& i
				&& block[i - 1].type == AST_STATEMENT_ASSIGNMENT
				&& block[i - 1].assignment.variables.size() == 1
				&& block[i - 1].assignment.variables.back().type == AST_VARIABLE_SLOT
				&& block[i - 1].assignment.expressions.back()->type == AST_EXPRESSION_FUNCTION) {
				*block[i].assignment.openSlots.back() = block[i - 1].assignment.expressions.back();
				block[i].assignment.openSlots.clear();
				block.merge(i);
				i--;
				(*block[i].assignment.variables.back().slotScope)->usages -= 2;
				break;
			}
		case AST_STATEMENT_GENERIC_FOR:
			{
				if (!i || function.is_valid_label(block[i].label)) break;

				if (block[i - 1].type == AST_STATEMENT_ASSIGNMENT && block[i - 1].assignment.variables.size() != 1) {
					if (block[i - 1].assignment.variables.back().slot != block[i].assignment.expressions.back()->variable->slot
						|| block[i - 1].assignment.variables.front().slot < block[i].assignment.expressions.front()->variable->slot)
						break;

					for (size_t j = block[i - 1].assignment.variables.size(); j--;) {
						if ((*block[i - 1].assignment.variables[j].slotScope)->usages != 1) break;
						if (j) continue;

						while (block[i].assignment.expressions.size() && block[i].assignment.expressions.back()->variable->slot >= block[i - 1].assignment.variables.front().slot) {
							function.slotScopeCollector.remove_scope(block[i].assignment.expressions.back()->variable->slot, block[i].assignment.expressions.back()->variable->slotScope);
							block[i].assignment.expressions.pop_back();
							block[i].assignment.openSlots.pop_back();
						}

						block[i].assignment.expressions.emplace_back(block[i - 1].assignment.expressions.back());
						block.merge(i);
						i--;
					}

					break;
				}

				switch (block[i - 1].type) {
				case AST_STATEMENT_FUNCTION_CALL:
				case AST_STATEMENT_VARARG:
					block[i].assignment.expressions.emplace_back(block[i - 1].assignment.expressions.back());
					block.merge(i);
					i--;
				}

				for (size_t next = get_next_assignment(i, block[i].type == AST_STATEMENT_LOCAL && block[i].assignment.expressions.size() == block[i].assignment.openSlots.size());
					next != INVALID_ID
					&& block[next].assignment.variables.back().type == AST_VARIABLE_SLOT
					&& block[next].assignment.variables.back().slot > block[i].assignment.expressions[block[i].assignment.openSlots.size() - 1]->variable->slot
					&& !(*block[next].assignment.variables.back().slotScope)->usages
					&& try_eliminate_condition(next, i, block[i].assignment.openSlots.back());
					next = get_next_assignment(i)) {
					block[i].assignment.expressions.emplace(block[i].assignment.expressions.begin() + block[i].assignment.openSlots.size(), block[next].assignment.expressions.back());
					function.slotScopeCollector.remove_scope(block[next].assignment.variables.back().slot, block[next].assignment.variables.back().slotScope);
					block.merge(i);
					i--;
				}
			}

			break;
		case AST_STATEMENT_ASSIGNMENT:
			switch (block[i].assignment.variables.back().type) {
			case AST_VARIABLE_SLOT:
				if (block[i].assignment.expressions.back()->type == AST_EXPRESSION_BINARY_OPERATION
					&& block[i].assignment.expressions.back()->binaryOperation->type != AST_BINARY_CONCATENATION
					&& block[i].assignment.openSlots.size() == 2
					&& i >= 2
					&& !function.is_valid_label(block[i].label)
					&& !function.is_valid_label(block[i - 1].label)
					&& block[i - 1].type == AST_STATEMENT_ASSIGNMENT
					&& block[i - 1].assignment.variables.size() == 1
					&& block[i - 1].assignment.variables.back().type == AST_VARIABLE_SLOT
					&& (*block[i - 1].assignment.variables.back().slotScope)->usages == 1
					&& block[i - 1].assignment.variables.back().slot == block[i].assignment.expressions.back()->binaryOperation->leftOperand->variable->slot
					&& get_constant_type(block[i - 1].assignment.expressions.back()) == NUMBER_CONSTANT
					&& block[i - 2].type == AST_STATEMENT_ASSIGNMENT
					&& block[i - 2].assignment.variables.size() == 1
					&& block[i - 2].assignment.variables.back().type == AST_VARIABLE_SLOT
					&& (*block[i - 2].assignment.variables.back().slotScope)->usages == 1
					&& block[i - 2].assignment.variables.back().slot == block[i].assignment.expressions.back()->binaryOperation->rightOperand->variable->slot)
					std::swap<Expression**>(block[i].assignment.openSlots[0], block[i].assignment.openSlots[1]);
				break;
			case AST_VARIABLE_TABLE_INDEX:
				if (block[i].assignment.openSlots.size() == 2
					&& i >= 2
					&& !function.is_valid_label(block[i].label)
					&& !function.is_valid_label(block[i - 1].label)
					&& block[i - 1].type == AST_STATEMENT_ASSIGNMENT
					&& block[i - 1].assignment.variables.size() == 1
					&& block[i - 1].assignment.variables.back().type == AST_VARIABLE_SLOT
					&& (*block[i - 1].assignment.variables.back().slotScope)->usages == 1
					&& block[i - 1].assignment.variables.back().slot == block[i].assignment.variables.back().tableIndex->variable->slot
					&& get_constant_type(block[i - 1].assignment.expressions.back())
					&& block[i - 2].type == AST_STATEMENT_ASSIGNMENT
					&& block[i - 2].assignment.variables.size() == 1
					&& block[i - 2].assignment.variables.back().type == AST_VARIABLE_SLOT
					&& (*block[i - 2].assignment.variables.back().slotScope)->usages == 1
					&& block[i - 2].assignment.variables.back().slot == block[i].assignment.expressions.back()->variable->slot
					&& (!get_constant_type(block[i - 2].assignment.expressions.back())
						|| get_constant_type(block[i - 1].assignment.expressions.back()) == NIL_CONSTANT))
					//TODO check for table constructor?
					std::swap<Expression**>(block[i].assignment.openSlots[0], block[i].assignment.openSlots[1]);
				break;
			}

			break;
		}

		for (size_t j = block[i].assignment.openSlots.size(); j--;) {
			size_t next = get_next_assignment(i);
			if (next == INVALID_ID
				|| (block[next].assignment.variables.back().type != AST_VARIABLE_SLOT
					&& block[next].assignment.variables.back().type != AST_VARIABLE_SLOT_MULTRES)
				|| (*block[next].assignment.variables.back().slotScope)->usages != 1)
				break;

			while (j && (*block[i].assignment.openSlots[j])->variable->slot != block[next].assignment.variables.back().slot) {
				if (j == 1
					&& block[i].assignment.expressions.front()->type == AST_EXPRESSION_FUNCTION_CALL
					&& i >= 2
					&& next + 1 == i
					&& !function.is_valid_label(block[next].label)
					&& block[next].assignment.variables.back().slot == block[i].assignment.expressions.back()->functionCall->function->variable->slot
					&& block[next].assignment.expressions.back()->type == AST_EXPRESSION_VARIABLE
					&& block[next].assignment.expressions.back()->variable->type == AST_VARIABLE_TABLE_INDEX
					&& block[next].assignment.expressions.back()->variable->table->type == AST_EXPRESSION_VARIABLE
					&& block[next].assignment.expressions.back()->variable->table->variable->type == AST_VARIABLE_SLOT
					&& block[next].assignment.expressions.back()->variable->tableIndex->type == AST_EXPRESSION_CONSTANT
					&& block[next].assignment.expressions.back()->variable->tableIndex->constant->type == AST_CONSTANT_STRING
					&& block[next].assignment.expressions.back()->variable->tableIndex->constant->isName
					&& block[next - 1].type == AST_STATEMENT_ASSIGNMENT
					&& block[next - 1].assignment.variables.size() == 1
					&& block[next - 1].assignment.variables.back().type == AST_VARIABLE_SLOT
					&& block[next - 1].assignment.variables.back().slot == block[i].assignment.expressions.back()->functionCall->arguments.front()->variable->slot
					&& (*block[next - 1].assignment.variables.back().slotScope)->usages == 1
					&& block[next - 1].assignment.expressions.back()->type == AST_EXPRESSION_VARIABLE
					&& block[next - 1].assignment.expressions.back()->variable->type == AST_VARIABLE_SLOT
					&& block[next - 1].assignment.expressions.back()->variable->slot == block[next].assignment.expressions.back()->variable->table->variable->slot) {
					block[i].assignment.expressions.back()->functionCall->isMethod = true;
					block[i].assignment.expressions.back()->functionCall->arguments.erase(block[i].assignment.expressions.back()->functionCall->arguments.begin());
					block[i].assignment.openSlots.erase(block[i].assignment.openSlots.begin() + 1);
					block[i].assignment.openSlots.emplace(block[i].assignment.openSlots.begin(), &block[next].assignment.expressions.back()->variable->table);
					function.slotScopeCollector.remove_scope(block[next - 1].assignment.variables.back().slot, block[next - 1].assignment.variables.back().slotScope);
					(*block[next - 1].assignment.expressions.back()->variable->slotScope)->usages--;
					block[next - 1].assignment.usedLocals.clear();
					block.merge(next);
					i--;
					next--;
					break;
				}

				j--;
			}

			if (!j && (*block[i].assignment.openSlots[j])->variable->slot != block[next].assignment.variables.back().slot) break;
			assert((*block[i].assignment.openSlots[j])->variable->type == block[next].assignment.variables.back().type, L"Multres type mismatch when trying to eliminate slot", DEBUG_INFO);
			Expression* slot = *block[i].assignment.openSlots[j];
			*block[i].assignment.openSlots[j] = block[next].assignment.expressions.back();

			if ((!j
					&& block[i].assignment.allowedConstantType < NUMBER_CONSTANT
					&& get_constant_type(block[i].assignment.expressions.back()) > block[i].assignment.allowedConstantType)
				|| !try_eliminate_condition(next, i, block[i].assignment.openSlots[j])) {
				*block[i].assignment.openSlots[j] = slot;
				break;
			}

			if (!j
				&& block[i].assignment.allowSlotSwap
				&& block[i].assignment.openSlots.size() == 2) {
				j++;
				std::swap<Expression*>(block[i].assignment.expressions[0], block[i].assignment.expressions[1]);
				block[i].assignment.swapped = true;
			}

			block[i].assignment.openSlots.erase(block[i].assignment.openSlots.begin() + j);
			function.slotScopeCollector.remove_scope(block[next].assignment.variables.back().slot, block[next].assignment.variables.back().slotScope);
			block.merge(i);
			i--;
		}

		assert(!block[i].assignment.openSlots.size() || (*block[i].assignment.openSlots.back())->type != AST_VARIABLE_SLOT_MULTRES, L"Failed to eliminate multres slot", DEBUG_INFO);

		for (size_t j = block[i].assignment.openSlots.size(); j--;) {
			block[i].assignment.usedLocals.emplace((*block[i].assignment.openSlots[j])->variable->slot);
		}

		switch (block[i].type) {
		case AST_STATEMENT_NUMERIC_FOR:
		case AST_STATEMENT_GENERIC_FOR:
		case AST_STATEMENT_LOOP:
		case AST_STATEMENT_LOCAL:
			eliminate_slots(function, block[i].block);
			continue;
		case AST_STATEMENT_ASSIGNMENT:
			size_t next = get_next_assignment(i);
			if (next == INVALID_ID) continue;

			if ((block[i].assignment.variables.back().type == AST_VARIABLE_TABLE_INDEX
					|| block[i].assignment.variables.back().type == AST_VARIABLE_TABLE_MULTRES)
				&& block[next].assignment.variables.back().type == AST_VARIABLE_SLOT
				&& block[next].assignment.variables.back().slot == block[i].assignment.variables.back().table->variable->slot) {
				if (block[next].assignment.expressions.back()->type == AST_EXPRESSION_TABLE
					&& (*block[next].assignment.variables.back().slotScope)->scopeBegin == block[next].id
					&& !block[next].assignment.expressions.back()->table->fields.back().value->isMultres
					&& (block[i].assignment.variables.back().type == AST_VARIABLE_TABLE_MULTRES
						|| get_constant_type(block[i].assignment.variables.back().tableIndex) <= NIL_CONSTANT
						|| !get_constant_type(block[i].assignment.expressions.back()))
					&& !block[i].assignment.usedLocals.contains(block[next].assignment.variables.back().slot)
					&& try_eliminate_condition(next, i)) {
					if (block[i].assignment.variables.back().type == AST_VARIABLE_TABLE_INDEX
						&& block[i].assignment.variables.back().tableIndex->type == AST_EXPRESSION_CONSTANT
						&& block[i].assignment.variables.back().tableIndex->constant->type == AST_CONSTANT_STRING) {
						for (size_t j = block[next].assignment.expressions.back()->table->constants.fields.size(); j--;) {
							if (block[next].assignment.expressions.back()->table->constants.fields[j].key->constant->type != AST_CONSTANT_STRING
								|| block[next].assignment.expressions.back()->table->constants.fields[j].key->constant->string != block[i].assignment.variables.back().tableIndex->constant->string)
								continue;
							if (block[next].assignment.expressions.back()->table->constants.fields[j].value->constant->type == AST_CONSTANT_NIL)
								block[next].assignment.expressions.back()->table->constants.fields.erase(block[next].assignment.expressions.back()->table->constants.fields.begin() + j);
							break;
						}
					}

					block[next].assignment.expressions.back()->table->fields.emplace_back(Table::Field { .key = block[i].assignment.variables.back().tableIndex, .value = block[i].assignment.expressions.back() });
					(*block[next].assignment.variables.back().slotScope)->usages--;
					block.merge(next, false);
					i--;
				} else if (block[i].assignment.variables.back().type == AST_VARIABLE_TABLE_INDEX
					&& (*block[next].assignment.variables.back().slotScope)->usages == 1
					&& try_eliminate_condition(next, i, &block[i].assignment.variables.back().table)) {
					block[i].assignment.variables.back().table = block[next].assignment.expressions.back();
					function.slotScopeCollector.remove_scope(block[next].assignment.variables.back().slot, block[next].assignment.variables.back().slotScope);
					block.merge(i);
					i--;
					continue;
				}
			}

			//TODO
			continue;
		}
	}
}

Ast::Expression* Ast::new_slot(const uint8_t& slot) {
	Expression* expression = new_expression(AST_EXPRESSION_VARIABLE);
	expression->variable->type = AST_VARIABLE_SLOT;
	expression->variable->slot = slot;
	return expression;
}

Ast::Expression* Ast::new_literal(const uint8_t& literal) {
	Expression* expression = new_expression(AST_EXPRESSION_CONSTANT);
	expression->constant->type = AST_CONSTANT_NUMBER;
	expression->constant->number = literal;
	return expression;
}

Ast::Expression* Ast::new_signed_literal(const uint16_t& signedLiteral) {
	Expression* expression = new_expression(AST_EXPRESSION_CONSTANT);
	expression->constant->type = AST_CONSTANT_NUMBER;
	expression->constant->number = std::bit_cast<int16_t>(signedLiteral);
	return expression;
}

Ast::Expression* Ast::new_primitive(const uint8_t& primitive) {
	Expression* expression = new_expression(AST_EXPRESSION_CONSTANT);

	switch (primitive) {
	case 0:
		expression->constant->type = AST_CONSTANT_NIL;
		break;
	case 1:
		expression->constant->type = AST_CONSTANT_FALSE;
		break;
	case 2:
		expression->constant->type = AST_CONSTANT_TRUE;
		break;
	default:
		assert(false, L"Invalid primitive", DEBUG_INFO);
	}

	return expression;
}

Ast::Expression* Ast::new_number(const Function& function, const uint16_t& index) {
	Expression* expression = new_expression(AST_EXPRESSION_CONSTANT);
	expression->constant->type = AST_CONSTANT_NUMBER;

	switch (function.get_number_constant(index).type) {
	case Bytecode::BC_KNUM_INT:
		expression->constant->number = std::bit_cast<int32_t>(function.get_number_constant(index).integer);
		break;
	case Bytecode::BC_KNUM_NUM:
		expression->constant->number = std::bit_cast<double>(function.get_number_constant(index).number);
		check_special_number(expression);
		break;
	}

	return expression;
}

Ast::Expression* Ast::new_string(const Function& function, const uint16_t& index) {
	Expression* expression = new_expression(AST_EXPRESSION_CONSTANT);
	expression->constant->type = AST_CONSTANT_STRING;
	assert(function.get_constant(index).type == Bytecode::BC_KGC_STR, L"Constant is not a string", DEBUG_INFO);
	expression->constant->string = &function.get_constant(index).string;
	expression->constant->isName = false;
	return expression;
}

Ast::Expression* Ast::new_table(const Function& function, const uint16_t& index) {
	auto new_table_constant = [this](const Bytecode::TableConstant& constant)->Expression* {
		Expression* expression = new_expression(AST_EXPRESSION_CONSTANT);

		switch (constant.type) {
		case Bytecode::BC_KTAB_NIL:
			expression->constant->type = AST_CONSTANT_NIL;
			break;
		case Bytecode::BC_KTAB_FALSE:
			expression->constant->type = AST_CONSTANT_FALSE;
			break;
		case Bytecode::BC_KTAB_TRUE:
			expression->constant->type = AST_CONSTANT_TRUE;
			break;
		case Bytecode::BC_KTAB_INT:
			expression->constant->type = AST_CONSTANT_NUMBER;
			expression->constant->number = std::bit_cast<int32_t>(constant.integer);
			break;
		case Bytecode::BC_KTAB_NUM:
			expression->constant->type = AST_CONSTANT_NUMBER;
			expression->constant->number = std::bit_cast<double>(constant.number);
			check_special_number(expression);
			break;
		case Bytecode::BC_KTAB_STR:
			expression->constant->type = AST_CONSTANT_STRING;
			expression->constant->string = &constant.string;
			break;
		}

		return expression;
	};

	Expression* expression = new_expression(AST_EXPRESSION_TABLE);
	assert(function.get_constant(index).type == Bytecode::BC_KGC_TAB, L"Constant is not a table", DEBUG_INFO);
	const Bytecode::Constant& table = function.get_constant(index);
	expression->table->constants.list.resize(table.array.size());

	for (size_t i = expression->table->constants.list.size(); i--;) {
		expression->table->constants.list[i] = new_table_constant(table.array[i]);
	}

	if (arguments.sortTables) {
		Table::Field falseField, trueField;
		std::vector<Table::Field> numberFields, stringFields;

		for (size_t i = table.table.size(); i--;) {
			Expression* key = new_table_constant(table.table[i].key);
			size_t position;

			switch (key->constant->type) {
			case AST_CONSTANT_FALSE:
				falseField.key = key;
				falseField.value = new_table_constant(table.table[i].value);
				continue;
			case AST_CONSTANT_TRUE:
				trueField.key = key;
				trueField.value = new_table_constant(table.table[i].value);
				continue;
			case AST_CONSTANT_NUMBER:
				for (position = numberFields.size(); position && numberFields[position - 1].key->constant->number > key->constant->number; position--);
				numberFields.emplace(numberFields.begin() + position, Table::Field { .key = key, .value = new_table_constant(table.table[i].value) });
				continue;
			case AST_CONSTANT_STRING:
				check_valid_name(key);
				for (position = stringFields.size(); position && stringFields[position - 1].key->constant->string->compare(*key->constant->string) > 0; position--);
				stringFields.emplace(stringFields.begin() + position, Table::Field { .key = key, .value = new_table_constant(table.table[i].value) });
				continue;
			}
		}

		if (falseField.key) expression->table->constants.fields.emplace_back(falseField);
		if (trueField.key) expression->table->constants.fields.emplace_back(trueField);
		expression->table->constants.fields.insert(expression->table->constants.fields.begin() + expression->table->constants.fields.size(), numberFields.begin(), numberFields.begin() + numberFields.size());
		expression->table->constants.fields.insert(expression->table->constants.fields.begin() + expression->table->constants.fields.size(), stringFields.begin(), stringFields.begin() + stringFields.size());
	} else {
		expression->table->constants.fields.resize(table.table.size());

		for (size_t i = expression->table->constants.fields.size(); i--;) {
			expression->table->constants.fields[i].key = new_table_constant(table.table[i].key);
			if (expression->table->constants.fields[i].key->constant->type == AST_CONSTANT_STRING) check_valid_name(expression->table->constants.fields[i].key);
			expression->table->constants.fields[i].value = new_table_constant(table.table[i].value);
		}
	}

	return expression;
}

Ast::Expression* Ast::new_cdata(const Function& function, const uint16_t& index) {
	Expression* expression = new_expression(AST_EXPRESSION_CONSTANT);

	switch (function.get_constant(index).type) {
	case Bytecode::BC_KGC_I64:
		expression->constant->type = AST_CONSTANT_CDATA_SIGNED;
		expression->constant->signed_integer = std::bit_cast<int64_t>(function.get_constant(index).cdata);
		break;
	case Bytecode::BC_KGC_U64:
		expression->constant->type = AST_CONSTANT_CDATA_UNSIGNED;
		expression->constant->unsigned_integer = function.get_constant(index).cdata;
		break;
	case Bytecode::BC_KGC_COMPLEX:
		expression->constant->type = AST_CONSTANT_CDATA_IMAGINARY;
		expression->constant->number = std::bit_cast<double>(function.get_constant(index).cdata);
		check_special_number(expression);
		break;
	default:
		assert(false, L"Constant is not cdata", DEBUG_INFO);
	}

	return expression;
}

size_t Ast::get_extended_id_from_statement(Statement& statement) {
	switch (statement.type) {
	case AST_STATEMENT_EMPTY:
	case AST_STATEMENT_GOTO:
	case AST_STATEMENT_BREAK:
		if (statement.instruction->type == Bytecode::BC_OP_JMP) return *statement.target;
	}

	return statement.id;
}

void Ast::check_valid_name(Expression* const& expression) {
	static const std::unordered_set<std::string> KEYWORDS = {
		"and", "break", "do", "else", "elseif", "end", "false",
		"for", "function", "if", "in", "local", "nil", "not",
		"or", "repeat", "return", "then", "true", "until", "while"
	};

	static const std::string KEYWORD_GOTO = "goto";

	if (!expression->constant->string->size() || expression->constant->string->front() < 'A') return;

	for (uint32_t i = expression->constant->string->size(); i--;) {
		if ((*expression->constant->string)[i] < '0') return;

		switch ((*expression->constant->string)[i]) {
		case ':':
		case ';':
		case '<':
		case '=':
		case '>':
		case '?':
		case '@':
		case '[':
		case '\\':
		case ']':
		case '^':
		case '`':
		case '{':
		case '|':
		case '}':
		case '~':
		case '\x7F':
			return;
		}
	}

	if ((!arguments.gotoIsKeyword
			|| *expression->constant->string != KEYWORD_GOTO)
		&& !KEYWORDS.contains(*expression->constant->string)) expression->constant->isName = true;
}

void Ast::check_special_number(Expression*& expression) {
	uint64_t rawDouble = std::bit_cast<uint64_t>(expression->constant->number);

	if ((rawDouble & DOUBLE_EXPONENT) != DOUBLE_SPECIAL) {
		assert(rawDouble != DOUBLE_NEGATIVE_ZERO || expression->constant->type == AST_CONSTANT_CDATA_IMAGINARY, L"Number constant is negative zero", DEBUG_INFO);
		return;
	}

	assert(!(rawDouble & DOUBLE_FRACTION), L"Number constant is NaN", DEBUG_INFO);

	//TODO do later because get_constant_type errors on slot + (1 / 0)
	/*
	if (expression->constant->type == AST_CONSTANT_CDATA_IMAGINARY) return;
	expression = new_expression(AST_EXPRESSION_BINARY_OPERATION);
	expression->binaryOperation->type = AST_BINARY_DIVISION;
	expression->binaryOperation->leftOperand = new_expression(AST_EXPRESSION_CONSTANT);
	expression->binaryOperation->leftOperand->constant->type = AST_CONSTANT_NUMBER;
	expression->binaryOperation->leftOperand->constant->number = rawDouble & DOUBLE_SIGN ? -1 : 1;
	expression->binaryOperation->rightOperand = new_expression(AST_EXPRESSION_CONSTANT);
	expression->binaryOperation->rightOperand->constant->type = AST_CONSTANT_NUMBER;
	expression->binaryOperation->rightOperand->constant->number = 0;
	*/
}

Ast::CONSTANT_TYPE Ast::get_constant_type(Expression* const& expression) {
	auto is_valid_number_constant = [](const double& number)->bool {
		uint64_t rawDouble = std::bit_cast<uint64_t>(number);
		return (rawDouble & DOUBLE_EXPONENT) == DOUBLE_SPECIAL ? !(rawDouble & DOUBLE_FRACTION) : rawDouble != DOUBLE_NEGATIVE_ZERO;
	};

	switch (expression->type) {
	case AST_EXPRESSION_CONSTANT:
		switch (expression->constant->type) {
		case AST_CONSTANT_NIL:
			return NIL_CONSTANT;
		case AST_CONSTANT_FALSE:
		case AST_CONSTANT_TRUE:
		case AST_CONSTANT_STRING:
			return BOOL_CONSTANT;
		case AST_CONSTANT_NUMBER:
			return NUMBER_CONSTANT;
		}

		break;
	case AST_EXPRESSION_BINARY_OPERATION:
		switch (expression->binaryOperation->type) {
		case AST_BINARY_ADDITION:
		case AST_BINARY_SUBTRACTION:
		case AST_BINARY_MULTIPLICATION:
		case AST_BINARY_DIVISION:
		case AST_BINARY_EXPONENTATION:
		case AST_BINARY_MODULO:
			if (get_constant_type(expression->binaryOperation->leftOperand) != NUMBER_CONSTANT || get_constant_type(expression->binaryOperation->rightOperand) != NUMBER_CONSTANT) break;
			double number;

			switch (expression->binaryOperation->type) {
			case AST_BINARY_ADDITION:
				number = expression->binaryOperation->leftOperand->constant->number + expression->binaryOperation->rightOperand->constant->number;
				break;
			case AST_BINARY_SUBTRACTION:
				number = expression->binaryOperation->leftOperand->constant->number - expression->binaryOperation->rightOperand->constant->number;
				break;
			case AST_BINARY_MULTIPLICATION:
				number = expression->binaryOperation->leftOperand->constant->number * expression->binaryOperation->rightOperand->constant->number;
				break;
			case AST_BINARY_DIVISION:
				number = expression->binaryOperation->leftOperand->constant->number / expression->binaryOperation->rightOperand->constant->number;
				break;
			case AST_BINARY_EXPONENTATION:
				number = std::pow(expression->binaryOperation->leftOperand->constant->number, expression->binaryOperation->rightOperand->constant->number);
				break;
			case AST_BINARY_MODULO:
				number = std::fmod(expression->binaryOperation->leftOperand->constant->number, expression->binaryOperation->rightOperand->constant->number);
				break;
			}

			if (!is_valid_number_constant(number)) break;
			return NUMBER_CONSTANT;
		case AST_BINARY_BIT_AND:
		case AST_BINARY_BIT_OR:
		case AST_BINARY_BIT_EXCLUSIVE_OR:
		case AST_BINARY_BIT_LEFT_SHIFT:
		case AST_BINARY_BIT_RIGHT_SHIFT:
		case AST_BINARY_BIT_ARITHMETIC_RIGHT_SHIFT:
			if (get_constant_type(expression->binaryOperation->leftOperand) != NUMBER_CONSTANT || get_constant_type(expression->binaryOperation->rightOperand) != NUMBER_CONSTANT) break;
			return NUMBER_CONSTANT;
		}

		break;
	case AST_EXPRESSION_UNARY_OPERATION:
		switch (expression->unaryOperation->type) {
		case AST_UNARY_NOT:
			if (get_constant_type(expression->unaryOperation->operand)) return BOOL_CONSTANT;
			break;
		case AST_UNARY_MINUS:
			if (expression->unaryOperation->operand->type == AST_EXPRESSION_CONSTANT) {
				switch (expression->unaryOperation->operand->constant->type) {
				case AST_CONSTANT_NUMBER:
					if (!is_valid_number_constant(-expression->unaryOperation->operand->constant->number)) break;
				case AST_CONSTANT_CDATA_SIGNED:
				case AST_CONSTANT_CDATA_UNSIGNED:
				case AST_CONSTANT_CDATA_IMAGINARY:
					return NUMBER_CONSTANT;
				}
			}

			break;
		case AST_UNARY_BIT_NOT:
			if (get_constant_type(expression->unaryOperation->operand) != NUMBER_CONSTANT) break;
			return NUMBER_CONSTANT;
		}

		break;
	}

	return INVALID_CONSTANT;
}
