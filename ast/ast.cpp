#include "..\main.h"

Ast::Ast(const Bytecode& bytecode) : bytecode(bytecode), chunk(*bytecode.main) {}

Ast::~Ast() {
	for (uint32_t i = statements.size(); i--;) {
		delete statements[i];
	}

	for (uint32_t i = functions.size(); i--;) {
		delete functions[i];
	}

	for (uint32_t i = expressions.size(); i--;) {
		delete expressions[i];
	}
}

Ast::Function*& Ast::new_function(const Bytecode::Prototype& prototype) {
	functions.emplace_back(new Function(prototype));
	return functions.back();
}

Ast::Statement*& Ast::new_statement(const AST_STATEMENT& type) {
	statements.emplace_back(new Statement(type));
	return statements.back();
}

Ast::Expression*& Ast::new_expression(const AST_EXPRESSION& type) {
	expressions.emplace_back(new Expression(type));
	return expressions.back();
}

void Ast::build() {
	if (bytecode.header.version == Bytecode::BC_VERSION_2) isFR2Enabled = bytecode.header.flags & Bytecode::BC_F_FR2;
	build_functions(chunk);
	functions.shrink_to_fit();
	statements.shrink_to_fit();
	expressions.shrink_to_fit();
}

void Ast::build_functions(Function& function) {
	function.isVariadic = function.prototype.header.flags & Bytecode::BC_PROTO_VARARG;
	function.slotScopeCollector.set_slot_infos(function.prototype.header.framesize, function.prototype.header.parameters);
	function.hasDebugInfo = function.prototype.header.hasDebugInfo;
	build_instructions(function);
	function.usedGlobals.shrink_to_fit();
	if (!function.hasDebugInfo) function.slotScopeCollector.build_upvalue_scopes();
	collect_slot_scopes(function, function.block);
	assert(function.slotScopeCollector.assert_scopes_closed(), "Failed to close slot scopes", bytecode.filePath, DEBUG_INFO);
	eliminate_slots(function, function.block);
	function.block.shrink_to_fit();

	for (uint32_t i = function.childFunctions.size(); i--;) {
		build_functions(*function.childFunctions[i]);
	}
}

void Ast::build_instructions(Function& function) {
	std::vector<uint8_t> upvalues;
	function.block.resize(function.prototype.instructions.size(), nullptr);

	for (uint32_t i = function.block.size(); i--;) {
		function.block[i] = new_statement(AST_STATEMENT_INSTRUCTION);
		function.block[i]->instruction.type = function.prototype.instructions[i].type;
		function.block[i]->instruction.a = function.prototype.instructions[i].a;
		function.block[i]->instruction.b = function.prototype.instructions[i].b;
		function.block[i]->instruction.c = function.prototype.instructions[i].c;
		function.block[i]->instruction.d = function.prototype.instructions[i].d;
		function.block[i]->instruction.info.id = i;

		switch (function.block[i]->instruction.type) {
		case Bytecode::BC_OP_FNEW:
			function.block[i]->function = new_function(*function.get_constant(function.block[i]->instruction.d).prototype);
			function.childFunctions.emplace_back(function.block[i]->function);
			function.block[i]->function->level = function.level + 1;
			function.block[i]->function->upvalues.resize(function.block[i]->function->prototype.upvalues.size());

			for (uint8_t j = function.block[i]->function->upvalues.size(); j--;) {
				function.block[i]->function->upvalues[j].slot = function.block[i]->function->prototype.upvalues[j];

				if (!(function.block[i]->function->prototype.upvalues[j] & Bytecode::BC_UV_LOCAL)) {
					function.block[i]->function->upvalues[j].slotScope = function.upvalues[function.block[i]->function->upvalues[j].slot].slotScope;
					continue;
				}

				function.block[i]->function->upvalues[j].local = true;
				upvalues.emplace_back(function.block[i]->function->upvalues[j].slot);
			}

			function.slotScopeCollector.add_upvalues(function.block[i]->instruction.info.id, upvalues);
			upvalues.clear();
			continue;
		case Bytecode::BC_OP_CALLMT:
		case Bytecode::BC_OP_CALLT:
		case Bytecode::BC_OP_RETM:
		case Bytecode::BC_OP_RET:
		case Bytecode::BC_OP_RET0:
		case Bytecode::BC_OP_RET1:
			function.block[i]->type = AST_STATEMENT_RETURN;
			continue;
		case Bytecode::BC_OP_UCLO:
		case Bytecode::BC_OP_ISNEXT:
		case Bytecode::BC_OP_FORI:
		case Bytecode::BC_OP_FORL:
		case Bytecode::BC_OP_ITERL:
		case Bytecode::BC_OP_LOOP:
		case Bytecode::BC_OP_JMP:
			function.block[i]->instruction.target = function.block[i]->instruction.info.id + (function.block[i]->instruction.d - Bytecode::BC_OP_JMP_BIAS + 1);
			continue;
		}
	}

	function.childFunctions.shrink_to_fit();
	if (function.block.back()->instruction.type == Bytecode::BC_OP_RET0) function.block.back()->type = AST_STATEMENT_EMPTY;
	return assign_debug_info(function);
}

void Ast::assign_debug_info(Function& function) {
	if (!function.hasDebugInfo) return group_jumps(function);

	for (uint32_t i = function.block.size(); i--;) {
		function.block[i]->instruction.info.lineOffset = function.prototype.lineMap[i];
	}

	std::vector<uint32_t> activeLocalScopes;
	function.parameterNames.resize(function.prototype.header.parameters);

	for (uint8_t i = function.parameterNames.size(); i--;) {
		assert(function.prototype.variableInfos[i].isParameter, "Function parameter count does not\nmatch with debug info", bytecode.filePath, DEBUG_INFO);
		function.parameterNames[i] = function.prototype.variableInfos[i].name;
		activeLocalScopes.emplace_back(function.prototype.variableInfos[i].scopeEnd);
	}

	for (uint32_t i = function.parameterNames.size(); i < function.prototype.variableInfos.size(); i++) {
		assert(!function.prototype.variableInfos[i].isParameter, "Function parameter count does not\nmatch with debug info", bytecode.filePath, DEBUG_INFO);

		while (activeLocalScopes.size() && function.prototype.variableInfos[i].scopeBegin > activeLocalScopes.back()) {
			activeLocalScopes.pop_back();
		}

		if (function.prototype.variableInfos[i].type != Bytecode::BC_VAR_STR) {
			activeLocalScopes.emplace_back(function.prototype.variableInfos[i].scopeEnd);
			continue;
		}

		if (!function.locals.size()
			|| function.prototype.variableInfos[i].scopeBegin != function.locals.back().scopeBegin
			|| function.prototype.variableInfos[i].scopeEnd != function.locals.back().scopeEnd) {
			if (function.locals.size() && function.prototype.variableInfos[i].scopeBegin <= function.locals.back().scopeEnd)
				assert(function.prototype.variableInfos[i].scopeEnd <= function.locals.back().scopeEnd, "Illegal variable scope border overlap", bytecode.filePath, DEBUG_INFO);
			function.locals.emplace_back();
			function.locals.back().baseSlot = activeLocalScopes.size();
			function.locals.back().scopeBegin = function.prototype.variableInfos[i].scopeBegin;
			function.locals.back().scopeEnd = function.prototype.variableInfos[i].scopeEnd;
		}

		function.locals.back().names.emplace_back(function.prototype.variableInfos[i].name);
		activeLocalScopes.emplace_back(function.locals.back().scopeEnd);
	}

	for (uint32_t i = function.locals.size(); i--;) {
		function.locals[i].names.shrink_to_fit();
	}

	function.locals.shrink_to_fit();
	return group_jumps(function);
}

void Ast::group_jumps(Function& function) {
    for (uint32_t i = function.block.size(); i--;) {
        switch (function.block[i]->instruction.type) {
        case Bytecode::BC_OP_UCLO:
            function.slotScopeCollector.add_upvalue_close(function.block[i]->instruction.info.id, function.block[i]->instruction.target, function.block[i]->instruction.a);

            if (function.block[i]->instruction.d == Bytecode::BC_OP_JMP_BIAS) {
                function.block[i]->type = AST_STATEMENT_EMPTY;
                continue;
            }
        case Bytecode::BC_OP_JMP:
            function.block[i]->type = AST_STATEMENT_GOTO;
            function.add_jump(function.block[i]->instruction.info.id, function.block[i]->instruction.target);
            continue;
        }
	}

    function.labels.shrink_to_fit();
	uint32_t index;

	for (uint32_t i = function.block.size(); i--;) {
        function.block[i]->instruction.info.attachedLabel = function.get_label_from_id(function.block[i]->instruction.info.id);

        switch (function.block[i]->instruction.type) {
        case Bytecode::BC_OP_ITERC:
            index = get_block_index_from_id(function.block, function.labels[function.block[i]->instruction.info.attachedLabel].jumpIds.front());
            function.block[index]->type = AST_STATEMENT_INSTRUCTION;
            function.remove_jump(function.block[index]->instruction.info.id, function.block[i]->instruction.info.id);
            continue;
        case Bytecode::BC_OP_JMP:
            if (function.block[i]->type != AST_STATEMENT_GOTO) continue;
            function.slotScopeCollector.add_jump(function.block[i]->instruction.info.id, function.block[i]->instruction.target);
            if (function.block[i]->instruction.target == function.block[i]->instruction.info.id || !i) continue;

            switch (function.block[i - 1]->instruction.type) {
            case Bytecode::BC_OP_JMP:
                if (function.block[i - 1]->instruction.d != Bytecode::BC_OP_JMP_BIAS) continue;
                function.remove_jump(function.block[i - 1]->instruction.info.id, function.block[i - 1]->instruction.info.id);
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
                i--;
                function.block[i]->type = AST_STATEMENT_CONDITION;
                function.block[i]->condition.jumpId = function.block[i + 1]->instruction.info.id;
                function.block[i]->instruction.target = function.block[i + 1]->instruction.target;
                function.block.erase(function.block.begin() + i + 1);
            }

            continue;
        }
	}

	return build_loops(function);
}

void Ast::build_loops(Function& function) {
	static void (* const build_break_statements)(std::vector<Statement*>&, const uint32_t&) = [](std::vector<Statement*>& block, const uint32_t& breakTarget)->void {
		for (uint32_t i = block.size(); i--;) {
			if (block[i]->type != AST_STATEMENT_GOTO || block[i]->instruction.target != breakTarget) continue;
			block[i]->type = AST_STATEMENT_BREAK;
		}
	};

	uint32_t targetIndex, breakTarget;

	for (uint32_t i = function.block.size(); i--;) {
		if (function.block[i]->type != AST_STATEMENT_INSTRUCTION) continue;

		switch (function.block[i]->instruction.type) {
		case Bytecode::BC_OP_ISNEXT:
		case Bytecode::BC_OP_JMP:
			function.block[i]->type = AST_STATEMENT_GENERIC_FOR;
			targetIndex = get_block_index_from_id(function.block, function.block[i]->instruction.target);
			breakTarget = get_extended_id_from_statement(function.block[targetIndex + 2]);
			function.block[i]->instruction = function.block[targetIndex]->instruction;
			function.block[i]->instruction.info.id = function.block[targetIndex + 1]->instruction.target - 1;
			function.block[i]->instruction.target = function.block[targetIndex + 1]->instruction.info.id + 1;
			function.block[targetIndex]->type = AST_STATEMENT_EMPTY;
			function.block[i]->block.reserve(targetIndex - i);
			function.block[i]->block.insert(function.block[i]->block.begin(), function.block.begin() + i + 1, function.block.begin() + targetIndex + 1);
			function.block.erase(function.block.begin() + i + 1, function.block.begin() + targetIndex + 2);
			function.slotScopeCollector.add_loop(function.block[i]->instruction.info.id, function.block[i]->instruction.target);
			build_break_statements(function.block[i]->block, breakTarget);
			build_local_scopes(function, function.block[i]->block);
			continue;
		case Bytecode::BC_OP_FORI:
			function.block[i]->type = AST_STATEMENT_NUMERIC_FOR;
			targetIndex = get_block_index_from_id(function.block, function.block[i]->instruction.target);
			breakTarget = get_extended_id_from_statement(function.block[targetIndex]);
			function.block[targetIndex - 1]->type = AST_STATEMENT_EMPTY;
			function.block[i]->block.reserve(targetIndex - 1 - i);
			function.block[i]->block.insert(function.block[i]->block.begin(), function.block.begin() + i + 1, function.block.begin() + targetIndex);
			function.block.erase(function.block.begin() + i + 1, function.block.begin() + targetIndex);
			function.slotScopeCollector.add_loop(function.block[i]->instruction.info.id, function.block[i]->instruction.target);
			build_break_statements(function.block[i]->block, breakTarget);
			build_local_scopes(function, function.block[i]->block);
			continue;
		case Bytecode::BC_OP_LOOP:
			assert(function.block[i]->instruction.target >= function.block[i]->instruction.info.id, "LOOP instruction has invalid jump target", bytecode.filePath, DEBUG_INFO);

			if (function.block[i]->instruction.target == function.block[i]->instruction.info.id) {
				assert(i + 1 < function.block.size() && function.block[i + 1]->type == AST_STATEMENT_GOTO, "JMP instruction after goto loop is missing", bytecode.filePath, DEBUG_INFO);
				assert(function.block[i + 1]->instruction.target <= function.block[i]->instruction.info.id, "JMP instruction after goto loop has invalid target", bytecode.filePath, DEBUG_INFO);
				function.block[i]->type = AST_STATEMENT_EMPTY;
				continue;
			}

			function.block[i]->type = AST_STATEMENT_LOOP;
			targetIndex = get_block_index_from_id(function.block, function.block[i]->instruction.target);
			breakTarget = get_extended_id_from_statement(function.block[targetIndex]);
			function.block[i]->block.reserve(targetIndex - 1 - i);
			function.block[i]->block.insert(function.block[i]->block.begin(), function.block.begin() + i + 1, function.block.begin() + targetIndex);
			function.block.erase(function.block.begin() + i + 1, function.block.begin() + targetIndex);
			function.slotScopeCollector.add_loop(function.block[i]->instruction.info.id, function.block[i]->instruction.target);
			build_break_statements(function.block[i]->block, breakTarget);

            if (function.block[i]->block.size()
                && function.block[i]->block.back()->type == AST_STATEMENT_CONDITION
                && function.block[i]->instruction.info.id != breakTarget) {
                for (uint32_t j = function.block[i]->block.size(); j--;) {
                    switch (function.block[i]->block[j]->type) {
                    case AST_STATEMENT_GOTO:
                    case AST_STATEMENT_BREAK:
                        break;
                    case AST_STATEMENT_CONDITION:
                        if (function.block[i]->block[j]->instruction.target == function.block[i]->instruction.info.id) {
                            function.block[i]->block.emplace_back(new_statement(AST_STATEMENT_BREAK));
                            function.block[i]->block.back()->instruction.type = Bytecode::BC_OP_JMP;
                            function.block[i]->block.back()->instruction.target = breakTarget;
                            function.block[i]->block.emplace_back(new_statement(AST_STATEMENT_GOTO));
                            function.block[i]->block.back()->instruction.type = Bytecode::BC_OP_JMP;
                            function.block[i]->block.back()->instruction.target = function.block[i]->instruction.info.id;
                            break;
                        }
                    default:
                        continue;
                    }

                    break;
                }
            }

			build_local_scopes(function, function.block[i]->block);
			continue;
		}
	}

	function.slotScopeCollector.upvalueInfos.shrink_to_fit();
	return build_local_scopes(function, function.block);
}

void Ast::build_local_scopes(Function& function, std::vector<Statement*>& block) {
	if (!function.hasDebugInfo) return build_expressions(function, block);
	uint32_t scopeBeginIndex, scopeEndIndex;

	for (uint32_t i = function.locals.size(); i--;) {
		scopeBeginIndex = get_block_index_from_id(block, function.locals[i].scopeBegin);
		if (scopeBeginIndex == ID_EMPTY) continue;

		switch (block[scopeBeginIndex]->type) {
		case AST_STATEMENT_NUMERIC_FOR:
		case AST_STATEMENT_GENERIC_FOR:
			block[scopeBeginIndex]->locals = &function.locals[i];
			continue;
		}

		scopeBeginIndex++;
		block.emplace(block.begin() + scopeBeginIndex, new_statement(AST_STATEMENT_DECLARATION));
		block[scopeBeginIndex]->instruction.target = function.locals[i].scopeBegin;
		block[scopeBeginIndex]->locals = &function.locals[i];
		scopeEndIndex = get_block_index_from_id(block, function.locals[i].scopeEnd + 1);
		if (scopeEndIndex == ID_EMPTY) scopeEndIndex = block.size();
		block[scopeBeginIndex]->block.reserve(scopeEndIndex - 1 - scopeBeginIndex);
		block[scopeBeginIndex]->block.insert(block[scopeBeginIndex]->block.begin(), block.begin() + scopeBeginIndex + 1, block.begin() + scopeEndIndex);
		block.erase(block.begin() + scopeBeginIndex + 1, block.begin() + scopeEndIndex);
		build_expressions(function, block[scopeBeginIndex]->block);
	}

	return build_expressions(function, block);
}

/*
void Ast::remove_unreachable_instructions(Function& function, std::vector<Statement*>& block) {
	uint32_t nextReachableInstruction;

	for (uint32_t i = 0; i < block.size(); i++) {
		switch (block[i]->type) {
		case AST_STATEMENT_GOTO:
		case AST_STATEMENT_BREAK:
			nextReachableInstruction = block[i]->instruction.info.id;

			if (nextReachableInstruction != ID_NOT_FOUND) {
				nextReachableInstruction = function.get_next_reachable_id(nextReachableInstruction);
				if (nextReachableInstruction != ID_NOT_FOUND) nextReachableInstruction = get_block_index_from_id(block, nextReachableInstruction);
			}

			if (nextReachableInstruction == ID_NOT_FOUND) nextReachableInstruction = block.size();

			for (uint32_t j = nextReachableInstruction - 1; j > i; j--) {
				switch (block[j]->type) {
				case AST_STATEMENT_CONDITION:
					function.deletedGotos.emplace_back(block[i]);
					break;
				case AST_STATEMENT_GOTO:
					function.deletedGotos.emplace_back(block[i]);
				case AST_STATEMENT_BREAK:
					block[i]->markedForDelete = true;
					continue;
				case AST_STATEMENT_NUMERIC_FOR:
				case AST_STATEMENT_GENERIC_FOR:
				case AST_STATEMENT_LOOP:
					for (std::vector<std::vector<Statement*>*> callstack(1, &block[i]->block); callstack.size();) {
						if (!callstack.back()->size()) {
							callstack.pop_back();
							continue;
						}

						switch (callstack.back()->front()->type) {
						case AST_STATEMENT_GOTO:
							callstack.back()->front()->markedForDelete = true;
						case AST_STATEMENT_CONDITION:
							function.deletedGotos.emplace_back(callstack.back()->front());
							break;
						case AST_STATEMENT_NUMERIC_FOR:
						case AST_STATEMENT_GENERIC_FOR:
						case AST_STATEMENT_LOOP:
							callstack.back()->front()->type = AST_STATEMENT_EMPTY;
							callstack.emplace_back(&callstack.back()->front()->block);
							continue;
						}

						callstack.back()->erase(callstack.back()->begin());
					}

					break;
				}

				block.erase(block.begin() + j);
			}
		}
	}

	return build_expressions(function, block);
}
*/

void Ast::build_expressions(Function& function, std::vector<Statement*>& block) {
	for (uint32_t i = block.size(); i--;) {
		switch (block[i]->type) {
		case AST_STATEMENT_INSTRUCTION:
			block[i]->type = AST_STATEMENT_ASSIGNMENT;
			block[i]->assignment.expressions.resize(1, nullptr);

			switch (block[i]->instruction.type) {
			case Bytecode::BC_OP_MOV:
				block[i]->assignment.expressions.back() = new_slot(block[i]->instruction.d);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back());
				break;
			case Bytecode::BC_OP_NOT:
			case Bytecode::BC_OP_UNM:
			case Bytecode::BC_OP_LEN:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_UNARY_OPERATION);

				switch (block[i]->instruction.type) {
				case Bytecode::BC_OP_NOT:
					block[i]->assignment.expressions.back()->unaryOperation->type = AST_UNARY_NOT;
					break;
				case Bytecode::BC_OP_UNM:
					block[i]->assignment.expressions.back()->unaryOperation->type = AST_UNARY_MINUS;
					break;
				case Bytecode::BC_OP_LEN:
					block[i]->assignment.expressions.back()->unaryOperation->type = AST_UNARY_LENGTH;
					break;
				}

				block[i]->assignment.expressions.back()->unaryOperation->operand = new_slot(block[i]->instruction.d);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->unaryOperation->operand);
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
			case Bytecode::BC_OP_CAT:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_BINARY_OPERATION);

				switch (block[i]->instruction.type) {
				case Bytecode::BC_OP_ADDVN:
				case Bytecode::BC_OP_ADDNV:
				case Bytecode::BC_OP_ADDVV:
					block[i]->assignment.expressions.back()->binaryOperation->type = AST_BINARY_ADDITION;
					break;
				case Bytecode::BC_OP_SUBVN:
				case Bytecode::BC_OP_SUBNV:
				case Bytecode::BC_OP_SUBVV:
					block[i]->assignment.expressions.back()->binaryOperation->type = AST_BINARY_SUBTRACTION;
					break;
				case Bytecode::BC_OP_MULVN:
				case Bytecode::BC_OP_MULNV:
				case Bytecode::BC_OP_MULVV:
					block[i]->assignment.expressions.back()->binaryOperation->type = AST_BINARY_MULTIPLICATION;
					break;
				case Bytecode::BC_OP_DIVVN:
				case Bytecode::BC_OP_DIVNV:
				case Bytecode::BC_OP_DIVVV:
					block[i]->assignment.expressions.back()->binaryOperation->type = AST_BINARY_DIVISION;
					break;
				case Bytecode::BC_OP_MODVN:
				case Bytecode::BC_OP_MODNV:
				case Bytecode::BC_OP_MODVV:
					block[i]->assignment.expressions.back()->binaryOperation->type = AST_BINARY_MODULO;
					break;
				case Bytecode::BC_OP_POW:
					block[i]->assignment.expressions.back()->binaryOperation->type = AST_BINARY_EXPONENTATION;
					break;
				case Bytecode::BC_OP_CAT:
					block[i]->assignment.expressions.back()->binaryOperation->type = AST_BINARY_CONCATENATION;
					break;
				}

				switch (block[i]->instruction.type) {
				case Bytecode::BC_OP_ADDVN:
				case Bytecode::BC_OP_SUBVN:
				case Bytecode::BC_OP_MULVN:
				case Bytecode::BC_OP_DIVVN:
				case Bytecode::BC_OP_MODVN:
					block[i]->assignment.expressions.back()->binaryOperation->leftOperand = new_slot(block[i]->instruction.b);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->binaryOperation->leftOperand);
					block[i]->assignment.expressions.back()->binaryOperation->rightOperand = new_number(function, block[i]->instruction.c);
					break;
				case Bytecode::BC_OP_ADDNV:
				case Bytecode::BC_OP_SUBNV:
				case Bytecode::BC_OP_MULNV:
				case Bytecode::BC_OP_DIVNV:
				case Bytecode::BC_OP_MODNV:
					block[i]->assignment.expressions.back()->binaryOperation->leftOperand = new_number(function, block[i]->instruction.c);
					block[i]->assignment.expressions.back()->binaryOperation->rightOperand = new_slot(block[i]->instruction.b);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->binaryOperation->rightOperand);
					break;
				case Bytecode::BC_OP_ADDVV:
				case Bytecode::BC_OP_SUBVV:
				case Bytecode::BC_OP_MULVV:
				case Bytecode::BC_OP_DIVVV:
				case Bytecode::BC_OP_MODVV:
				case Bytecode::BC_OP_POW:
					block[i]->assignment.expressions.back()->binaryOperation->leftOperand = new_slot(block[i]->instruction.b);
					block[i]->assignment.expressions.back()->binaryOperation->rightOperand = new_slot(block[i]->instruction.c);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->binaryOperation->leftOperand, block[i]->assignment.expressions.back()->binaryOperation->rightOperand);
					break;
				case Bytecode::BC_OP_CAT:
					block[i]->assignment.expressions.back()->binaryOperation->leftOperand = new_slot(block[i]->instruction.b);

					for (Expression* expression = block[i]->assignment.expressions.back(); true; expression = expression->binaryOperation->rightOperand) {
						block[i]->assignment.register_open_slots(expression->binaryOperation->leftOperand);

						if (expression->binaryOperation->leftOperand->variable->slot == block[i]->instruction.c - 1) {
							expression->binaryOperation->rightOperand = new_slot(block[i]->instruction.c);
							block[i]->assignment.register_open_slots(expression->binaryOperation->rightOperand);
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
				block[i]->assignment.expressions.back() = new_string(function, block[i]->instruction.d);
				break;
			case Bytecode::BC_OP_KCDATA:
				block[i]->assignment.expressions.back() = new_cdata(function, block[i]->instruction.d);
				break;
			case Bytecode::BC_OP_KSHORT:
				block[i]->assignment.expressions.back() = new_signed_literal(block[i]->instruction.d);
				break;
			case Bytecode::BC_OP_KNUM:
				block[i]->assignment.expressions.back() = new_number(function, block[i]->instruction.d);
				break;
			case Bytecode::BC_OP_KPRI:
				block[i]->assignment.expressions.back() = new_primitive(block[i]->instruction.d);
				break;
			case Bytecode::BC_OP_KNIL: //TODO
				while (true) {
					block[i]->assignment.expressions.resize(1, nullptr);
					block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_CONSTANT);
					block[i]->assignment.expressions.back()->constant->type = AST_CONSTANT_NIL;
					block[i]->assignment.variables.resize(1);
					block[i]->assignment.variables.back().type = AST_VARIABLE_SLOT;
					block[i]->assignment.variables.back().slot = block[i]->instruction.d;
					if (block[i]->instruction.d == block[i]->instruction.a) break;
					block.emplace(block.begin() + i, new_statement(AST_STATEMENT_ASSIGNMENT));
					block[i]->instruction = block[i + 1]->instruction;
					block[i + 1]->instruction.info.id = ID_EMPTY;
					block[i + 1]->instruction.info.attachedLabel = ID_EMPTY;
					block[i]->instruction.d--;
				}

				continue;

				/*
				block[i]->assignment.variables.resize((block[i]->instruction.d - block[i]->instruction.a) + 1);
				block[i]->assignment.expressions.resize(block[i]->assignment.variables.size(), nullptr);

				for (uint8_t j = block[i]->assignment.variables.size(); j--;) {
					block[i]->assignment.variables[j].type = AST_VARIABLE_SLOT;
					block[i]->assignment.variables[j].slot = block[i]->instruction.a + j;
					block[i]->assignment.expressions[j] = new_expression(AST_EXPRESSION_CONSTANT);
					block[i]->assignment.expressions[j]->constant->type = AST_CONSTANT_NIL;
				}

				continue;
				*/
			case Bytecode::BC_OP_UGET:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_VARIABLE);
				block[i]->assignment.expressions.back()->variable->type = AST_VARIABLE_UPVALUE;
				block[i]->assignment.expressions.back()->variable->slotScope = function.upvalues[block[i]->instruction.d].slotScope;
				break;
			case Bytecode::BC_OP_USETV:
			case Bytecode::BC_OP_USETS:
			case Bytecode::BC_OP_USETN:
			case Bytecode::BC_OP_USETP:
				block[i]->assignment.variables.resize(1);
				block[i]->assignment.variables.back().type = AST_VARIABLE_UPVALUE;
				block[i]->assignment.variables.back().slotScope = function.upvalues[block[i]->instruction.a].slotScope;

				switch (block[i]->instruction.type) {
				case Bytecode::BC_OP_USETV:
					block[i]->assignment.expressions.back() = new_slot(block[i]->instruction.d);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back());
					break;
				case Bytecode::BC_OP_USETS:
					block[i]->assignment.expressions.back() = new_string(function, block[i]->instruction.d);
					break;
				case Bytecode::BC_OP_USETN:
					block[i]->assignment.expressions.back() = new_number(function, block[i]->instruction.d);
					break;
				case Bytecode::BC_OP_USETP:
					block[i]->assignment.expressions.back() = new_primitive(block[i]->instruction.d);
					break;
				}

				continue;
			case Bytecode::BC_OP_FNEW:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_FUNCTION);
				block[i]->assignment.expressions.back()->function = block[i]->function;
				break;
			case Bytecode::BC_OP_TNEW:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_TABLE);
				block[i]->assignment.isTableConstructor = true;
				break;
			case Bytecode::BC_OP_TDUP:
				block[i]->assignment.expressions.back() = new_table(function, block[i]->instruction.d);
				block[i]->assignment.isTableConstructor = true;
				break;
			case Bytecode::BC_OP_GGET:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_VARIABLE);
				block[i]->assignment.expressions.back()->variable->type = AST_VARIABLE_GLOBAL;
				block[i]->assignment.expressions.back()->variable->name = function.get_constant(block[i]->instruction.d).string;
				if (function.hasDebugInfo) function.usedGlobals.emplace_back(&function.get_constant(block[i]->instruction.d).string);
				break;
			case Bytecode::BC_OP_GSET:
				block[i]->assignment.variables.resize(1);
				block[i]->assignment.variables.back().type = AST_VARIABLE_GLOBAL;
				block[i]->assignment.variables.back().name = function.get_constant(block[i]->instruction.d).string;
				if (function.hasDebugInfo) function.usedGlobals.emplace_back(&function.get_constant(block[i]->instruction.d).string);
				block[i]->assignment.expressions.back() = new_slot(block[i]->instruction.a);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back());
				continue;
			case Bytecode::BC_OP_TGETV:
			case Bytecode::BC_OP_TGETS:
			case Bytecode::BC_OP_TGETB:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_VARIABLE);
				block[i]->assignment.expressions.back()->variable->type = AST_VARIABLE_TABLE_INDEX;
				block[i]->assignment.expressions.back()->variable->table = new_slot(block[i]->instruction.b);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->variable->table);

				switch (block[i]->instruction.type) {
				case Bytecode::BC_OP_TGETV:
					block[i]->assignment.expressions.back()->variable->tableIndex = new_slot(block[i]->instruction.c);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->variable->tableIndex);
					break;
				case Bytecode::BC_OP_TGETS:
					block[i]->assignment.expressions.back()->variable->tableIndex = new_string(function, block[i]->instruction.c);
					check_valid_name(block[i]->assignment.expressions.back()->variable->tableIndex->constant);
					break;
				case Bytecode::BC_OP_TGETB:
					block[i]->assignment.expressions.back()->variable->tableIndex = new_literal(block[i]->instruction.c);
					break;
				}

				break;
			case Bytecode::BC_OP_TSETV:
			case Bytecode::BC_OP_TSETS:
			case Bytecode::BC_OP_TSETB:
				block[i]->assignment.variables.resize(1);
				block[i]->assignment.variables.back().type = AST_VARIABLE_TABLE_INDEX;
				block[i]->assignment.variables.back().table = new_slot(block[i]->instruction.b);

				switch (block[i]->instruction.type) {
				case Bytecode::BC_OP_TSETV:
					block[i]->assignment.variables.back().tableIndex = new_slot(block[i]->instruction.c);
					block[i]->assignment.register_open_slots(block[i]->assignment.variables.back().tableIndex);
					break;
				case Bytecode::BC_OP_TSETS:
					block[i]->assignment.variables.back().tableIndex = new_string(function, block[i]->instruction.c);
					check_valid_name(block[i]->assignment.variables.back().tableIndex->constant);
					break;
				case Bytecode::BC_OP_TSETB:
					block[i]->assignment.variables.back().tableIndex = new_literal(block[i]->instruction.c);
					break;
				}

				block[i]->assignment.expressions.back() = new_slot(block[i]->instruction.a);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back());
				continue;
			case Bytecode::BC_OP_TSETM:
				block[i]->assignment.variables.resize(1);
				block[i]->assignment.variables.back().type = AST_VARIABLE_TABLE_INDEX;
				block[i]->assignment.variables.back().isMultres = true;
				block[i]->assignment.variables.back().table = new_slot(block[i]->instruction.a - 1);
				assert(function.get_number_constant(block[i]->instruction.d).type == Bytecode::BC_KNUM_NUM, "Multres table index is not a valid number constant", bytecode.filePath, DEBUG_INFO);
				block[i]->assignment.variables.back().multresIndex = function.get_number_constant(block[i]->instruction.d).number;
				block[i]->assignment.expressions.back() = new_slot(block[i]->instruction.a);
				block[i]->assignment.expressions.back()->variable->isMultres = true;
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back());
				continue;
			case Bytecode::BC_OP_CALLM:
			case Bytecode::BC_OP_CALL:
				if (block[i]->instruction.b) {
					if (block[i]->instruction.b == 1) {
						block[i]->type = AST_STATEMENT_FUNCTION_CALL;
					} else {
						block[i]->assignment.variables.resize(block[i]->instruction.b - 1);

						for (uint8_t j = block[i]->assignment.variables.size(); j--;) {
							block[i]->assignment.variables[j].type = AST_VARIABLE_SLOT;
							block[i]->assignment.variables[j].slot = block[i]->instruction.a + j;
						}
					}
				} else {
					block[i]->assignment.variables.resize(1);
					block[i]->assignment.variables.back().type = AST_VARIABLE_SLOT;
					block[i]->assignment.variables.back().slot = block[i]->instruction.a;
					block[i]->assignment.variables.back().isMultres = true;
				}

				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_FUNCTION_CALL);
				block[i]->assignment.expressions.back()->functionCall->function = new_slot(block[i]->instruction.a);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->functionCall->function);
				block[i]->assignment.expressions.back()->functionCall->arguments.resize(block[i]->instruction.c + (block[i]->instruction.type == Bytecode::BC_OP_CALLM ? 0 : -1), nullptr);
				if (block[i]->assignment.expressions.back()->functionCall->arguments.size()) block[i]->assignment.isPotentialMethod = true;

				for (uint8_t j = 0; j < block[i]->assignment.expressions.back()->functionCall->arguments.size(); j++) {
					block[i]->assignment.expressions.back()->functionCall->arguments[j] = new_slot(block[i]->instruction.a + (isFR2Enabled ? 2 : 1) + j);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->functionCall->arguments[j]);
				}

				if (block[i]->instruction.type == Bytecode::BC_OP_CALLM) {
					block[i]->assignment.expressions.back()->functionCall->multresArgument = new_slot(block[i]->instruction.a + (isFR2Enabled ? 2 : 1) + block[i]->instruction.c);
					block[i]->assignment.expressions.back()->functionCall->multresArgument->variable->isMultres = true;
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->functionCall->multresArgument);
				}

				continue;
			case Bytecode::BC_OP_VARG:
				if (block[i]->instruction.b) {
					assert(block[i]->instruction.b != 1, "Vararg has invalid number of returns", bytecode.filePath, DEBUG_INFO);
					block[i]->assignment.variables.resize(block[i]->instruction.b - 1);

					for (uint8_t j = block[i]->assignment.variables.size(); j--;) {
						block[i]->assignment.variables[j].type = AST_VARIABLE_SLOT;
						block[i]->assignment.variables[j].slot = block[i]->instruction.a + j;
					}
				} else {
					block[i]->assignment.variables.resize(1);
					block[i]->assignment.variables.back().type = AST_VARIABLE_SLOT;
					block[i]->assignment.variables.back().slot = block[i]->instruction.a;
					block[i]->assignment.variables.back().isMultres = true;
				}

				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_VARARG);
				continue;
			}

			block[i]->assignment.variables.resize(1);
			block[i]->assignment.variables.back().type = AST_VARIABLE_SLOT;
			block[i]->assignment.variables.back().slot = block[i]->instruction.a;
			continue;
		case AST_STATEMENT_RETURN:
			switch (block[i]->instruction.type) {
			case Bytecode::BC_OP_CALLMT:
			case Bytecode::BC_OP_CALLT:
				block[i]->assignment.expressions.resize(1, nullptr);
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_FUNCTION_CALL);
				block[i]->assignment.expressions.back()->functionCall->function = new_slot(block[i]->instruction.a);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->functionCall->function);
				block[i]->assignment.expressions.back()->functionCall->arguments.resize(block[i]->instruction.d + (block[i]->instruction.type == Bytecode::BC_OP_CALLMT ? 0 : -1), nullptr);
				if (block[i]->assignment.expressions.back()->functionCall->arguments.size()) block[i]->assignment.isPotentialMethod = true;

				for (uint8_t j = 0; j < block[i]->assignment.expressions.back()->functionCall->arguments.size(); j++) {
					block[i]->assignment.expressions.back()->functionCall->arguments[j] = new_slot(block[i]->instruction.a + (isFR2Enabled ? 2 : 1) + j);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->functionCall->arguments[j]);
				}

				if (block[i]->instruction.type == Bytecode::BC_OP_CALLMT) {
					block[i]->assignment.expressions.back()->functionCall->multresArgument = new_slot(block[i]->instruction.a + (isFR2Enabled ? 2 : 1) + block[i]->instruction.d);
					block[i]->assignment.expressions.back()->functionCall->multresArgument->variable->isMultres = true;
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->functionCall->multresArgument);
				}

				break;
			case Bytecode::BC_OP_RETM:
			case Bytecode::BC_OP_RET:
			case Bytecode::BC_OP_RET1:
				block[i]->assignment.expressions.resize(block[i]->instruction.d + (block[i]->instruction.type == Bytecode::BC_OP_RETM ? 0 : -1), nullptr);

				for (uint8_t j = 0; j < block[i]->assignment.expressions.size(); j++) {
					block[i]->assignment.expressions[j] = new_slot(block[i]->instruction.a + j);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions[j]);
				}

				if (block[i]->instruction.type == Bytecode::BC_OP_RETM) {
					block[i]->assignment.multresReturn = new_slot(block[i]->instruction.a + block[i]->instruction.d);
					block[i]->assignment.multresReturn->variable->isMultres = true;
					block[i]->assignment.register_open_slots(block[i]->assignment.multresReturn);
				}

				break;
			}

			continue;
		case AST_STATEMENT_CONDITION:
			block[i]->assignment.expressions.resize(1, nullptr);

			switch (block[i]->instruction.type) {
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
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_BINARY_OPERATION);
				block[i]->assignment.expressions.back()->binaryOperation->type = AST_BINARY_CONDITION;
				block[i]->assignment.expressions.back()->binaryOperation->leftOperand = new_slot(block[i]->instruction.a);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->binaryOperation->leftOperand);

				switch (block[i]->instruction.type) {
				case Bytecode::BC_OP_ISLT:
				case Bytecode::BC_OP_ISGE:
				case Bytecode::BC_OP_ISLE:
				case Bytecode::BC_OP_ISGT:
					block[i]->condition.allowSlotSwap = true;
				case Bytecode::BC_OP_ISEQV:
				case Bytecode::BC_OP_ISNEV:
					block[i]->assignment.expressions.back()->binaryOperation->rightOperand = new_slot(block[i]->instruction.d);
					block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->binaryOperation->rightOperand);
					break;
				case Bytecode::BC_OP_ISEQS:
				case Bytecode::BC_OP_ISNES:
					block[i]->assignment.expressions.back()->binaryOperation->rightOperand = new_string(function, block[i]->instruction.d);
					break;
				case Bytecode::BC_OP_ISEQN:
				case Bytecode::BC_OP_ISNEN:
					block[i]->assignment.expressions.back()->binaryOperation->rightOperand = new_number(function, block[i]->instruction.d);
					break;
				case Bytecode::BC_OP_ISEQP:
				case Bytecode::BC_OP_ISNEP:
					block[i]->assignment.expressions.back()->binaryOperation->rightOperand = new_primitive(block[i]->instruction.d);
					break;
				}

				break;
			case Bytecode::BC_OP_ISTC:
			case Bytecode::BC_OP_ISFC:
				block[i]->assignment.variables.resize(1);
				block[i]->assignment.variables.back().type = AST_VARIABLE_SLOT;
				block[i]->assignment.variables.back().slot = block[i]->instruction.a;
			case Bytecode::BC_OP_IST:
			case Bytecode::BC_OP_ISF:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_UNARY_OPERATION);
				block[i]->assignment.expressions.back()->unaryOperation->type = AST_UNARY_CONDITION;
				block[i]->assignment.expressions.back()->unaryOperation->operand = new_slot(block[i]->instruction.d);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions.back()->unaryOperation->operand);
				break;
			case Bytecode::BC_OP_JMP:
				block[i]->assignment.expressions.back() = new_expression(AST_EXPRESSION_CONSTANT);
				block[i]->assignment.expressions.back()->constant->type = AST_CONSTANT_CONDITION;
				break;
			}

			continue;
		case AST_STATEMENT_NUMERIC_FOR:
			block[i]->assignment.variables.resize(1);
			block[i]->assignment.variables.back().type = AST_VARIABLE_SLOT;
			block[i]->assignment.variables.back().slot = block[i]->instruction.a + 3;
			if (function.hasDebugInfo) assert(block[i]->locals && block[i]->assignment.variables.back().slot == block[i]->locals->baseSlot,
				"Numeric for loop variable does not match with debug info", bytecode.filePath, DEBUG_INFO);
			block[i]->assignment.expressions.resize(3, nullptr);
			block[i]->assignment.expressions[0] = new_slot(block[i]->instruction.a);
			block[i]->assignment.expressions[1] = new_slot(block[i]->instruction.a + 1);
			block[i]->assignment.expressions[2] = new_slot(block[i]->instruction.a + 2);
			block[i]->assignment.register_open_slots(block[i]->assignment.expressions[0], block[i]->assignment.expressions[1], block[i]->assignment.expressions[2]);
			continue;
		case AST_STATEMENT_GENERIC_FOR:
			block[i]->assignment.variables.resize(block[i]->instruction.b - 1);

			for (uint8_t j = block[i]->assignment.variables.size(); j--;) {
				block[i]->assignment.variables[j].type = AST_VARIABLE_SLOT;
				block[i]->assignment.variables[j].slot = block[i]->instruction.a + j;
			}

			if (function.hasDebugInfo) assert(block[i]->locals && block[i]->assignment.variables.front().slot == block[i]->locals->baseSlot,
				"Generic for loop variables do not match with debug info", bytecode.filePath, DEBUG_INFO);
			block[i]->assignment.expressions.resize(3, nullptr);
			block[i]->assignment.expressions[0] = new_slot(block[i]->instruction.a - 3);
			block[i]->assignment.expressions[1] = new_slot(block[i]->instruction.a - 2);
			block[i]->assignment.expressions[2] = new_slot(block[i]->instruction.a - 1);
			block[i]->assignment.register_open_slots(block[i]->assignment.expressions[0], block[i]->assignment.expressions[1], block[i]->assignment.expressions[2]);
			continue;
		case AST_STATEMENT_DECLARATION:
			block[i]->assignment.variables.resize(block[i]->locals->names.size());
			block[i]->assignment.expressions.resize(block[i]->assignment.variables.size(), nullptr);

			for (uint8_t j = 0; j < block[i]->assignment.variables.size(); j++) {
				block[i]->assignment.variables[j].type = AST_VARIABLE_SLOT;
				block[i]->assignment.variables[j].slot = block[i]->locals->baseSlot + j;
				block[i]->assignment.expressions[j] = new_slot(block[i]->assignment.variables[j].slot);
				block[i]->assignment.register_open_slots(block[i]->assignment.expressions[j]);
			}

			continue;
		}
	}
}

void Ast::collect_slot_scopes(Function& function, std::vector<Statement*>& block) {
	uint32_t id;

	for (uint32_t i = block.size(); i--;) {
		switch (block[i]->type) {
		case AST_STATEMENT_GOTO:
		case AST_STATEMENT_BREAK:
			function.slotScopeCollector.previousLabel =
				block[i]->instruction.type == Bytecode::BC_OP_JMP && block[i]->instruction.info.id != ID_EMPTY ? function.get_label_from_id(block[i]->instruction.target) : ID_EMPTY;
			break;
		case AST_STATEMENT_NUMERIC_FOR:
		case AST_STATEMENT_GENERIC_FOR:
		case AST_STATEMENT_LOOP:
			for (uint32_t j = block[i]->assignment.variables.size(); j--;) {
				function.slotScopeCollector.begin_scope(block[i]->assignment.variables[j].slot, block[i]->instruction.target - 1);
			}

			function.slotScopeCollector.extend_scopes(block[i]->instruction.info.id);
			collect_slot_scopes(function, block[i]->block);
			function.slotScopeCollector.merge_scopes(block[i]->instruction.target - 1);
			function.slotScopeCollector.previousLabel = ID_EMPTY;
			break;
		case AST_STATEMENT_DECLARATION:
			for (uint32_t j = block[i]->assignment.variables.size(); j--;) {
				function.slotScopeCollector.begin_scope(block[i]->assignment.variables[j].slot, function.slotScopeCollector.previousId - 1);
			}

			function.slotScopeCollector.extend_scopes(block[i]->instruction.target);
			collect_slot_scopes(function, block[i]->block);
			break;
		default:
            if (function.slotScopeCollector.previousLabel != ID_EMPTY
                && block[i]->assignment.variables.size() == 1
                && block[i]->assignment.variables.back().type == AST_VARIABLE_SLOT
                && !block[i]->assignment.variables.back().isMultres
                && function.slotScopeCollector.slotInfos[block[i]->assignment.variables.back().slot].activeSlotScope
                && function.slotScopeCollector.slotInfos[block[i]->assignment.variables.back().slot].minScopeBegin == function.labels[function.slotScopeCollector.previousLabel].jumpIds.front() - 1) {
                //TODO
            }

            function.slotScopeCollector.previousLabel = ID_EMPTY;
			break;
		}

		id = block[i]->instruction.info.id == ID_EMPTY ? function.slotScopeCollector.previousId - 1 : block[i]->instruction.info.id;
		function.slotScopeCollector.begin_upvalue_scopes(id);

		if (block[i]->function) {
			for (uint8_t j = block[i]->function->upvalues.size(); j--;) {
				if (!block[i]->function->upvalues[j].local) continue;
				if (block[i]->function->upvalues[j].slot == block[i]->assignment.variables.back().slot) block[i]->function->assignmentSlotIsUpvalue = true;
				block[i]->assignment.usedSlots.emplace_back(block[i]->function->upvalues[j].slot);
				function.slotScopeCollector.add_to_scope(block[i]->function->upvalues[j].slot, block[i]->function->upvalues[j].slotScope, id);
			}
		}

		for (uint8_t j = block[i]->assignment.variables.size(); j--;) {
			switch (block[i]->assignment.variables[j].type) {
			case AST_VARIABLE_SLOT:
				function.slotScopeCollector.complete_scope(block[i]->assignment.variables[j].slot, block[i]->assignment.variables[j].slotScope, id);
				continue;
			case AST_VARIABLE_TABLE_INDEX:
				function.slotScopeCollector.add_to_scope(block[i]->assignment.variables[j].table->variable->slot, block[i]->assignment.variables[j].table->variable->slotScope, id);
				continue;
			}
		}

		for (uint8_t j = block[i]->assignment.openSlots.size(); j--;) {
			function.slotScopeCollector.add_to_scope((*block[i]->assignment.openSlots[j])->variable->slot, (*block[i]->assignment.openSlots[j])->variable->slotScope, id);
		}

		if (block[i]->instruction.info.id != ID_EMPTY) {
			if (function.is_valid_label(block[i]->instruction.info.attachedLabel)) {
				function.slotScopeCollector.previousLabel = block[i]->instruction.info.attachedLabel;
				id = function.get_scope_end_from_label(block[i]->instruction.info.attachedLabel);
				function.slotScopeCollector.merge_scopes(id);
				function.slotScopeCollector.extend_scopes(function.get_scope_begin_from_label(block[i]->instruction.info.attachedLabel, id));
			}

			function.slotScopeCollector.previousId = block[i]->instruction.info.id;
		}
	}
}

void Ast::eliminate_slots(Function& function, std::vector<Statement*>& block) {
	for (uint32_t i = 0; i < block.size(); i++) {
		if (block[i]->condition.allowSlotSwap
			&& i >= 2
			&& !function.is_valid_label(block[i]->instruction.info.attachedLabel)
			&& !function.is_valid_label(block[i - 1]->instruction.info.attachedLabel)
			&& block[i - 1]->type == AST_STATEMENT_ASSIGNMENT
			&& block[i - 1]->assignment.variables.size() == 1
			&& block[i - 1]->assignment.variables.back().type == AST_VARIABLE_SLOT
			&& (*block[i - 1]->assignment.variables.back().slotScope)->usages == 1
			&& block[i - 1]->assignment.variables.back().slot == block[i]->assignment.expressions.back()->binaryOperation->leftOperand->variable->slot
			&& block[i - 2]->type == AST_STATEMENT_ASSIGNMENT
			&& block[i - 2]->assignment.variables.size() == 1
			&& block[i - 2]->assignment.variables.back().type == AST_VARIABLE_SLOT
			&& (*block[i - 2]->assignment.variables.back().slotScope)->usages == 1
			&& block[i - 2]->assignment.variables.back().slot == block[i]->assignment.expressions.back()->binaryOperation->rightOperand->variable->slot) {
			Expression* expression = block[i]->assignment.expressions.back()->binaryOperation->leftOperand;
			block[i]->assignment.expressions.back()->binaryOperation->leftOperand = block[i]->assignment.expressions.back()->binaryOperation->rightOperand;
			block[i]->assignment.expressions.back()->binaryOperation->rightOperand = expression;
			block[i]->condition.swapped = true;
		}

		for (uint8_t j = block[i]->assignment.openSlots.size(); j--
			&& i
			&& !function.is_valid_label(block[i]->instruction.info.attachedLabel)
			&& block[i - 1]->type == AST_STATEMENT_ASSIGNMENT
			&& block[i - 1]->assignment.variables.size() == 1
			&& block[i - 1]->assignment.variables.back().type == AST_VARIABLE_SLOT
			&& (*block[i - 1]->assignment.variables.back().slotScope)->usages == 1;) {
			if (j == 1
				&& block[i]->assignment.isPotentialMethod
				&& i >= 2
				&& !function.is_valid_label(block[i - 1]->instruction.info.attachedLabel)
				&& block[i - 1]->assignment.variables.back().slot == block[i]->assignment.expressions.back()->functionCall->function->variable->slot
				&& block[i - 1]->assignment.expressions.size() == 1
				&& block[i - 1]->assignment.usedSlots.size() == 1
				&& block[i - 1]->assignment.expressions.back()->type == AST_EXPRESSION_VARIABLE
				&& block[i - 1]->assignment.expressions.back()->variable->type == AST_VARIABLE_TABLE_INDEX
				&& block[i - 1]->assignment.expressions.back()->variable->table->type == AST_EXPRESSION_VARIABLE
				&& block[i - 1]->assignment.expressions.back()->variable->table->variable->type == AST_VARIABLE_SLOT
				&& block[i - 1]->assignment.expressions.back()->variable->tableIndex->type == AST_EXPRESSION_CONSTANT
				&& block[i - 1]->assignment.expressions.back()->variable->tableIndex->constant->type == AST_CONSTANT_STRING
				&& block[i - 1]->assignment.expressions.back()->variable->tableIndex->constant->isName
				&& block[i - 2]->type == AST_STATEMENT_ASSIGNMENT
				&& block[i - 2]->assignment.variables.size() == 1
				&& block[i - 2]->assignment.variables.back().type == AST_VARIABLE_SLOT
				&& (*block[i - 2]->assignment.variables.back().slotScope)->usages == 1
				&& block[i - 2]->assignment.variables.back().slot == block[i]->assignment.expressions.back()->functionCall->arguments.front()->variable->slot
				&& block[i - 2]->assignment.expressions.size() == 1
				&& block[i - 2]->assignment.usedSlots.size() == 1
				&& block[i - 2]->assignment.expressions.back()->type == AST_EXPRESSION_VARIABLE
				&& block[i - 2]->assignment.expressions.back()->variable->type == AST_VARIABLE_SLOT
				&& block[i - 2]->assignment.expressions.back()->variable->slot == block[i - 1]->assignment.expressions.back()->variable->table->variable->slot) {
				block[i]->assignment.expressions.back()->functionCall->isMethod = true;
				block[i]->assignment.expressions.back()->functionCall->arguments.erase(block[i]->assignment.expressions.back()->functionCall->arguments.begin());
				block[i]->assignment.openSlots.erase(block[i]->assignment.openSlots.begin() + j);
				block[i]->assignment.openSlots.emplace(block[i]->assignment.openSlots.begin(), &block[i - 1]->assignment.expressions.back()->variable->table);
				function.slotScopeCollector.remove_scope(block[i - 2]->assignment.variables.back().slot, block[i - 2]->assignment.variables.back().slotScope);
				block[i - 1]->instruction.info = block[i - 2]->instruction.info;
				(*block[i - 2]->assignment.expressions.back()->variable->slotScope)->usages--;
				i--;
				block.erase(block.begin() + i - 1);
			}

			if (block[i - 1]->assignment.variables.back().slot != (*block[i]->assignment.openSlots[j])->variable->slot) continue;
			assert(block[i - 1]->assignment.variables.back().isMultres == (*block[i]->assignment.openSlots[j])->variable->isMultres,
				"Multres type mismatch when trying to eliminate slot", bytecode.filePath, DEBUG_INFO);
			*block[i]->assignment.openSlots[j] = block[i - 1]->assignment.expressions.back();
			function.slotScopeCollector.remove_scope(block[i - 1]->assignment.variables.back().slot, block[i - 1]->assignment.variables.back().slotScope);
			block[i]->assignment.usedSlots.reserve(block[i]->assignment.usedSlots.size() + block[i - 1]->assignment.usedSlots.size());
			block[i]->assignment.usedSlots.insert(block[i]->assignment.usedSlots.end(), block[i - 1]->assignment.usedSlots.begin(), block[i - 1]->assignment.usedSlots.end());
			block[i]->instruction.info = block[i - 1]->instruction.info;
			i--;
			block.erase(block.begin() + i);
		}

		assert(!block[i]->assignment.openSlots.size()
			|| (*block[i]->assignment.openSlots.back())->type != AST_EXPRESSION_VARIABLE
			|| !(*block[i]->assignment.openSlots.back())->variable->isMultres,
			"Unable to eliminate multres slot", bytecode.filePath, DEBUG_INFO);

		for (uint8_t j = block[i]->assignment.variables.size(); j--;) {
			switch (block[i]->assignment.variables[j].type) {
			case AST_VARIABLE_SLOT: //TODO
				continue;
			case AST_VARIABLE_TABLE_INDEX:
				if (i
					&& !function.is_valid_label(block[i]->instruction.info.attachedLabel)
					&& block[i - 1]->type == AST_STATEMENT_ASSIGNMENT
					&& block[i - 1]->assignment.variables.size() == 1
					&& block[i - 1]->assignment.variables.back().type == AST_VARIABLE_SLOT
					&& block[i - 1]->assignment.variables.back().slot == block[i]->assignment.variables[j].table->variable->slot) {
					if (block[i - 1]->assignment.isTableConstructor && !block[i - 1]->assignment.expressions.back()->table->multresField) {
						for (uint32_t k = block[i]->assignment.usedSlots.size(); k-- && block[i]->assignment.usedSlots[k] != block[i]->assignment.variables[j].table->variable->slot;) {
							block[i]->assignment.usedSlots.erase(block[i]->assignment.usedSlots.begin() + k);
						}

						if (block[i]->assignment.variables[j].isMultres) {
							assert(!block[i]->assignment.usedSlots.size(), "Table multres assignment has illegal reference to itself", bytecode.filePath, DEBUG_INFO);
							block[i - 1]->assignment.expressions.back()->table->multresIndex = block[i]->assignment.variables[j].multresIndex;
							block[i - 1]->assignment.expressions.back()->table->multresField = block[i]->assignment.expressions.back();
						} else {
							if (block[i]->assignment.usedSlots.size()
								|| (get_constant_type(block[i]->assignment.variables[j].tableIndex).type
									&& get_constant_type(block[i]->assignment.expressions.back()).type))
								continue;
							block[i - 1]->assignment.expressions.back()->table->fields.emplace_back();
							block[i - 1]->assignment.expressions.back()->table->fields.back().key = block[i]->assignment.variables[j].tableIndex;
							block[i - 1]->assignment.expressions.back()->table->fields.back().value = block[i]->assignment.expressions.back();
						}

						block[i]->instruction.info = block[i - 1]->instruction.info;
						block[i]->assignment = block[i - 1]->assignment;
						i--;
						block.erase(block.begin() + i);
						(*block[i]->assignment.variables.back().slotScope)->usages--;
					} else if (block[i]->assignment.variables[j].isMultres) {
						assert(false, "Unable to eliminate multres table index", bytecode.filePath, DEBUG_INFO);
					} else if ((*block[i - 1]->assignment.variables.back().slotScope)->usages == 1) {
						block[i]->assignment.variables[j].table = block[i - 1]->assignment.expressions.back();
						function.slotScopeCollector.remove_scope(block[i - 1]->assignment.variables.back().slot, block[i - 1]->assignment.variables.back().slotScope);
						block[i]->instruction.info = block[i - 1]->instruction.info;
						i--;
						block.erase(block.begin() + i);
					}
				}

				continue;
			}
		}

		switch (block[i]->type) {
		case AST_STATEMENT_NUMERIC_FOR:
		case AST_STATEMENT_GENERIC_FOR:
		case AST_STATEMENT_LOOP:
		case AST_STATEMENT_DECLARATION:
			eliminate_slots(function, block[i]->block);
		}
	}
}

/*struct Ast::ConditionGrouper {
	struct ConditionBlock {
		uint32_t jumpTargetId = 0;
		uint32_t endTargetId = 0;
		uint32_t conditionEnd = 0;
		std::vector<Statement*> condition;
	};

	ConditionGrouper() = delete;
	ConditionGrouper(SimpleAst& simpleAst, std::vector<Statement*>& statementBlock) : simpleAst(simpleAst), statementBlock(statementBlock) {}

	const bool& add_condition(const Statement::Instruction& condition, const uint32_t& index) {
		for (uint32_t i = 0; i < conditionBlocks.size(); i++) {
			if (condition.jumpTargetId > conditionBlocks[i].endTargetId) {
				if (condition.id == conditionBlocks[i].endTargetId - 2) {
					conditionBlocks[i].jumpTargetId = condition.jumpTargetId;
					conditionBlocks[i].conditionEnd = index + 1;
				}

				continue;
			}

			if (condition.jumpTargetId < conditionBlocks[i].endTargetId) {
				conditionBlocks.emplace(conditionBlocks.begin() + i);
				conditionBlocks[i].endTargetId = condition.jumpTargetId;
				return true;
			}

			return false;
		}

		conditionBlocks.emplace_back();
		conditionBlocks.back().endTargetId = condition.jumpTargetId;
		return true;
	}

	Statement* const& get_if_statement(const uint32_t& conditionStart, const uint32_t& conditionEnd) {
		Statement* const ifStatement = simpleAst.new_statement(AST_STATEMENT_IF);
		const uint32_t blockEndTargetId = statementBlock[conditionEnd - 1]->instruction.jumpTargetId;
		const uint32_t blockJumpTargetId = conditionEnd == statementBlock.size() ? blockEndTargetId : get_target_id_from_statement(statementBlock[conditionEnd]);

		for (uint32_t i = conditionBlocks.size(); i--;) {
			if (conditionBlocks[i].endTargetId == blockJumpTargetId || conditionBlocks[i].endTargetId == blockEndTargetId) conditionBlocks.erase(conditionBlocks.begin() + i);
		}

		conditionBlocks.emplace_back();
		conditionBlocks.back().jumpTargetId = blockEndTargetId;
		conditionBlocks.back().endTargetId = blockJumpTargetId;
		conditionBlocks.back().conditionEnd = conditionEnd;

		for (uint32_t i = conditionBlocks.size(); i--;) {
			assert(statementBlock[conditionBlocks[i].conditionEnd - 1]->instruction.jumpTargetId == conditionBlocks[i].jumpTargetId,
				"Condition at end of condition block has\ninvalid JMP instruction target", simpleAst.bytecode.filePath, DEBUG_INFO);
			const uint32_t& conditionStop = i ? conditionBlocks[i - 1].conditionEnd : conditionStart;
			const bool isExitAtJumpTarget = conditionBlocks[i].jumpTargetId == blockEndTargetId;
			conditionBlocks[i].condition.emplace_back(statementBlock[conditionBlocks[i].conditionEnd - 1]);
			conditionBlocks[i].condition.back()->negated = isExitAtJumpTarget;

			for (uint32_t j = conditionBlocks[i].conditionEnd - 1; j-- > conditionStop;) {
				if (statementBlock[j]->type == AST_STATEMENT_CONDITION) {
					assert(statementBlock[j]->instruction.jumpTargetId == conditionBlocks[i].jumpTargetId || statementBlock[j]->instruction.jumpTargetId == conditionBlocks[i].endTargetId,
						"Condition inside condition block has\ninvalid JMP instruction target", simpleAst.bytecode.filePath, DEBUG_INFO);
					conditionBlocks[i].condition.emplace_back(simpleAst.new_statement(
						isExitAtJumpTarget == (statementBlock[j]->instruction.jumpTargetId == conditionBlocks[i].jumpTargetId) ? AST_STATEMENT_AND : AST_STATEMENT_OR));
					conditionBlocks[i].condition.back()->statements = conditionBlocks[i].condition;
					conditionBlocks[i].condition.erase(conditionBlocks[i].condition.begin(), conditionBlocks[i].condition.end() - 1);
					conditionBlocks[i].condition.front()->statements.pop_back();
					conditionBlocks[i].condition.front()->statements.shrink_to_fit();
					conditionBlocks[i].condition.front()->condition = statementBlock[j];
					conditionBlocks[i].condition.front()->negated = conditionBlocks[i].condition.front()->type == AST_STATEMENT_AND;
					conditionBlocks[i].condition.front()->instruction = conditionBlocks[i].condition.front()->condition->instruction;
				} else {
					conditionBlocks[i].condition.emplace(conditionBlocks[i].condition.begin(), statementBlock[j]);
				}
			}

			conditionBlocks[i].condition.shrink_to_fit();
		}

		//ifStatement->instruction = ifStatement->condition->instruction;
		ifStatement->instruction.jumpTargetId = blockEndTargetId;
		conditionBlocks.clear();
		conditionBlocks.shrink_to_fit();
		return ifStatement;
	}

	SimpleAst& simpleAst;
	std::vector<Statement*>& statementBlock;
	std::vector<ConditionBlock> conditionBlocks;
};*/

void Ast::build_conditions(Function& function, std::vector<Statement*>& block) {
	//ConditionGrouper conditionGrouper(*this, currentBlock); uint32_t maxEndTarget, nextMaxEndTarget, endTarget;

	for (uint32_t i = 0; i < block.size(); i++) {
		/*if (currentBlock[i]->type != AST_STATEMENT_CONDITION) continue;
		endTarget = i + 1;

		while (true) {
			//if (currentBlock[j]->type != AST_STATEMENT_CONDITION || !conditionGrouper.add_condition(currentBlock[j]->instruction, j)) continue;
			maxEndTarget = get_index_from_statement_id(currentBlock, currentBlock[endTarget - 1]->instruction.jumpTargetId);
			if (maxEndTarget == ID_NOT_FOUND || maxEndTarget <= i) maxEndTarget = currentBlock.size();

			for (uint32_t j = endTarget; j < maxEndTarget; j++) {
				switch (currentBlock[j]->type) {
				case AST_STATEMENT_CONDITION:
					nextMaxEndTarget = get_index_from_statement_id(currentBlock, currentBlock[j]->instruction.jumpTargetId);
					if (nextMaxEndTarget == ID_NOT_FOUND || nextMaxEndTarget <= i) nextMaxEndTarget = currentBlock.size();
					if (nextMaxEndTarget > i && nextMaxEndTarget <= j) break;

					for (uint32_t k = j + 1; k < maxEndTarget; k++) {

					}
				case AST_STATEMENT_INSTRUCTION:
				case AST_STATEMENT_FUNCTION:
					continue;
				}

				break;
			}*/


			/*for (uint32_t k = j + 1; k < jumpTarget; k++) {
				switch (currentBlock[k]->type) {
				case AST_STATEMENT_GOTO:
				case AST_STATEMENT_BREAK:
					if (get_target_id_from_statement(currentBlock[k]) == currentBlock[j]->instruction.jumpTargetId) {
						jumpTarget = k;
						break;
					}
				default:
					continue;
				}

				break;
			}

			if (jumpTarget > endTarget
				&& currentBlock[jumpTarget - 1]->type == AST_STATEMENT_CONDITION
				&& jumpTarget != currentBlock.size()
				&& get_target_id_from_statement(currentBlock[jumpTarget]) != currentBlock[jumpTarget - 1]->instruction.jumpTargetId)
				endTarget = jumpTarget;*/
		//}

		/*currentBlock.emplace(currentBlock.begin() + endTarget, conditionGrouper.get_if_statement(i, endTarget));
		//currentFunction.add_goto(currentBlock[endTarget]->instruction);
		currentBlock.erase(currentBlock.begin() + i, currentBlock.begin() + endTarget);
		i++;*/
	}
}

uint32_t Ast::get_block_index_from_id(const std::vector<Statement*>& block, const uint32_t& id) {
	for (uint32_t i = block.size(); i-- && (block[i]->instruction.info.id == ID_EMPTY || block[i]->instruction.info.id >= id);) {
		if (block[i]->instruction.info.id == id) return i;
	}

	return ID_EMPTY;
}

uint32_t Ast::get_extended_id_from_statement(Statement* const& statement) {
	switch (statement->type) {
	case AST_STATEMENT_GOTO:
	case AST_STATEMENT_BREAK:
		if (statement->instruction.type == Bytecode::BC_OP_JMP) return statement->instruction.target;
	}

	return statement->instruction.info.id;
}

void Ast::check_valid_name(Constant* const& constant) {
	static constexpr uint8_t KEYWORDS_COUNT = 21;

	static const std::string KEYWORDS[KEYWORDS_COUNT] = {
		"and", "break", "do", "else", "elseif", "end", "false",
		"for", "function", "if", "in", "local", "nil", "not",
		"or", "repeat", "return", "then", "true", "until", "while"
	};

	if (!constant->string.size() || constant->string.front() < 'A') return;

	for (uint32_t i = constant->string.size(); i--;) {
		if (constant->string[i] < '0' || constant->string[i] > 'z') return;

		switch (constant->string[i]) {
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
			return;
		}
	}

	for (uint8_t i = KEYWORDS_COUNT; i--;) {
		if (KEYWORDS[i] == constant->string) return;
	}

	constant->isName = true;
}

void Ast::check_special_number(Expression* const& expression, const bool& convertInfinity) {
	const uint64_t raw_double = std::bit_cast<uint64_t>(expression->constant->number);
	if ((raw_double & DOUBLE_EXPONENT) != DOUBLE_SPECIAL) return assert(raw_double != DOUBLE_NEGATIVE_ZERO, "Number constant is negative zero", bytecode.filePath, DEBUG_INFO);
	assert(!(raw_double & DOUBLE_FRACTION), "Number constant is NaN", bytecode.filePath, DEBUG_INFO);
	if (!convertInfinity) return;
	expression->set_type(AST_EXPRESSION_BINARY_OPERATION);
	expression->binaryOperation->type = AST_BINARY_DIVISION;
	expression->binaryOperation->leftOperand = new_expression(AST_EXPRESSION_CONSTANT);
	expression->binaryOperation->leftOperand->constant->type = AST_CONSTANT_NUMBER;
	expression->binaryOperation->leftOperand->constant->number = raw_double & DOUBLE_SIGN ? -1 : 1;
	expression->binaryOperation->rightOperand = new_expression(AST_EXPRESSION_CONSTANT);
	expression->binaryOperation->rightOperand->constant->type = AST_CONSTANT_NUMBER;
	expression->binaryOperation->rightOperand->constant->number = 0;
}

Ast::ConstantType Ast::get_constant_type(Expression* const& expression) {
	static bool (* const is_valid_number_constant)(const double&) = [](const double& number)->bool {
		const uint64_t raw_double = std::bit_cast<uint64_t>(number);
		return (raw_double & DOUBLE_EXPONENT) == DOUBLE_SPECIAL ? !(raw_double & DOUBLE_FRACTION) : raw_double != DOUBLE_NEGATIVE_ZERO;
	};

	ConstantType constantType;

	switch (expression->type) {
	case AST_EXPRESSION_CONSTANT:
		switch (expression->constant->type) {
		case AST_CONSTANT_NIL:
		case AST_CONSTANT_FALSE:
			constantType.type = ConstantType::FALSY_CONSTANT;
			break;
		case AST_CONSTANT_TRUE:
		case AST_CONSTANT_STRING:
			constantType.type = ConstantType::TRUTHY_CONSTANT;
			break;
		case AST_CONSTANT_NUMBER:
			constantType.type = ConstantType::NUMBER_CONSTANT;
			constantType.number = expression->constant->number;
			break;
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
		case AST_BINARY_AND:
		case AST_BINARY_OR:
			constantType = get_constant_type(expression->binaryOperation->leftOperand);

			switch (expression->binaryOperation->type) {
			case AST_BINARY_ADDITION:
			case AST_BINARY_SUBTRACTION:
			case AST_BINARY_MULTIPLICATION:
			case AST_BINARY_DIVISION:
			case AST_BINARY_EXPONENTATION:
			case AST_BINARY_MODULO:
				if (constantType.type == ConstantType::NUMBER_CONSTANT) {
					ConstantType rightConstantType = get_constant_type(expression->binaryOperation->rightOperand);

					if (rightConstantType.type == ConstantType::NUMBER_CONSTANT) {
						switch (expression->binaryOperation->type) {
						case AST_BINARY_ADDITION:
							constantType.number += rightConstantType.number;
							break;
						case AST_BINARY_SUBTRACTION:
							constantType.number -= rightConstantType.number;
							break;
						case AST_BINARY_MULTIPLICATION:
							constantType.number *= rightConstantType.number;
							break;
						case AST_BINARY_DIVISION:
							constantType.number /= rightConstantType.number;
							break;
						case AST_BINARY_EXPONENTATION:
							constantType.number = std::pow(constantType.number, rightConstantType.number);
							break;
						case AST_BINARY_MODULO:
							constantType.number = std::fmod(constantType.number, rightConstantType.number);
							break;
						}

						if (is_valid_number_constant(constantType.number)) break;
					}
				}

				constantType.type = ConstantType::INVALID_CONSTANT;
				break;
			case AST_BINARY_AND: //TODO
				switch (constantType.type) {
				case ConstantType::FALSY_CONSTANT:
					constantType.type = ConstantType::INVALID_CONSTANT;
					break;
				case ConstantType::TRUTHY_CONSTANT:
				case ConstantType::NUMBER_CONSTANT:
					constantType = get_constant_type(expression->binaryOperation->rightOperand);
					break;
				}

				break;
			case AST_BINARY_OR: //TODO
				switch (constantType.type) {
				case ConstantType::FALSY_CONSTANT:
					constantType = get_constant_type(expression->binaryOperation->rightOperand);
					break;
				case ConstantType::TRUTHY_CONSTANT:
				case ConstantType::NUMBER_CONSTANT:
					constantType.type = ConstantType::INVALID_CONSTANT;
					break;
				}

				break;
			}

			break;
		}

		break;
	case AST_EXPRESSION_UNARY_OPERATION:
		switch (expression->unaryOperation->type) {
		case AST_UNARY_MINUS:
			constantType = get_constant_type(expression->unaryOperation->operand);

			if (constantType.type == ConstantType::NUMBER_CONSTANT) {
				constantType.number = -constantType.number;
				if (is_valid_number_constant(constantType.number)) break;
			}

			constantType.type = ConstantType::INVALID_CONSTANT;
			break;
		case AST_UNARY_NOT:
			switch (get_constant_type(expression->unaryOperation->operand).type) {
			case ConstantType::FALSY_CONSTANT:
				constantType.type = ConstantType::TRUTHY_CONSTANT;
				break;
			case ConstantType::TRUTHY_CONSTANT:
			case ConstantType::NUMBER_CONSTANT:
				constantType.type = ConstantType::FALSY_CONSTANT;
				break;
			}

			break;
		}

		break;
	}

	return constantType;
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

Ast::Expression* Ast::new_primitive(const uint16_t& primitive) {
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
		check_special_number(expression, true);
		break;
	}

	return expression;
}

Ast::Expression* Ast::new_string(const Function& function, const uint16_t& index) {
	Expression* expression = new_expression(AST_EXPRESSION_CONSTANT);
	expression->constant->type = AST_CONSTANT_STRING;
	expression->constant->string = function.get_constant(index).string;
	return expression;
}

Ast::Expression* Ast::new_table(const Function& function, const uint16_t& index) {
	static Expression* (* const new_table_constant)(Ast&, const Bytecode::TableConstant&) = [](Ast& ast, const Bytecode::TableConstant& constant)->Expression* {
		Expression* expression = ast.new_expression(AST_EXPRESSION_CONSTANT);

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
			ast.check_special_number(expression, true);
			break;
		case Bytecode::BC_KTAB_STR:
			expression->constant->type = AST_CONSTANT_STRING;
			expression->constant->string = constant.string;
			break;
		}

		return expression;
	};

	Expression* expression = new_expression(AST_EXPRESSION_TABLE);
	expression->table->constants.list.resize(function.get_constant(index).array.size(), nullptr);

	for (uint32_t i = 0; i < expression->table->constants.list.size(); i++) {
		expression->table->constants.list[i] = new_table_constant(*this, function.get_constant(index).array[i]);
	}

	expression->table->constants.fields.resize(function.get_constant(index).table.size());

	for (uint32_t i = 0; i < expression->table->fields.size(); i++) {
		expression->table->constants.fields[i].key = new_table_constant(*this, function.get_constant(index).table[i].key);
		if (expression->table->constants.fields[i].key->type == AST_EXPRESSION_CONSTANT && expression->table->constants.fields[i].key->constant->type == AST_CONSTANT_STRING)
			check_valid_name(expression->table->constants.fields[i].key->constant);
		expression->table->constants.fields[i].value = new_table_constant(*this, function.get_constant(index).table[i].value);
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
		check_special_number(expression, false);
		break;
	}

	return expression;
}
