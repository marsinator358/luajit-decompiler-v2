#include "..\main.h"

Lua::Lua(const Bytecode& bytecode, const Ast& ast, const std::string& filePath, const bool& forceOverwrite, const bool& minimizeDiffs, const bool& unrestrictedAscii)
	: bytecode(bytecode), ast(ast), filePath(filePath), forceOverwrite(forceOverwrite), minimizeDiffs(minimizeDiffs), unrestrictedAscii(unrestrictedAscii) {}

Lua::~Lua() {
	close_file();
}

void Lua::operator()() {
	print_progress_bar();
	prototypeDataLeft = bytecode.prototypesTotalSize;
	write_header();
	if (ast.chunk->block.size()) write_block(*ast.chunk, ast.chunk->block);
	prototypeDataLeft -= ast.chunk->prototype.prototypeSize;
	print_progress_bar(bytecode.prototypesTotalSize - prototypeDataLeft, bytecode.prototypesTotalSize);
	create_file();
	write_file();
	close_file();
	erase_progress_bar();
}

void Lua::write_header() {
	if (!unrestrictedAscii) write(UTF8_BOM);
	if (!bytecode.header.chunkname.size()) return;
	write("-- chunkname: ");
	write_string(bytecode.header.chunkname);
	write(NEW_LINE, NEW_LINE);
}

void Lua::write_block(const Ast::Function& function, const std::vector<Ast::Statement*>& block) {
	std::vector<Ast::Statement*>* elseBlock;
	bool isFunctionDefinition;
	bool previousLineIsEmpty = true;

	if (!block.size()) {
		write_indent();
		write("-- block empty", NEW_LINE);
		return;
	}

	for (uint32_t i = 0; i < block.size(); i++) {
		if (!previousLineIsEmpty) {
			switch (block[i - 1]->type) {
			case Ast::AST_STATEMENT_RETURN:
				if (block[i]->type != Ast::AST_STATEMENT_RETURN) previousLineIsEmpty = true;
				break;
			case Ast::AST_STATEMENT_GOTO:
			case Ast::AST_STATEMENT_BREAK:
				if (block[i]->type != Ast::AST_STATEMENT_GOTO && block[i]->type != Ast::AST_STATEMENT_BREAK) previousLineIsEmpty = true;
				break;
			case Ast::AST_STATEMENT_DECLARATION:
				if (block[i]->type != Ast::AST_STATEMENT_DECLARATION) previousLineIsEmpty = true;
				break;
			case Ast::AST_STATEMENT_ASSIGNMENT:
				if (block[i]->type != Ast::AST_STATEMENT_ASSIGNMENT) previousLineIsEmpty = true;
				break;
			case Ast::AST_STATEMENT_FUNCTION_CALL:
				if (block[i]->type != Ast::AST_STATEMENT_FUNCTION_CALL) previousLineIsEmpty = true;
				break;
			case Ast::AST_STATEMENT_LABEL:
				if (block[i]->type != Ast::AST_STATEMENT_LABEL) previousLineIsEmpty = true;
				break;
			case Ast::AST_STATEMENT_NUMERIC_FOR:
			case Ast::AST_STATEMENT_GENERIC_FOR:
			case Ast::AST_STATEMENT_IF:
			case Ast::AST_STATEMENT_ELSE:
			case Ast::AST_STATEMENT_WHILE:
			case Ast::AST_STATEMENT_REPEAT:
			case Ast::AST_STATEMENT_DO:
				previousLineIsEmpty = true;
				break;
			}

			if (previousLineIsEmpty) write(NEW_LINE);
		}

		switch (block[i]->type) {
		case Ast::AST_STATEMENT_RETURN:
			write_indent();
			if (i != block.size() - 1) write("do ");
			write("return");

			if (block[i]->assignment.expressions.size() || block[i]->assignment.multresReturn) {
				write(" ");
				write_expression_list(block[i]->assignment.expressions, block[i]->assignment.multresReturn);
			}

			if (i != block.size() - 1) write(" end");
			break;
		case Ast::AST_STATEMENT_GOTO:
			write_indent();
			write("goto ", function.labels[block[i]->instruction.label].name);
			break;
		case Ast::AST_STATEMENT_NUMERIC_FOR:
			write_indent();
			write("for ");
			write_variable(block[i]->assignment.variables.back(), false);
			write(" = ");
			write_expression(*block[i]->assignment.expressions[0], false);
			write(", ");
			write_expression(*block[i]->assignment.expressions[1], false);

			if (block[i]->assignment.expressions.size() == 3) {
				write(", ");
				write_expression(*block[i]->assignment.expressions[2], false);
			}

			write(" do", NEW_LINE);
			indentLevel++;
			write_block(function, block[i]->block);
			indentLevel--;
			write_indent();
			write("end");
			break;
		case Ast::AST_STATEMENT_GENERIC_FOR:
			write_indent();
			write("for ");
			write_assignment(block[i]->assignment.variables, block[i]->assignment.expressions, " in ", false);
			write(" do", NEW_LINE);
			indentLevel++;
			write_block(function, block[i]->block);
			indentLevel--;
			write_indent();
			write("end");
			break;
		case Ast::AST_STATEMENT_BREAK:
			write_indent();
			if (i != block.size() - 1) write("do ");
			write("break");
			if (i != block.size() - 1) write(" end");
			break;
		case Ast::AST_STATEMENT_DECLARATION:
			isFunctionDefinition = false;

			if (block[i]->assignment.variables.size() == 1
				&& block[i]->assignment.expressions.size() == 1
				&& block[i]->assignment.expressions.back()->type == Ast::AST_EXPRESSION_FUNCTION) {
				isFunctionDefinition = true;

				if (!block[i]->assignment.expressions.back()->function->assignmentSlotIsUpvalue) {
					for (uint8_t j = block[i]->assignment.expressions.back()->function->upvalues.size(); j--;) {
						if ((*block[i]->assignment.expressions.back()->function->upvalues[j].slotScope)->name != (*block[i]->assignment.variables.back().slotScope)->name) continue;
						isFunctionDefinition = false;
						break;
					}

					if (isFunctionDefinition) {
						for (uint32_t j = block[i]->assignment.expressions.back()->function->usedGlobals.size(); j--;) {
							if (*block[i]->assignment.expressions.back()->function->usedGlobals[j] != (*block[i]->assignment.variables.back().slotScope)->name) continue;
							isFunctionDefinition = false;
							break;
						}
					}
				}
			}

			if (isFunctionDefinition) {
				if (!previousLineIsEmpty) write(NEW_LINE);
				write_indent();
				write("local function ");
				write_variable(block[i]->assignment.variables.back(), false);
				write_function_definition(*block[i]->assignment.expressions.back()->function, false);

				if (i != block.size() - 1) {
					write(NEW_LINE, NEW_LINE);
					previousLineIsEmpty = true;
					continue;
				}
			} else {
				write_indent();
				write("local ");
				write_assignment(block[i]->assignment.variables, block[i]->assignment.expressions, " = ", false);
			}

			break;
		case Ast::AST_STATEMENT_ASSIGNMENT:
			isFunctionDefinition = false;

			if (block[i]->assignment.variables.size() == 1
				&& block[i]->assignment.expressions.size() == 1
				&& block[i]->assignment.expressions.back()->type == Ast::AST_EXPRESSION_FUNCTION) {
				for (Ast::Variable* variable = &block[i]->assignment.variables.back(); true; variable = variable->table->variable) {
					switch (variable->type) {
					case Ast::AST_VARIABLE_SLOT:
					case Ast::AST_VARIABLE_UPVALUE:
					case Ast::AST_VARIABLE_GLOBAL:
						isFunctionDefinition = true;
						break;
					case Ast::AST_VARIABLE_TABLE_INDEX:
						if (variable->table->type == Ast::AST_EXPRESSION_VARIABLE
							&& variable->tableIndex->type == Ast::AST_EXPRESSION_CONSTANT
							&& variable->tableIndex->constant->isName)
							continue;
					}

					break;
				}
			}

			if (isFunctionDefinition) {
				if (!previousLineIsEmpty) write(NEW_LINE);
				write_indent();
				write("function ");

				if (block[i]->assignment.variables.back().type == Ast::AST_VARIABLE_TABLE_INDEX
					&& block[i]->assignment.expressions.back()->function->parameterNames.size()
					&& block[i]->assignment.expressions.back()->function->parameterNames.front() == "self") {
					write_variable(*block[i]->assignment.variables.back().table->variable, false);
					write(":", block[i]->assignment.variables.back().tableIndex->constant->string);
					write_function_definition(*block[i]->assignment.expressions.back()->function, true);
				} else {
					write_variable(block[i]->assignment.variables.back(), false);
					write_function_definition(*block[i]->assignment.expressions.back()->function, false);
				}

				if (i != block.size() - 1) {
					write(NEW_LINE, NEW_LINE);
					previousLineIsEmpty = true;
					continue;
				}

				break;
			}

			write_indent();
			write_assignment(block[i]->assignment.variables, block[i]->assignment.expressions, " = ", i);
			break;
		case Ast::AST_STATEMENT_FUNCTION_CALL:
			write_indent();
			write_function_call(*block[i]->assignment.expressions.back()->functionCall, i);
			break;
		case Ast::AST_STATEMENT_IF:
			write_indent();
			write("if ");
			write_expression(*block[i]->assignment.expressions.back(), false);
			write(" then", NEW_LINE);
			indentLevel++;
			write_block(function, block[i]->block);
			indentLevel--;
			write_indent();

			if (i + 1 < block.size() && block[i + 1]->type == Ast::AST_STATEMENT_ELSE) {
				i++;
				elseBlock = &block[i]->block;

				while (true) {
					if (elseBlock->size() == 1 && elseBlock->front()->type == Ast::AST_STATEMENT_IF) {
						write("elseif ");
						write_expression(*elseBlock->front()->assignment.expressions.back(), false);
						write(" then", NEW_LINE);
						indentLevel++;
						write_block(function, elseBlock->front()->block);
						indentLevel--;
						write_indent();
					} else if (elseBlock->size() == 2
						&& elseBlock->front()->type == Ast::AST_STATEMENT_IF
						&& elseBlock->back()->type == Ast::AST_STATEMENT_ELSE) {
						write("elseif ");
						write_expression(*elseBlock->front()->assignment.expressions.back(), false);
						write(" then", NEW_LINE);
						indentLevel++;
						write_block(function, elseBlock->front()->block);
						indentLevel--;
						write_indent();
						elseBlock = &elseBlock->back()->block;
						continue;
					} else {
						write("else", NEW_LINE);
						indentLevel++;
						write_block(function, *elseBlock);
						indentLevel--;
						write_indent();
					}

					break;
				}
			}

			write("end");
			break;
		case Ast::AST_STATEMENT_WHILE:
			write_indent();
			write("while ");
			write_expression(*block[i]->assignment.expressions.back(), false);
			write(" do", NEW_LINE);
			indentLevel++;
			write_block(function, block[i]->block);
			indentLevel--;
			write_indent();
			write("end");
			break;
		case Ast::AST_STATEMENT_REPEAT:
			write_indent();
			write("repeat", NEW_LINE);
			indentLevel++;
			write_block(function, block[i]->block);
			indentLevel--;
			write_indent();
			write("until ");
			write_expression(*block[i]->assignment.expressions.back(), false);
			break;
		case Ast::AST_STATEMENT_DO:
			write_indent();
			write("do", NEW_LINE);
			indentLevel++;
			write_block(function, block[i]->block);
			indentLevel--;
			write_indent();
			write("end");
			break;
		case Ast::AST_STATEMENT_LABEL:
			write_indent();
			write("::", function.labels[block[i]->instruction.label].name, "::");
			break;
		default:
			throw;
		}

		write(NEW_LINE);
		previousLineIsEmpty = false;
	}
}

void Lua::write_expression(const Ast::Expression& expression, const bool& useParentheses) {
	uint32_t nextListIndex, nextFieldIndex;
	uint8_t operatorPrecedence, operandPrecedence;
	bool parentheses, isFirstField, isFieldFound;
	if (useParentheses) write("(");

	switch (expression.type) {
	case Ast::AST_EXPRESSION_CONSTANT:
		switch (expression.constant->type) {
		case Ast::AST_CONSTANT_NIL:
			write("nil");
			break;
		case Ast::AST_CONSTANT_FALSE:
			write("false");
			break;
		case Ast::AST_CONSTANT_TRUE:
			write("true");
			break;
		case Ast::AST_CONSTANT_NUMBER:
			write_number(expression.constant->number);
			break;
		case Ast::AST_CONSTANT_CDATA_SIGNED:
			write(std::to_string(expression.constant->signed_integer), "LL");
			break;
		case Ast::AST_CONSTANT_CDATA_UNSIGNED:
			write(std::to_string(expression.constant->unsigned_integer), "ULL");
			break;
		case Ast::AST_CONSTANT_CDATA_IMAGINARY:
			write_number(expression.constant->number);
			write("i");
			break;
		case Ast::AST_CONSTANT_STRING:
			write("\"");
			write_string(expression.constant->string);
			write("\"");
			break;
		}

		break;
	case Ast::AST_EXPRESSION_VARARG:
		write("...");
		break;
	case Ast::AST_EXPRESSION_FUNCTION:
		write("function");
		write_function_definition(*expression.function, false);
		break;
	case Ast::AST_EXPRESSION_VARIABLE:
		write_variable(*expression.variable, false);
		break;
	case Ast::AST_EXPRESSION_FUNCTION_CALL:
		write_function_call(*expression.functionCall, false);
		break;
	case Ast::AST_EXPRESSION_TABLE:
		if (!expression.table->constants.list.size()
			&& !expression.table->constants.fields.size()
			&& !expression.table->fields.size()
			&& !expression.table->multresField) {
			write("{}");
			break;
		}

		write("{", NEW_LINE);
		indentLevel++;
		write_indent();
		nextListIndex = 1;
		nextFieldIndex = 0;
		isFirstField = true;

		if (expression.table->constants.list.size() && expression.table->constants.list.front()->constant->type != Ast::AST_CONSTANT_NIL) {
			write("[0] = ");
			write_expression(*expression.table->constants.list.front(), false);
			isFirstField = false;
		}

		while (!expression.table->multresField || nextListIndex < expression.table->multresIndex) {
			if (nextListIndex < expression.table->constants.list.size() && expression.table->constants.list[nextListIndex]->constant->type != Ast::AST_CONSTANT_NIL) {
				if (!isFirstField) {
					write(",", NEW_LINE);
					write_indent();
				}

				write_expression(*expression.table->constants.list[nextListIndex], false);
				isFirstField = false;
				nextListIndex++;
				continue;
			}

			isFieldFound = false;

			for (uint32_t i = nextFieldIndex; i < expression.table->fields.size(); i++) {
				if (expression.table->fields[i].key->type != Ast::AST_EXPRESSION_CONSTANT
					|| expression.table->fields[i].key->constant->type != Ast::AST_CONSTANT_NUMBER
					|| expression.table->fields[i].key->constant->number != nextListIndex)
					continue;
				isFieldFound = true;

				while (nextFieldIndex < i) {
					if (!isFirstField) {
						write(",", NEW_LINE);
						write_indent();
					}

					if (expression.table->fields[nextFieldIndex].key->type == Ast::AST_EXPRESSION_CONSTANT && expression.table->fields[nextFieldIndex].key->constant->isName) {
						write(expression.table->fields[nextFieldIndex].key->constant->string);
					} else {
						write("[");
						write_expression(*expression.table->fields[nextFieldIndex].key, false);
						write("]");
					}

					write(" = ");
					write_expression(*expression.table->fields[nextFieldIndex].value, false);
					isFirstField = false;
					nextFieldIndex++;
				}

				break;
			}

			if (isFieldFound) {
				if (!isFirstField) {
					write(",", NEW_LINE);
					write_indent();
				}

				if (!expression.table->multresField
					&& nextFieldIndex == expression.table->fields.size() - 1
					&& !expression.table->constants.fields.size()
					&& (!expression.table->constants.list.size()
						|| nextListIndex >= expression.table->constants.list.size() - 1)) {
					switch (expression.table->fields.back().value->type) {
					case Ast::AST_EXPRESSION_VARARG:
					case Ast::AST_EXPRESSION_FUNCTION_CALL:
						write_expression(*expression.table->fields.back().value, true);
						break;
					default:
						write_expression(*expression.table->fields.back().value, false);
						break;
					}

					nextListIndex++;
					nextFieldIndex++;
					break;
				}

				write_expression(*expression.table->fields[nextFieldIndex].value, false);
				nextFieldIndex++;
			} else if (!expression.table->multresField && nextListIndex >= expression.table->constants.list.size()) {
				break;
			} else {
				if (!isFirstField) {
					write(",", NEW_LINE);
					write_indent();
				}

				write("nil");
			}

			isFirstField = false;
			nextListIndex++;
		}

		for (uint32_t i = nextListIndex; i < expression.table->constants.list.size(); i++) {
			if (expression.table->constants.list[i]->constant->type == Ast::AST_CONSTANT_NIL) continue;

			if (!isFirstField) {
				write(",", NEW_LINE);
				write_indent();
			}

			write("[", std::to_string(i), "] = ");
			write_expression(*expression.table->constants.list[i], false);
			isFirstField = false;
		}

		for (uint32_t i = 0; i < expression.table->constants.fields.size(); i++) {
			if (!isFirstField) {
				write(",", NEW_LINE);
				write_indent();
			}

			if (expression.table->constants.fields[i].key->constant->isName) {
				write(expression.table->constants.fields[i].key->constant->string);
			} else {
				write("[");
				write_expression(*expression.table->constants.fields[i].key, false);
				write("]");
			}

			write(" = ");
			write_expression(*expression.table->constants.fields[i].value, false);
			isFirstField = false;
		}

		for (uint32_t i = nextFieldIndex; i < expression.table->fields.size(); i++) {
			if (!isFirstField) {
				write(",", NEW_LINE);
				write_indent();
			}

			if (expression.table->fields[i].key->type == Ast::AST_EXPRESSION_CONSTANT && expression.table->fields[i].key->constant->isName) {
				write(expression.table->fields[i].key->constant->string);
			} else {
				write("[");
				write_expression(*expression.table->fields[i].key, false);
				write("]");
			}

			write(" = ");
			write_expression(*expression.table->fields[i].value, false);
			isFirstField = false;
		}

		if (expression.table->multresField) {
			if (!isFirstField) {
				write(",", NEW_LINE);
				write_indent();
			}

			write_expression(*expression.table->multresField, false);
		}

		indentLevel--;
		if (minimizeDiffs) write(",");
		write(NEW_LINE);
		write_indent();
		write("}");
		break;
	case Ast::AST_EXPRESSION_BINARY_OPERATION:
		operatorPrecedence = get_operator_precedence(expression);
		operandPrecedence = get_operator_precedence(*expression.binaryOperation->leftOperand);
		parentheses = false;

		if (operandPrecedence == operatorPrecedence) {
			switch (operandPrecedence) {
			case 3:
			case 7:
				parentheses = true;
			}
		} else if (operandPrecedence < operatorPrecedence) {
			parentheses = true;
		} else if (operatorPrecedence == 7 && expression.binaryOperation->leftOperand->type == Ast::AST_EXPRESSION_CONSTANT) {
			switch (expression.binaryOperation->leftOperand->constant->type) {
			case Ast::AST_CONSTANT_NUMBER:
			case Ast::AST_CONSTANT_CDATA_IMAGINARY:
				if (std::bit_cast<uint64_t>(expression.binaryOperation->leftOperand->constant->number) & DOUBLE_SIGN) parentheses = true;
				break;
			case Ast::AST_CONSTANT_CDATA_SIGNED:
				if (expression.binaryOperation->leftOperand->constant->signed_integer < 0) parentheses = true;
				break;
			}
		}

		write_expression(*expression.binaryOperation->leftOperand, parentheses);

		switch (expression.binaryOperation->type) {
		case Ast::AST_BINARY_ADDITION:
			write(" + ");
			break;
		case Ast::AST_BINARY_SUBTRACTION:
			write(" - ");
			break;
		case Ast::AST_BINARY_MULTIPLICATION:
			write(" * ");
			break;
		case Ast::AST_BINARY_DIVISION:
			write(" / ");
			break;
		case Ast::AST_BINARY_EXPONENTATION:
			write("^");
			break;
		case Ast::AST_BINARY_MODULO:
			write(" % ");
			break;
		case Ast::AST_BINARY_CONCATENATION:
			write(" .. ");
			break;
		case Ast::AST_BINARY_LESS_THAN:
			write(" < ");
			break;
		case Ast::AST_BINARY_LESS_EQUAL:
			write(" <= ");
			break;
		case Ast::AST_BINARY_GREATER_THEN:
			write(" > ");
			break;
		case Ast::AST_BINARY_GREATER_EQUAL:
			write(" >= ");
			break;
		case Ast::AST_BINARY_EQUAL:
			write(" == ");
			break;
		case Ast::AST_BINARY_NOT_EQUAL:
			write(" ~= ");
			break;
		case Ast::AST_BINARY_AND:
			write(" and ");
			break;
		case Ast::AST_BINARY_OR:
			write(" or ");
			break;
		}

		parentheses = false;

		if (expression.binaryOperation->rightOperand->type == Ast::AST_EXPRESSION_BINARY_OPERATION) {
			operandPrecedence = get_operator_precedence(*expression.binaryOperation->rightOperand);

			if (operandPrecedence == operatorPrecedence) {
				switch (operandPrecedence) {
				case 2:
				case 4:
				case 5:
					parentheses = true;
				}
			} else if (operandPrecedence < operatorPrecedence) {
				parentheses = true;
			}
		}

		write_expression(*expression.binaryOperation->rightOperand, parentheses);
		break;
	case Ast::AST_EXPRESSION_UNARY_OPERATION:
		parentheses = get_operator_precedence(*expression.unaryOperation->operand) < 6;

		switch (expression.unaryOperation->type) {
		case Ast::AST_UNARY_MINUS:
			if (!parentheses
				&& expression.unaryOperation->operand->type == Ast::AST_EXPRESSION_UNARY_OPERATION
				&& expression.unaryOperation->operand->unaryOperation->type == Ast::AST_UNARY_MINUS)
				parentheses = true;
			write("-");
			break;
		case Ast::AST_UNARY_NOT:
			write("not ");
			break;
		case Ast::AST_UNARY_LENGTH:
			write("#");
			break;
		}

		write_expression(*expression.unaryOperation->operand, parentheses);
		break;
	}

	if (useParentheses) write(")");
}

void Lua::write_prefix_expression(const Ast::Expression& expression, const bool& isLineStart) {
	switch (expression.type) {
	case Ast::AST_EXPRESSION_VARIABLE:
		write_variable(*expression.variable, isLineStart);
		break;
	case Ast::AST_EXPRESSION_FUNCTION_CALL:
		write_function_call(*expression.functionCall, isLineStart);
		break;
	default:
		if (isLineStart) write(";");
		write_expression(expression, true);
		break;
	}
}

void Lua::write_variable(const Ast::Variable& variable, const bool& isLineStart) {
	switch (variable.type) {
	case Ast::AST_VARIABLE_SLOT:
	case Ast::AST_VARIABLE_UPVALUE:
		if (!(*variable.slotScope)->name.size()) throw;
		write((*variable.slotScope)->name);
		break;
	case Ast::AST_VARIABLE_GLOBAL:
		write(variable.name);
		break;
	case Ast::AST_VARIABLE_TABLE_INDEX:
		write_prefix_expression(*variable.table, isLineStart);

		if (variable.tableIndex->type == Ast::AST_EXPRESSION_CONSTANT && variable.tableIndex->constant->isName) {
			write(".", variable.tableIndex->constant->string);
			break;
		}

		write("[");
		write_expression(*variable.tableIndex, false);
		write("]");
		break;
	}
}

void Lua::write_function_call(const Ast::FunctionCall& functionCall, const bool& isLineStart) {
	if (functionCall.isMethod) {
		write_prefix_expression(*functionCall.function->variable->table, isLineStart);
		write(":", functionCall.function->variable->tableIndex->constant->string);
	} else {
		write_prefix_expression(*functionCall.function, isLineStart);
	}

	write("(");
	write_expression_list(functionCall.arguments, functionCall.multresArgument);
	write(")");
}

void Lua::write_assignment(const std::vector<Ast::Variable>& variables, const std::vector<Ast::Expression*>& expressions, const std::string& seperator, const bool& isLineStart) {
	for (uint8_t i = 0; i < variables.size(); i++) {
		write_variable(variables[i], i ? false : isLineStart);
		if (i != variables.size() - 1) write(", ");
	}

	if (!expressions.size()) return;
	write(seperator);

	for (uint8_t i = 0; i < expressions.size(); i++) {
		if (i != expressions.size() - 1) {
			write_expression(*expressions[i], false);
			write(", ");
			continue;
		}

		if (expressions.size() != variables.size()) {
			switch (expressions[i]->type) {
			case Ast::AST_EXPRESSION_VARARG:
				if (expressions[i]->returnCount == 1) {
					write_expression(*expressions[i], true);
					continue;
				}

				break;
			case Ast::AST_EXPRESSION_FUNCTION_CALL:
				if (expressions[i]->functionCall->returnCount == 1) {
					write_expression(*expressions[i], true);
					continue;
				}

				break;
			}
		}

		write_expression(*expressions[i], false);
	}
}

void Lua::write_expression_list(const std::vector<Ast::Expression*>& expressions, const Ast::Expression* const& multres) {
	for (uint8_t i = 0; i < expressions.size(); i++) {
		if (i != expressions.size() - 1 || multres) {
			write_expression(*expressions[i], false);
			write(", ");
			continue;
		}

		switch (expressions[i]->type) {
		case Ast::AST_EXPRESSION_VARARG:
		case Ast::AST_EXPRESSION_FUNCTION_CALL:
			write_expression(*expressions[i], true);
			continue;
		}

		write_expression(*expressions[i], false);
	}

	if (multres) write_expression(*multres, false);
}

void Lua::write_function_definition(const Ast::Function& function, const bool& isMethod) {
	write("(");

	for (uint8_t i = isMethod ? 1 : 0; i < function.parameterNames.size(); i++) {
		write(function.parameterNames[i]);
		if (i != function.parameterNames.size() - 1 || function.isVariadic) write(", ");
	}

	if (function.isVariadic) write("...");
	write(")", NEW_LINE);
	indentLevel++;
#if defined _DEBUG
	write_indent();
	write("-- function ", std::to_string(function.id), NEW_LINE);
#endif
	if (function.block.size()) {
		write_block(function, function.block);
	} else {
		write_indent();
		write("return", NEW_LINE);
	}

	indentLevel--;
	write_indent();
	write("end");
	prototypeDataLeft -= function.prototype.prototypeSize;
	print_progress_bar(bytecode.prototypesTotalSize - prototypeDataLeft, bytecode.prototypesTotalSize);
}

void Lua::write_number(const double& number) {
	const uint64_t rawDouble = std::bit_cast<uint64_t>(number);

	if ((rawDouble & DOUBLE_EXPONENT) == DOUBLE_SPECIAL) {
		write(rawDouble & DOUBLE_SIGN ? "-1e309" : "1e309");
		return;
	}

	std::string string;
	string.resize(std::snprintf(nullptr, 0, "%1.15g", number));
	std::snprintf(string.data(), string.size() + 1, "%1.15g", number);

	if (std::stod(string) != number) {
		string.resize(std::snprintf(nullptr, 0, "%1.16g", number));
		std::snprintf(string.data(), string.size() + 1, "%1.16g", number);

		if (std::stod(string) != number) {
			string.resize(std::snprintf(nullptr, 0, "%1.17g", number));
			std::snprintf(string.data(), string.size() + 1, "%1.17g", number);
			assert(std::stod(string) == number, "Failed to convert number to valid string", filePath, DEBUG_INFO);
		}
	}

	write(string);
}

void Lua::write_string(const std::string& string) {
	char escapeSequence[] = "\\x00";
	uint32_t value;
	uint8_t digit;

	for (uint32_t i = 0; i < string.size(); i++) {
		value = string[i];

		if (unrestrictedAscii || !(value & 0x80)) {
			if ((string[i] >= ' '
					&& string[i] <= '~')
				|| (unrestrictedAscii
					&& string[i] >= 0x80)) {
				switch (string[i]) {
				case '"':
				case '\\':
					writeBuffer += '\\';
				}

				writeBuffer += string[i];
				continue;
			}

			switch (string[i]) {
			case '\a':
				write("\\a");
				continue;
			case '\b':
				write("\\b");
				continue;
			case '\t':
				write("\\t");
				continue;
			case '\n':
				write("\\n");
				continue;
			case '\v':
				write("\\v");
				continue;
			case '\f':
				write("\\f");
				continue;
			case '\r':
				write("\\r");
				continue;
			}
		} else if ((value & 0xE0) == 0xC0) {
			if (i + 1 < string.size()) {
				value <<= 8;
				value |= string[i + 1];

				if ((value & 0xC0) == 0x80
					&& value >= 0xC2A0
					&& value <= 0xDFBF) {
					writeBuffer += string[i];
					writeBuffer += string[i + 1];
					i++;
					continue;
				}
			}
		} else if ((value & 0xF0) == 0xE0) {
			if (i + 2 < string.size()) {
				value <<= 16;
				value |= (uint16_t)string[i + 1] << 8;
				value |= string[i + 2];

				if ((value & 0xC0C0) == 0x8080
					&& ((value >= 0xE0A080
							&& value < 0xEDA080)
						|| (value > 0xEDBFBF
							&& value <= 0xEFBFBF))) {
					writeBuffer += string[i];
					writeBuffer += string[i + 1];
					writeBuffer += string[i + 2];
					i += 2;
					continue;
				}
			}
		} else if ((value & 0xF8) == 0xF0) {
			if (i + 3 < string.size()) {
				value <<= 24;
				value |= (uint32_t)string[i + 1] << 16;
				value |= (uint16_t)string[i + 2] << 8;
				value |= string[i + 3];

				if ((value & 0xC0C0C0) == 0x808080
					&& value >= 0xF0908080
					&& value <= 0xF48FBFBF) {
					writeBuffer += string[i];
					writeBuffer += string[i + 1];
					writeBuffer += string[i + 2];
					writeBuffer += string[i + 3];
					i += 3;
					continue;
				}
			}
		}

		for (uint8_t j = 2; j--;) {
			digit = (string[i] >> j * 4) & 0xF;
			escapeSequence[3 - j] = digit >= 0xA ? 'A' + digit - 0xA : '0' + digit;
		}

		writeBuffer += escapeSequence;
	}
}

uint8_t Lua::get_operator_precedence(const Ast::Expression& expression) {
	switch (expression.type) {
	case Ast::AST_EXPRESSION_BINARY_OPERATION:
		switch (expression.binaryOperation->type) {
		case Ast::AST_BINARY_EXPONENTATION:
			return 7;
		case Ast::AST_BINARY_MULTIPLICATION:
		case Ast::AST_BINARY_DIVISION:
		case Ast::AST_BINARY_MODULO:
			return 5;
		case Ast::AST_BINARY_ADDITION:
		case Ast::AST_BINARY_SUBTRACTION:
			return 4;
		case Ast::AST_BINARY_CONCATENATION:
			return 3;
		case Ast::AST_BINARY_LESS_THAN:
		case Ast::AST_BINARY_LESS_EQUAL:
		case Ast::AST_BINARY_GREATER_THEN:
		case Ast::AST_BINARY_GREATER_EQUAL:
		case Ast::AST_BINARY_EQUAL:
		case Ast::AST_BINARY_NOT_EQUAL:
			return 2;
		case Ast::AST_BINARY_AND:
			return 1;
		case Ast::AST_BINARY_OR:
			return 0;
		}
	case Ast::AST_EXPRESSION_UNARY_OPERATION:
		return 6;
	}

	return 8;
}

void Lua::write(const std::string& string) {
	writeBuffer += string;
}

template <typename... Strings>
void Lua::write(const std::string& string, const Strings&... strings) {
	write(string);
	return write(strings...);
}

void Lua::write_indent() {
	return write(std::string(indentLevel, '\t'));
}

void Lua::create_file() {
#ifndef _DEBUG
	if (!forceOverwrite) {
		file = CreateFileA(filePath.c_str(), GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

		if (file != INVALID_HANDLE_VALUE) {
			close_file();
			assert(MessageBoxA(NULL, ("The file " + filePath + " already exists.\n\nDo you want to overwrite it?").c_str(), PROGRAM_NAME, MB_ICONWARNING | MB_YESNO | MB_DEFBUTTON2) == IDYES,
				"File already exists", filePath, DEBUG_INFO);
		}
	}
#endif
	file = CreateFileA(filePath.c_str(), GENERIC_WRITE, NULL, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL);
	assert(file != INVALID_HANDLE_VALUE, "Unable to create file", filePath, DEBUG_INFO);
}

void Lua::close_file() {
	if (file == INVALID_HANDLE_VALUE) return;
	CloseHandle(file);
	file = INVALID_HANDLE_VALUE;
}

void Lua::write_file() {
	DWORD charsWritten = 0;
	assert(WriteFile(file, writeBuffer.data(), writeBuffer.size(), &charsWritten, NULL) && !(writeBuffer.size() - charsWritten), "Failed writing to file", filePath, DEBUG_INFO);
	writeBuffer.clear();
	writeBuffer.shrink_to_fit();
}
