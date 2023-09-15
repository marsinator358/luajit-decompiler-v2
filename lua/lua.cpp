#include "..\main.h"

Lua::Lua(const Bytecode& bytecode, const Ast& ast, const std::string& filePath) : bytecode(bytecode), ast(ast), filePath(filePath) {}

Lua::~Lua() {
	close_file();
}

void Lua::operator()() {
	print_progress_bar();
	prototypeDataLeft = bytecode.prototypesTotalSize;
	write_chunkname();
	write_block(*ast.chunk, ast.chunk->block);
	prototypeDataLeft -= ast.chunk->prototype.prototypeSize;
	print_progress_bar(bytecode.prototypesTotalSize - prototypeDataLeft, bytecode.prototypesTotalSize);
	create_file();
	write_file();
	close_file();
	erase_progress_bar();
}

void Lua::write_chunkname() {
	if (!bytecode.header.chunkname.size()) return;
	write("-- chunkname: ");
	write_string(bytecode.header.chunkname, false);
	write(NEW_LINE, NEW_LINE);
}

void Lua::write_block(const Ast::Function& function, const std::vector<Ast::Statement*>& block) {
	std::vector<Ast::Statement*>* elseBlock;
	bool isFunctionDefinition;
	bool previousLineIsEmpty = true;

	for (uint32_t i = 0; i < block.size(); i++) {
		if (!previousLineIsEmpty) {
			switch (block[i - 1]->type) {
			case Ast::AST_STATEMENT_DECLARATION:
				if (block[i]->type == Ast::AST_STATEMENT_DECLARATION) break;
			case Ast::AST_STATEMENT_NUMERIC_FOR:
			case Ast::AST_STATEMENT_GENERIC_FOR:
			case Ast::AST_STATEMENT_IF:
			case Ast::AST_STATEMENT_ELSE:
			case Ast::AST_STATEMENT_WHILE:
			case Ast::AST_STATEMENT_REPEAT:
			case Ast::AST_STATEMENT_DO:
				write_indent();
				write(NEW_LINE);
				previousLineIsEmpty = true;
				break;
			default:
				switch (block[i]->type) {
				case Ast::AST_STATEMENT_NUMERIC_FOR:
				case Ast::AST_STATEMENT_GENERIC_FOR:
				case Ast::AST_STATEMENT_IF:
				case Ast::AST_STATEMENT_WHILE:
				case Ast::AST_STATEMENT_REPEAT:
				case Ast::AST_STATEMENT_DO:
					write_indent();
					write(NEW_LINE);
					previousLineIsEmpty = true;
				}

				break;
			}
		}

		write_indent();

		switch (block[i]->type) {
		case Ast::AST_STATEMENT_RETURN:
			if (i != block.size() - 1) write("do ");
			write("return ");
			write_expression_list(block[i]->assignment.expressions, block[i]->assignment.multresReturn);
			if (i != block.size() - 1) write(" end");
			break;
		case Ast::AST_STATEMENT_GOTO:
			write("goto ", function.labels[block[i]->instruction.attachedLabel].name);
			break;
		case Ast::AST_STATEMENT_NUMERIC_FOR:
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
				if (!previousLineIsEmpty) {
					write_indent();
					write(NEW_LINE);
				}

				write("local function ", (*block[i]->assignment.variables.back().slotScope)->name);
				write_function_definition(*block[i]->assignment.expressions.back()->function, false);

				if (i != block.size() - 1) {
					write(NEW_LINE);
					write_indent();
					write(NEW_LINE);
					previousLineIsEmpty = true;
					continue;
				}
			} else {
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
				if (!previousLineIsEmpty) {
					write_indent();
					write(NEW_LINE);
				}

				write("function ");
				write_variable(block[i]->assignment.variables.back(), false);
				write_function_definition(*block[i]->assignment.expressions.back()->function,
					block[i]->assignment.variables.back().type == Ast::AST_VARIABLE_TABLE_INDEX
					&& block[i]->assignment.expressions.back()->function->parameterNames.size()
					&& block[i]->assignment.expressions.back()->function->parameterNames.front() == "self");

				if (i != block.size() - 1) {
					write(NEW_LINE);
					write_indent();
					write(NEW_LINE);
					previousLineIsEmpty = true;
					continue;
				}
			} else {
				write_assignment(block[i]->assignment.variables, block[i]->assignment.expressions, " = ", true);
			}

			break;
		case Ast::AST_STATEMENT_FUNCTION_CALL:
			write_function_call(*block[i]->assignment.expressions.back()->functionCall, true);
			break;
		case Ast::AST_STATEMENT_IF:
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
						write("else if ");
						write_expression(*elseBlock->front()->assignment.expressions.back(), false);
						write("then", NEW_LINE);
						indentLevel++;
						write_block(function, elseBlock->front()->block);
						indentLevel--;
						write_indent();
					} else if (elseBlock->size() == 2
						&& elseBlock->front()->type == Ast::AST_STATEMENT_IF
						&& elseBlock->back()->type == Ast::AST_STATEMENT_ELSE) {
						write("elseif ");
						write_expression(*elseBlock->front()->assignment.expressions.back(), false);
						write("then", NEW_LINE);
						indentLevel++;
						write_block(function, elseBlock->front()->block);
						indentLevel--;
						write_indent();
						elseBlock = &elseBlock->back()->block;
						continue;
					} else {
						write("else", NEW_LINE);
						indentLevel++;
						write_block(function, elseBlock->front()->block);
						indentLevel--;
						write_indent();
					}

					break;
				}
			}

			write("end");
			break;
		case Ast::AST_STATEMENT_WHILE:
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
			write("repeat", NEW_LINE);
			indentLevel++;
			write_block(function, block[i]->block);
			indentLevel--;
			write_indent();
			write("until ");
			write_expression(*block[i]->assignment.expressions.back(), false);
			break;
		case Ast::AST_STATEMENT_DO:
			write("do", NEW_LINE);
			indentLevel++;
			write_block(function, block[i]->block);
			indentLevel--;
			write_indent();
			write("end");
			break;
		case Ast::AST_STATEMENT_LABEL:
			write("::", function.labels[block[i]->instruction.attachedLabel].name, "::");
			break;
		}

		write(NEW_LINE);
		previousLineIsEmpty = false;
	}
}

void Lua::write_expression(const Ast::Expression& expression, const bool& useParentheses) {
	uint8_t precedence, operatorPrecedence;
	bool parentheses;
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
			write_string(expression.constant->string, true);
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
	case Ast::AST_EXPRESSION_TABLE: //TODO
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

		if (expression.table->constants.list.size()) {
			write("[0] = ");
			write_expression(*expression.table->constants.list.front(), false);
		}

		for (uint32_t i = 1; i < expression.table->constants.list.size(); i++) {
			write(",", NEW_LINE);
			write_indent();

			if (expression.table->multresField) {
				if (i >= expression.table->multresIndex) {
					write("[", std::to_string(i), "] = ");
					write_expression(*expression.table->constants.list[i], false);
					continue;
				}
			} else if (i == expression.table->constants.list.size() - 1
				&& !expression.table->constants.fields.size()
				&& !expression.table->fields.size()) {
				switch (expression.table->constants.list[i]->type) {
				case Ast::AST_EXPRESSION_VARARG:
				case Ast::AST_EXPRESSION_FUNCTION_CALL:
					write_expression(*expression.table->constants.list[i], true);
					continue;
				}
			}

			write_expression(*expression.table->constants.list[i], false);
		}

		for (uint32_t i = 0; i < expression.table->constants.fields.size(); i++) {
			if (i || expression.table->constants.list.size()) {
				write(",", NEW_LINE);
				write_indent();
			}

			if (expression.table->constants.fields[i].key->type == Ast::AST_EXPRESSION_CONSTANT && expression.table->constants.fields[i].key->constant->isName) {
				write(expression.table->constants.fields[i].key->constant->string);
			} else {
				write("[");
				write_expression(*expression.table->constants.fields[i].key, false);
				write("]");
			}

			write(" = ");
			write_expression(*expression.table->constants.fields[i].value, false);
		}

		for (uint32_t i = 0; i < expression.table->fields.size(); i++) {
			if (i
				|| expression.table->constants.list.size()
				|| expression.table->constants.fields.size()) {
				write(",", NEW_LINE);
				write_indent();
			}

			if (expression.table->fields[i].key->type == Ast::AST_EXPRESSION_CONSTANT && expression.table->fields[i].key->constant->isName) {
				write(expression.table->fields[i].key->constant->string);
			}
			else {
				write("[");
				write_expression(*expression.table->fields[i].key, false);
				write("]");
			}

			write(" = ");
			write_expression(*expression.table->fields[i].value, false);
		}

		if (expression.table->multresField) {
			if (expression.table->constants.list.size()
				|| expression.table->constants.fields.size()
				|| expression.table->fields.size()) {
				write(",", NEW_LINE);
				write_indent();
			}

			for (uint32_t i = expression.table->constants.list.size() ? expression.table->constants.list.size() : 1; i < expression.table->multresIndex; i++) {
				write("nil,", NEW_LINE);
				write_indent();
			}

			write_expression(*expression.table->multresField, false);
		}

		indentLevel--;
		write(NEW_LINE);
		write_indent();
		write("}");
		break;
	case Ast::AST_EXPRESSION_BINARY_OPERATION:
		precedence = get_operator_precedence(expression);
		operatorPrecedence = get_operator_precedence(*expression.binaryOperation->leftOperand);

		if (operatorPrecedence < precedence) {
			parentheses = true;
		} else if (operatorPrecedence == precedence) {
			switch (precedence) {
			case 3:
			case 7:
				parentheses = true;
				break;
			default:
				parentheses = false;
				break;
			}
		} else {
			parentheses = false;
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

		operatorPrecedence = get_operator_precedence(*expression.binaryOperation->rightOperand);

		if (operatorPrecedence < precedence) {
			parentheses = true;
		} else if (operatorPrecedence == precedence) {
			switch (precedence) {
			case 2:
			case 4:
			case 5:
				parentheses = true;
				break;
			default:
				parentheses = false;
				break;
			}
		} else {
			parentheses = false;
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
	
	for (uint8_t i = 0; i < function.parameterNames.size(); i++) {
		if (!i && isMethod) continue;

		if (i != function.parameterNames.size() - 1 || function.isVariadic) {
			write(function.parameterNames[i], ", ");
			continue;
		}

		write(function.parameterNames[i]);
	}

	if (function.isVariadic) write("...");
	write(")", NEW_LINE);
	indentLevel++;
	write_block(function, function.block);
	indentLevel--;
	write_indent();
	write("end");
	prototypeDataLeft -= function.prototype.prototypeSize;
	print_progress_bar(bytecode.prototypesTotalSize - prototypeDataLeft, bytecode.prototypesTotalSize);
}

void Lua::write_number(const double& number) {
	//TODO
	const uint64_t rawDouble = std::bit_cast<uint64_t>(number);
	write((rawDouble & DOUBLE_EXPONENT) == DOUBLE_SPECIAL ? (rawDouble & DOUBLE_SIGN ? "-1e309" : "1e309") : std::to_string(number));
}

void Lua::write_string(const std::string& string, const bool& escapeChars) {
	char escapeSequence[] = "\\x00";
	uint8_t value;

	for (uint32_t i = 0; i < string.size(); i++) {
		if (string[i] >= ' ' && string[i] <= '~') {
			if (escapeChars) {
				switch (string[i]) {
				case '"':
				case '\\':
					writeBuffer += '\\';
				}
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

		for (uint8_t j = 2; j--;) {
			value = (std::bit_cast<uint8_t>(string[i]) >> j * 4) & 0xF;

			switch (value) {
			case 0xA:
			case 0xB:
			case 0xC:
			case 0xD:
			case 0xE:
			case 0xF:
				escapeSequence[3 - j] = 'A' + value - 0xA;
				continue;
			}

			escapeSequence[3 - j] = '0' + value;
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
	file = CreateFileA(filePath.c_str(), GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (file != INVALID_HANDLE_VALUE) {
		CloseHandle(file);
		assert(MessageBoxA(NULL, ("File " + filePath + " already exists.\n\nDo you want to overwrite it?").c_str(), PROGRAM_NAME, MB_ICONWARNING | MB_YESNO | MB_DEFBUTTON2) == IDYES,
			"File already exists", filePath, DEBUG_INFO);
	}

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
