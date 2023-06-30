enum AST_SIMPLE_EXPRESSION {
	AST_SIMPLE_EXPRESSION_CONSTANT,
	AST_SIMPLE_EXPRESSION_VARARG,
	AST_SIMPLE_EXPRESSION_FUNCTION,
	AST_SIMPLE_EXPRESSION_VARIABLE,
	AST_SIMPLE_EXPRESSION_FUNCTION_CALL,
	AST_SIMPLE_EXPRESSION_TABLE,
	AST_SIMPLE_EXPRESSION_BINARY_OPERATION,
	AST_SIMPLE_EXPRESSION_UNARY_OPERATION
};

struct Ast::SimpleAst::Expression {
	Expression(const AST_SIMPLE_EXPRESSION& type) {
		set_type(type);
	}

	~Expression() {
		delete_type();
	}

	void set_type(const AST_SIMPLE_EXPRESSION& type) {
		delete_type();
		this->type = type;

		switch (type) {
		case AST_SIMPLE_EXPRESSION_CONSTANT:
			constant = new Constant;
			break;
		case AST_SIMPLE_EXPRESSION_VARIABLE:
			variable = new Variable;
			break;
		case AST_SIMPLE_EXPRESSION_FUNCTION_CALL:
			functionCall = new FunctionCall;
			break;
		case AST_SIMPLE_EXPRESSION_TABLE:
			table = new Table;
			break;
		case AST_SIMPLE_EXPRESSION_BINARY_OPERATION:
			binaryOperation = new BinaryOperation;
			break;
		case AST_SIMPLE_EXPRESSION_UNARY_OPERATION:
			unaryOperation = new UnaryOperation;
			break;
		}
	}

	void delete_type() {
		switch (type) {
		case AST_SIMPLE_EXPRESSION_CONSTANT:
			delete constant;
			constant = nullptr;
			break;
		case AST_SIMPLE_EXPRESSION_FUNCTION:
			function = nullptr;
			break;
		case AST_SIMPLE_EXPRESSION_VARIABLE:
			delete variable;
			variable = nullptr;
			break;
		case AST_SIMPLE_EXPRESSION_FUNCTION_CALL:
			delete functionCall;
			functionCall = nullptr;
			break;
		case AST_SIMPLE_EXPRESSION_TABLE:
			delete table;
			table = nullptr;
			break;
		case AST_SIMPLE_EXPRESSION_BINARY_OPERATION:
			delete binaryOperation;
			binaryOperation = nullptr;
			break;
		case AST_SIMPLE_EXPRESSION_UNARY_OPERATION:
			delete unaryOperation;
			unaryOperation = nullptr;
			break;
		}
	}

	AST_SIMPLE_EXPRESSION type;

	union {
		Constant* constant = nullptr;
		Function* function;
		Variable* variable;
		FunctionCall* functionCall;
		Table* table;
		BinaryOperation* binaryOperation;
		UnaryOperation* unaryOperation;
	};
};

enum AST_SIMPLE_CONSTANT {
	AST_SIMPLE_CONSTANT_CONDITION,
	AST_SIMPLE_CONSTANT_NIL,
	AST_SIMPLE_CONSTANT_FALSE,
	AST_SIMPLE_CONSTANT_TRUE,
	AST_SIMPLE_CONSTANT_NUMBER,
	AST_SIMPLE_CONSTANT_CDATA_SIGNED,
	AST_SIMPLE_CONSTANT_CDATA_UNSIGNED,
	AST_SIMPLE_CONSTANT_CDATA_IMAGINARY,
	AST_SIMPLE_CONSTANT_STRING
};

struct Ast::SimpleAst::Constant {
	AST_SIMPLE_CONSTANT type;

	union {
		double number;
		int64_t signed_integer;
		uint64_t unsigned_integer = 0;
	};

	std::string string;
	bool isName = false;
};

enum AST_SIMPLE_VARIABLE {
	AST_SIMPLE_VARIABLE_UNUSED,
	AST_SIMPLE_VARIABLE_SLOT,
	AST_SIMPLE_VARIABLE_UPVALUE,
	AST_SIMPLE_VARIABLE_GLOBAL,
	AST_SIMPLE_VARIABLE_TABLE_INDEX
};

struct Ast::SimpleAst::Variable {
	AST_SIMPLE_VARIABLE type;
	uint8_t slot = 0;
	SlotScope** slotScope = nullptr;
	std::string name;
	Expression* table = nullptr;
	Expression* tableIndex = nullptr;
	bool isMultres = false;
	uint32_t multresIndex = 0;
};

struct Ast::SimpleAst::FunctionCall {
	Expression* function = nullptr;
	std::vector<Expression*> arguments;
	Expression* multresArgument = nullptr;
	bool isMethod = false;
};

struct Ast::SimpleAst::Table {
	struct Field {
		Expression* key = nullptr;
		Expression* value = nullptr;
	};

	struct {
		std::vector<Expression*> list;
		std::vector<Field> fields;
	} constants;

	std::vector<Field> fields;
	uint32_t multresIndex = 0;
	Expression* multresField = nullptr;
};

enum AST_SIMPLE_BINARY_OPERATION {
	AST_SIMPLE_BINARY_CONDITION,
	AST_SIMPLE_BINARY_ADDITION,
	AST_SIMPLE_BINARY_SUBTRACTION,
	AST_SIMPLE_BINARY_MULTIPLICATION,
	AST_SIMPLE_BINARY_DIVISION,
	AST_SIMPLE_BINARY_EXPONENTATION,
	AST_SIMPLE_BINARY_MODULO,
	AST_SIMPLE_BINARY_CONCATENATION,
	AST_SIMPLE_BINARY_LESS_THAN,
	AST_SIMPLE_BINARY_LESS_EQUAL,
	AST_SIMPLE_BINARY_GREATER_THEN,
	AST_SIMPLE_BINARY_GREATER_EQUAL,
	AST_SIMPLE_BINARY_EQUAL,
	AST_SIMPLE_BINARY_NOT_EQUAL,
	AST_SIMPLE_BINARY_AND,
	AST_SIMPLE_BINARY_OR
};

struct Ast::SimpleAst::BinaryOperation {
	AST_SIMPLE_BINARY_OPERATION type;
	Expression* leftOperand = nullptr;
	Expression* rightOperand = nullptr;
};

enum AST_SIMPLE_UNARY_OPERATION {
	AST_SIMPLE_UNARY_CONDITION,
	AST_SIMPLE_UNARY_MINUS,
	AST_SIMPLE_UNARY_NOT,
	AST_SIMPLE_UNARY_LENGTH
};

struct Ast::SimpleAst::UnaryOperation {
	AST_SIMPLE_UNARY_OPERATION type;
	Expression* operand = nullptr;
};

enum AST_SIMPLE_STATEMENT {
	AST_SIMPLE_STATEMENT_EMPTY,
	AST_SIMPLE_STATEMENT_INSTRUCTION,
	AST_SIMPLE_STATEMENT_RETURN,
	AST_SIMPLE_STATEMENT_CONDITION,
	AST_SIMPLE_STATEMENT_GOTO,
	AST_SIMPLE_STATEMENT_NUMERIC_FOR,
	AST_SIMPLE_STATEMENT_GENERIC_FOR,
	AST_SIMPLE_STATEMENT_LOOP,
	AST_SIMPLE_STATEMENT_BREAK,
	AST_SIMPLE_STATEMENT_DECLARATION,
	AST_SIMPLE_STATEMENT_ASSIGNMENT,
	AST_SIMPLE_STATEMENT_FUNCTION_CALL
};

struct Ast::SimpleAst::Statement {
	Statement(const AST_SIMPLE_STATEMENT& type) : type(type) {}

	AST_SIMPLE_STATEMENT type;

	struct {
		Bytecode::BC_OP type = Bytecode::BC_OP_MAX;
		uint8_t a = 0;
		uint8_t b = 0;
		uint8_t c = 0;
		uint16_t d = 0;
		uint32_t target = ID_EMPTY;

		struct {
			uint32_t id = ID_EMPTY;
			uint32_t lineOffset = 0;
			uint32_t attachedLabel = ID_EMPTY;
		} info;
	} instruction;

	Function* function = nullptr;
	std::vector<Statement*> block;
	Local* locals = nullptr;

	struct {
		uint32_t jumpId = ID_EMPTY;
		bool allowSlotSwap = false;
		bool swapped = false;
	} condition;

	struct {
		void register_open_slots(Expression*& expression) {
			usedSlots.emplace_back(expression->variable->slot);
			openSlots.emplace_back(&expression);
		}

		void register_open_slots(Expression*& expression, Expression*& expressions...) {
			usedSlots.emplace_back(expression->variable->slot);
			openSlots.emplace_back(&expression);
			return register_open_slots(expressions);
		}

		bool isPotentialMethod = false;
		bool isTableConstructor = false;
		std::vector<Variable> variables;
		std::vector<Expression*> expressions;
		std::vector<uint8_t> usedSlots;
		std::vector<Expression**> openSlots;
		Expression* multresReturn = nullptr;
	} assignment;
};

struct Ast::SimpleAst::Local {
	std::vector<std::string> names;
	uint8_t baseSlot = 0;
	uint32_t scopeBegin = ID_EMPTY;
	uint32_t scopeEnd = ID_EMPTY;
};

struct Ast::SimpleAst::SlotScope {
	SlotScope* slotScope = this;
	std::string* name = nullptr;
	uint32_t scopeBegin = ID_EMPTY;
	uint32_t scopeEnd = ID_EMPTY;
	uint32_t usages = 0;
};

struct Ast::SimpleAst::Function {
	struct Upvalue {
		uint8_t slot = 0;
		SlotScope** slotScope = nullptr;
		bool local = false;
	};

	struct Label {
		uint32_t target = ID_EMPTY;
		std::vector<uint32_t> jumpIds;
	};

	Function(const Bytecode::Prototype& prototype) : prototype(prototype) {}

	~Function() {
		for (uint32_t i = slotScopeCollector.slotScopes.size(); i--;) {
			delete slotScopeCollector.slotScopes[i];
		}
	}

	const Bytecode::Constant& get_constant(const uint16_t& index) const {
		return prototype.constants[prototype.constants.size() - 1 - index];
	}

	const Bytecode::NumberConstant& get_number_constant(const uint16_t& index) const {
		return prototype.numberConstants[index];
	}

	void add_jump(const uint32_t& id, const uint32_t& target) {
		for (uint32_t i = 0; i < labels.size(); i++) {
			if (target > labels[i].target) continue;

			if (target != labels[i].target) {
				labels.emplace(labels.begin() + i);
				labels[i].target = target;
			}

			for (uint32_t j = 0; j < labels[i].jumpIds.size(); j++) {
				if (id > labels[i].jumpIds[j]) continue;
				if (id < labels[i].jumpIds[j]) labels[i].jumpIds.emplace(labels[i].jumpIds.begin() + j, id);
				return;
			}

			labels[i].jumpIds.emplace_back(id);
			return;
		}

		labels.emplace_back();
		labels.back().target = target;
		labels.back().jumpIds.emplace_back(id);
	}

	void remove_jump(const uint32_t& id, const uint32_t& target) {
		for (uint32_t i = labels.size(); i--;) {
			if (labels[i].target != target) continue;

			for (uint32_t j = labels[i].jumpIds.size(); j--;) {
				if (labels[i].jumpIds[j] != id) continue;
				labels[i].jumpIds.erase(labels[i].jumpIds.begin() + j);
				if (!labels[i].jumpIds.size()) labels.erase(labels.begin() + i);
				return;
			}
		}
	}

	uint32_t get_next_reachable_id(const uint32_t& lastReachableId) {
		for (uint32_t i = 0; i < labels.size(); i++) {
			if (labels[i].target <= lastReachableId || labels[i].jumpIds.front() > lastReachableId) continue;
			uint32_t nextReachableId = labels[i].target;

			for (uint32_t j = i; j-- && labels[j].target > lastReachableId;) {
				if (labels[j].jumpIds.back() >= nextReachableId) nextReachableId = labels[j].target;
			}

			return nextReachableId;
		}

		return ID_EMPTY;
	}

	uint32_t get_label_from_id(const uint32_t& id) {
		for (uint32_t i = labels.size(); i-- && labels[i].target >= id;) {
			if (labels[i].target != id) continue;
			return i;
		}

		return ID_EMPTY;
	}

	uint32_t get_scope_begin_from_label(const uint32_t& label, const uint32_t& scopeEnd) {
		uint32_t scopeBegin = labels[label].target - 1;

		for (uint32_t i = label; i < labels.size() && labels[i].target <= scopeEnd; i++) {
			if (labels[i].jumpIds.front() <= scopeBegin) scopeBegin = labels[i].jumpIds.front() - 1;
		}

		return scopeBegin;
	}

	uint32_t get_scope_end_from_label(const uint32_t& label) {
		uint32_t scopeEnd = labels[label].target;

		for (uint32_t i = label; i < labels.size() && labels[i].target <= scopeEnd; i++) {
			if (labels[i].jumpIds.back() > scopeEnd) scopeEnd = labels[i].jumpIds.back();
		}

		return scopeEnd;
	}

	/*
	bool is_valid_block_range(const uint32_t& blockStartId, const uint32_t& blockEndId) {
		for (uint32_t i = 0; i < labels.size() && labels[i].target <= blockEndId; i++) {
			if (labels[i].target != blockStartId) continue;

			for (uint32_t j = 0; j < labels[i].gotoIds.size(); j++) {
				if (labels[i].gotoIds[j] < blockStartId || labels[i].gotoIds[j] > blockEndId) return false;
			}
		}

		return true;
	}
	*/

	const Bytecode::Prototype& prototype;
	uint32_t level = 0;
	bool isVariadic = false;
	bool hasDebugInfo = false;
	bool assignmentSlotIsUpvalue = false;
	std::vector<Local> locals;
	std::vector<Upvalue> upvalues;
	std::vector<Label> labels;
	std::vector<std::string> parameterNames;
	std::vector<Ast::SimpleAst::Statement*> block;
	std::vector<Ast::SimpleAst::Function*> childFunctions;
	std::vector<const std::string*> usedGlobals;

	struct {
		struct UpvalueInfo {
			enum TYPE {
				JUMP,
				UPVALUES,
				UPVALUE_CLOSE
			} type;

			uint32_t id = ID_EMPTY;
			uint32_t target = ID_EMPTY;
			std::vector<uint8_t> upvalues;
			uint16_t upvalueClose = 0;
		};

		struct UpvalueScope {
			uint8_t slot = 0;
			uint32_t minScopeBegin = ID_EMPTY;
			uint32_t minScopeEnd = ID_EMPTY;
		};

		struct SlotInfo {
			bool isParameter = false;
			SlotScope** activeSlotScope = nullptr;
			uint32_t minScopeBegin = ID_EMPTY;
			std::vector<SlotScope**> slotScopes;
		};

		SlotScope** new_slot_scope() {
			slotScopes.emplace_back(new SlotScope);
			return &slotScopes.back()->slotScope;
		}

		void set_slot_infos(const uint8_t& framesize, const uint8_t& parameters) {
			slotInfos.resize(framesize);

			for (uint8_t i = parameters; i--;) {
				slotInfos[i].isParameter = true;
				slotInfos[i].activeSlotScope = new_slot_scope();
			}
		}

		uint32_t add_upvalue_info(const uint32_t& id, const UpvalueInfo::TYPE& type) {
			for (uint32_t i = 0; i < upvalueInfos.size(); i++) {
				if (id > upvalueInfos[i].id) continue;
				upvalueInfos.emplace(upvalueInfos.begin() + i);
				upvalueInfos[i].type = type;
				upvalueInfos[i].id = id;
				return i;
			}

			upvalueInfos.emplace_back();
			upvalueInfos.back().type = type;
			upvalueInfos.back().id = id;
			return upvalueInfos.size() - 1;
		}

		void add_upvalues(const uint32_t& id, std::vector<uint8_t>& upvalues) {
			for (uint8_t i = upvalues.size(); i--;) {
				if (slotInfos[upvalues[i]].isParameter) upvalues.erase(upvalues.begin() + i);
			}

			if (upvalues.size()) upvalueInfos[add_upvalue_info(id, UpvalueInfo::UPVALUES)].upvalues = upvalues;
		}

		void add_jump(const uint32_t& id, const uint32_t& target) {
			upvalueInfos[add_upvalue_info(id, UpvalueInfo::JUMP)].target = target;
		}

		void add_upvalue_close(const uint32_t& id, const uint32_t& target, const uint16_t& upvalueClose) {
			uint32_t index = add_upvalue_info(id, UpvalueInfo::UPVALUE_CLOSE);
			upvalueInfos[index].target = target;
			upvalueInfos[index].upvalueClose = upvalueClose;
		}

		void add_loop(const uint32_t& id, const uint32_t& target) {
			uint32_t index = add_upvalue_info(id, UpvalueInfo::JUMP);
			upvalueInfos[index].target = target;

			while (++index < upvalueInfos.size() && upvalueInfos[index].id < target) {}

			upvalueInfos.emplace(upvalueInfos.begin() + index);
			upvalueInfos[index].id = target - 1;
			upvalueInfos[index].target = id;
		}

		void add_upvalue_scope(const uint8_t& slot, const uint32_t& minScopeBegin, const uint32_t& minScopeEnd) {
			for (uint32_t i = 0; i < upvalueScopes.size(); i++) {
				if (minScopeEnd > upvalueScopes[i].minScopeEnd) continue;
				upvalueScopes.emplace(upvalueScopes.begin() + i);
				upvalueScopes[i].slot = slot;
				upvalueScopes[i].minScopeBegin = minScopeBegin;
				upvalueScopes[i].minScopeEnd = minScopeEnd;
				return;
			}

			upvalueScopes.emplace_back();
			upvalueScopes.back().slot = slot;
			upvalueScopes.back().minScopeBegin = minScopeBegin;
			upvalueScopes.back().minScopeEnd = minScopeEnd;
		}

		void build_upvalue_scopes() {
			uint32_t index, minScopeBegin, minScopeEnd;

			for (uint32_t i = upvalueInfos.size(); i--;) {
				if (upvalueInfos[i].type != UpvalueInfo::UPVALUES) continue;

				for (uint8_t j = upvalueInfos[i].upvalues.size(); j--;) {
					index = i;
					minScopeBegin = upvalueInfos[i].id;
					minScopeEnd = upvalueInfos[i].id;

					for (uint32_t k = i + 1; k < upvalueInfos.size(); k++) {
						switch (upvalueInfos[k].type) {
						case UpvalueInfo::UPVALUE_CLOSE:
							if (upvalueInfos[i].upvalues[j] >= upvalueInfos[k].upvalueClose) {
								if (minScopeEnd > upvalueInfos[k].id) continue;
								minScopeEnd = upvalueInfos[k].id;
								break;
							}
						case UpvalueInfo::JUMP:
							if (minScopeEnd < upvalueInfos[k].target) {
								minScopeEnd = upvalueInfos[k].target;
							} else if (minScopeBegin >= upvalueInfos[k].target) {
								minScopeBegin = upvalueInfos[k].target - 1;

								for (uint32_t l = index; l-- && minScopeBegin < upvalueInfos[l].id; index = l) {
									switch (upvalueInfos[l].type) {
									case UpvalueInfo::UPVALUE_CLOSE:
										if (upvalueInfos[i].upvalues[j] >= upvalueInfos[l].upvalueClose) continue;
									case UpvalueInfo::JUMP:
										if (minScopeEnd < upvalueInfos[l].target) {
											minScopeEnd = upvalueInfos[l].target;
										} else if (minScopeBegin >= upvalueInfos[l].target) {
											minScopeBegin = upvalueInfos[l].target - 1;
										}
									}
								}
							}
						default:
							continue;
						}

						break;
					}

					add_upvalue_scope(upvalueInfos[i].upvalues[j], minScopeBegin, minScopeEnd);
				}
			}
		}

		void begin_scope(const uint8_t& slot, const uint32_t& id) {
			if (slotInfos[slot].activeSlotScope) return;
			slotInfos[slot].slotScopes.emplace_back(new_slot_scope());
			slotInfos[slot].activeSlotScope = slotInfos[slot].slotScopes.back();
			(*slotInfos[slot].activeSlotScope)->scopeEnd = id;
		}

		void begin_upvalue_scopes(const uint32_t& id) {
			while (upvalueScopes.size() && upvalueScopes.back().minScopeEnd >= id) {
				if (upvalueScopes.back().minScopeBegin <= id) {
					begin_scope(upvalueScopes.back().slot, id);
					extend_scope(upvalueScopes.back().slot, upvalueScopes.back().minScopeBegin);
				}

				upvalueScopes.pop_back();
			}
		}

		void add_to_scope(const uint8_t& slot, SlotScope**& slotScope, const uint32_t& id) {
			begin_scope(slot, id);
			slotScope = slotInfos[slot].activeSlotScope;
			(*slotInfos[slot].activeSlotScope)->usages++;
		}

		void complete_scope(const uint8_t& slot, SlotScope**& slotScope, const uint32_t& id) {
			if (slotInfos[slot].isParameter
				|| (slotInfos[slot].minScopeBegin != ID_EMPTY
					&& slotInfos[slot].minScopeBegin < id))
				return add_to_scope(slot, slotScope, id);
			begin_scope(slot, id);
			slotScope = slotInfos[slot].activeSlotScope;
			(*slotInfos[slot].activeSlotScope)->scopeBegin = id;
			slotInfos[slot].activeSlotScope = nullptr;
			slotInfos[slot].minScopeBegin = ID_EMPTY;
		}

		void extend_scope(const uint8_t& slot, const uint32_t& id) {
			if (!slotInfos[slot].isParameter
				&& slotInfos[slot].activeSlotScope
				&& (slotInfos[slot].minScopeBegin == ID_EMPTY
					|| slotInfos[slot].minScopeBegin > id))
				slotInfos[slot].minScopeBegin = id;
		}

		void extend_scopes(const uint32_t& id) {
			for (uint8_t i = slotInfos.size(); i--;) {
				extend_scope(i, id);
			}
		}

		void merge_scopes(const uint32_t& id) {
			for (uint8_t i = slotInfos.size(); i--;) {
				if (slotInfos[i].isParameter || !slotInfos[i].activeSlotScope) continue;

				for (uint32_t j = slotInfos[i].slotScopes.size() - 1; j-- && (*slotInfos[i].slotScopes[j])->scopeBegin <= id;) {
					(*slotInfos[i].activeSlotScope)->scopeEnd = (*slotInfos[i].slotScopes[j])->scopeEnd;
					(*slotInfos[i].activeSlotScope)->usages += (*slotInfos[i].slotScopes[j])->usages + 1;
					*slotInfos[i].slotScopes[j] = *slotInfos[i].activeSlotScope;
					slotInfos[i].slotScopes.erase(slotInfos[i].slotScopes.begin() + j);
				}

				if ((*slotInfos[i].activeSlotScope)->scopeEnd < id) (*slotInfos[i].activeSlotScope)->scopeEnd = id;
			}
		}

		bool assert_scopes_closed() {
			for (uint8_t i = slotInfos.size(); i--;) {
				if (!slotInfos[i].isParameter && slotInfos[i].activeSlotScope) return false;
			}

			return true;
		}

		bool lock_scope(const uint8_t& slot, SlotScope** const& slotScope) {
			if (slotInfos[slot].activeSlotScope) return false;
			slotInfos[slot].activeSlotScope = slotScope;
			return true;
		}

		void unlock_scope(const uint8_t& slot) {
			slotInfos[slot].activeSlotScope = nullptr;
		}

		void remove_scope(const uint8_t& slot, SlotScope** const& slotScope) {
			for (uint32_t i = slotInfos[slot].slotScopes.size(); i--;) {
				if (slotInfos[slot].slotScopes[i] != slotScope) continue;
				slotInfos[slot].slotScopes.erase(slotInfos[slot].slotScopes.begin() + i);
				return;
			}
		}

		void merge_locked_scope(const uint8_t& slot, SlotScope** const& slotScope) {
			if (slotInfos[slot].isParameter || !slotInfos[slot].activeSlotScope) return;
			(*slotInfos[slot].activeSlotScope)->scopeEnd = (*slotScope)->scopeEnd;
			(*slotInfos[slot].activeSlotScope)->usages += (*slotScope)->usages + 1;
			*slotScope = *slotInfos[slot].activeSlotScope;
			remove_scope(slot, slotScope);
		}

		std::vector<UpvalueInfo> upvalueInfos;
		std::vector<UpvalueScope> upvalueScopes;
		std::vector<SlotInfo> slotInfos;
		std::vector<SlotScope*> slotScopes;
		uint32_t previousId = ID_EMPTY;
		uint32_t previousLabel = ID_EMPTY;
	} slotScopeCollector;
};
