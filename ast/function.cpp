#include "..\main.h"

Ast::Function::Function(const Bytecode::Prototype& prototype) : prototype(prototype), block(*this, nullptr) {
	slotScopeCollector.slotInfos.resize(prototype.header.framesize);

	for (size_t i = prototype.header.parameters; i--;) {
		slotScopeCollector.slotInfos[i].isParameter = true;
		slotScopeCollector.slotInfos[i].activeSlotScope = slotScopeCollector.new_slot_scope();
	}
}

Ast::Function::~Function() {
	for (size_t i = slotScopeCollector.slotScopes.size(); i--;) {
		delete slotScopeCollector.slotScopes[i];
	}
}

void Ast::Function::register_jump(Statement& statement, const size_t& target) {
	assert(target < masterBlock.size(), L"Invalid jump target", DEBUG_INFO);

	for (size_t i = 0; i < labels.size(); i++) {
		if (labels[i].target < target) continue;

		if (labels[i].target != target) {
			labels.emplace(labels.begin() + i);
			labels[i].target = *masterBlock[target];
		}

		for (size_t j = 0; j < labels[i].jumps.size(); j++) {
			if (labels[i].jumps[j] < statement.id) continue;
			labels[i].jumps.emplace(labels[i].jumps.begin() + j);
			labels[i].jumps[j] = statement;
			return;
		}

		labels[i].jumps.emplace_back();
		labels[i].jumps.back() = statement;
		return;
	}

	labels.emplace_back();
	labels.back().target = *masterBlock[target];
	labels.back().jumps.emplace_back();
	labels.back().jumps.back() = statement;
}

void Ast::Function::remove_jump(Statement& statement) {
	Label* label = masterBlock[*statement.target]->label;

	for (size_t i = label->jumps.size(); i--;) {
		if (label->jumps[i] != statement.id) continue;
		label->jumps[i].clear();
		label->jumps.erase(label->jumps.begin() + i);
		statement.target = nullptr;
		return;
	}
}

void Ast::Function::apply_labels() {
	for (size_t i = labels.size(); i--;) {
		masterBlock[labels[i].target]->label = &labels[i];

		for (size_t j = labels[i].jumps.size(); j--;) {
			masterBlock[labels[i].jumps[j]]->target = &labels[i].target;
		}
	}
}

Ast::Statement& Ast::Function::get_statement_from_id(const size_t& id) {
	return *masterBlock[id];
}

Ast::Label* Ast::Function::get_label_from_id(const size_t& id, const bool& getExtendedLabel, const bool& ignoreLocal) {
	if (id >= masterBlock.size()) return nullptr;
	Statement* statement = masterBlock[id];

	if (ignoreLocal && statement->type == AST_STATEMENT_LOCAL) {
		if (id + 1 >= masterBlock.size()) return nullptr;
		statement = masterBlock[id + 1];
	}

	switch (statement->type) {
	case AST_STATEMENT_EMPTY:
	case AST_STATEMENT_GOTO:
		if (getExtendedLabel && statement->instruction->type == Bytecode::BC_OP_JMP) return masterBlock[*statement->target]->label;
	}

	return is_valid_label(statement->label) ? statement->label : nullptr;
}

size_t Ast::Function::get_index_from_label(Label* const& label) {
	return label - labels.data();
}

bool Ast::Function::is_valid_label(Label* const& label) {
	return label && label->jumps.size();
}

void Ast::Function::build_upvalue_scope(const size_t& id, const uint8_t& slot) {
	slotScopeCollector.slotInfos[slot].activeSlotScope->isUpvalue = true;
	if (slotScopeCollector.slotInfos[slot].isParameter) return;
	size_t minScopeBegin = id;
	size_t minScopeEnd = id;

	auto extend_minScopeBegin = [&](const size_t& target)->void {
		if (minScopeBegin < target) return;
		assert(target, L"Failed to build upvalue scope", DEBUG_INFO);
		size_t index = minScopeBegin;
		minScopeBegin = target - 1;

		do {
			switch (masterBlock[index]->type) {
			case AST_STATEMENT_GOTO:
				if (masterBlock[index]->instruction->type == Bytecode::BC_OP_UCLO && slot >= masterBlock[index]->instruction->a) break;

				if (minScopeEnd < *masterBlock[index]->target) {
					minScopeEnd = *masterBlock[index]->target;
					break;
				}

				if (minScopeBegin >= *masterBlock[index]->target) {
					assert(*masterBlock[index]->target, L"Failed to build upvalue scope", DEBUG_INFO);
					minScopeBegin = *masterBlock[index]->target - 1;
				}

				break;
			case AST_STATEMENT_NUMERIC_FOR:
			case AST_STATEMENT_GENERIC_FOR:
			case AST_STATEMENT_LOOP:
				if (minScopeEnd < masterBlock[index]->block.get_end_id()) minScopeEnd = masterBlock[index]->block.get_end_id();
				break;
			}

			if (loopEnds.contains(masterBlock[index]) && minScopeBegin > loopEnds[masterBlock[index]]) minScopeBegin = loopEnds[masterBlock[index]];
		} while (index-- && index >= minScopeBegin);
	};

	for (size_t i = minScopeEnd; i < masterBlock.size() - 1; i++) {
		switch (masterBlock[i]->type) {
		case AST_STATEMENT_EMPTY:
		case AST_STATEMENT_GOTO:
			if (masterBlock[i]->instruction->type == Bytecode::BC_OP_UCLO && slot >= masterBlock[i]->instruction->a) {
				if (minScopeEnd > i) break;
				minScopeEnd = i;
				i = masterBlock.size() - 1;
				continue;
			}

			if (masterBlock[i]->type == AST_STATEMENT_EMPTY) break;
		case AST_STATEMENT_CONDITION:
			{
				size_t target = masterBlock[i]->type == AST_STATEMENT_CONDITION ? *masterBlock[i]->block.back().target : *masterBlock[i]->target;

				if (minScopeEnd < target) {
					minScopeEnd = target;
					break;
				}

				extend_minScopeBegin(target);
			}

			break;
		case AST_STATEMENT_NUMERIC_FOR:
		case AST_STATEMENT_GENERIC_FOR:
		case AST_STATEMENT_LOOP:
			if (minScopeEnd < masterBlock[i]->block.get_end_id()) minScopeEnd = masterBlock[i]->block.get_end_id();
			break;
		}

		if (loopEnds.contains(masterBlock[i + 1])) extend_minScopeBegin(loopEnds[masterBlock[i + 1]]);
	}

	assert(minScopeEnd != INVALID_ID
		&& masterBlock[minScopeEnd]->instruction->type == Bytecode::BC_OP_UCLO
		&& slot >= masterBlock[minScopeEnd]->instruction->a,
		L"Failed to build upvalue scope", DEBUG_INFO);
	IdReference scopeEnd = *masterBlock[minScopeEnd];
	IdReference scopeBegin = *masterBlock[minScopeBegin];

	for (size_t i = 0; i < labels.size() && labels[i].target <= scopeEnd; i++) {
		if (labels[i].target <= id
			|| !labels[i].jumps.size()
			|| (labels[i].target == masterBlock[minScopeEnd]->id
				&& labels[i].jumps.back() < masterBlock[minScopeEnd]->id))
			continue;
		if (scopeEnd < labels[i].jumps.back()) scopeEnd = labels[i].jumps.back();
		if (scopeBegin > labels[i].jumps.front()) scopeBegin = labels[i].jumps.front();
		/*
		if (scopeBegin > labels[i].jumps.front()
			&& (labels[i].target != scopeEnd
				|| masterBlock[scopeEnd]->instruction->type != Bytecode::BC_OP_UCLO
				|| slot < masterBlock[scopeEnd]->instruction->a))
			scopeBegin = labels[i].jumps.front();
		*/
	}

	slotScopeCollector.merge_scope(slot, scopeEnd);
	slotScopeCollector.extend_scope(slot, scopeBegin);
}

void Ast::Function::verify_upvalue_scopes() {
	//TODO
	for (size_t i = slotScopeCollector.slotInfos.size(); i-- && !slotScopeCollector.slotInfos[i].isParameter;) {
		for (size_t j = slotScopeCollector.slotInfos[i].slotScopes.size(); j--;) {
			for (size_t k = slotScopeCollector.slotInfos[i].slotScopes[j]->scopeBegin; k <= slotScopeCollector.slotInfos[i].slotScopes[j]->scopeEnd; k++) {
				switch (masterBlock[k]->type) {
				case AST_STATEMENT_EMPTY:
				case AST_STATEMENT_GOTO:
					assert(masterBlock[k]->instruction->type != Bytecode::BC_OP_UCLO
						|| i < masterBlock[k]->instruction->a
						|| (masterBlock[k]->type != AST_STATEMENT_EMPTY
							&& (*masterBlock[k]->target > slotScopeCollector.slotInfos[i].slotScopes[j]->scopeEnd
								|| *masterBlock[k]->target <= slotScopeCollector.slotInfos[i].slotScopes[j]->scopeBegin))
						|| (masterBlock[k]->type == AST_STATEMENT_EMPTY
							&& (k == slotScopeCollector.slotInfos[i].slotScopes[j]->scopeEnd
								|| masterBlock[k + 1]->type == AST_STATEMENT_RETURN)),
						L"Invalid upvalue scope", DEBUG_INFO);
					continue;
				}
			}
		}
	}

	/*
	for (size_t i = slotScopeCollector.slotInfos.size(); i--;) {
		if (slotScopeCollector.slotInfos[i].isParameter) {
			if (!slotScopeCollector.slotInfos[i].activeSlotScope->isUpvalue) continue;

			for (size_t j = 0; j < masterBlock.size(); j++) {
				switch (masterBlock[j]->type) {
				case AST_STATEMENT_EMPTY:
				case AST_STATEMENT_GOTO:
				case AST_STATEMENT_BREAK:
					if (masterBlock[j]->instruction->type != Bytecode::BC_OP_UCLO || i < masterBlock[j]->instruction->a) continue;

					if (masterBlock[j]->type == AST_STATEMENT_EMPTY && masterBlock[j + 1]->type == AST_STATEMENT_RETURN) {
						j++;
						continue;
					}
				case AST_STATEMENT_RETURN:
					assert(false, L"Invalid upvalue scope", DEBUG_INFO);
				}
			}

			continue;
		}

		for (size_t j = slotScopeCollector.slotInfos[i].slotScopes.size(); j--;) {
			if (!slotScopeCollector.slotInfos[i].slotScopes[j]->isUpvalue) continue;

			for (size_t k = slotScopeCollector.slotInfos[i].slotScopes[j]->scopeBegin; k <= slotScopeCollector.slotInfos[i].slotScopes[j]->scopeEnd; k++) {
				switch (masterBlock[k]->type) {
				case AST_STATEMENT_EMPTY:
				case AST_STATEMENT_GOTO:
				case AST_STATEMENT_BREAK:
					if (masterBlock[k]->instruction->type != Bytecode::BC_OP_UCLO || i < masterBlock[k]->instruction->a) {
						if (masterBlock[k]->type == AST_STATEMENT_EMPTY && masterBlock[k]->instruction->type != Bytecode::BC_OP_UCLO) continue;
						assert((masterBlock[k]->type == AST_STATEMENT_EMPTY
								&& masterBlock[k + 1]->id <= slotScopeCollector.slotInfos[i].slotScopes[j]->scopeEnd)
							|| (masterBlock[k]->type != AST_STATEMENT_EMPTY
								&& *masterBlock[k]->target > slotScopeCollector.slotInfos[i].slotScopes[j]->scopeBegin
								&& *masterBlock[k]->target <= slotScopeCollector.slotInfos[i].slotScopes[j]->scopeEnd),
							L"Invalid upvalue scope", DEBUG_INFO);
						continue;
					}

					if ((masterBlock[k]->type == AST_STATEMENT_EMPTY
							&& masterBlock[k + 1]->id > slotScopeCollector.slotInfos[i].slotScopes[j]->scopeEnd)
						|| (masterBlock[k]->type != AST_STATEMENT_EMPTY
							&& (*masterBlock[k]->target <= slotScopeCollector.slotInfos[i].slotScopes[j]->scopeBegin
								|| *masterBlock[k]->target > slotScopeCollector.slotInfos[i].slotScopes[j]->scopeEnd)))
						continue;

					if (masterBlock[k]->type == AST_STATEMENT_EMPTY && masterBlock[k + 1]->type == AST_STATEMENT_RETURN) {
						k++;
						continue;
					}
				case AST_STATEMENT_RETURN:
					assert(false, L"Invalid upvalue scope", DEBUG_INFO);
				}
			}
		}
	}
	*/
}

Ast::IdReference Ast::Function::get_scope_begin_from_label(Label* const& label, const size_t& scopeEnd) {
	size_t scopeBegin = label->target;

	for (size_t i = get_index_from_label(label); i < labels.size() && labels[i].target <= scopeEnd; i++) {
		if (labels[i].jumps.size() && scopeBegin > labels[i].jumps.front()) scopeBegin = labels[i].jumps.front();
	}

	return get_statement_from_id(scopeBegin);
}

Ast::IdReference Ast::Function::get_scope_end_from_label(Label* const& label) {
	size_t scopeEnd = label->target;

	for (size_t i = get_index_from_label(label); i < labels.size() && labels[i].target <= scopeEnd; i++) {
		if (labels[i].jumps.size() && scopeEnd < labels[i].jumps.back()) scopeEnd = labels[i].jumps.back();
	}

	return get_statement_from_id(scopeEnd);
}

bool Ast::Function::is_valid_block(const size_t& blockBegin, const size_t& blockEnd, const bool& ignoreFrontLabel) {
	for (size_t i = labels.size(); i-- && labels[i].target >= blockBegin;) {
		if (labels[i].target <= blockEnd
			&& labels[i].jumps.size()
			&& ((labels[i].jumps.front() < blockBegin
				&& (labels[i].target != blockBegin
					|| !ignoreFrontLabel))
				|| labels[i].jumps.back() > blockEnd))
			return false;
	}

	return true;
}

const Bytecode::Constant& Ast::Function::get_constant(const uint16_t& index) const {
	assert(index < prototype.constants.size(), L"Invalid constant table index", DEBUG_INFO);
	return prototype.constants[prototype.constants.size() - 1 - index];
}

const Bytecode::NumberConstant& Ast::Function::get_number_constant(const uint16_t& index) const {
	assert(index < prototype.numberConstants.size(), L"Invalid number constant table index", DEBUG_INFO);
	return prototype.numberConstants[index];
}

Ast::SlotScope* Ast::Function::SlotScopeCollector::new_slot_scope() {
	return slotScopes.emplace_back(new SlotScope);
}

void Ast::Function::SlotScopeCollector::begin_scope(const IdReference& id, const uint8_t& slot) {
	assert(slot < slotInfos.size(), L"Slot is out of range", DEBUG_INFO);
	if (slotInfos[slot].activeSlotScope) return;
	slotInfos[slot].slotScopes.emplace_back(new_slot_scope());
	slotInfos[slot].activeSlotScope = slotInfos[slot].slotScopes.back();
	slotInfos[slot].activeSlotScope->scopeEnd = id;
	slotInfos[slot].minScopeBegin = id;
}

void Ast::Function::SlotScopeCollector::attach_scope(const IdReference& id, const uint8_t& slot, SlotScope**& slotScope) {
	begin_scope(id, slot);
	slotScope = &slotInfos[slot].activeSlotScope->reference;
	slotInfos[slot].activeSlotScope->usages++;
}

void Ast::Function::SlotScopeCollector::end_scope(const IdReference& id, const uint8_t& slot, SlotScope**& slotScope) {
	begin_scope(id, slot);

	if (slotInfos[slot].isParameter || id > slotInfos[slot].minScopeBegin) {
		attach_scope(id, slot, slotScope);
		return;
	}

	slotScope = &slotInfos[slot].activeSlotScope->reference;
	slotInfos[slot].activeSlotScope->scopeBegin = id;
	slotInfos[slot].activeSlotScope = nullptr;
	slotInfos[slot].minScopeBegin.clear();
}

void Ast::Function::SlotScopeCollector::remove_scope(const uint8_t& slot, SlotScope** const& slotScope) {
	for (size_t i = slotInfos[slot].slotScopes.size(); i--;) {
		if (slotInfos[slot].slotScopes[i] != (*slotScope)->reference) continue;
		slotInfos[slot].slotScopes.erase(slotInfos[slot].slotScopes.begin() + i);
		return;
	}
}

void Ast::Function::SlotScopeCollector::extend_scopes(const IdReference& scopeBegin) {
	for (size_t i = slotInfos.size(); i--;) {
		extend_scope(i, scopeBegin);
	}
}

void Ast::Function::SlotScopeCollector::extend_scope(const uint8_t& slot, const IdReference& scopeBegin) {
	if (!slotInfos[slot].isParameter
		&& slotInfos[slot].activeSlotScope
		&& scopeBegin < slotInfos[slot].minScopeBegin)
		slotInfos[slot].minScopeBegin = scopeBegin;
}

void Ast::Function::SlotScopeCollector::merge_scopes(const IdReference& scopeEnd) {
	for (size_t i = slotInfos.size(); i--;) {
		merge_scope(i, scopeEnd);
	}
}

void Ast::Function::SlotScopeCollector::merge_scope(const uint8_t& slot, const IdReference& scopeEnd) {
	if (slotInfos[slot].isParameter || !slotInfos[slot].activeSlotScope) return;

	for (size_t i = slotInfos[slot].slotScopes.size() - 1; i-- && slotInfos[slot].slotScopes[i]->scopeBegin <= scopeEnd;) {
		if (slotInfos[slot].slotScopes[i]->isUpvalue) slotInfos[slot].activeSlotScope->isUpvalue = true;
		if (slotInfos[slot].activeSlotScope->scopeEnd < slotInfos[slot].slotScopes[i]->scopeEnd) slotInfos[slot].activeSlotScope->scopeEnd = slotInfos[slot].slotScopes[i]->scopeEnd;
		slotInfos[slot].activeSlotScope->usages += slotInfos[slot].slotScopes[i]->usages + 1;
		slotInfos[slot].slotScopes[i]->reference = slotInfos[slot].activeSlotScope;
		mergedScopes[slotInfos[slot].activeSlotScope].emplace_back(&slotInfos[slot].slotScopes[i]->reference);

		if (mergedScopes.contains(slotInfos[slot].slotScopes[i])) {
			for (size_t j = mergedScopes[slotInfos[slot].slotScopes[i]].size(); j--;) {
				(*mergedScopes[slotInfos[slot].slotScopes[i]][j]) = slotInfos[slot].activeSlotScope;
			}

			mergedScopes[slotInfos[slot].activeSlotScope].insert(mergedScopes[slotInfos[slot].activeSlotScope].begin(), mergedScopes[slotInfos[slot].slotScopes[i]].begin(), mergedScopes[slotInfos[slot].slotScopes[i]].end());
			mergedScopes.erase(slotInfos[slot].slotScopes[i]);
		}

		slotInfos[slot].slotScopes.erase(slotInfos[slot].slotScopes.begin() + i);
	}

	if (slotInfos[slot].activeSlotScope->scopeEnd < scopeEnd) slotInfos[slot].activeSlotScope->scopeEnd = scopeEnd;
}

void Ast::Function::SlotScopeCollector::verify_scopes_closed() {
	for (size_t i = slotInfos.size(); i-- && !slotInfos[i].isParameter;) {
		assert(!slotInfos[i].activeSlotScope, L"Failed to close slot scope", DEBUG_INFO);
	}
}
