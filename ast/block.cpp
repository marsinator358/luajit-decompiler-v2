#include "..\main.h"

Ast::Block::Block(Function& function, Statement* const& statement) : function(function), parentId(statement ? statement->id : INVALID_ID) {}

Ast::Block::~Block() {}

Ast::Statement& Ast::Block::operator[](const size_t& index) {
	return *statements[index];
}

Ast::Statement& Ast::Block::back() {
	return *statements.back();
}

void Ast::Block::emplace(const size_t& index, Statement* const& statement) {
	size_t masterIndex = get_master_index(index);
	function.masterBlock.emplace(function.masterBlock.begin() + masterIndex, statement);
	update_statements(masterIndex);
	statements.emplace(statements.begin() + index, statement);
}

/*
void Ast::Block::erase(const size_t& index) {
	size_t masterIndex = get_master_index(index);
	function.masterBlock.erase(function.masterBlock.begin() + masterIndex);
	update_statements(masterIndex);
	statements[index]->id = INVALID_ID;
	statements.erase(statements.begin() + index);
}
*/

void Ast::Block::move(const size_t& index, Block& block, const size_t& begin, const size_t& end) {
	if (begin == end) return;
	size_t masterIndex = block.get_master_index(begin);
	size_t masterUpdateIndex = masterIndex;
	std::vector<Statement*> movedStatements(function.masterBlock.begin() + masterIndex, function.masterBlock.begin() + masterIndex + (block[end - 1].block.get_end_id() - block[begin].id));
	function.masterBlock.erase(function.masterBlock.begin() + masterIndex, function.masterBlock.begin() + masterIndex + movedStatements.size());
	masterIndex = get_master_index(index);
	if (masterIndex < masterUpdateIndex) masterUpdateIndex = masterIndex;
	function.masterBlock.insert(function.masterBlock.begin() + masterIndex, movedStatements.begin(), movedStatements.end());
	update_statements(masterUpdateIndex);

	if (&block != this) {
		statements.insert(statements.begin() + index, block.statements.begin() + begin, block.statements.begin() + end);
		block.statements.erase(block.statements.begin() + begin, block.statements.begin() + end);
		return;
	}

	movedStatements.clear();
	movedStatements.insert(movedStatements.begin(), statements.begin() + begin, statements.begin() + end);
	statements.erase(statements.begin() + begin, statements.begin() + end);
	statements.insert(statements.begin() + (begin < index ? index - movedStatements.size() : index), movedStatements.begin(), movedStatements.end());
}

void Ast::Block::merge(const size_t& index, const bool& mergeAbove) {
	size_t mergedIndex = mergeAbove ? index - 1 : index + 1;
	if (mergeAbove) statements[index]->label = statements[mergedIndex]->label;
	statements[index]->assignment.usedLocals.merge(statements[mergedIndex]->assignment.usedLocals);

	if (function.mergedStatements.contains(statements[mergedIndex])) {
		for (size_t i = function.mergedStatements[statements[mergedIndex]].size(); i--;) {
			(*function.mergedStatements[statements[mergedIndex]][i]) = statements[index];
		}

		function.mergedStatements[statements[index]].insert(function.mergedStatements[statements[index]].begin(), function.mergedStatements[statements[mergedIndex]].begin(), function.mergedStatements[statements[mergedIndex]].end());
		function.mergedStatements.erase(statements[mergedIndex]);
	}

	size_t masterIndex = get_master_index(mergedIndex);

	for (size_t i = masterIndex + statements[mergedIndex]->block.size() + 1; i-- && i >= masterIndex;) {
		function.masterBlock[i]->reference = statements[index];
		function.mergedStatements[statements[index]].emplace_back(&function.masterBlock[i]->reference);
		function.masterBlock[i]->id = INVALID_ID;
		function.masterBlock.erase(function.masterBlock.begin() + i);
	}

	update_statements(masterIndex);
	statements.erase(statements.begin() + mergedIndex);
}

size_t Ast::Block::size() {
	return statements.size();
}

size_t Ast::Block::get_index_from_id(const size_t& id) {
	for (size_t i = statements.size(); i-- && statements[i]->id >= id;) {
		if (statements[i]->id == id) return i;
	}

	return INVALID_ID;
}

size_t Ast::Block::get_master_index(const size_t& index) {
	return index < statements.size() ? statements[index]->id : get_end_id();
}

size_t Ast::Block::get_end_id() {
	return statements.size() ? statements.back()->block.get_end_id() : (parentId != INVALID_ID ? parentId + 1 : 0);
}

void Ast::Block::update_statements(const size_t& index) {
	for (size_t i = function.masterBlock.size(); i-- > index;) {
		function.masterBlock[i]->id = i;
	}
}
