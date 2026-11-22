#include "..\main.h"

Ast::IdReference::IdReference() {}

Ast::IdReference::IdReference(const Statement& statement) {
	operator=(statement);
}

Ast::IdReference::operator size_t() const {
	/*
	if (!statement) return INVALID_ID;

	if ((*statement) != (*statement)->reference) {
		std::vector<Statement*> statements;

		do {
			statements.emplace_back((*statement)->reference);
			(*statement) = (*statement)->reference;
		} while ((*statement) != (*statement)->reference);

		for (size_t i = statements.size(); i--;) {
			statements[i]->reference = (*statement);
		}
	}
	*/

	return statement ? (*statement)->id : INVALID_ID;
}

void Ast::IdReference::operator=(const Statement& statement) {
	this->statement = &statement.reference;
}

void Ast::IdReference::clear() {
	statement = nullptr;
}
