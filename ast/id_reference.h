class IdReference {
public:

	IdReference();
	IdReference(const Statement& statement);

	operator size_t() const;
	void operator=(const Statement& statement);
	void clear();

private:

	const Statement* const* statement = nullptr;
};
