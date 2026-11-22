class Block {
public:

	Block(Function& function, Statement* const& statement);
	~Block();

	Statement& operator[](const size_t& index);
	Statement& back();
	void emplace(const size_t& index, Statement* const& statement);
	void move(const size_t& index, Block& block, const size_t& begin, const size_t& end);
	void merge(const size_t& index, const bool& mergeAbove = true);
	size_t size();
	size_t get_index_from_id(const size_t& id);
	size_t get_end_id();

private:

	//void erase(const size_t& index);
	size_t get_master_index(const size_t& index);
	void update_statements(const size_t& index);

	Function& function;
	const size_t& parentId;
	std::vector<Statement*> statements;
};
