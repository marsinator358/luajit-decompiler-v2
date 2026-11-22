struct Local {
	std::vector<const std::string*> names;
	uint8_t baseSlot = 0;
	IdReference scopeBegin;
	IdReference scopeEnd;
	bool excludeFromScope = false;
};

struct Upvalue {
	const Bytecode::Upvalue* upvalue = nullptr;
	SlotScope** slotScope = nullptr;
};

struct SlotInfo {
	bool isParameter = false;
	SlotScope* activeSlotScope = nullptr;
	IdReference minScopeBegin;
	std::vector<SlotScope*> slotScopes;
};

struct SlotScope {
	SlotScope* reference = this;
	const std::string* name = nullptr;
	bool isUpvalue = false;
	IdReference scopeBegin;
	IdReference scopeEnd;
	uint32_t usages = 0;
};

struct Label {
	IdReference target;
	std::vector<IdReference> jumps;
	std::string name;
};

class Function {
public:

	Function(const Bytecode::Prototype& prototype);
	~Function();

	void register_jump(Statement& statement, const size_t& target);
	void remove_jump(Statement& statement);
	void apply_labels();
	bool is_valid_label(Label* const& label);
	void build_upvalue_scope(const size_t& id, const uint8_t& slot);
	void verify_upvalue_scopes();
	IdReference get_scope_begin_from_label(Label* const& label, const size_t& scopeEnd);
	IdReference get_scope_end_from_label(Label* const& label);
	Statement& get_statement_from_id(const size_t& id);
	Label* get_label_from_id(const size_t& id, const bool& getExtendedLabel, const bool& ignoreLocal);
	size_t get_index_from_label(Label* const& label);
	bool is_valid_block(const size_t& blockBegin, const size_t& blockEnd, const bool& ignoreFrontLabel);
	const Bytecode::Constant& get_constant(const uint16_t& index) const;
	const Bytecode::NumberConstant& get_number_constant(const uint16_t& index) const;

	uint32_t id = 0;
	uint32_t level = 0;
	std::vector<Statement*> masterBlock;
	std::unordered_map<Statement*, std::vector<Statement**>> mergedStatements;
	std::unordered_map<Statement*, IdReference> loopEnds;
	Block block;
	std::vector<Local> locals;
	std::vector<Upvalue> upvalues;
	std::vector<Label> labels;
	std::vector<Function*> childFunctions;
	std::unordered_set<std::string> usedNames;
	const Bytecode::Prototype& prototype;

	struct SlotScopeCollector {
		SlotScope* new_slot_scope();
		void begin_scope(const IdReference& id, const uint8_t& slot);
		void attach_scope(const IdReference& id, const uint8_t& slot, SlotScope**& slotScope);
		void end_scope(const IdReference& id, const uint8_t& slot, SlotScope**& slotScope);
		void remove_scope(const uint8_t& slot, SlotScope** const& slotScope);
		void extend_scopes(const IdReference& scopeBegin);
		void extend_scope(const uint8_t& slot, const IdReference& scopeBegin);
		void merge_scopes(const IdReference& scopeEnd);
		void merge_scope(const uint8_t& slot, const IdReference& scopeEnd);
		void verify_scopes_closed();

		std::vector<SlotInfo> slotInfos;
		std::vector<SlotScope*> slotScopes;
		std::unordered_map<SlotScope*, std::vector<SlotScope**>> mergedScopes;
	} slotScopeCollector;
};
