class Bytecode::Prototype {
	friend Bytecode;

public:

	struct {
		bool child = false;
		bool vararg = false;
		bool ffi = false;
		bool bitop = false;
		uint8_t parameters = 0;
		uint8_t framesize = 0;
		bool debugInfo = false;
		uint32_t firstLine = 0;
		uint32_t lineCount = 0;
	} header;

	uint32_t prototypeBytes = 0;
	std::vector<Instruction> instructions;
	std::vector<Upvalue> upvalues;
	std::vector<Constant> constants;
	std::vector<NumberConstant> numberConstants;
	std::vector<DebugInfo> debugInfo;

private:

	Prototype(const Bytecode& bytecode);
	~Prototype();

	void operator()(std::vector<Prototype*>& unlinkedPrototypes);
	void read_header();
	void read_instructions();
	void read_upvalues();
	void read_constants(std::vector<Prototype*>& unlinkedPrototypes);
	void read_number_constants();
	void read_debug_info();
	TableConstant get_table_constant();
	std::string get_string();
	uint32_t get_uleb128();
	uint32_t get_uleb128_33();
	uint8_t get_uint8();
	uint16_t get_uint16();
	uint32_t get_uint32();
	template<typename Type>
	Type get_bytes();

	const Bytecode& bytecode;
};
