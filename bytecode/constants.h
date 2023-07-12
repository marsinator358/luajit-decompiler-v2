static constexpr uint8_t BC_HEADER[] = { '\x1B', 'L', 'J' };
static constexpr uint8_t BC_VERSION_1 = 1;
static constexpr uint8_t BC_VERSION_2 = 2;
static constexpr uint8_t BC_F_BE = 0x01;
static constexpr uint8_t BC_F_STRIP = 0x02;
static constexpr uint8_t BC_F_FFI = 0x04;
static constexpr uint8_t BC_F_FR2 = 0x08;
static constexpr uint8_t BC_PROTO_CHILD = 0x01;
static constexpr uint8_t BC_PROTO_VARARG = 0x02;
static constexpr uint8_t BC_PROTO_FFI = 0x04;
static constexpr uint16_t BC_UV_IMMUTABLE = 0x4000;
static constexpr uint16_t BC_UV_LOCAL = 0x8000;

enum BC_KTAB {
	BC_KTAB_NIL, // primitive nil
	BC_KTAB_FALSE, // primitive false
	BC_KTAB_TRUE, // primitive true
	BC_KTAB_INT, // integer constant
	BC_KTAB_NUM, // number constant
	BC_KTAB_STR  // string constant
};

struct Bytecode::TableConstant {
	BC_KTAB type;

	union {
		uint32_t integer;
		uint64_t number = 0;
	};

	std::string string;
};

struct Bytecode::TableNode {
	TableConstant key;
	TableConstant value;
};

enum BC_KGC {
	BC_KGC_CHILD, // child prototype
	BC_KGC_TAB, // table constant
	BC_KGC_I64, // signed integer cdata constant
	BC_KGC_U64, // unsigned integer cdata constant
	BC_KGC_COMPLEX, // imaginary number cdata constant
	BC_KGC_STR // string constant
};

struct Bytecode::Constant {
	BC_KGC type;
	const Prototype* prototype = nullptr;
	std::vector<TableConstant> array;
	std::vector<TableNode> table;
	uint64_t cdata = 0;
	std::string string;
};

enum BC_KNUM {
	BC_KNUM_INT, // integer constant
	BC_KNUM_NUM // number constant
};

struct Bytecode::NumberConstant {
	BC_KNUM type;
	
	union {
		uint32_t integer;
		uint64_t number = 0;
	};
};

enum BC_VAR {
	BC_VAR_END, // end of variable info
	BC_VAR_FOR_IDX, // for numeric loop index
	BC_VAR_FOR_STOP, // for numeric loop limit
	BC_VAR_FOR_STEP, // for numeric loop step
	BC_VAR_FOR_GEN, // for generic loop generator
	BC_VAR_FOR_STATE, // for generic loop state
	BC_VAR_FOR_CTL, // for generic loop control
	BC_VAR_STR // local variable name
};

struct Bytecode::VariableInfo {
	BC_VAR type;
	std::string name;
	bool isParameter = false;
	uint32_t scopeBegin = 0;
	uint32_t scopeEnd = 0;
};
