class Bytecode {
public:

	class Prototype;
	struct Constant;
	struct NumberConstant;
	struct TableConstant;
	struct TableNode;
	struct VariableInfo;
	struct Instruction;
	#include "prototype.h"
	#include "constants.h"
	#include "instructions.h"

	Bytecode(const std::string& filePath);
	~Bytecode();

	void read();

	const std::string filePath;

	struct {
		uint8_t version = 0;
		uint8_t flags = 0;
		std::string chunkname;
	} header;

	const Prototype* main = nullptr;

private:

	static constexpr uint8_t MIN_PROTO_SIZE = 7;
	static constexpr uint8_t MIN_FILE_SIZE = MIN_PROTO_SIZE + 7;

	void read_header();
	void read_prototypes();
	void open_file();
	void close_file();
	void read_file(const uint32_t& byteCount);
	uint32_t read_uleb128();
	bool buffer_next_block();

	HANDLE file = INVALID_HANDLE_VALUE;
	uint64_t bytesUnread = 0;
	std::vector<uint8_t> fileBuffer;
	std::vector<Prototype*> prototypes;
};
