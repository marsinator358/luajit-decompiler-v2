class Bytecode {
public:

	class Prototype;
	friend Prototype;

	#include "constants.h"
	#include "instructions.h"

	Bytecode(const HANDLE& file);
	~Bytecode();

	void operator()();

	struct {
		uint8_t version = 0;
		bool bigEndian = false;
		bool stripped = false;
		bool ffi = false;
		bool fr2 = false;
		bool bitop = false;
		std::string name;
	} header;

	const Prototype* main = nullptr;
	uint64_t totalPrototypeBytes = 0;

private:

	void read_header();
	void read_prototypes();
	bool read_uleb128();
	void read_bytes(const uint32_t& bytes);

	const HANDLE& file;
	std::vector<Prototype*> prototypes;
	std::vector<uint8_t> buffer;
};

#include "prototype.h"
