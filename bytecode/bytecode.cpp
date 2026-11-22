#include "..\main.h"

Bytecode::Bytecode(const HANDLE& file) : file(file) {}

Bytecode::~Bytecode() {
	for (size_t i = prototypes.size(); i--;) {
		delete prototypes[i];
	}
}

void Bytecode::operator()() {
	print_progress_bar();
	read_header();
	read_prototypes();
	buffer.clear();
	buffer.shrink_to_fit();
	erase_progress_bar();
}

void Bytecode::read_header() {
	read_bytes(5);
	assert(buffer[0] == BC_HEADER[0] && buffer[1] == BC_HEADER[1] && buffer[2] == BC_HEADER[2],
		L"Invalid bytecode header:\nExpected bytes " + byte_to_wstring(BC_HEADER[0]) + L" " + byte_to_wstring(BC_HEADER[1]) + L" " + byte_to_wstring(BC_HEADER[2])
		+ L", got " + byte_to_wstring(buffer[0]) + L" " + byte_to_wstring(buffer[1]) + L" " + byte_to_wstring(buffer[2])
		+ L"\n\nFile does not contain valid LuaJIT bytecode", DEBUG_INFO);
	header.version = buffer[3];
	assert(header.version == BC_VERSION_1 || header.version == BC_VERSION_2, L"Invalid bytecode version (" + byte_to_wstring(buffer[3]) + L")", DEBUG_INFO);
	assert(!(buffer[4] & ~(BC_F_BE | BC_F_STRIP | BC_F_FFI | (header.version == BC_VERSION_2 ? BC_F_FR2 | BC_F_BITOP : 0))), L"Invalid bytecode flags (" + byte_to_wstring(buffer[4]) + L")", DEBUG_INFO);
	header.bigEndian = buffer[4] & BC_F_BE;
	header.stripped = buffer[4] & BC_F_STRIP;
	header.ffi = buffer[4] & BC_F_FFI;
	header.fr2 = buffer[4] & BC_F_FR2;
	header.bitop = buffer[4] & BC_F_BITOP;
	if (!header.stripped && read_uleb128()) header.name.insert(header.name.begin(), buffer.begin(), buffer.end());
}

void Bytecode::read_prototypes() {
	uint64_t fileSize = 0;
	fileSize |= (uint64_t)GetFileSize(file, (LPDWORD)&fileSize) << 32;
	fileSize = (fileSize >> 32) | (fileSize << 32);

	auto get_remaining_bytes = [this, &fileSize]()->uint64_t {
		uint64_t bytes = 0;
		bytes |= (uint64_t)SetFilePointer(file, 0, (PLONG)&bytes, FILE_CURRENT) << 32;
		return fileSize - ((bytes >> 32) | (bytes << 32));
	};

	uint64_t totalBytes = get_remaining_bytes() - 1;
	std::vector<Prototype*> unlinkedPrototypes;

	while (read_uleb128()) {
		prototypes.emplace_back(new Prototype(*this));
		(*prototypes.back())(unlinkedPrototypes);
		totalPrototypeBytes += prototypes.back()->prototypeBytes;
		print_progress_bar(totalBytes - get_remaining_bytes(), totalBytes);
	}

	assert(!get_remaining_bytes(), L"Failed to reach end of file", DEBUG_INFO);
	assert(unlinkedPrototypes.size() == 1, L"Failed to link main prototype", DEBUG_INFO);
	main = unlinkedPrototypes.back();
	assert(main->header.vararg && !main->header.parameters && !main->upvalues.size(), L"Main prototype has invalid header", DEBUG_INFO);
}

bool Bytecode::read_uleb128() {
	read_bytes(1);
	uint32_t uleb128 = buffer[0];

	if (uleb128 >= 0x80) {
		uleb128 &= 0x7F;
		uint8_t bitShift = 0;

		do {
			bitShift += 7;
			assert(bitShift <= 28, L"Invalid uleb128", DEBUG_INFO);
			read_bytes(1);
			uleb128 |= (buffer[0] & 0x7F) << bitShift;
		} while (buffer[0] >= 0x80);
	}

	read_bytes(uleb128);
	return uleb128;
}

void Bytecode::read_bytes(const uint32_t& byteCount) {
	buffer.resize(byteCount);
	DWORD bytesRead = 0;
	assert(ReadFile(file, buffer.data(), byteCount, &bytesRead, NULL) && !(byteCount - bytesRead), L"Failed to read file", DEBUG_INFO);
}
