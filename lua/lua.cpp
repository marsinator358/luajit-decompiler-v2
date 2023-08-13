#include "..\main.h"

Lua::Lua(const Ast& ast, const std::string& filePath) : ast(ast), filePath(filePath) {}

Lua::~Lua() {
	close_file();
}

void Lua::operator()() {
	create_file();
	close_file();
	writeBuffer.shrink_to_fit();
}

void Lua::create_file() {
	file = CreateFileA(filePath.c_str(), GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (file != INVALID_HANDLE_VALUE) {
		assert(MessageBoxA(NULL, ("File " + filePath + " already exists.\n\nDo you want to overwrite it?").c_str(), PROGRAM_NAME, MB_ICONWARNING | MB_YESNO | MB_DEFBUTTON2) == IDYES,
			"File already exists", filePath, DEBUG_INFO);
		CloseHandle(file);
	}

	file = CreateFileA(filePath.c_str(), GENERIC_WRITE, NULL, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL);
	assert(file != INVALID_HANDLE_VALUE, "Unable to create file", filePath, DEBUG_INFO);
}

void Lua::close_file() {
	if (file == INVALID_HANDLE_VALUE) return;
	CloseHandle(file);
	file = INVALID_HANDLE_VALUE;
}

void Lua::write_file() {
	DWORD charsWritten = 0;
	assert(WriteFile(file, writeBuffer.data(), writeBuffer.size(), &charsWritten, NULL) && !(writeBuffer.size() - charsWritten), "Failed writing to file", filePath, DEBUG_INFO);
	writeBuffer.clear();
}
