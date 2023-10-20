#include "main.h"

struct Assertion {
	const std::string message;
};

static const HANDLE CONSOLE_OUTPUT = GetStdHandle(STD_OUTPUT_HANDLE);
static const HANDLE CONSOLE_INPUT = GetStdHandle(STD_INPUT_HANDLE);
static bool isProgressBarActive = false;

struct Directory {
	const std::string path;
	std::vector<Directory> folders;
	std::vector<std::string> files;
};

static void findFilesRecursively(const std::string& inputPath, Directory& directory) {
	WIN32_FIND_DATAA pathData;
	HANDLE handle = FindFirstFileA((inputPath + directory.path + '*').c_str(), &pathData);
	if (handle == INVALID_HANDLE_VALUE) return;

	do {
		if (pathData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
			if (!std::strcmp(pathData.cFileName, ".") || !std::strcmp(pathData.cFileName, "..")) continue;
			directory.folders.emplace_back(Directory{ .path = directory.path + pathData.cFileName + "\\" });
			findFilesRecursively(inputPath, directory.folders.back());
			if (!directory.folders.back().files.size() && !directory.folders.back().folders.size()) directory.folders.pop_back();
			continue;
		}

		directory.files.emplace_back(pathData.cFileName);
	} while (FindNextFileA(handle, &pathData));

	FindClose(handle);
}

static bool decompileFilesRecursively(const std::string& inputPath, const std::string& outputPath, const Directory& directory) {
	CreateDirectoryA((outputPath + directory.path).c_str(), NULL);
	std::string outputFile;

	for (uint32_t i = 0; i < directory.files.size(); i++) {
		outputFile = directory.files[i];
		PathRemoveExtensionA(outputFile.data());
		outputFile = outputFile.c_str();
		outputFile += ".lua";

		Bytecode bytecode(inputPath + directory.path + directory.files[i]);
		Ast ast(bytecode);
		Lua lua(bytecode, ast, outputPath + directory.path + outputFile);

		try {
			print("--------------------\nInput file: " + bytecode.filePath + "\nReading bytecode...");
			bytecode();
			print("Building ast...");
			ast();
			print("Writing lua source...");
			lua();
			print("Output file: " + lua.filePath);
		} catch (const Assertion& assertion) {
			erase_progress_bar();

			switch (MessageBoxA(NULL, assertion.message.c_str(), PROGRAM_NAME, MB_ICONERROR | MB_CANCELTRYCONTINUE | MB_DEFBUTTON3)) {
			case IDCANCEL:
				return false;
			case IDTRYAGAIN:
				print("Retrying...");
				i--;
				continue;
			case IDCONTINUE:
				print("File skipped.");
			}
		} catch (...) {
			MessageBoxA(NULL, std::string("Unknown exception\n\nFile: " + bytecode.filePath).c_str(), PROGRAM_NAME, MB_ICONERROR | MB_OK);
			throw;
		}
	}

	for (uint32_t i = 0; i < directory.folders.size(); i++) {
		if (!decompileFilesRecursively(inputPath, outputPath, directory.folders[i])) return false;
	}

	return true;
}

int main(const int argc, const char* const argv[]) {
	print(std::string(PROGRAM_NAME) + "\nCompiled on " + __DATE__);
	std::string inputPath = argc > 1 ? argv[1] : "";

	if (!inputPath.size()) {
		print("No file path specified!\nPlease drag and drop a valid LuaJIT bytecode file or a folder containing such files\nonto this program window and press enter to continue or press enter to exit.");
		inputPath = input();
		if (!inputPath.size()) return EXIT_FAILURE;
	}

	if (inputPath.size() >= 2
		&& inputPath.front() == '"'
		&& inputPath.back() == '"') {
		inputPath.erase(inputPath.begin());
		inputPath.pop_back();
	}

	const DWORD pathAttributes = GetFileAttributesA(inputPath.c_str());

	if (pathAttributes == INVALID_FILE_ATTRIBUTES) {
		print("Failed to retrieve attributes for path: " + inputPath + "\nPress enter to exit.");
		input();
		return EXIT_FAILURE;
	}

	Directory root;

	if (pathAttributes & FILE_ATTRIBUTE_DIRECTORY) {
		inputPath += '\\';
		findFilesRecursively(inputPath, root);

		if (!root.files.size() && !root.folders.size()) {
			print("No files found in path: " + inputPath + "\nPress enter to exit.");
			input();
			return EXIT_FAILURE;
		}
	} else {
		root.files.emplace_back(PathFindFileNameA(inputPath.c_str()));
		PathRemoveFileSpecA(inputPath.data());
		inputPath = inputPath.c_str();
		inputPath += '\\';
	}

	std::string outputPath = argv[0];
	PathRemoveFileSpecA(outputPath.data());
	outputPath = outputPath.c_str();
	outputPath += "\\output\\";

	try {
		if (!decompileFilesRecursively(inputPath, outputPath, root)) {
			print("--------------------\nAborted! Press enter to exit.");
			input();
			return EXIT_FAILURE;
		}
	} catch (...) {
		throw;
	}

#if !defined _DEBUG
	print("--------------------\nDone! Press enter to exit.");
	input();
#endif
	return EXIT_SUCCESS;
}

void print(const std::string& message) {
	WriteConsoleA(CONSOLE_OUTPUT, (message + '\n').data(), message.size() + 1, NULL, NULL);
}

std::string input() {
	static char BUFFER[1024];

	DWORD charsRead;
	return ReadConsoleA(CONSOLE_INPUT, BUFFER, 1024, &charsRead, NULL) && charsRead > 2 ? std::string(BUFFER, charsRead - 2) : "";
}

void print_progress_bar(const double& progress, const double& total) {
	static char PROGRESS_BAR[] = "\r[====================]";

	const uint8_t threshold = std::round(20 / total * progress);

	for (uint8_t i = 20; i--;) {
		PROGRESS_BAR[i + 2] = i < threshold ? '=' : ' ';
	}

	WriteConsoleA(CONSOLE_OUTPUT, PROGRESS_BAR, 23, NULL, NULL);
	isProgressBarActive = true;
}

void erase_progress_bar() {
	if (!isProgressBarActive) return;
	WriteConsoleA(CONSOLE_OUTPUT, "\r                      \r", 24, NULL, NULL);
	isProgressBarActive = false;
}

void assert(const bool& assertion, const std::string& message, const std::string& filePath, const std::string& function, const std::string& source, const uint32_t& line) {
	if (!assertion) throw Assertion{ .message = "Error running " + function + "()\nSource: " + source + ":" + std::to_string(line) + "\n\nFile: " + filePath + "\n\n" + message };
}

std::string byte_to_string(const uint8_t& byte) {
	char string[] = "0x00";
	uint8_t digit;
	
	for (uint8_t i = 2; i--;) {
		digit = (byte >> i * 4) & 0xF;
		string[3 - i] = digit >= 0xA ? 'A' + digit - 0xA : '0' + digit;
	}

	return string;
}
