#include "main.h"

struct Assertion {
	const std::string message;
	const std::string filePath;
	const std::string function;
	const std::string source;
	const std::string line;
};

static const HANDLE CONSOLE_OUTPUT = GetStdHandle(STD_OUTPUT_HANDLE);
static const HANDLE CONSOLE_INPUT = GetStdHandle(STD_INPUT_HANDLE);
static bool isCommandLine;
static bool isProgressBarActive = false;
static uint32_t filesSkipped = 0;

static struct {
	bool showHelp = false;
	bool silentAssertions = false;
	bool ignoreDebugInfo = false;
	bool minimizeDiffs = false;
	std::string inputPath;
	std::string outputPath;
	std::string extensionFilter;
} arguments;

struct Directory {
	const std::string path;
	std::vector<Directory> folders;
	std::vector<std::string> files;
};

static std::string string_to_lowercase(const std::string& string) {
	std::string lowercaseString = string;

	for (uint32_t i = lowercaseString.size(); i--;) {
		if (lowercaseString[i] < 'A' || lowercaseString[i] > 'Z') continue;
		lowercaseString[i] += 'a' - 'A';
	}

	return lowercaseString;
}

static void findFilesRecursively(Directory& directory) {
	WIN32_FIND_DATAA pathData;
	HANDLE handle = FindFirstFileA((arguments.inputPath + directory.path + '*').c_str(), &pathData);
	if (handle == INVALID_HANDLE_VALUE) return;

	do {
		if (pathData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
			if (!std::strcmp(pathData.cFileName, ".") || !std::strcmp(pathData.cFileName, "..")) continue;
			directory.folders.emplace_back(Directory{ .path = directory.path + pathData.cFileName + "\\" });
			findFilesRecursively(directory.folders.back());
			if (!directory.folders.back().files.size() && !directory.folders.back().folders.size()) directory.folders.pop_back();
			continue;
		}

		if (!arguments.extensionFilter.size() || arguments.extensionFilter == string_to_lowercase(PathFindExtensionA(pathData.cFileName))) directory.files.emplace_back(pathData.cFileName);
	} while (FindNextFileA(handle, &pathData));

	FindClose(handle);
}

static bool decompileFilesRecursively(const Directory& directory) {
	CreateDirectoryA((arguments.outputPath + directory.path).c_str(), NULL);
	std::string outputFile;

	for (uint32_t i = 0; i < directory.files.size(); i++) {
		outputFile = directory.files[i];
		PathRemoveExtensionA(outputFile.data());
		outputFile = outputFile.c_str();
		outputFile += ".lua";

		Bytecode bytecode(arguments.inputPath + directory.path + directory.files[i]);
		Ast ast(bytecode, arguments.ignoreDebugInfo, arguments.minimizeDiffs);
		Lua lua(bytecode, ast, arguments.outputPath + directory.path + outputFile, arguments.minimizeDiffs);

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

			if (arguments.silentAssertions) {
				print("\nError running " + assertion.function + "\nSource: " + assertion.source + ":" + assertion.line + "\n\n" + assertion.message);
				filesSkipped++;
				continue;
			}

			switch (MessageBoxA(NULL, ("Error running " + assertion.function + "\nSource: " + assertion.source + ":" + assertion.line + "\n\nFile: " + assertion.filePath + "\n\n" + assertion.message).c_str(),
				PROGRAM_NAME, MB_ICONERROR | MB_CANCELTRYCONTINUE | MB_DEFBUTTON3)) {
			case IDCANCEL:
				return false;
			case IDTRYAGAIN:
				print("Retrying...");
				i--;
				continue;
			case IDCONTINUE:
				print("File skipped.");
				filesSkipped++;
			}
		} catch (...) {
			MessageBoxA(NULL, std::string("Unknown exception\n\nFile: " + bytecode.filePath).c_str(), PROGRAM_NAME, MB_ICONERROR | MB_OK);
			throw;
		}
	}

	for (uint32_t i = 0; i < directory.folders.size(); i++) {
		if (!decompileFilesRecursively(directory.folders[i])) return false;
	}

	return true;
}

static char* parseArguments(const int& argc, char** const& argv) {
	if (argc < 2) return nullptr;
	arguments.inputPath = argv[1];
#ifndef _DEBUG
	if (!isCommandLine) return nullptr;
#endif
	bool isInputPathSet = true;

	if (arguments.inputPath.size() && arguments.inputPath.front() == '-') {
		arguments.inputPath.clear();
		isInputPathSet = false;
	}

	std::string argument;

	for (uint32_t i = isInputPathSet ? 2 : 1; i < argc; i++) {
		argument = argv[i];

		if (argument.size() >= 2 || argument.front() == '-') {
			if (argument[1] == '-') {
				argument = argument.c_str() + 2;

				if (argument == "extension") {
					if (i <= argc - 2) {
						i++;
						arguments.extensionFilter = argv[i];
						continue;
					}
				} else if (argument == "help") {
					arguments.showHelp = true;
					continue;
				} else if (argument == "ignore_debug_info") {
					arguments.ignoreDebugInfo = true;
					continue;
				} else if (argument == "minimize_diffs") {
					arguments.minimizeDiffs = true;
				} else if (argument == "output") {
					if (i <= argc - 2) {
						i++;
						arguments.outputPath = argv[i];
						continue;
					}
				} else if (argument == "silent_assertions") {
					arguments.silentAssertions = true;
					continue;
				}
			} else if (argument.size() == 2) {
				switch (argument[1]) {
				case 'e':
					if (i > argc - 2) break;
					i++;
					arguments.extensionFilter = argv[i];
					continue;
				case '?':
				case 'h':
					arguments.showHelp = true;
					continue;
				case 'i':
					arguments.ignoreDebugInfo = true;
					continue;
				case 'm':
					arguments.minimizeDiffs = true;
					continue;
				case 'o':
					if (i > argc - 2) break;
					i++;
					arguments.outputPath = argv[i];
					continue;
				case 's':
					arguments.silentAssertions = true;
					continue;
				}
			}
		}

		return argv[i];
	}

	return nullptr;
}

static void wait_for_exit() {
	if (isCommandLine) return;
	print("Press enter to exit.");
	input();
}

int main(int argc, char* argv[]) {
	print(std::string(PROGRAM_NAME) + "\nCompiled on " + __DATE__);

	{
		DWORD consoleProcessId;
		GetWindowThreadProcessId(GetConsoleWindow(), &consoleProcessId);
#ifdef _DEBUG
		isCommandLine = false;
#else
		isCommandLine = consoleProcessId != GetCurrentProcessId();
#endif
	}
	
	if (parseArguments(argc, argv)) {
		print("Invalid argument: " + std::string(parseArguments(argc, argv)) + "\nUse -? to show usage and options.");
		return EXIT_FAILURE;
	}
	
	if (arguments.showHelp) {
		print(
			"Usage: luajit-decompiler-v2.exe INPUT_PATH [options]\n"
			"\n"
			"Available options:\n"
			"  -h, -?, --help\t\tShow this message\n"
			"  -o, --output OUTPUT_PATH\tOverride output directory\n"
			"  -e, --extension EXTENSION\tOnly decompile files with the specified extension\n"
			"  -s, --silent_assertions\tDisable assertion error pop-up window\n"
			"\t\t\t\t  and auto skip files that fail to decompile\n"
			"  -i, --ignore_debug_info\tIgnore bytecode debug info\n"
			"  -m, --minimize_diffs\t\tOptimize output formatting to help minimize diffs"
		);
		return EXIT_SUCCESS;
	}
	
	if (!arguments.inputPath.size()) {
		print("No input path specified!");
		if (isCommandLine) return EXIT_FAILURE;
		arguments.inputPath.resize(MAX_PATH, NULL);
		OPENFILENAMEA dialogInfo = {
			.lStructSize = sizeof(OPENFILENAMEA),
			.hwndOwner = NULL,
			.lpstrFilter = NULL,
			.lpstrCustomFilter = NULL,
			.lpstrFile = arguments.inputPath.data(),
			.nMaxFile = (DWORD)arguments.inputPath.size(),
			.lpstrFileTitle = NULL,
			.lpstrInitialDir = NULL,
			.lpstrTitle = PROGRAM_NAME,
			.Flags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST,
			.lpstrDefExt = NULL,
			.FlagsEx = NULL
		};
		print("Please select a valid LuaJIT bytecode file.");
		if (!GetOpenFileNameA(&dialogInfo)) return EXIT_FAILURE;
		arguments.inputPath = arguments.inputPath.c_str();
	}

	DWORD pathAttributes;

	if (!arguments.outputPath.size()) {
		arguments.outputPath.resize(MAX_PATH);
		GetModuleFileNameA(NULL, arguments.outputPath.data(), arguments.outputPath.size());
		*PathFindFileNameA(arguments.outputPath.data()) = '\x00';
		arguments.outputPath = arguments.outputPath.c_str();
		arguments.outputPath += "output\\";
		arguments.outputPath.shrink_to_fit();
	} else {
		pathAttributes = GetFileAttributesA(arguments.outputPath.c_str());

		if (pathAttributes == INVALID_FILE_ATTRIBUTES) {
			print("Failed to open output path: " + arguments.outputPath);
			return EXIT_FAILURE;
		}

		if (!(pathAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
			print("Output path is not a folder!");
			return EXIT_FAILURE;
		}

		switch (arguments.outputPath.back()) {
		case '/':
		case '\\':
			break;
		default:
			arguments.outputPath += '\\';
			break;
		}
	}

	if (arguments.extensionFilter.size()) {
		if (arguments.extensionFilter.front() != '.') arguments.extensionFilter.insert(arguments.extensionFilter.begin(), '.');
		arguments.extensionFilter = string_to_lowercase(arguments.extensionFilter);
	}

	pathAttributes = GetFileAttributesA(arguments.inputPath.c_str());

	if (pathAttributes == INVALID_FILE_ATTRIBUTES) {
		print("Failed to open input path: " + arguments.inputPath);
		wait_for_exit();
		return EXIT_FAILURE;
	}

	Directory root;

	if (pathAttributes & FILE_ATTRIBUTE_DIRECTORY) {
		switch (arguments.inputPath.back()) {
		case '/':
		case '\\':
			break;
		default:
			arguments.inputPath += '\\';
			break;
		}

		findFilesRecursively(root);

		if (!root.files.size() && !root.folders.size()) {
			print("No files " + (arguments.extensionFilter.size() ? "with extension " + arguments.extensionFilter + " " : "") + "found in path: " + arguments.inputPath);
			wait_for_exit();
			return EXIT_FAILURE;
		}
	} else {
		root.files.emplace_back(PathFindFileNameA(arguments.inputPath.c_str()));
		*PathFindFileNameA(arguments.inputPath.c_str()) = '\x00';
		arguments.inputPath = arguments.inputPath.c_str();
	}

	try {
		if (!decompileFilesRecursively(root)) {
			print("--------------------\nAborted!");
			wait_for_exit();
			return EXIT_FAILURE;
		}
	} catch (...) {
		throw;
	}

#ifndef _DEBUG
	print("--------------------\n" + (filesSkipped ? "Failed to decompile " + std::to_string(filesSkipped) + " file" + (filesSkipped > 1 ? "s" : "") + ".\n" : "") + "Done!");
	wait_for_exit();
#endif
	return EXIT_SUCCESS;
}

void print(const std::string& message) {
	WriteConsoleA(CONSOLE_OUTPUT, (message + '\n').data(), message.size() + 1, NULL, NULL);
}

std::string input() {
	static char BUFFER[1024];

	FlushConsoleInputBuffer(CONSOLE_INPUT);
	DWORD charsRead;
	return ReadConsoleA(CONSOLE_INPUT, BUFFER, sizeof(BUFFER), &charsRead, NULL) && charsRead > 2 ? std::string(BUFFER, charsRead - 2) : "";
}

void print_progress_bar(const double& progress, const double& total) {
	static char PROGRESS_BAR[] = "\r[====================]";

	const uint8_t threshold = std::round(20 / total * progress);

	for (uint8_t i = 20; i--;) {
		PROGRESS_BAR[i + 2] = i < threshold ? '=' : ' ';
	}

	WriteConsoleA(CONSOLE_OUTPUT, PROGRESS_BAR, sizeof(PROGRESS_BAR) - 1, NULL, NULL);
	isProgressBarActive = true;
}

void erase_progress_bar() {
	static constexpr char PROGRESS_BAR_ERASER[] = "\r                      \r";

	if (!isProgressBarActive) return;
	WriteConsoleA(CONSOLE_OUTPUT, PROGRESS_BAR_ERASER, sizeof(PROGRESS_BAR_ERASER) - 1, NULL, NULL);
	isProgressBarActive = false;
}

void assert(const bool& assertion, const std::string& message, const std::string& filePath, const std::string& function, const std::string& source, const uint32_t& line) {
	if (!assertion) throw Assertion{
		.message = message,
		.filePath = filePath,
		.function = function,
		.source = source,
		.line = std::to_string(line)
	};
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
