
#include "main.h"
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <iostream>
#include <cstring>

struct Error {
	const std::string message;
	const std::string filePath;
	const std::string function;
	const std::string source;
	const std::string line;
};


static bool isCommandLine = true;
static bool isProgressBarActive = false;
static uint32_t filesSkipped = 0;

static struct {
	bool showHelp = false;
	bool silentAssertions = false;
	bool forceOverwrite = false;
	bool ignoreDebugInfo = false;
	bool minimizeDiffs = false;
	bool unrestrictedAscii = false;
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

static void find_files_recursively(Directory& directory) {
	std::string fullPath = arguments.inputPath + directory.path;
	DIR* dir = opendir(fullPath.c_str());
	if (!dir) return;
	struct dirent* entry;
	while ((entry = readdir(dir)) != nullptr) {
		if (std::strcmp(entry->d_name, ".") == 0 || std::strcmp(entry->d_name, "..") == 0) continue;
		std::string entryPath = directory.path + entry->d_name;
		std::string fullEntryPath = fullPath + entry->d_name;
		struct stat st;
		std::string statPath = fullPath + entry->d_name;
		if (stat(statPath.c_str(), &st) == 0 && S_ISDIR(st.st_mode)) {
			entryPath += "/";
			directory.folders.emplace_back(Directory{ .path = entryPath });
			find_files_recursively(directory.folders.back());
			if (!directory.folders.back().files.size() && !directory.folders.back().folders.size()) directory.folders.pop_back();
			continue;
		}
		// Only add files matching extension filter
		std::string ext;
		size_t dot = std::string(entry->d_name).find_last_of('.');
		if (dot != std::string::npos) ext = string_to_lowercase(std::string(entry->d_name).substr(dot));
		if (!arguments.extensionFilter.size() || arguments.extensionFilter == ext) directory.files.emplace_back(entry->d_name);
	}
	closedir(dir);
}


static bool decompile_files_recursively(const Directory& directory) {
	std::string outDir = arguments.outputPath + directory.path;
	mkdir(outDir.c_str(), 0755);
	std::string outputFile;

	for (uint32_t i = 0; i < directory.files.size(); i++) {
		outputFile = directory.files[i];
		// Remove extension (portable)
		size_t dot = outputFile.find_last_of('.');
		if (dot != std::string::npos) {
			outputFile = outputFile.substr(0, dot);
		}
		outputFile += ".lua";

		Bytecode bytecode(arguments.inputPath + directory.path + directory.files[i]);
		Ast ast(bytecode, arguments.ignoreDebugInfo, arguments.minimizeDiffs);
		Lua lua(bytecode, ast, arguments.outputPath + directory.path + outputFile, arguments.forceOverwrite, arguments.minimizeDiffs, arguments.unrestrictedAscii);

		try {
			print("--------------------\nInput file: " + bytecode.filePath + "\nReading bytecode...");
			bytecode();
			print("Building ast...");
			ast();
			print("Writing lua source...");
			lua();
			print("Output file: " + lua.filePath);
		} catch (const Error& error) {
			erase_progress_bar();

			if (arguments.silentAssertions) {
				print("\nError running " + error.function + "\nSource: " + error.source + ":" + error.line + "\n\n" + error.message);
				filesSkipped++;
				continue;
			}

				print("\nError running " + error.function + "\nSource: " + error.source + ":" + error.line + "\n\nFile: " + error.filePath + "\n\n" + error.message);
				filesSkipped++;
				continue;
			} catch (...) {
				print("Unknown exception\n\nFile: " + bytecode.filePath);
				throw;
			}
	}

	for (uint32_t i = 0; i < directory.folders.size(); i++) {
		if (!decompile_files_recursively(directory.folders[i])) return false;
	}

	return true;
}

static char* parse_arguments(const int& argc, char** const& argv) {
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

		if (argument.size() >= 2 && argument.front() == '-') {
			if (argument[1] == '-') {
				argument = argument.c_str() + 2;

				if (argument == "extension") {
					if (i <= argc - 2) {
						i++;
						arguments.extensionFilter = argv[i];
						continue;
					}
				} else if (argument == "force_overwrite") {
					arguments.forceOverwrite = true;
					continue;
				} else if (argument == "help") {
					arguments.showHelp = true;
					continue;
				} else if (argument == "ignore_debug_info") {
					arguments.ignoreDebugInfo = true;
					continue;
				} else if (argument == "minimize_diffs") {
					arguments.minimizeDiffs = true;
					continue;
				} else if (argument == "output") {
					if (i <= argc - 2) {
						i++;
						arguments.outputPath = argv[i];
						continue;
					}
				} else if (argument == "silent_assertions") {
					arguments.silentAssertions = true;
					continue;
				} else if (argument == "unrestricted_ascii") {
					arguments.unrestrictedAscii = true;
					continue;
				}
			} else if (argument.size() == 2) {
				switch (argument[1]) {
				case 'e':
					if (i > argc - 2) break;
					i++;
					arguments.extensionFilter = argv[i];
					continue;
				case 'f':
					arguments.forceOverwrite = true;
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
				case 'u':
					arguments.unrestrictedAscii = true;
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
	print("Press Enter to exit.");
	std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

int main(int argc, char* argv[]) {
    // Linux: always assume command line
    isCommandLine = true;

	print(std::string(PROGRAM_NAME) + "\nCompiled on " + __DATE__);
	
	if (parse_arguments(argc, argv)) {
		print("Invalid argument: " + std::string(parse_arguments(argc, argv)) + "\nUse -? to show usage and options.");
		return EXIT_FAILURE;
	}
	
	if (arguments.showHelp) {
		print(
			"Usage: luajit-decompiler-v2.exe INPUT_PATH [options]\n"
			"\n"
			"Available options:\n"
			"  -h, -?, --help\t\tShow this message\n"
			"  -o, --output OUTPUT_PATH\tOverride default output directory\n"
			"  -e, --extension EXTENSION\tOnly decompile files with the specified extension\n"
			"  -s, --silent_assertions\tDisable assertion error pop-up window\n"
			"\t\t\t\t  and auto skip files that fail to decompile\n"
			"  -f, --force_overwrite\t\tAlways overwrite existing files\n"
			"  -i, --ignore_debug_info\tIgnore bytecode debug info\n"
			"  -m, --minimize_diffs\t\tOptimize output formatting to help minimize diffs\n"
			"  -u, --unrestricted_ascii\tDisable default UTF-8 encoding and string restrictions"
		);
		return EXIT_SUCCESS;
	}
	
	if (!arguments.inputPath.size()) {
		print("No input path specified!");
		return EXIT_FAILURE;
	}

	struct stat pathStat;

	if (!arguments.outputPath.size()) {
		arguments.outputPath.resize(4096);
		ssize_t count = readlink("/proc/self/exe", arguments.outputPath.data(), arguments.outputPath.size() - 1);
		if (count != -1) {
			arguments.outputPath[count] = '\0';
			std::string exePath = arguments.outputPath.c_str();
			size_t slash = exePath.find_last_of("/");
			if (slash != std::string::npos) {
				arguments.outputPath = exePath.substr(0, slash + 1);
			} else {
				arguments.outputPath = "./";
			}
		} else {
			arguments.outputPath = "./";
		}
		arguments.outputPath += "output/";
		arguments.outputPath.shrink_to_fit();
	} else {
		if (stat(arguments.outputPath.c_str(), &pathStat) != 0) {
			print("Failed to open output path: " + arguments.outputPath);
			return EXIT_FAILURE;
		}
		if (!S_ISDIR(pathStat.st_mode)) {
			print("Output path is not a folder!");
			return EXIT_FAILURE;
		}
		switch (arguments.outputPath.back()) {
		case '/':
			break;
		default:
			arguments.outputPath += '/';
			break;
		}
	}

	if (arguments.extensionFilter.size()) {
		if (arguments.extensionFilter.front() != '.') arguments.extensionFilter.insert(arguments.extensionFilter.begin(), '.');
		arguments.extensionFilter = string_to_lowercase(arguments.extensionFilter);
	}

	if (stat(arguments.inputPath.c_str(), &pathStat) != 0) {
		print("Failed to open input path: " + arguments.inputPath);
		wait_for_exit();
		return EXIT_FAILURE;
	}

	Directory root;

	if (S_ISDIR(pathStat.st_mode)) {
		switch (arguments.inputPath.back()) {
		case '/':
			break;
		default:
			arguments.inputPath += '/';
			break;
		}

		find_files_recursively(root);

		if (!root.files.size() && !root.folders.size()) {
			print("No files " + (arguments.extensionFilter.size() ? "with extension " + arguments.extensionFilter + " " : "") + "found in path: " + arguments.inputPath);
			wait_for_exit();
			return EXIT_FAILURE;
		}
	} else {
		// Extract filename from path (portable)
		std::string inputFile = arguments.inputPath;
		size_t slash = inputFile.find_last_of("/");
		if (slash == std::string::npos) {
			root.files.emplace_back(inputFile);
			arguments.inputPath.clear();
		} else {
			root.files.emplace_back(inputFile.substr(slash + 1));
			arguments.inputPath = inputFile.substr(0, slash + 1);
		}
	}

	try {
		if (!decompile_files_recursively(root)) {
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
	std::cout << message << std::endl;
}

/*
std::string input() {
	static char BUFFER[1024];

	FlushConsoleInputBuffer(CONSOLE_INPUT);
	DWORD charsRead;
	return ReadConsoleA(CONSOLE_INPUT, BUFFER, sizeof(BUFFER), &charsRead, NULL) && charsRead > 2 ? std::string(BUFFER, charsRead - 2) : "";
}
*/


void print_progress_bar(const double& progress, const double& total) {
	static char PROGRESS_BAR[] = "\r[====================]";
	const uint8_t threshold = std::round(20 / total * progress);
	for (uint8_t i = 20; i--;) {
		PROGRESS_BAR[i + 2] = i < threshold ? '=' : ' ';
	}
	std::cout << PROGRESS_BAR << std::flush;
	isProgressBarActive = true;
}

void erase_progress_bar() {
	static constexpr char PROGRESS_BAR_ERASER[] = "\r                      \r";
	if (!isProgressBarActive) return;
	std::cout << PROGRESS_BAR_ERASER << std::flush;
	isProgressBarActive = false;
}

void assert(const bool& assertion, const std::string& message, const std::string& filePath, const std::string& function, const std::string& source, const uint32_t& line) {
	if (!assertion) throw Error{
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
