#include "main.h"

static const HANDLE CONSOLE_OUTPUT = GetStdHandle(STD_OUTPUT_HANDLE);
static const HANDLE CONSOLE_INPUT = GetStdHandle(STD_INPUT_HANDLE);
static bool isProgressBarActive = false;

int main(const int argc, const char* const argv[]) {
	print(PROGRAM_NAME);

	if (argc < 2 || argv[1] == "") {
		print("No file path specified! Press enter to exit.");
		input();
		return EXIT_FAILURE;
	}

	/*
	WIN32_FIND_DATAA fileData;
	HANDLE file = INVALID_HANDLE_VALUE;
	std::vector<std::string> folders(1, "./");
	std::string folder;
	print("--------------------");

	while (folders.size()) {
		folder = folders.back();
		print("[" + folder + "]");
		folders.back() += '*';
		file = FindFirstFileA(folders.back().c_str(), &fileData);
		folders.pop_back();

		if (file != INVALID_HANDLE_VALUE) {
			do {
				if (fileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
					folders.emplace_back(fileData.cFileName);

					if (folders.back() == "." || folders.back() == "..") {
						folders.pop_back();
					} else {
						folders.back() = folder + folders.back() + '\\';
					}
				} else {
					print(fileData.cFileName);
				}
			} while (FindNextFileA(file, &fileData));

			FindClose(file);
		}
	}

	print("--------------------");
	*/

#if defined _DEBUG
	std::string outputFile = argv[1];
	PathRemoveExtensionA(outputFile.data());
	outputFile = outputFile.data();
	outputFile += "_decompiled.lua";
#else
	std::string outputFile = argv[0];
	GetCurrentDirectoryA(outputFile.size(), outputFile.data());
	outputFile = outputFile.data();
	outputFile += "\\output\\";
	CreateDirectoryA(outputFile.c_str(), NULL);
	outputFile += PathFindFileNameA(argv[1]);
	PathRemoveExtensionA(outputFile.data());
	outputFile = outputFile.data();
	outputFile += ".lua";
#endif

	while (true) {
		Bytecode bytecode(argv[1]);
		Ast ast(bytecode);
		Lua lua(bytecode, ast, outputFile);

		try {
			print("Input file: " + bytecode.filePath);
			print("Reading bytecode...");
			bytecode();
			print("Building ast...");
			ast();
			print("Writing lua source...");
			lua();
			print("Output file: " + lua.filePath);
		} catch (const int& button) {
			erase_progress_bar();

			switch (button) {
			case IDCANCEL:
				print("Aborted! Press enter to exit.");
				input();
				return EXIT_FAILURE;
			case IDTRYAGAIN:
				print("Retrying...");
				continue;
			case IDCONTINUE:
				print("File skipped.");
				break;
			}
		}

		break;
	}

	print("Done! Press enter to exit.");
	input();
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
	if (!assertion) throw MessageBoxA(NULL, ("Error running " + function + "()\nSource: " + source + ":" + std::to_string(line)
		+ "\n\nFile: " + filePath + "\n\n" + message).c_str(), PROGRAM_NAME, MB_ICONERROR | MB_CANCELTRYCONTINUE | MB_DEFBUTTON3);
}

std::string byte_to_string(const uint8_t& byte) {
	char string[] = "0x00";
	uint8_t value;
	
	for (uint8_t i = 2; i--;) {
		value = (byte >> i * 4) & 0xF;

		switch (value) {
		case 0xA:
		case 0xB:
		case 0xC:
		case 0xD:
		case 0xE:
		case 0xF:
			string[3 - i] = 'A' + value - 0xA;
			continue;
		}

		string[3 - i] = '0' + value;
	}

	return string;
}
