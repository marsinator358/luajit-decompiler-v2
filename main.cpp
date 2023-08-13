#include "main.h"

static const HANDLE CONSOLE_OUTPUT = GetStdHandle(STD_OUTPUT_HANDLE);
static const HANDLE CONSOLE_INPUT = GetStdHandle(STD_INPUT_HANDLE);
static bool isLoadingBarActive = false;

int main(const int argc, const char* const argv[]) {
	print(PROGRAM_NAME);

	if (argc < 2 || argv[1] == "") {
		print("No file path specified! Press enter to exit.");
		input();
		return EXIT_FAILURE;
	}

	while (true) {
		Bytecode bytecode(argv[1]);
		Ast ast(bytecode);
		Lua lua(ast, std::string(argv[1]) + ".lua");

		try {
			print("Input file: " + bytecode.filePath);
			print("Reading bytecode...");
			bytecode();
			print("Building ast...");
			ast();
			print("Writing lua source...");
			//lua();
			print("Output file: " + lua.filePath);
		} catch (const int& button) {
			erase_loading_bar();

			switch (button) {
			case IDCANCEL:
				print("Failed! Press enter to exit.");
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

void print_loading_bar(const double& progress, const double& total) {
	static char LOADING_BAR[] = "\r[====================]";

	const uint8_t threshold = std::round(20 / total * progress);

	for (uint8_t i = 20; i--;) {
		LOADING_BAR[i + 2] = i < threshold ? '=' : ' ';
	}

	WriteConsoleA(CONSOLE_OUTPUT, LOADING_BAR, 23, NULL, NULL);
	isLoadingBarActive = true;
}

void erase_loading_bar() {
	if (!isLoadingBarActive) return;
	WriteConsoleA(CONSOLE_OUTPUT, "\r                      \r", 24, NULL, NULL);
	isLoadingBarActive = false;
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
