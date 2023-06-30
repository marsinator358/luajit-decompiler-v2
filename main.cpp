#include "main.h"

int main(const int argc, const char* const argv[]) {
	print(PROGRAM_NAME);

	if (argc < 2 || argv[1] == "") {
		print("No file path specified! Press enter to exit.");
		input();
		return EXIT_FAILURE;
	}

	while (true) {
		Bytecode bytecode(argv[1]);
		print("Input file: " + bytecode.filePath);
		Ast ast(bytecode);
		//Lua lua(ast, argv[2]);

		try {
			bytecode.read();
			ast.build();
			//lua.write();
			//print("Output file: " + lua.filePath);
		} catch (const int& button) {
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
	static const HANDLE CONSOLE = GetStdHandle(STD_OUTPUT_HANDLE);

	WriteConsoleA(CONSOLE, (message + '\n').data(), message.size() + 1, NULL, NULL);
}

std::string input() {
	static const HANDLE CONSOLE = GetStdHandle(STD_INPUT_HANDLE);
	static std::string BUFFER(1024, '\0');

	DWORD charsRead = 0;
	return ReadConsoleA(CONSOLE, BUFFER.data(), BUFFER.size(), &charsRead, NULL) && charsRead > 2 ? BUFFER.substr(0, charsRead - 2) : "";
}

void assert(const bool& assertion, const std::string& message, const std::string& filePath, const std::string& function, const std::string& source, const uint32_t& line) {
	if (!assertion) throw MessageBoxA(NULL, ("Error running " + function + "()\nSource: " + source + ":" + std::to_string(line)
		+ "\n\nFile: " + filePath + "\n\n" + message).c_str(), PROGRAM_NAME, MB_ICONERROR | MB_CANCELTRYCONTINUE | MB_DEFBUTTON3);
}

std::string byte_to_string(const uint8_t& byte) {
	std::string string = "0x00";
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
