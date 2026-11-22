#include "main.h"
//thread_local int myNumber = 0;

struct Error {
	const std::wstring message;
	const std::wstring function;
	const std::wstring source;
	const uint32_t line;
};

static const HANDLE CONSOLE = GetStdHandle(STD_OUTPUT_HANDLE);
static bool isCommandLine;
static bool isProgressBarActive = false;

static HANDLE open_file(const std::wstring& path) {
	HANDLE file = CreateFileW(path.c_str(), GENERIC_READ, NULL, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL);
	assert(file != INVALID_HANDLE_VALUE, L"Unable to open file", DEBUG_INFO);
	return file;
}

static HANDLE create_file(const std::wstring& path) {
	//TODO
}

static void close_file(HANDLE& file) {
	if (file == INVALID_HANDLE_VALUE) return;
	CloseHandle(file);
	file = INVALID_HANDLE_VALUE;
}

static int wait_for_exit(const bool& success = true) {
	if (!isCommandLine) {
		//FlushConsoleInputBuffer(CONSOLE_INPUT);
		print("Press any key to exit.");

		while (!_kbhit()) {
			Sleep(0);
		}
	}

	return success ? EXIT_SUCCESS : EXIT_FAILURE;
}

int wmain(int argc, wchar_t* argv[]) {
	SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_SYSTEM_AWARE);
	print(PROGRAM_NAME, true);
	print(std::string("Compiled on ") + __DATE__);
	//TODO

	std::wstring filePath(MAX_PATH, NULL);
	OPENFILENAMEW dialogInfo = {
		.lStructSize = sizeof(OPENFILENAMEW),
		.hwndOwner = NULL,
		.lpstrFilter = NULL,
		.lpstrCustomFilter = NULL,
		.lpstrFile = filePath.data(),
		.nMaxFile = (DWORD)filePath.size(),
		.lpstrFileTitle = NULL,
		.lpstrInitialDir = NULL,
		.lpstrTitle = W_PROGRAM_NAME,
		.Flags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST,
		.lpstrDefExt = NULL,
		.FlagsEx = NULL
	};
	print("SELECT FILE");

	if (GetOpenFileNameW(&dialogInfo)) {
		filePath = filePath.c_str();

		try {
			HANDLE file = open_file(filePath);

			if (file != INVALID_HANDLE_VALUE) {
				print("DECOMPILING...");
				Bytecode bytecode(file);
				bytecode();
				close_file(file);
				Ast ast(bytecode);
				ast();
				print("DONE");
			} else {
				print("FILE ERROR");
			}
		} catch (const Error& error) {
			print("ERROR:");
			print(error.message);
		} catch (...) {
			print("UNKNOWN ERROR");
		}
	} else {
		print("NO FILE");
	}

	return wait_for_exit();
}

void print(const std::string& message, const bool& bright) {
	if (!bright) {
		WriteConsoleA(CONSOLE, (message + '\n').data(), message.size() + 1, NULL, NULL);
		return;
	}

	SetConsoleTextAttribute(CONSOLE, FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_INTENSITY);
	WriteConsoleA(CONSOLE, (message + '\n').data(), message.size() + 1, NULL, NULL);
	SetConsoleTextAttribute(CONSOLE, FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED);
}

void print(const std::wstring& message, const bool& bright) {
	if (!bright) {
		WriteConsoleW(CONSOLE, (message + L'\n').data(), message.size() + 1, NULL, NULL);
		return;
	}

	SetConsoleTextAttribute(CONSOLE, FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_INTENSITY);
	WriteConsoleW(CONSOLE, (message + L'\n').data(), message.size() + 1, NULL, NULL);
	SetConsoleTextAttribute(CONSOLE, FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED);
}

void print_progress_bar(const double& progress, const double& total) {
	//TODO
}

void erase_progress_bar() {
	//TODO
}

void assert(const bool& assertion, const std::wstring& message, const std::wstring& function, const std::wstring& source, const uint32_t& line) {
	if (!assertion) throw Error {
		.message = message,
		.function = function,
		.source = source,
		.line = line
	};
}

std::wstring byte_to_wstring(const uint8_t& byte) {
	std::wstring string = L"0x00";

	for (uint8_t i = 2; i--;) {
		uint8_t digit = (byte >> i * 4) & 0xF;
		string[3 - i] = digit >= 0xA ? L'A' + digit - 0xA : L'0' + digit;
	}

	return string;
}
