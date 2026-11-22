/*
LuaJIT Decompiler v2
https://github.com/marsinator358/luajit-decompiler-v2

Requirements:
  Visual Studio
  C++20
  Windows API
  Default char is unsigned [/J]
*/

#ifndef _CHAR_UNSIGNED
#error Default char needs to be unsigned!
#endif

#pragma comment(linker, "/stack:268435456")
#pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
//#pragma comment(lib, "shlwapi.lib")

#include <bit>
#include <cmath>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <conio.h>
//#include <fileapi.h>
//#include <shlwapi.h>
#include <tchar.h>
#include <windows.h>

#define DEBUG_INFO _TEXT(__FUNCTION__), _TEXT(__FILE__), __LINE__

constexpr char PROGRAM_NAME[] = "LuaJIT Decompiler v2";
constexpr wchar_t W_PROGRAM_NAME[] = L"LuaJIT Decompiler v2";
constexpr uint64_t DOUBLE_SIGN = 0x8000000000000000;
constexpr uint64_t DOUBLE_EXPONENT = 0x7FF0000000000000;
constexpr uint64_t DOUBLE_FRACTION = 0x000FFFFFFFFFFFFF;
constexpr uint64_t DOUBLE_SPECIAL = DOUBLE_EXPONENT;
constexpr uint64_t DOUBLE_NEGATIVE_ZERO = DOUBLE_SIGN;

static struct {
	bool constDeclaration = false;
	bool continueStatement = false;
	bool customaryOperators = false;
	bool disableSyntaxExtensions = false;
	bool enableUTF8 = false;
	bool forceOverwrite = false;
	bool formatStrings = false;
	bool gotoIsKeyword = false;
	bool ignoreDebugInfo = false;
	bool minimizeDiffs = false;
	bool numberUnderscores = false;
	bool shortFunctionExpression = false;
	bool showHelp = false;
	bool silentAssertions = false;
	bool sortTables = false;
	bool stringDumpCompatibility = false;
	uint32_t threadCount = 0;
	std::vector<std::wstring> inputPaths;
	std::wstring outputPath;
	std::wstring extensionFilter;
} arguments;

void print(const std::string& message, const bool& bright = false);
void print(const std::wstring& message, const bool& bright = false);
void print_progress_bar(const double& progress = 0, const double& total = 100);
void erase_progress_bar();
void assert(const bool& assertion, const std::wstring& message, const std::wstring& function, const std::wstring& source, const uint32_t& line);
std::wstring byte_to_wstring(const uint8_t& byte);

class Bytecode;
class Ast;
class Lua;

#include "bytecode\bytecode.h"
#include "ast\ast.h"
#include "lua\lua.h"
