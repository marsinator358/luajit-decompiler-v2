#include <bit>
#include <cmath>
#include <cstdint>
#include <string>
#include <vector>
#include <windows.h>
#include <fileapi.h>

#define DEBUG_INFO __FUNCTION__, __FILE__, __LINE__

constexpr char PROGRAM_NAME[] = "LuaJIT Decompiler v2";

void print(const std::string& message);
std::string input();
void print_loading_bar(const double& progress = 0, const double& total = 100);
void erase_loading_bar();
void assert(const bool& assertion, const std::string& message, const std::string& filePath, const std::string& function, const std::string& source, const uint32_t& line);
std::string byte_to_string(const uint8_t& byte);

class Bytecode;
class Ast;
class Lua;

#include "bytecode\bytecode.h"
#include "ast\ast.h"
#include "lua\lua.h"
