/*
Requirements:
  C++20
  Default char is unsigned
*/

#ifndef _CHAR_UNSIGNED
#error Default char is not unsigned!
#endif

#include <bit>
#include <cmath>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <algorithm>
#include <cstring>

// Define a macro to provide function name, file, and line number for debugging purposes
#define DEBUG_INFO __FUNCTION__, __FILE__, __LINE__

// Define program name and constants
const char PROGRAM_NAME[] = "LuaJIT Decompiler v2";
const uint64_t DOUBLE_SIGN = 0x8000000000000000;
const uint64_t DOUBLE_EXPONENT = 0x7FF0000000000000;
const uint64_t DOUBLE_FRACTION = 0x000FFFFFFFFFFFFF;
const uint64_t DOUBLE_SPECIAL = DOUBLE_EXPONENT;
const uint64_t DOUBLE_NEGATIVE_ZERO = DOUBLE_SIGN;

// Function declarations
void print(const std::string& message);
std::string input();
void print_progress_bar(const double& progress = 0, const double& total = 100);
void erase_progress_bar();
#ifdef assert
#undef assert
#endif
void assert(const bool& assertion, const std::string& message, const std::string& filePath, const std::string& function, const std::string& source, const uint32_t& line);
std::string byte_to_string(const uint8_t& byte);

// Forward declarations of classes
class Bytecode;
class Ast;
class Lua;

// Include necessary headers for the classes
#include "bytecode/bytecode.h"
#include "ast/ast.h"
#include "lua/lua.h"
