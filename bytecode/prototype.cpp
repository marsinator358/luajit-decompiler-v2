#include "..\main.h"

Bytecode::Prototype::Prototype(const Bytecode& bytecode) : bytecode(bytecode) {}

Bytecode::Prototype::~Prototype() {}

void Bytecode::Prototype::operator()(std::vector<Prototype*>& unlinkedPrototypes) {
	read_header();
	read_instructions();
	read_upvalues();
	read_constants(unlinkedPrototypes);
	read_number_constants();
	read_debug_info();
	assert(prototypeBytes == bytecode.buffer.size(), L"Failed to read full prototype", DEBUG_INFO);
	unlinkedPrototypes.emplace_back(this);
}

void Bytecode::Prototype::read_header() {
	uint8_t flags = get_uint8();
	assert(!(flags & ~(BC_PROTO_CHILD | BC_PROTO_VARARG | (bytecode.header.ffi ? BC_PROTO_FFI : 0) | (bytecode.header.bitop ? BC_PROTO_BITOP : 0))), L"Prototype has invalid flags (" + byte_to_wstring(flags) + L")", DEBUG_INFO);
	header.child = flags & BC_PROTO_CHILD;
	header.vararg = flags & BC_PROTO_VARARG;
	header.ffi = flags & BC_PROTO_FFI;
	header.bitop = flags & BC_PROTO_BITOP;
	header.parameters = get_uint8();
	header.framesize = get_uint8();
	assert(header.parameters <= header.framesize, L"Prototype has invalid framesize", DEBUG_INFO);
	upvalues.resize(get_uint8());
	constants.resize(get_uleb128());
	numberConstants.resize(get_uleb128());
	instructions.resize(get_uleb128());
	assert(instructions.size(), L"Prototype has no instructions", DEBUG_INFO);
	if (bytecode.header.stripped || !get_uleb128()) return;
	header.debugInfo = true;
	header.firstLine = get_uleb128();
	header.lineCount = get_uleb128();
}

void Bytecode::Prototype::read_instructions() {
	for (size_t i = 0; i < instructions.size(); i++) {
		uint32_t instruction = get_uint32();

		if ((uint8_t)instruction < BC_OP_INVALID) {
			if (bytecode.header.version == BC_VERSION_1) {
				if ((uint8_t)instruction >= BC_OP_ISTYPE) instruction += 2;
				if ((uint8_t)instruction >= BC_OP_TGETR) instruction++;
				if ((uint8_t)instruction >= BC_OP_TSETR) instruction++;
			}

			if ((uint8_t)instruction >= BC_OP_BNOT && !bytecode.header.bitop) instruction += 7;
		}

		instructions[i].type = (BC_OP)instruction;
		assert(instructions[i].type < BC_OP_INVALID, L"Prototype has invalid instruction (" + byte_to_wstring(instruction) + L")", DEBUG_INFO);
#ifdef _DEBUG
		instructions[i].pc = i;
#endif
		instructions[i].a = (instruction & 0x0000FF00) >> 8;

		switch (instructions[i].type) {
		case BC_OP_ISTYPE:
		case BC_OP_ISNUM:
		case BC_OP_TGETR:
		case BC_OP_TSETR:
		case BC_OP_JFORI:
		case BC_OP_IFORL:
		case BC_OP_JFORL:
		case BC_OP_IITERL:
		case BC_OP_JITERL:
		case BC_OP_ILOOP:
		case BC_OP_JLOOP:
		case BC_OP_FUNCF:
		case BC_OP_IFUNCF:
		case BC_OP_JFUNCF:
		case BC_OP_FUNCV:
		case BC_OP_IFUNCV:
		case BC_OP_JFUNCV:
		case BC_OP_FUNCC:
		case BC_OP_FUNCCW:
			assert(false, L"Prototype has unsupported instruction (" + byte_to_wstring(instruction) + L")", DEBUG_INFO);
		case BC_OP_ADDVN:
		case BC_OP_SUBVN:
		case BC_OP_MULVN:
		case BC_OP_DIVVN:
		case BC_OP_MODVN:
		case BC_OP_ADDNV:
		case BC_OP_SUBNV:
		case BC_OP_MULNV:
		case BC_OP_DIVNV:
		case BC_OP_MODNV:
		case BC_OP_ADDVV:
		case BC_OP_SUBVV:
		case BC_OP_MULVV:
		case BC_OP_DIVVV:
		case BC_OP_MODVV:
		case BC_OP_POW:
		case BC_OP_CAT:
		case BC_OP_TGETV:
		case BC_OP_TGETS:
		case BC_OP_TGETB:
		case BC_OP_TSETV:
		case BC_OP_TSETS:
		case BC_OP_TSETB:
		case BC_OP_CALLM:
		case BC_OP_CALL:
		case BC_OP_ITERC:
		case BC_OP_ITERN:
		case BC_OP_VARG:
		case BC_OP_BAND:
		case BC_OP_BOR:
		case BC_OP_BXOR:
		case BC_OP_BSHL:
		case BC_OP_BSHR:
		case BC_OP_BSAR:
			instructions[i].b = (instruction & 0xFF000000) >> 24;
			instructions[i].c = (instruction & 0x00FF0000) >> 16;
			continue;
		default:
			instructions[i].d = (instruction & 0xFFFF0000) >> 16;
			continue;
		}
	}
}

void Bytecode::Prototype::read_upvalues() {
	for (size_t i = 0; i < upvalues.size(); i++) {
		uint16_t upvalue = get_uint16();
		assert(!((upvalue & 0xFF00) & ~(BC_UV_IMMUTABLE | BC_UV_LOCAL)), L"Upvalue has invalid flags (" + byte_to_wstring(upvalue >> 8) + L")", DEBUG_INFO);
		upvalues[i].immutable = upvalue & BC_UV_IMMUTABLE;
		upvalues[i].local = upvalue & BC_UV_LOCAL;
		upvalues[i].slot = upvalue;
	}
}

void Bytecode::Prototype::read_constants(std::vector<Prototype*>& unlinkedPrototypes) {
	for (size_t i = 0; i < constants.size(); i++) {
		constants[i].type = (BC_KGC)get_uleb128();

		switch (constants[i].type) {
		case BC_KGC_CHILD:
			assert(unlinkedPrototypes.size(), L"Failed to link child prototype", DEBUG_INFO);
			constants[i].prototype = unlinkedPrototypes.back();
			unlinkedPrototypes.pop_back();
			continue;
		case BC_KGC_TAB:
			constants[i].array.resize(get_uleb128());
			constants[i].table.resize(get_uleb128());

			for (size_t j = 0; j < constants[i].array.size(); j++) {
				constants[i].array[j] = get_table_constant();
			}

			for (size_t j = 0; j < constants[i].table.size(); j++) {
				constants[i].table[j].key = get_table_constant();
				assert(constants[i].table[j].key.type != BC_KTAB_NIL, L"Prototype constant table key is nil", DEBUG_INFO);
				constants[i].table[j].value = get_table_constant();
			}

			continue;
		case BC_KGC_COMPLEX:
			assert(!get_uleb128() && !get_uleb128(), L"Prototype has invalid complex cdata constant", DEBUG_INFO);
		case BC_KGC_I64:
		case BC_KGC_U64:
			constants[i].cdata = get_uleb128();
			constants[i].cdata |= (uint64_t)get_uleb128() << 32;
			continue;
		default:
			constants[i].string.resize(constants[i].type - BC_KGC_STR);
			constants[i].type = BC_KGC_STR;

			for (size_t j = 0; j < constants[i].string.size(); j++) {
				constants[i].string[j] = get_uint8();
			}

			continue;
		}
	}
}

void Bytecode::Prototype::read_number_constants() {
	for (size_t i = 0; i < numberConstants.size(); i++) {
		if (bytecode.buffer[prototypeBytes] & 0x01) {
			numberConstants[i].type = BC_KNUM_NUM;
			numberConstants[i].number = get_uleb128_33();
			numberConstants[i].number |= (uint64_t)get_uleb128() << 32;
		} else {
			numberConstants[i].type = BC_KNUM_INT;
			numberConstants[i].integer = get_uleb128_33();
		}
	}
}

void Bytecode::Prototype::read_debug_info() {
	if (!header.debugInfo) return;

	if (header.lineCount < 256) {
		for (size_t i = 0; i < instructions.size(); i++) {
			instructions[i].line = get_uint8();
		}
	} else if (header.lineCount < 65536) {
		for (size_t i = 0; i < instructions.size(); i++) {
			instructions[i].line = get_uint16();
		}
	} else {
		for (size_t i = 0; i < instructions.size(); i++) {
			instructions[i].line = get_uint32();
		}
	}

	for (size_t i = 0; i < upvalues.size(); i++) {
		upvalues[i].name = get_string();
	}

	uint32_t scopeOffset = 0;

	for (uint8_t byte = get_uint8(); byte; byte = get_uint8()) {
		debugInfo.emplace_back();

		if (byte >= BC_VAR_STR) {
			debugInfo.back().type = BC_VAR_STR;
			debugInfo.back().name += byte;
			debugInfo.back().name += get_string();
		} else {
			debugInfo.back().type = (BC_VAR)byte;
		}

		scopeOffset += get_uleb128();
		assert(scopeOffset != 1 && scopeOffset < instructions.size() + 1, L"Prototype has invalid debug info", DEBUG_INFO);

		if (!scopeOffset) {
			debugInfo.back().parameter = true;
			debugInfo.back().scopeEnd = get_uleb128();
			assert(debugInfo.size() <= header.parameters && debugInfo.back().scopeEnd >= 2, L"Prototype has invalid parameter debug info", DEBUG_INFO);
			debugInfo.back().scopeEnd -= 2;
		} else {
			debugInfo.back().scopeBegin = scopeOffset - 2;
			debugInfo.back().scopeEnd = debugInfo.back().scopeBegin + get_uleb128();
		}

		assert(debugInfo.back().scopeEnd < instructions.size(), L"Prototype debug info exceeds instruction count", DEBUG_INFO);
	}

	assert(!header.parameters || (debugInfo.size() >= header.parameters && debugInfo[header.parameters - 1].parameter), L"Prototype parameter count does not match with debug info", DEBUG_INFO);
}

Bytecode::TableConstant Bytecode::Prototype::get_table_constant() {
	TableConstant tableConstant;
	tableConstant.type = (BC_KTAB)get_uleb128();

	switch (tableConstant.type) {
	case BC_KTAB_NIL:
	case BC_KTAB_FALSE:
	case BC_KTAB_TRUE:
		break;
	case BC_KTAB_INT:
		tableConstant.integer = get_uleb128();
		break;
	case BC_KTAB_NUM:
		tableConstant.number = get_uleb128();
		tableConstant.number |= (uint64_t)get_uleb128() << 32;
		break;
	default:
		tableConstant.string.resize(tableConstant.type - BC_KGC_STR);
		tableConstant.type = BC_KTAB_STR;

		for (size_t i = 0; i < tableConstant.string.size(); i++) {
			tableConstant.string[i] = get_uint8();
		}

		break;
	}

	return tableConstant;
}

std::string Bytecode::Prototype::get_string() {
	std::string string;

	for (uint8_t byte = get_uint8(); byte; byte = get_uint8()) {
		string += byte;
	}

	return string;
}

uint32_t Bytecode::Prototype::get_uleb128() {
	uint32_t uleb128 = get_uint8();

	if (uleb128 >= 0x80) {
		uleb128 &= 0x7F;
		uint8_t byte, bitShift = 0;

		do {
			bitShift += 7;
			assert(bitShift <= 28, L"Invalid uleb128", DEBUG_INFO);
			byte = get_uint8();
			uleb128 |= (uint32_t)(byte & 0x7F) << bitShift;
		} while (byte >= 0x80);
	}

	return uleb128;
}

uint32_t Bytecode::Prototype::get_uleb128_33() {
	uint32_t uleb128_33 = get_uint8() >> 1;

	if (uleb128_33 >= 0x40) {
		uleb128_33 &= 0x3F;
		uint8_t byte;
		int8_t bitShift = -1;

		do {
			bitShift += 7;
			assert(bitShift <= 27, L"Invalid uleb128_33", DEBUG_INFO);
			byte = get_uint8();
			uleb128_33 |= (uint32_t)(byte & 0x7F) << bitShift;
		} while (byte >= 0x80);
	}

	return uleb128_33;
}

uint8_t Bytecode::Prototype::get_uint8() {
	return get_bytes<uint8_t>();
}

uint16_t Bytecode::Prototype::get_uint16() {
	return bytecode.header.bigEndian ? _byteswap_ushort(get_bytes<uint16_t>()) : get_bytes<uint16_t>();
}

uint32_t Bytecode::Prototype::get_uint32() {
	return bytecode.header.bigEndian ? _byteswap_ulong(get_bytes<uint32_t>()) : get_bytes<uint32_t>();
}

template<typename Type>
Type Bytecode::Prototype::get_bytes() {
	assert(prototypeBytes + sizeof(Type) <= bytecode.buffer.size(), L"Prototype exceeds size of buffer", DEBUG_INFO);
	Type* pointer = (Type*)&bytecode.buffer[prototypeBytes];
	prototypeBytes += sizeof(Type);
	return *pointer;
}
