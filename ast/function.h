struct Local {
	std::vector<std::string> names;
	uint8_t baseSlot = 0;
	uint32_t scopeBegin = INVALID_ID;
	uint32_t scopeEnd = INVALID_ID;
	bool excludeBlock = false;
};

struct SlotScope {
	SlotScope* slotScope = this;
	std::vector<SlotScope**> mergedScopes;
	std::string name;
	uint32_t scopeBegin = INVALID_ID;
	uint32_t scopeEnd = INVALID_ID;
	uint32_t usages = 0;
};

struct Function {
	struct Upvalue {
		uint8_t slot = 0;
		SlotScope** slotScope = nullptr;
		bool local = false;
	};

	struct Label {
		std::string name;
		uint32_t target = INVALID_ID;
		std::vector<uint32_t> jumpIds;
	};

	Function(const Bytecode::Prototype& prototype, const uint32_t& level, const bool& ignoreDebugInfo)
		: prototype(prototype), isVariadic(prototype.header.flags& Bytecode::BC_PROTO_VARARG), level(level), hasDebugInfo(!ignoreDebugInfo && prototype.header.hasDebugInfo) {
		slotScopeCollector.slotInfos.resize(prototype.header.framesize);

		for (uint8_t i = prototype.header.parameters; i--;) {
			slotScopeCollector.slotInfos[i].isParameter = true;
			slotScopeCollector.slotInfos[i].activeSlotScope = slotScopeCollector.new_slot_scope();
		}

		slotScopeCollector.previousId = prototype.instructions.size();
	}

	~Function() {
		for (uint32_t i = slotScopeCollector.slotScopes.size(); i--;) {
			delete slotScopeCollector.slotScopes[i];
		}
	}

	const Bytecode::Constant& get_constant(const uint16_t& index) const {
		return prototype.constants[prototype.constants.size() - 1 - index];
	}

	const Bytecode::NumberConstant& get_number_constant(const uint16_t& index) const {
		return prototype.numberConstants[index];
	}

	void add_jump(const uint32_t& id, const uint32_t& target) {
		for (uint32_t i = 0; i < labels.size(); i++) {
			if (target > labels[i].target) continue;

			if (target != labels[i].target) {
				labels.emplace(labels.begin() + i);
				labels[i].target = target;
			}

			for (uint32_t j = 0; j < labels[i].jumpIds.size(); j++) {
				if (id > labels[i].jumpIds[j]) continue;
				if (id < labels[i].jumpIds[j]) labels[i].jumpIds.emplace(labels[i].jumpIds.begin() + j, id);
				return;
			}

			labels[i].jumpIds.emplace_back(id);
			return;
		}

		labels.emplace_back();
		labels.back().target = target;
		labels.back().jumpIds.emplace_back(id);
	}

	void remove_jump(const uint32_t& id, const uint32_t& target) {
		for (uint32_t i = labels.size(); i--;) {
			if (labels[i].target != target) continue;

			for (uint32_t j = labels[i].jumpIds.size(); j--;) {
				if (labels[i].jumpIds[j] != id) continue;
				labels[i].jumpIds.erase(labels[i].jumpIds.begin() + j);
				return;
			}
		}
	}

	uint32_t get_label_from_id(const uint32_t& id) {
		for (uint32_t i = labels.size(); i-- && labels[i].target >= id;) {
			if (labels[i].target != id) continue;
			return i;
		}

		return INVALID_ID;
	}

	bool is_valid_label(const uint32_t& label) {
		return label != INVALID_ID && labels[label].jumpIds.size();
	}

	uint32_t get_scope_begin_from_label(const uint32_t& label, const uint32_t& scopeEnd) {
		uint32_t scopeBegin = labels[label].target - 1;

		for (uint32_t i = label; i < labels.size() && labels[i].target <= scopeEnd; i++) {
			if (labels[i].jumpIds.size() && labels[i].jumpIds.front() <= scopeBegin) scopeBegin = labels[i].jumpIds.front() - 1;
		}

		return scopeBegin;
	}

	uint32_t get_scope_end_from_label(const uint32_t& label) {
		uint32_t scopeEnd = labels[label].target;

		for (uint32_t i = label; i < labels.size() && labels[i].target <= scopeEnd; i++) {
			if (labels[i].jumpIds.size() && labels[i].jumpIds.back() > scopeEnd) scopeEnd = labels[i].jumpIds.back();
		}

		return scopeEnd;
	}

	bool is_valid_block_range(const uint32_t& blockBegin, const uint32_t& blockEnd, const bool& ignoreFrontLabel) {
		for (uint32_t i = labels.size(); i-- && labels[i].target >= blockBegin;) {
			if (labels[i].target <= blockEnd
				&& labels[i].jumpIds.size()
				&& ((labels[i].jumpIds.front() < blockBegin
						&& (labels[i].target != blockBegin
							|| !ignoreFrontLabel))
					|| labels[i].jumpIds.back() > blockEnd))
				return false;
		}

		return true;
	}

	const Bytecode::Prototype& prototype;
	const bool isVariadic, hasDebugInfo;
	const uint32_t level;
	uint32_t id = 0;
	bool assignmentSlotIsUpvalue = false;
	std::vector<Local> locals;
	std::vector<Upvalue> upvalues;
	std::vector<Label> labels;
	std::vector<std::string> parameterNames;
	std::vector<Statement*> block;
	std::vector<Function*> childFunctions;
	std::vector<const std::string*> usedGlobals;

	struct SlotScopeCollector {
		struct UpvalueInfo {
			enum TYPE {
				JUMP,
				UPVALUES,
				UPVALUE_CLOSE
			} type;

			uint32_t id = INVALID_ID;
			uint32_t target = INVALID_ID;
			std::vector<uint8_t> upvalues;
			uint8_t baseSlot = 0;
		};

		struct UpvalueScope {
			uint8_t slot = 0;
			uint32_t minScopeBegin = INVALID_ID;
			uint32_t minScopeEnd = INVALID_ID;
		};

		struct SlotInfo {
			bool isParameter = false;
			SlotScope** activeSlotScope = nullptr;
			uint32_t minScopeBegin = INVALID_ID;
			std::vector<SlotScope**> slotScopes;
		};

		SlotScope** new_slot_scope() {
			return &slotScopes.emplace_back(new SlotScope)->slotScope;
		}

		uint32_t add_upvalue_info(const uint32_t& id, const UpvalueInfo::TYPE& type) {
			for (uint32_t i = 0; i < upvalueInfos.size(); i++) {
				if (id > upvalueInfos[i].id) continue;
				upvalueInfos.emplace(upvalueInfos.begin() + i);
				upvalueInfos[i].type = type;
				upvalueInfos[i].id = id;
				return i;
			}

			upvalueInfos.emplace_back();
			upvalueInfos.back().type = type;
			upvalueInfos.back().id = id;
			return upvalueInfos.size() - 1;
		}

		void add_upvalues(const uint32_t& id, std::vector<uint8_t>& upvalues) {
			upvalueInfos[add_upvalue_info(id, UpvalueInfo::UPVALUES)].upvalues = upvalues;
		}

		void add_jump(const uint32_t& id, const uint32_t& target) {
			upvalueInfos[add_upvalue_info(id, UpvalueInfo::JUMP)].target = target;
		}

		void add_upvalue_close(const uint32_t& id, const uint32_t& target, const uint8_t& baseSlot) {
			const uint32_t index = add_upvalue_info(id, UpvalueInfo::UPVALUE_CLOSE);
			upvalueInfos[index].target = target;
			upvalueInfos[index].baseSlot = baseSlot;
		}

		void add_loop(const uint32_t& id, const uint32_t& target) {
			uint32_t index = add_upvalue_info(id, UpvalueInfo::JUMP);
			upvalueInfos[index].target = target;

			do {
				index++;
			} while (index < upvalueInfos.size() && upvalueInfos[index].id < target);

			upvalueInfos.emplace(upvalueInfos.begin() + index);
			upvalueInfos[index].id = target - 1;
			upvalueInfos[index].target = id;
		}

		void add_upvalue_scope(const uint8_t& slot, const uint32_t& minScopeBegin, const uint32_t& minScopeEnd) {
			for (uint32_t i = 0; i < upvalueScopes.size(); i++) {
				if (minScopeEnd > upvalueScopes[i].minScopeEnd) continue;
				upvalueScopes.emplace(upvalueScopes.begin() + i);
				upvalueScopes[i].slot = slot;
				upvalueScopes[i].minScopeBegin = minScopeBegin;
				upvalueScopes[i].minScopeEnd = minScopeEnd;
				return;
			}

			upvalueScopes.emplace_back();
			upvalueScopes.back().slot = slot;
			upvalueScopes.back().minScopeBegin = minScopeBegin;
			upvalueScopes.back().minScopeEnd = minScopeEnd;
		}

		void build_upvalue_scopes() {
			uint32_t index, minScopeBegin, minScopeEnd;

			for (uint32_t i = upvalueInfos.size(); i--;) {
				if (upvalueInfos[i].type != UpvalueInfo::UPVALUES) continue;

				for (uint8_t j = upvalueInfos[i].upvalues.size(); j--;) {
					index = i;
					minScopeBegin = upvalueInfos[i].id;
					minScopeEnd = upvalueInfos[i].id;

					for (uint32_t k = i + 1; k < upvalueInfos.size(); k++) {
						switch (upvalueInfos[k].type) {
						case UpvalueInfo::UPVALUE_CLOSE:
							if (upvalueInfos[i].upvalues[j] >= upvalueInfos[k].baseSlot) {
								if (minScopeEnd > upvalueInfos[k].id) continue;
								minScopeEnd = upvalueInfos[k].id;
								break;
							}
						case UpvalueInfo::JUMP:
							if (minScopeEnd < upvalueInfos[k].target) {
								minScopeEnd = upvalueInfos[k].target;
							} else if (minScopeBegin >= upvalueInfos[k].target) {
								minScopeBegin = upvalueInfos[k].target - 1;

								for (uint32_t l = index; l-- && minScopeBegin < upvalueInfos[l].id; index = l) {
									switch (upvalueInfos[l].type) {
									case UpvalueInfo::UPVALUE_CLOSE:
										if (upvalueInfos[i].upvalues[j] >= upvalueInfos[l].baseSlot) continue;
									case UpvalueInfo::JUMP:
										if (minScopeEnd < upvalueInfos[l].target) {
											minScopeEnd = upvalueInfos[l].target;
										} else if (minScopeBegin >= upvalueInfos[l].target) {
											minScopeBegin = upvalueInfos[l].target - 1;
										}
									}
								}
							}
						default:
							continue;
						}

						break;
					}

					add_upvalue_scope(upvalueInfos[i].upvalues[j], minScopeBegin, minScopeEnd);
				}
			}
		}

		void begin_scope(const uint8_t& slot, const uint32_t& id) {
			if (slotInfos[slot].activeSlotScope) return;
			slotInfos[slot].slotScopes.emplace_back(new_slot_scope());
			slotInfos[slot].activeSlotScope = slotInfos[slot].slotScopes.back();
			(*slotInfos[slot].activeSlotScope)->scopeEnd = id;
		}

		void begin_upvalue_scopes(const uint32_t& id) {
			while (upvalueScopes.size() && upvalueScopes.back().minScopeEnd >= id) {
				if (upvalueScopes.back().minScopeBegin <= id) {
					begin_scope(upvalueScopes.back().slot, id);
					extend_scope(upvalueScopes.back().slot, upvalueScopes.back().minScopeBegin);
				}

				upvalueScopes.pop_back();
			}
		}

		void add_to_scope(const uint8_t& slot, SlotScope**& slotScope, const uint32_t& id) {
			begin_scope(slot, id);
			slotScope = slotInfos[slot].activeSlotScope;
			(*slotInfos[slot].activeSlotScope)->usages++;
		}

		void close_scope(const uint8_t& slot, SlotScope**& slotScope, const uint32_t& id) {
			if (slotInfos[slot].isParameter
				|| (slotInfos[slot].minScopeBegin != INVALID_ID
					&& slotInfos[slot].minScopeBegin < id))
				return add_to_scope(slot, slotScope, id);
			begin_scope(slot, id);
			slotScope = slotInfos[slot].activeSlotScope;
			(*slotInfos[slot].activeSlotScope)->scopeBegin = id;
			slotInfos[slot].activeSlotScope = nullptr;
			slotInfos[slot].minScopeBegin = INVALID_ID;
		}

		void extend_scope(const uint8_t& slot, const uint32_t& id) {
			if (!slotInfos[slot].isParameter
				&& slotInfos[slot].activeSlotScope
				&& (slotInfos[slot].minScopeBegin == INVALID_ID
					|| slotInfos[slot].minScopeBegin > id))
				slotInfos[slot].minScopeBegin = id;
		}

		void extend_scopes(const uint32_t& id) {
			for (uint8_t i = slotInfos.size(); i--;) {
				extend_scope(i, id);
			}
		}

		void merge_scopes(const uint32_t& id) {
			for (uint8_t i = slotInfos.size(); i--;) {
				if (slotInfos[i].isParameter || !slotInfos[i].activeSlotScope) continue;

				for (uint32_t j = slotInfos[i].slotScopes.size() - 1; j-- && (*slotInfos[i].slotScopes[j])->scopeBegin <= id;) {
					(*slotInfos[i].activeSlotScope)->scopeEnd = (*slotInfos[i].slotScopes[j])->scopeEnd;
					(*slotInfos[i].activeSlotScope)->usages += (*slotInfos[i].slotScopes[j])->usages + 1;

					for (uint32_t k = (*slotInfos[i].slotScopes[j])->mergedScopes.size(); k--;) {
						*(*slotInfos[i].slotScopes[j])->mergedScopes[k] = *slotInfos[i].activeSlotScope;
					}

					(*slotInfos[i].activeSlotScope)->mergedScopes.insert((*slotInfos[i].activeSlotScope)->mergedScopes.end(), (*slotInfos[i].slotScopes[j])->mergedScopes.begin(), (*slotInfos[i].slotScopes[j])->mergedScopes.end());
					(*slotInfos[i].activeSlotScope)->mergedScopes.emplace_back(&(*slotInfos[i].slotScopes[j])->slotScope);
					*slotInfos[i].slotScopes[j] = *slotInfos[i].activeSlotScope;
					slotInfos[i].slotScopes.erase(slotInfos[i].slotScopes.begin() + j);
				}

				if ((*slotInfos[i].activeSlotScope)->scopeEnd < id) (*slotInfos[i].activeSlotScope)->scopeEnd = id;
			}
		}

		bool assert_scopes_closed() {
			for (uint8_t i = slotInfos.size(); i--;) {
				if (!slotInfos[i].isParameter && slotInfos[i].activeSlotScope) return false;
			}

			return true;
		}

		void remove_scope(const uint8_t& slot, SlotScope** const& slotScope) {
			for (uint32_t i = slotInfos[slot].slotScopes.size(); i--;) {
				if (slotInfos[slot].slotScopes[i] != slotScope) continue;
				slotInfos[slot].slotScopes.erase(slotInfos[slot].slotScopes.begin() + i);
				return;
			}
		}

		std::vector<UpvalueInfo> upvalueInfos;
		std::vector<UpvalueScope> upvalueScopes;
		std::vector<SlotInfo> slotInfos;
		std::vector<SlotScope*> slotScopes;
		uint32_t previousId = INVALID_ID;
	} slotScopeCollector;
};
