static constexpr uint16_t BC_OP_JMP_BIAS = 0x8000;

enum BC_OP {
	BC_OP_ISLT, // if A<VAR> < D<VAR> then JMP
	BC_OP_ISGE, // if not (A<VAR> < D<VAR>) then JMP
	BC_OP_ISLE, // if A<VAR> <= D<VAR> then JMP
	BC_OP_ISGT, // if not (A<VAR> <= D<VAR>) then JMP
	BC_OP_ISEQV, // if A<VAR> == D<VAR> then JMP
	BC_OP_ISNEV, // if A<VAR> ~= D<VAR> then JMP
	BC_OP_ISEQS, // if A<VAR> == D<STR> then JMP
	BC_OP_ISNES, // if A<VAR> ~= D<STR> then JMP
	BC_OP_ISEQN, // if A<VAR> == D<NUM> then JMP
	BC_OP_ISNEN, // if A<VAR> ~= D<NUM> then JMP
	BC_OP_ISEQP, // if A<VAR> == D<PRI> then JMP
	BC_OP_ISNEP, // if A<VAR> ~= D<PRI> then JMP
	BC_OP_ISTC, // if D<VAR> then A<DST> = D and JMP
	BC_OP_ISFC, // if not D<VAR> then A<DST> = D and JMP
	BC_OP_IST, // if D<VAR> then JMP
	BC_OP_ISF, // if not D<VAR> then JMP
	BC_OP_ISTYPE, // unsupported
	BC_OP_ISNUM, // unsupported
	BC_OP_MOV, // A<DST> = D<VAR>
	BC_OP_NOT, // A<DST> = not D<VAR>
	BC_OP_UNM, // A<DST> = -D<VAR>
	BC_OP_LEN, // A<DST> = #D<VAR>
	BC_OP_ADDVN, // A<DST> = B<VAR> + C<NUM>
	BC_OP_SUBVN, // A<DST> = B<VAR> - C<NUM>
	BC_OP_MULVN, // A<DST> = B<VAR> * C<NUM>
	BC_OP_DIVVN, // A<DST> = B<VAR> / C<NUM>
	BC_OP_MODVN, // A<DST> = B<VAR> % C<NUM>
	BC_OP_ADDNV, // A<DST> = C<NUM> + B<VAR>
	BC_OP_SUBNV, // A<DST> = C<NUM> - B<VAR>
	BC_OP_MULNV, // A<DST> = C<NUM> * B<VAR>
	BC_OP_DIVNV, // A<DST> = C<NUM> / B<VAR>
	BC_OP_MODNV, // A<DST> = C<NUM> % B<VAR>
	BC_OP_ADDVV, // A<DST> = B<VAR> + C<VAR>
	BC_OP_SUBVV, // A<DST> = B<VAR> - C<VAR>
	BC_OP_MULVV, // A<DST> = B<VAR> * C<VAR>
	BC_OP_DIVVV, // A<DST> = B<VAR> / C<VAR>
	BC_OP_MODVV, // A<DST> = B<VAR> % C<VAR>
	BC_OP_POW, // A<DST> = B<VAR> ^ C<VAR>
	BC_OP_CAT, // A<DST> = B<RBASE> .. B++ -> C<RBASE>
	BC_OP_KSTR, // A<DST> = D<STR>
	BC_OP_KCDATA, // A<DST> = D<CDATA>
	BC_OP_KSHORT, // A<DST> = D<LITS>
	BC_OP_KNUM, // A<DST> = D<NUM>
	BC_OP_KPRI, // A<DST> = D<PRI>
	BC_OP_KNIL, // A<BASE>, A++ -> D<BASE> = nil
	BC_OP_UGET, // A<DST> = D<UV>
	BC_OP_USETV, // A<UV> = D<VAR>
	BC_OP_USETS, // A<UV> = D<STR>
	BC_OP_USETN, // A<UV> = D<NUM>
	BC_OP_USETP, // A<UV> = D<PRI>
	BC_OP_UCLO, // upvalue close for A<RBASE>, A++ -> framesize; goto D<JUMP>
	BC_OP_FNEW, // A<DST> = D<FUNC>
	BC_OP_TNEW, // A<DST> = {}
	BC_OP_TDUP, // A<DST> = D<TAB>
	BC_OP_GGET, // A<DST> = _G.D<STR>
	BC_OP_GSET, // _G.D<STR> = A<VAR>
	BC_OP_TGETV, // A<DST> = B<VAR>[C<VAR>]
	BC_OP_TGETS, // A<DST> = B<VAR>[C<STR>]
	BC_OP_TGETB, // A<DST> = B<VAR>[C<LIT>]
	BC_OP_TGETR, // unsupported
	BC_OP_TSETV, // B<VAR>[C<VAR>] = A<VAR>
	BC_OP_TSETS, // B<VAR>[C<STR>] = A<VAR>
	BC_OP_TSETB, // B<VAR>[C<LIT>] = A<VAR>
	BC_OP_TSETM, // A-1<BASE>[D&0xFFFFFFFF<NUM>] <- A (<- multres)
	BC_OP_TSETR, // unsupported
	BC_OP_CALLM, // if B<LIT> == 0 then A<BASE> (<- multres) <- A(A+FR2?2:1, A++ -> for C<LIT>, A++ (<- multres)) else A, A++ -> for B-1 = A(A+FR2?2:1, A++ -> for C, A++ (<- multres))
	BC_OP_CALL, // if B<LIT> == 0 then A<BASE> (<- multres) <- A(A+FR2?2:1, A++ -> for C-1<LIT>) else A, A++ -> for B-1 = A(A+FR2?2:1, A++ -> for C-1)
	BC_OP_CALLMT, // return A<BASE>(A+FR2?2:1, A++ -> for D<LIT>, A++ (<- multres))
	BC_OP_CALLT, // return A<BASE>(A+FR2?2:1, A++ -> for D-1<LIT>)
	BC_OP_ITERC, // for A<BASE>, A++ -> for B-1<LIT> in A-3, A-2, A-1 do
	BC_OP_ITERN, // for A<BASE>, A++ -> for B-1<LIT> in A-3, A-2, A-1 do
	BC_OP_VARG, // if B<LIT> == 0 then A<BASE> (<- multres) <- ... else A, A++ -> for B-1 = ...
	BC_OP_ISNEXT, // goto ITERN at D<JUMP>
	BC_OP_RETM, // return A<BASE>, A++ -> for D<LIT>, A++ (<- multres)
	BC_OP_RET, // return A<RBASE>, A++ -> for D-1<LIT>
	BC_OP_RET0, // return
	BC_OP_RET1, // return A<RBASE>
	BC_OP_FORI, // for A+3<BASE> = A, A+1, A+2 do; exit at D<JUMP>
	BC_OP_JFORI, // unsupported
	BC_OP_FORL, // end of numeric for loop; start at D<JUMP>
	BC_OP_IFORL, // unsupported
	BC_OP_JFORL, // unsupported
	BC_OP_ITERL, // end of generic for loop; start at D<JUMP>
	BC_OP_IITERL, // unsupported
	BC_OP_JITERL, // unsupported
	BC_OP_LOOP, // if D<JUMP> == 32767 then goto loop else while/repeat loop; exit at D
	BC_OP_ILOOP, // unsupported
	BC_OP_JLOOP, // unsupported
	BC_OP_JMP, // goto D<JUMP> or if true then JMP or goto ITERC at D
	BC_OP_FUNCF, // unsupported
	BC_OP_IFUNCF, // unsupported
	BC_OP_JFUNCF, // unsupported
	BC_OP_FUNCV, // unsupported
	BC_OP_IFUNCV, // unsupported
	BC_OP_JFUNCV, // unsupported
	BC_OP_FUNCC, // unsupported
	BC_OP_FUNCCW, // unsupported
	BC_OP_INVALID
};

struct Instruction {
	BC_OP type;
	uint8_t a = 0;
	uint8_t b = 0;
	uint8_t c = 0;
	uint16_t d = 0;
};

static BC_OP get_op_type(const uint8_t& byte, const uint8_t& version) {
	return (BC_OP)(version == Bytecode::BC_VERSION_1 && byte >= BC_OP_ISTYPE ? (byte >= BC_OP_TGETR - 2 ? (byte >= BC_OP_TSETR - 3 ? byte + 4 : byte + 3) : byte + 2) : byte);
}

static bool is_op_abc_format(const BC_OP& instruction) {
	switch (instruction) {
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
	case BC_OP_TGETR:
	case BC_OP_TSETV:
	case BC_OP_TSETS:
	case BC_OP_TSETB:
	case BC_OP_TSETR:
	case BC_OP_CALLM:
	case BC_OP_CALL:
	case BC_OP_ITERC:
	case BC_OP_ITERN:
	case BC_OP_VARG:
		return true;
	}

	return false;
}
