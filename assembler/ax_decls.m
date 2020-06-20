!MXA Assembler Global Decls

global const compilerversion="2018.1.22"

!STREC usage::

!symbol=
! namesym			name, truename=""
! namedconstsym		name, value=(labeldef,value)
! labelsym			name, labdefined, value, segment, scope, lineno, stindex, offset

! kdirectivesym		// not done yet
! kopcodesym		name="mov" etc, subcode=m_mov etc
! kregsym			name="r0" etc, subcode=r0/etc, regsize=1/2/4/8
! kxregsym			name="xmm0" etc, subcode=r0/etc
! kfregsym			name="st0" etc, subcode=t0/etc
! kmregsym			name="mmx0" etc, subcode=r0/etc
! kjmpccsym			name="jz"/etc, subcode=z_cond/etc
! ksetccsym			name="setz"/etc, subcode=z_cond/etc
! kmovccsym			name="cmovz"/etc, subcode=z_cond/etc
! ksegnamesym,		name="code" etc, subcode=code_seg/etc

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end

global record opndrec = !24 bytes
	ref strec labeldef	!nil, or handle of strec for label
	union
		int64 value		!const value/extra offset/cond code/string for comments
		real64 xvalue	!floating point value
		ref char svalue
	end
	byte mode		!a_reg etc, low level operand details
	byte size		!byte size of operand: 1,2,4,8
	byte reg		!0, or main register
	byte regix		!0, or index register

	byte scale		!0, or scale factor for regix
	byte addrsize	!4 or 8 for a_mem when regs are involved
	byte valtype	!0 (no value or int) or 'R'/'S'
	byte spare2
end

global record strec =
	ichar name			!name of symbol (named token/keyword or identifier)
	ref fwdrec fwdrefs	!fwd ref chain
!	union
!		ref valuerec value	!named constants: valuerec([label],[value])
		ref opndrec expr	!named constants: valuerec([label],[value])
!		struct
			int32 offset		!label (pass 2): offset of label when encountered
			int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
			int32 importindex	!genexe: index into import table
!		end
!	end

	byte symbol			!type of token, eg. namesym
	byte ksymbol		!type of keyword, eg. opcodesym
	byte subcode		!when used as keyword
	byte regsize		!for reg keywords

	byte scope			!label pass 1: fwd/extern/local/global
	byte reftype		!label pass 2: extern/back/fwd
	byte segment		!label pass 2: code_seg etc or 0
	byte namelen

	ref strec basedef		!nil, or global/import def using this name
	ref strec nextdef		!in module name list
	ref strec nextdupl		!when part of in global import list

	int32 moduleno
!	word16 htindex				!index into hashtable
!	word16 htfirstindex			!initial index before stepping to avoid clashes
	word32 htindex				!index into hashtable
	word32 htfirstindex			!initial index before stepping to avoid clashes
	[48]BYTE SPARE
end

global record relocrec =			!informal version
	ref relocrec nextreloc
	int reloctype
	int offset
	int stindex
end

!record used for expanding buffers. Expansion is not automatic: buffercheck(n)
!is needed at strategic points to ensure that are at least n bytes left
global record dbuffer =
	ref byte pstart
	union
		ref byte pcurr
		ref word16 pcurr16
		ref word32 pcurr32
		ref word64 pcurr64
	end
	ref byte pend
	int alloc
end

global record modulerec =
	ichar filename
	ichar name
	ichar source
end

global record stlistrec =
	ref strec def
	ref stlistrec nextitem
end

global int lxfileno=0	!*@ current source file number
global int lxlineno=0	!*@ current source line number

global int nsourcefiles=0	!no. of linear file names

global const maxmodules=200
global const maxsearchlibs=30
global [maxmodules]modulerec moduletable
global [maxsearchlibs]ichar searchlibs
global int nmodules
global int nsearchlibs

global const hstsize=65536

global const hstmask=hstsize-1
global [0:hstsize]ref strec lexhashtable
global [0:hstsize]ref strec dupltable		!link dupl names

global ref void logdev		!dest for diagnostics and output of tables

global int fverbose=0		!whether to display message for each pass
global int fquiet=0

global int LINECOUNT=0

global int nundefined=0
global int alineno=0

global int ss_zdatalen
global ref dbuffer ss_zdata			!used for error checking only (should be empty at end)
global ref dbuffer ss_idata
global ref dbuffer ss_code
global ref relocrec ss_idatarelocs
global ref relocrec ss_coderelocs
global int ss_nidatarelocs
global int ss_ncoderelocs

global const init_ss_symbols=16384
global ref []ref strec ss_symboltable
global int ss_nsymbols
global int ss_symboltablesize

global ref stlistrec globalimportlist		!all global vars and imports across all moduls

global ref strec modulenamelist			!all defs defined in last module
global int currmoduleno

GLOBAL INT NMCLASM
GLOBAL INT NMCLOPNDSASM
