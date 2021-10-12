import* pci_core

global type psymbol = ref pstrec
global type pcl = ref pclrec

global record fwdrec =
	ref fwdrec nextfwd
	int32 offset
	int16 reltype
	int16 seg
end

global record pstrec =
	ichar name			!name of symbol (named token/keyword or identifier)
	ref fwdrec fwdrefs	!fwd ref chain
	pcl pcldef			!points to pcl instruction that defines this name

	byte symbol			!type of token, eg. namesym
	byte ksymbol		!type of keyword, eg. opcodesym
	union
		byte subcode		!when used as keyword
		byte rtsindex	!proc names: rts index if it's an rts function
	end
	byte pclop			!pcl op used to define this: klocal etc

	byte scope			!label pass 1: fwd/extern/local/global
	byte reftype		!label pass 2: extern/back/fwd
	byte segment		!label pass 2: code_seg etc or 0
	byte namelen

	int32 offset		!label (pass 2): offset of label when encountered
	int32 stindex		!label pass 2: 0, or 1-based index within coff symboltable
	int32 labelno		!assigned global label when a proc/static
	int16 importindex	!genexe: index into import table

	word16 flags:(isdefined:1, isimported:1, addrof:1,
				isexported:1, isfloat:1, noreg:1, atvar:1,
				isthreaded:1, istruename:1, isrts:1)

	int16 nrefs
	byte reg
	[13]byte spare
end

global record pclrec =
	byte opndtype
	byte opcode
	byte flags:(isexported:1, isimported:1)
	byte mode				!t in tables

	int32 size

	union
		struct
			union
				int64 value
				real64 xvalue
				real32 xvalue32
				ichar svalue
				int tempno
				int labelno
				psymbol def
				ref void asmcode
			end
			union						!two 32-bit params used according to opcode
				struct
					int32 x				!common access to these 1/2 extra attribs
					int32 y
				end

				struct					! (x,y) pointer ops
					int32 scale			! scale factor for offset
					int32 extra			! extra constant byte offset, already scaled
				end
				struct					! (x,y) call/etc
					int32 nargs			! number of args
					int32 nvariadics	! 0, or arg # that is first variadic
				end
				struct					! (x,y) switch
					int32 minlab
					int32 maxlab
				end

				int32 oldmode			! (x) u in tables
				int32 stepx				! (x) always +ve fixed step size for forup/fordown; also INCR
				int32 truncmode			! (x) convert/truncate: truncated mode
				int32 align
				int32 extvariadics		! (x) for an extproc; 0 or start param of variadics
				int32 nret				! (x) setretmult: no. of return values

			end
		end
		int128 value128
	end
	u32 seqno
	u32 spare
end

global int optimflag

global [rtsnames.len]psymbol rtsproctable		!nil, or an rts proc has been defined in pcl code

global const maxpdlllib=50
global int npdllnametable
global [maxpdlllib]ichar pdllnametable
