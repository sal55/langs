!Selected declarations

global int ss_zdatalen
global ref dbuffer ss_zdata         !used for error checking only (should be empty at end)
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


global record strec =
    ichar name          !name of symbol (named token/keyword or identifier)
    ref fwdrec fwdrefs  !fwd ref chain
    ref opndrec expr    !named constants: valuerec([label],[value])
    int32 offset        !label (pass 2): offset of label when encountered
    int32 stindex       !label pass 2: 0, or 1-based index within coff symboltable
    int32 importindex   !genexe: index into import table
    int32 PADDING

    byte symbol         !type of token, eg. namesym
    byte ksymbol        !type of keyword, eg. opcodesym
    byte subcode        !when used as keyword
    byte regsize        !for reg keywords

    byte XXX            !label pass 1: fwd/extern/local/global
    byte reftype        !label pass 2: extern/back/fwd
    byte segment        !label pass 2: code_seg etc or 0
    byte namelen

    ref strec basedef       !nil, or global/import def using this name
    ref strec nextdef       !in module name list
    ref strec nextdupl      !when part of in global import list

    int32 moduleno
    word32 htindex              !index into hashtable
    word32 htfirstindex         !initial index before stepping to avoid clashes

    word32 impindex         !for mcx o/p: 0 or index into compact import/export table
    word32 expindex

    [36]BYTE PADDING2
end

global record relocrec =            !informal version
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

