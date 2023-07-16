import mlib
import clib
import oslib

import mm_decls
import mm_tables
import mm_support
import mm_lex
import mm_lib

int currlineno



global proc printmodelist(filehandle f)=		!PRINTMODELIST
end

global proc printst(filehandle f,ref strec p,int level=0)=	!PRINTST
end

proc printstrec(filehandle f,ref strec p,int level)=		!PRINTSTREC
end

global proc printstflat(filehandle f)=
end

global proc printcode(filehandle f,ichar caption)=
end

global proc printmodulecode(filehandle f,ref strec m)=
end

global proc printunit(ref unitrec p,int level=0,ichar prefix="*",filehandle dev=nil)=		!PRINTUNIT
end

proc printunitlist(filehandle dev,ref unitrec p,int level=0,ichar prefix="*")=		!PRINTUNIT
end

global proc printoverloads(filehandle f)=
end

global function writepclcode(ichar caption)ref strbuffer=
gs_str(dest,"hello")
dest
end

global proc showprojectinfo(filehandle dev)=
end

global proc showlogfile=
CPL "NO DIAGS MODULE"
end

global proc showast(ichar filename)=
end

global proc strpcl(pcl p)=
end

global function writeallpcl:ref strbuffer=
	nil
end

global proc showopndstack=
end

global function strpclstr(pcl p)ichar=
	"PCLSTR"
end

global function stropndstack(int indent=0)ichar=
nil
end

global proc printsymbol(ref tokenrec lp)=
end
