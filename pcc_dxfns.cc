! Extra support functions for DISP module.
! These dx-function sare not direct do-handler functions; they are called when
! a particular handler becomes too complex, or needs to be split into different parts

#include "pcc_hdr.h"
#include "pcc_dxfns.cl"

global proc dx_callhost(int hostfn,int nparams) =		!DX_CALLHOST
! Called direct from dispatcher. Call host function <hostfn>
!  Nparams gives number of params on the stack, with :=128 added if this is a function
!  returning a value.
!  Host procs with N params:
!   N items have been pushed onto the stack.
!   Return with stack unchanged.
!   Subsequent opcodes will free the params and adjust the stack.
!  Host functions with N params:
!   A void value has been pushed on the stack to receive any result (passed as last param to
!   the host function).
!   After stack cleanup by subsequent opcodes, result remains on the stack

u32 fnaddr;

fnaddr:=dllproctable[hostfn].address;

if (!fnaddr) then
	fnaddr:=findhostfn(hostfn);
fi

if (nparams & 0x80) then		!functions

//PRINTF("CALLHOST NPARAMS:=%d\n",nparams);

	switch (nparams&:=0x7f)
	when 0 then
		(( void(*)(varrec*) )fnaddr)(sptr);
	when 1 then
		((void(*)(varrec*,varrec*))fnaddr)(sptr,sptr+1);
	when 2 then
		(( void(*)(varrec*,varrec*,varrec*)  )fnaddr)(sptr,sptr+1,sptr+2);
	when 3 then
		(( void(*)(varrec*,varrec*,varrec*,varrec*)  )fnaddr)(sptr,sptr+1,sptr+2,sptr+3);
	when 4 then
		(( void(*)(varrec*,varrec*,varrec*,varrec*,varrec*)  )fnaddr)(sptr,sptr+1,sptr+2,sptr+3,sptr+4);
	elsesw
		pcerror("callhost/fn");
	endsw
else					!procs

	switch (nparams)
	when 0 then
		(( void(*)(void)  )fnaddr)();
	when 1 then
		(( void(*)(varrec*)   )fnaddr)(sptr);
	when 2 then
		(( void(*)(varrec*,varrec*)   )fnaddr)(sptr,sptr+1);
	when 3 then
		(( void(*)(varrec*,varrec*,varrec*))fnaddr)(sptr,sptr+1,sptr+2);
	when 4 then
		(( void(*)(varrec*,varrec*,varrec*,varrec*)  )fnaddr)(sptr,sptr+1,sptr+2,sptr+3);
	elsesw
		pcerror("callhost/proc");
	endsw
fi
end

global function int dx_mixed (varrec* x, varrec* y) =		!DX_MIXED
!Q! function dx_mixed (ref varrec x,y)int
! x,y are different types; try and coerce one to the dominant type of the other,
! if they are numeric. Then return this dominant type.
! Or return 0 if they cannot be coerced.
! Assumes that caller has already established that the types are different.

switch (x->tag)
when ti32 then
	switch (y->tag)
	when ti64 then
		x->dvalue:=x->value;
		x->tag:=ti64;
		return ti64;

	when tr64 then
		x->xvalue:=x->value;
		x->tag:=tr64;
		return tr64;

! when trefvar,trefpack then
!  y->tag:=ti32;
!  return ti32;

	when tvoid then
		goto dxvoid;
	elsesw
		return 0;
	endsw

when ti64 then
	switch (y->tag)
	when ti32 then
		y->dvalue:=y->value;
		y->tag:=ti64;
		return ti64;

	when tr64 then
		x->xvalue:=x->dvalue;
		x->tag:=tr64;
		return tr64;
	when tvoid then
		goto dxvoid;

	elsesw
		return 0;
	endsw
when tr64 then
	switch (y->tag)
	when ti32 then
		y->xvalue:=y->value;
		y->tag:=tr64;
		return tr64;

	when ti64 then
		y->xvalue:=y->dvalue;
		y->tag:=tr64;
		return tr64;
	when tvoid then
		goto dxvoid;

	elsesw
		return 0;
	endsw

when tvoid then
dxvoid:
	pcerror("dxmix/void");


!when trefvar then
! switch (y->tag)
! when ti32 then
!  y->tag:=trefvar;
!  return trefvar;
!
! elsesw
!  return 0;
! endsw

!when trefpack then
! switch (y->tag)
! when ti32 then
!  y->tag:=trefpack;
!  return trefpack;
!
! elsesw
!  return 0;
! endsw

elsesw
	if (y->tag=tvoid) then
		goto dxvoid;
	fi
	return 0;
endsw

#if 0
switch(ttmap[x->tag][y->tag])
when tt_i32_i64 then
	x->dvalue:=x->value;
	x->tag:=ti64;
	return ti64;
when tt_i32_r64 then
	x->xvalue:=x->value;
	x->tag:=tr64;
	return tr64;
when tt_i64_i32 then
		y->dvalue:=y->value;
		y->tag:=ti64;
		return ti64;
when tt_i64_r64 then
	x->xvalue:=x->dvalue;
	x->tag:=tr64;
	return tr64;
when tt_r64_i32 then
	y->xvalue:=y->value;
	y->tag:=tr64;
	return tr64;
when tt_r64_i64 then
	y->xvalue:=y->dvalue;
	y->tag:=tr64;
	return tr64;
elsesw
	return 0;
endsw
#endif
end

global proc dx_checkstack(void) =		!DX_CHECKSTACK
varrec* p;
p:=sptr;

while (p<=&varstack[stacksize]) do
	if (p->tag=ti32 && p->copy=3) then
		ABORT("CHECK HAS TI32/COPY:=3");
	fi
	++p;
od
end

global proc dx_popptrlist(varrec* p, varrec *q) =		!DX_POPPTRLIST
! p is a refvar pointing to a list (of lvalues).
! q shoudl be a refvar to a mult-value item (such as a list, record, range etc)
! Store each element in turn to successive destinations in the list.
! Excess elements are ignored; if there are too few, void is stored.

int i,nleft,nright;
varrec v;
varrec *pdata,*qdata;

nleft:=p->length;
pdata:=p->vptr;
v.tagx:=tvoid;

switch (ttbasetype[q->tag])
when tlist then
	nright:=q->length;
dolist:
	qdata:=q->vptr;
	FOR (i,1,nleft) do
		if (i<=nright) then
			pc_storeptr(pdata,qdata++,0);	!still some values on rhs
		else
			pc_storeptr(pdata,&v,0);		!fill with void
		fi
		++pdata;  
	od

when trange then			!expand to two ints
	FOR (i,1,nleft) do
		if (i<=2) then
			v.tagx:=ti32;
			v.value:=(i=1?q->value:q->value2);
			pc_storeptr(pdata,&v,0);
		else
			v.tagx:=tvoid;
			pc_storeptr(pdata,&v,0);
		fi
		++pdata;
	od

when trecord then
	nright:=ttlength[q->tag];
	goto dolist;

when tcarray then
	PCERROR("POPPTRLIST CARRAY");
when tarray then
	PCERROR("POPPTRLIST ARRAY");

elsesw
	pcustype("popptrlist",q->tag);
endsw

end

global proc dx_pushixref_li(varrec* a,int b,varrec* c) =		!DX_PUSHIXREF_LI
! a points to var containining a list (a will have been a refvar target in the caller)
! b is the index;
! Construct pointer to list element (will be a refvar) and store to c.
! Note that b and c can point to the same stack element.

int i;
byte* pp;

int index,bytes,newlen;
int newbytes,oldbytes;
varrec* newptr;

!CPL "DXIXREF1"
!testfreelist("DX1");
index:=b-a->lower;			!0-based index

if ((unsigned)index<(unsigned)(a->length)) then	!within current bounds
!CPL "DXIXREF2"
//PRINTF("INSIDE BOUNDS %X %X\n",a,a->vptr);

	c->tagx:=trefvar;			!construct refvar to the element
	c->vptr:=a->vptr+index;
	return;
fi

!testfreelist("DX2");
!CPL "DXIXREF3"
newlen:=index+1;
bytes:=newlen*varsize;			!total bytes needed to accommodate new index
if (bytes<=allocupper[a->alloc]) then	!Within current allocation
!CPL "DXIXREF4"
//PRINTF("INSIDE ALLOC %X %X\n",a,a->vptr);
	c->tagx:=trefvar;			!construct refvar to the element
	c->vptr:=a->vptr+index;
	a->length:=newlen;		!new length
	a->copy:=cc_owner;		!in case started as empty
	return;
fi

/* need to extend list */
!/PRINTF("EXTEND ALLOC %X %X\n",a,a->vptr);

oldbytes:=allocupper[a->alloc];
newptr:=pcm_alloc(newlen*varsize);
a->alloc:=alloccode;
newbytes:=allocupper[alloccode];

pcm_copymem4(newptr,a->vptr,oldbytes);
pcm_clearmem(newptr+oldbytes/varsize,newbytes-oldbytes);	!remaining extra vars to void
pcm_free(a->vptr,oldbytes);
a->vptr:=newptr;
a->length:=newlen;
a->copy:=cc_owner;		!in case started as empty

c->tagx:=trefvar;
c->vptr:=a->vptr+index;
end

global proc inline_n dx_syscall (void) =	!DO_SYSCALL		!DX_SYSCALL
/* A syscall calls an internal function of the host.
		There are 0,1 or 2 stacked params, which are variants.
		Some might have a return value.
		Usually these implement basic input and output (include file i/o, but they don't
		include file-handling functions).
		There is another category of functions called host functions:
		* Declared normally in the language, imported from a dynamic library "host" (syscalls are
				only directly accessible internally; a print statement may map to a serious of syscalls)
		* Any number of variant params, possible variant return value
*/
varrec *x,*y;
varrec vresult;
int sysfn,nparams,fresult;

sysfn:=getopnda;
nparams:=getopndb;

fresult:=0;
if (nparams & 0x80) then
	nparams &:=0x7f;
	fresult:=1;
fi

x:=y:=NULL;
switch (nparams)
when 1 then x:=sptr;
when 2 then x:=sptr+1;
	    y:=sptr;
endsw
vresult.tag:=tvoid;

//PRINTF("SYSCALL: %s NPARAMS:=%d\n",syscallnames[sysfn],nparams);

switch(sysfn)
when startprintfn then
	pc_startprint(x);

when startprintconfn then
	vresult.tagx:=ti32;			!borrow vresult to set up 0 param
	vresult.value:=0;
	pc_startprint(&vresult);
	vresult.tagx:=tvoid;

when strstartprintfn then
	pc_strstartprint();

when printfn then
	pc_print(x,y);

when endprintfn then
	pc_endprint();

when strendprintfn then
	pc_strendprint(&vresult);

when printlnfn then
	pc_println();

when printnogapfn then
	pc_printnogap();

when setformatfn then
	pc_setformat(x);

when readlnfn then
	pc_readln(x);

when sreadfn then
	pc_sread(x,&vresult);

when ismainfn then
	pch_ismain(x,&vresult);

elsesw

	PRINTF("%s\n",syscallnames[sysfn]);
	pcerror("SYSCALL");
endsw

switch (nparams)
when 1 then
	PC_CFREE(x);
	++sptr;
when 2 then
	PC_CFREE(x);
	PC_CFREE(y);
	sptr+:=2;
endsw

if (fresult) then
	*(--sptr):=vresult;
fi
end

global proc dx_pushixref_packi(varrec* p,int index,varrec* dest) =		!DX_PUSHIXREF
!p is a refpack type pointing to a packed object
!return pointer to p^[index] in dest
!a might be a refpack, refbit, packslice, etc. but assume it is a user type
!This might include ref/slice usertypes, but ignore for now. When needed, use use ttbasetype
byte* ptr;
int atag;

ptr:=p->ptr;
atag:=p->elemtype;

switch (ttbasetype[atag])
when tarray then
! index-:=ttlower[atag];
! if ((unsigned)index>=(unsigned)(ttlength[atag])) then
!  pcerror("ixref/rpack/ax bounds");
! fi
! dest->tagx:=trefpack;
! dest->elemtype:=ttelemtype[atag];
! dest->ptr:=ptr+index*ttsize[ttelemtype[atag]];
	pcerror("packi[sect]");
when tstring then
	pcerror("packi[sect]");
when tbits then
	pcerror("packi[sect]");
when ti32,tu32 then
	if (index<0 || index>=32) then
		pcerror("packi-i32 bounds");
	fi
doi32:
	dest->tagx:=trefbit;
	dest->elemtype:=tu1;
	dest->ptr:=ptr+(index>>3);
	dest->bitslower:=0;
	dest->bitoffset:=index & 7;
when ti16,tu16 then
	if (index<0 || index>=16) then
		pcerror("packi-i16 bounds");
	fi
	goto doi32;
when ti64,tu64 then
	pcerror("packi[sect]");
when tcarray then
	pcerror("packi[sect]");
when tset then
	pcerror("packi[sect]");
when tstruct then
	pcerror("packi[sect]");
elsesw
	pcustype("packi[sect]",atag);
endsw
end

global proc dx_pushixref_packn(varrec* p,int i,int j,varrec* dest) =		!DX_PUSHIXREF_PACKN
!a is a refpack type pointing to a packed object
!return pointer to slice a^[i..j] in dest
!a might be a refpack, refbit, packslice, etc. but assume it is a user type
byte* ptr;
int atag;

ptr:=p->ptr;
atag:=p->elemtype;

switch (ttbasetype[atag])
when tstring then
	pcerror("packn[sect]");
when tarray then
	pcerror("packn[sect]");
when tbits then
	pcerror("packn[sect]");
when trange then
	pcerror("packn[sect]");
when ti32 then
	pcerror("packn[sect]");
when ti64,tu64 then
	pcerror("packn[sect]");
when tcarray then
	pcerror("packn[sect]");
when tset then
	pcerror("packn[sect]");
when tstruct then
	pcerror("packn[sect]");
elsesw
	pcustype("packn[sect]",atag);
endsw
end

global proc dx_pushixref_ai(varrec* a,int index,varrec* dest) =		!DX_PUSHIXREF_AI
! a points to var containining an array of T (a will have been a refvar target in the caller)
! Construct pointer to array element (will be a ref) and store to dest.

int bytes,newlen,etag,esize;
int newbytes,oldbytes;
byte* newptr;

index-:=a->shortlower;			!0-based index
etag:=a->elemtype;
esize:=ttsize[etag];

if ((unsigned)index<(unsigned)(a->length)) then	!within current bounds
!PRINTF("INSIDE BOUNDS %X %X\n",a,a->vptr);

	dest->tagx:=trefpack;				!construct ref to the element
	dest->elemtype:=etag;
	dest->ptr:=a->ptr+index*esize;
	return;
fi

newlen:=index+1;				!index is 0-based at this point
bytes:=newlen*esize;				!total bytes needed to accommodate new index
if (bytes<=allocupper[a->alloc]) then		!Within current allocation
//PRINTF("INSIDE ALLOC %X %X\n",a,a->vptr);
	dest->tagx:=trefpack;					!construct refvar to the element
	dest->ptr:=a->ptr+index*esize;
	dest->elemtype:=etag;
	a->length:=newlen;				!new length
	return;
fi

/* need to extend list */
//PRINTF("EXTEND ALLOC %X %X\n",a,a->vptr);
oldbytes:=allocupper[a->alloc];
newptr:=pcm_alloc(newlen*esize);
a->alloc:=alloccode;
newbytes:=allocupper[alloccode];
pcm_copymem4(newptr,a->vptr,oldbytes);
pcm_clearmem((byte*)newptr+oldbytes,newbytes-oldbytes);	!remaining extra vars to void
pcm_free(a->vptr,oldbytes);
a->ptr:=newptr;
a->length:=newlen;

dest->tagx:=trefpack;
dest->ptr:=a->ptr+index*esize;
dest->elemtype:=etag;
end

global proc dx_pushixref_bi(varrec* a,int index,varrec* dest) =		!DX_PUSHIXREF_BI
! a points to var containining a bitarray of tu1/2/4 (a will have been a refvar target in the caller)
! Construct pointer to array element (will be a refbit) and store to dest.

int bytes,newlen,etag,esize;
int newbytes,oldbytes;
byte* newptr;

index-:=a->shortlower;			!0-based index
etag:=a->elemtype;
esize:=ttsize[etag];

if ((unsigned)index<(unsigned)(a->length)) then	!within current bounds
!PRINTF("INSIDE BOUNDS %X %X\n",a,a->vptr);

	dest->tagx:=trefbit;				!construct ref to the element
	dest->elemtype:=etag;
	switch (etag)
	when tu1 then
		dest->ptr:=a->ptr+(index>>3);
		dest->bitoffset:=index&7;
	when tu4 then
		dest->ptr:=a->ptr+(index>>1);
		dest->bitoffset:=(index&1)*4;
	elsesw
		pcustype("ixref_bi",etag);
	endsw
	return;
fi

PCERROR("IXREF/BIT OUT OF RANGE");

newlen:=index+1;				!index is 0-based at this point
bytes:=newlen*esize;				!total bytes needed to accommodate new index
if (bytes<=allocupper[a->alloc]) then		!Within current allocation
//PRINTF("INSIDE ALLOC %X %X\n",a,a->vptr);
	dest->tagx:=trefpack;					!construct refvar to the element
	dest->ptr:=a->ptr+index*esize;
	dest->elemtype:=etag;
	a->length:=newlen;				!new length
	return;
fi

/* need to extend list */
//PRINTF("EXTEND ALLOC %X %X\n",a,a->vptr);
oldbytes:=allocupper[a->alloc];
newptr:=pcm_alloc(newlen*esize);
a->alloc:=alloccode;
newbytes:=allocupper[alloccode];
pcm_copymem4(newptr,a->vptr,oldbytes);
pcm_clearmem((byte*)newptr+oldbytes,newbytes-oldbytes);	!remaining extra vars to void
pcm_free(a->vptr,oldbytes);
a->ptr:=newptr;
a->length:=newlen;

dest->tagx:=trefpack;
dest->ptr:=a->ptr+index*esize;
dest->elemtype:=etag;
end

global function int dx_varinvar(varrec* x,varrec* y) =		!DX_VARINVAR
!test whether x is in y; return index of x in y, or 0
!a,b are freed if necessary
int i,xt,yt,n;
varrec* p;

xt:=x->tag;
yt:=y->tag;

switch (xt)
!when ti32,ti64 then
when ti32 then
	n:=x->value;
	switch (yt)
	when tset then
doi64inset:
		if ((unsigned)n>=(unsigned)y->length) then	!out of bounds; so not in set
			PC_CFREE(y);
			return 0;
		fi
		n:=testelem(y->ptr,n);
		PC_CFREE(y);
		return n;
	when tlist then
		n:=y->length;
		p:=y->vptr;
		FOR (i,1,n) do
			if (p->tag=ti32 && x->value=p->value) then
				PC_CFREE(y);
				return i;
			fi
			++p;
		od
		PC_CFREE(y);
		return 0;
	when trange then
		return n>=y->value && n<=y->value2;
	endsw

when tstring then
	switch (yt)
	when tstring then
!PCERROR("DX STR IN STR");
		n:=pc_strinstr(x,y);
		PC_CFREE(x);
		PC_CFREE(y);
		return n;
	when tlist then
		n:=y->length;
		p:=y->vptr;
		FOR (i,1,n) do
			if (p->tag=tstring) then
				if (pc_eqstr(x,p)) then
					PC_CFREE(x);
					PC_CFREE(y);
					return i;
				fi 
			fi
			++p;
		od
		return 0;
	endsw
elsesw
	if (yt=tset && xt=ti64) then
		n:=x->value;
		goto doi64inset;
	fi

	switch(yt)
	when tlist then		!x can be anything
		n:=y->length;
		p:=y->vptr;
		FOR (i,1,n) do
!CPL "VIV X=%s P=%s N=%D\n",ttname[xt],ttname[p->tag],n
			if (pc_equal(x,p)=1) then
				PC_CFREE(x);
				PC_CFREE(y);
				return i;
			fi
			++p;
		od
		PC_CFREE(x);
		PC_CFREE(y);
		return 0;
	endsw
endsw
pcmxtypes("varinvar:",xt,yt);
return 0;
end

global proc dx_iorset(varrec* x,varrec* y,varrec* dest) =		!DX_IORSET
!x,y are on the stack, and usually dest coincides with x
!add/ior set x to y
int xlen,ylen;
!int n;
!int *p;

xlen:=x->length;
ylen:=y->length;

!CPL "DXIORSET XLEN:%D YLEN:%D\N",xlen,ylen

if (ylen=0) then		!x/[] + []; return X unchanged
elsif (xlen=0) then		![] + y; set x to y
	*x:=*y;			!y won't be used again; so x takes over copy bits
else				!x + y
	PC_CDUPL(x);			!make independent copy if needed (note: can be inefficient if resize needed anyway)
	pc_iresizeset(x,ylen);		!make sure x is at least as big as y
	pc_iorsetbits(x->iptr,y->iptr,ylen);
	PC_CFREE(y);
fi
end

global proc dx_subset(varrec* x,varrec* y,varrec* dest) =		!DX_SUBSET
!x,y are on the stack, and usually dest coincides with x
!subtract set y from set x
int xlen,ylen;

xlen:=x->length;
ylen:=y->length;

if (xlen && ylen) then		!x-y
	PC_CDUPL(x);			!make independent copy if needed (note: can be inefficient if resize needed anyway)
	pc_iresizeset(x,ylen);		!make sure x is at least as big as y
	pc_subsetbits(x->iptr,y->iptr,ylen);
fi				!else: []-y, x-[], []-[] all have x as result, so leave unchanged
PC_CFREE(y);
end

global proc dx_iandset(varrec* x,varrec* y,varrec* dest) =		!DX_IANDSET
!x,y are on the stack, and usually dest coincides with x
!subtract set y from set x
int xlen,ylen;

xlen:=x->length;
ylen:=y->length;

if (xlen && ylen) then		!x iand y
	PC_CDUPL(x);			!make independent copy if needed (note: can be inefficient if resize needed anyway)
	pc_iresizeset(x,ylen);		!make sure x is at least as big as y
	pc_iandsetbits(x->iptr,y->iptr,ylen);
	PC_CFREE(y);
else				!either or both is []: result is []
	if (xlen) then			!means y was []
		PC_CFREE(x);
		x->tagx:=tset;			!lose .copy etc
		x->ptr:=NULL;
		x->length:=0;
	fi
fi
end

global proc dx_ixorset(varrec* x,varrec* y,varrec* dest) =		!DX_IXORSET
!x,y are on the stack, and usually dest coincides with x
!ixor set x to y
int xlen,ylen;

xlen:=x->length;
ylen:=y->length;

if (ylen=0) then		!x/[] ixor []; return X unchanged
elsif (xlen=0) then		![] + y; set x to y
	*x:=*y;
else				!x + y
	PC_CDUPL(x);			!make independent copy if needed (note: can be inefficient if resize needed anyway)
	pc_iresizeset(x,ylen);		!make sure x is at least as big as y
	pc_ixorsetbits(x->iptr,y->iptr,ylen);
	PC_CFREE(y);
fi
end

global proc dx_calldll_c(int fnindex,int offset,int nparams,int restype,varrec* dest) =		!DX_CALLDLL_C
u32 fnaddr;
i64 retval64;
i32 retval32;
!CPL "DX/CALLDLL"

!CPL "CALLDLL/C FNINDEX=%D NPARAMS=%D RESTYPE=%S \N",fnindex,nparams, ttname[restype]
!PCERROR("CALLDLL/C");


fnaddr:=dllproctable[fnindex].address;

if (!fnaddr) then
	cpl dllproctable[fnindex].name
	pcerror("Calldll null fn:");
fi

!CPL "CALLDLL RESTYPE=%s\n",ttname[restype]

switch (restype)
when tvoid,ti32,tu32,trefvar,trefpack,trefproc,trefcstring,ti64,trange,tu64 then
!CPL "NPARAMS %d\n",nparams
	switch (nparams)
	when 0 then
		retval64:=((i64(*)(void))fnaddr)
		();
	when 1 then
		retval64:=((i64(*)(int))fnaddr)
		(dllparams[offset+1]);
	when 2 then
		retval64:=((i64(*)(int,int))fnaddr)
		(dllparams[offset+1],dllparams[offset+2]);
	when 3 then
		retval64:=((i64(*)(int,int,int))fnaddr)
		(dllparams[offset+1],dllparams[offset+2],dllparams[offset+3]);
	when 4 then
		retval64:=((i64(*)(int,int,int,int))fnaddr)
		(dllparams[offset+1],dllparams[offset+2],dllparams[offset+3],dllparams[offset+4]);
	elsesw
		pcerror("calldll/i64 too many params");
	endsw
!CPL "RETURN1"

	if (restype<>tvoid) then		!when tvoid, no return value location provided
!CPL "RETURNED2 %x\n",dest
		dest->tagx:=restype;
		dest->dvalue:=retval64;
	fi

when tr64 then				!tr64 return value
!CPL "NPARAMS %d\n",nparams
	switch (nparams)
	when 0 then
		dest->xvalue:=((r64(*)(void))fnaddr)
		();
	when 1 then
		dest->xvalue:=((r64(*)(int))fnaddr)
		(dllparams[offset+1]);
	when 2 then
		dest->xvalue:=((r64(*)(int,int))fnaddr)
		(dllparams[offset+1],dllparams[offset+2]);
! when 3 then
!  dest->xvalue:=((r64(*)(int,int,int))fnaddr)
!		(dllparams[offset+1],dllparams[offset+2],dllparams[offset+3]);
! when 4 then
!  dest->xvalue:=((r64(*)(int,int,int,int))fnaddr)
!		(dllparams[offset+1],dllparams[offset+2],dllparams[offset+3],dllparams[offset+4]);
	elsesw
		pcerror("calldll/r64 too many params");
	endsw

	if (restype<>tvoid) then		!when tvoid, no return value location provided
		dest->tagx:=restype;
	fi
elsesw					!assume 4: error category (not defined)
	PCERROR("CALLDLL RETTYPE?");
endsw

!CPL "RETURNED FROM DLL CALL"

end

global function int dx_carraylen(int ctag,int elemtag) =		!DX_CARRAYLEN
int n;

n:=ttlength[ctag];
if (n) then
	return n;
fi
switch (elemtag)
when tu1,tu2,tu4 then
	return cxbits/ttbitwidth[elemtag];
elsesw
	return cxbytes/ttsize[elemtag];
endsw
end

global proc dx_pushix_cxi(varrec* a,int index,varrec* dest) =		!DX_PUSHIX_CXI
!push carray[int] value

if (pc_cxisbits(a)) then
	pc_bitix((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,0,index-pc_cxlower(a),dest);
else
	pc_packix((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,index-pc_cxlower(a),dest);
fi
end

global proc dx_pushix_cxn(varrec* a,int i,int j,varrec* dest) =		!DX_PUSHIX_CXN
!push carray[range] slice
if (pc_cxisbits(a)) then
	pc_bitslice((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,0,i-pc_cxlower(a),j-pc_cxlower(a),sptr);
else
	pc_packslice((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,i-pc_cxlower(a),j-pc_cxlower(a),sptr);
fi
end

global proc dx_pushsect_cx(varrec* a,int doright,int n,varrec* dest) =		!DX_PUSHSECT_CX
!push carray[n:] or [:n] slice
int i,j,length;

length:=pc_cxlength(a);
i:=getsectslice(doright,n,length,&j);
if (pc_cxisbits(a)) then
	pc_bitslice((byte*)&(a->cdata),length,a->celemtype,0,i,j,sptr);
else
	pc_packslice((byte*)&(a->cdata),length,a->celemtype,i,j,sptr);
fi
end

global proc dx_pushixref_cxi(varrec* a,int index,varrec* dest) =		!DX_PUSHIXREF_CXI
if (pc_cxisbits(a)) then
	pc_refbitix((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,0,index-pc_cxlower(a),dest);
else
	pc_refpackix((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,index-pc_cxlower(a),dest);
fi
end

global proc dx_pushixref_cxn(varrec* a,int i,int j,varrec* dest) =		!DX_PUSHIXREF_CXN
if (pc_cxisbits(a)) then
	pc_refbitslice((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,0, i-pc_cxlower(a),j-pc_cxlower(a),dest);
else
	pc_refpackslice((byte*)&(a->cdata),pc_cxlength(a),a->celemtype, i-pc_cxlower(a),j-pc_cxlower(a),dest);
fi
end

global function varrec* dx_keyindict(varrec* dict,varrec* key)=
!search for key in dict
!return var pointer to value data, or nil if not found (caller can convert to none)
varrec *p;
int i,n,keyvalue,slen;

p:=dict->vptr;			!p points to key:value pairs
n:=dict->length;		!n is number of pairs

switch (key->tag)
when ti32 then			!fast scan for int keys
	keyvalue:=key->value;

	TO (i,n) do
		if (p->tag=ti32 && keyvalue=p->value) then
			return p+1;
		fi
		p+:=2;
	od

when tstring then
	slen:=key->length;
	TO (i,n) do
!  if (p->tag=tstring && pc_eqstr(key,p)) then
		if (p->tag=tstring && p->length=slen) then
			if (strncmp(key->strptr,p->strptr,slen)=0) then
				return p+1;
			fi
		fi
		p+:=2;
	od

else				!general search

	TO(i,n) do
		if (pc_equal(p,key)) then	!match on key
			return p+1;			!return pointer to value
		fi
		p+:=2;				!next key
	od
endsw
return 0;			!not found
end
