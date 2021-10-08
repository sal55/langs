// Extra support functions for DISP module.
// These dx-function sare not direct do-handler functions; they are called when
// a particular handler becomes too complex, or needs to be split into different parts

#include "pcc_hdr.h"
#include "pcc_dxfns.cl"

void  dx_callhost(int hostfn,int nparams) {		//DX_CALLHOST
// Called direct from dispatcher. Call host function <hostfn>
//  Nparams gives number of params on the stack, with :=128 added if this is a function
//  returning a value.
//  Host procs with N params:
//   N items have been pushed onto the stack.
//   Return with stack unchanged.
//   Subsequent opcodes will free the params and adjust the stack.
//  Host functions with N params:
//   A void value has been pushed on the stack to receive any result (passed as last param to
//   the host function).
//   After stack cleanup by subsequent opcodes, result remains on the stack

u32 fnaddr;

fnaddr=dllproctable[hostfn].address;

if (!fnaddr) {
	fnaddr=findhostfn(hostfn);
}

if (nparams & 0x80) {		//functions

//PRINTF("CALLHOST NPARAMS:=%d\n",nparams);

	switch (nparams&=0x7f) {
	break; case 0: 
		(( void(*)(varrec*) )fnaddr)(sptr);
	break; case 1: 
		((void(*)(varrec*,varrec*))fnaddr)(sptr,sptr+1);
	break; case 2: 
		(( void(*)(varrec*,varrec*,varrec*)  )fnaddr)(sptr,sptr+1,sptr+2);
	break; case 3: 
		(( void(*)(varrec*,varrec*,varrec*,varrec*)  )fnaddr)(sptr,sptr+1,sptr+2,sptr+3);
	break; case 4: 
		(( void(*)(varrec*,varrec*,varrec*,varrec*,varrec*)  )fnaddr)(sptr,sptr+1,sptr+2,sptr+3,sptr+4);
	break; default:
		pcerror("callhost/fn");
	}
} else {					//procs

	switch (nparams) {
	break; case 0: 
		(( void(*)(void)  )fnaddr)();
	break; case 1: 
		(( void(*)(varrec*)   )fnaddr)(sptr);
	break; case 2: 
		(( void(*)(varrec*,varrec*)   )fnaddr)(sptr,sptr+1);
	break; case 3: 
		(( void(*)(varrec*,varrec*,varrec*))fnaddr)(sptr,sptr+1,sptr+2);
	break; case 4: 
		(( void(*)(varrec*,varrec*,varrec*,varrec*)  )fnaddr)(sptr,sptr+1,sptr+2,sptr+3);
	break; default:
		pcerror("callhost/proc");
	}
}
}

int dx_mixed (varrec* x, varrec* y) {		//DX_MIXED
//Q! function dx_mixed (ref varrec x,y)int
// x,y are different types; try and coerce one to the dominant type of the other,
// if they are numeric. Then return this dominant type.
// Or return 0 if they cannot be coerced.
// Assumes that caller has already established that the types are different.

switch (x->tag) {
break; case ti32: 
	switch (y->tag) {
	break; case ti64: 
		x->dvalue=x->value;
		x->tag=ti64;
		return ti64;

	break; case tr64: 
		x->xvalue=x->value;
		x->tag=tr64;
		return tr64;

// when trefvar,trefpack then
//  y->tag:=ti32;
//  return ti32;

	break; case tvoid: 
		goto dxvoid;
	break; default:
		return 0;
	}

break; case ti64: 
	switch (y->tag) {
	break; case ti32: 
		y->dvalue=y->value;
		y->tag=ti64;
		return ti64;

	break; case tr64: 
		x->xvalue=x->dvalue;
		x->tag=tr64;
		return tr64;
	break; case tvoid: 
		goto dxvoid;

	break; default:
		return 0;
	}
break; case tr64: 
	switch (y->tag) {
	break; case ti32: 
		y->xvalue=y->value;
		y->tag=tr64;
		return tr64;

	break; case ti64: 
		y->xvalue=y->dvalue;
		y->tag=tr64;
		return tr64;
	break; case tvoid: 
		goto dxvoid;

	break; default:
		return 0;
	}

break; case tvoid: 
dxvoid:
	pcerror("dxmix/void");


//when trefvar then
// switch (y->tag)
// when ti32 then
//  y->tag:=trefvar;
//  return trefvar;
//
// elsesw
//  return 0;
// endsw

//when trefpack then
// switch (y->tag)
// when ti32 then
//  y->tag:=trefpack;
//  return trefpack;
//
// elsesw
//  return 0;
// endsw

break; default:
	if (y->tag==tvoid) {
		goto dxvoid;
	}
	return 0;
}

#if 0
switch(ttmap[x->tag][y->tag]) {
break; case tt_i32_i64: 
	x->dvalue=x->value;
	x->tag=ti64;
	return ti64;
break; case tt_i32_r64: 
	x->xvalue=x->value;
	x->tag=tr64;
	return tr64;
break; case tt_i64_i32: 
		y->dvalue=y->value;
		y->tag=ti64;
		return ti64;
break; case tt_i64_r64: 
	x->xvalue=x->dvalue;
	x->tag=tr64;
	return tr64;
break; case tt_r64_i32: 
	y->xvalue=y->value;
	y->tag=tr64;
	return tr64;
break; case tt_r64_i64: 
	y->xvalue=y->dvalue;
	y->tag=tr64;
	return tr64;
break; default:
	return 0;
}
#endif
}

void  dx_checkstack(void) {		//DX_CHECKSTACK
varrec* p;
p=sptr;

while (p<=&varstack[stacksize]) {
	if (p->tag==ti32 && p->copy==3) {
		ABORT("CHECK HAS TI32/COPY=3");
	}
	++p;
}
}

void  dx_popptrlist(varrec* p, varrec *q) {		//DX_POPPTRLIST
// p is a refvar pointing to a list (of lvalues).
// q shoudl be a refvar to a mult-value item (such as a list, record, range etc)
// Store each element in turn to successive destinations in the list.
// Excess elements are ignored; if there are too few, void is stored.

int i,nleft,nright;
varrec v;
varrec *pdata,*qdata;

nleft=p->length;
pdata=p->vptr;
v.tagx=tvoid;

switch (ttbasetype[q->tag]) {
break; case tlist: 
	nright=q->length;
dolist:
	qdata=q->vptr;
	FOR (i,1,nleft) {
		if (i<=nright) {
			pc_storeptr(pdata,qdata++,0);	//still some values on rhs
		} else {
			pc_storeptr(pdata,&v,0);		//fill with void
		}
		++pdata;  
	}

break; case trange: 			//expand to two ints
	FOR (i,1,nleft) {
		if (i<=2) {
			v.tagx=ti32;
			v.value=(i==1?q->value:q->value2);
			pc_storeptr(pdata,&v,0);
		} else {
			v.tagx=tvoid;
			pc_storeptr(pdata,&v,0);
		}
		++pdata;
	}

break; case trecord: 
	nright=ttlength[q->tag];
	goto dolist;

break; case tcarray: 
	PCERROR("POPPTRLIST CARRAY");
break; case tarray: 
	PCERROR("POPPTRLIST ARRAY");

break; default:
	pcustype("popptrlist",q->tag);
}

}

void  dx_pushixref_li(varrec* a,int b,varrec* c) {		//DX_PUSHIXREF_LI
// a points to var containining a list (a will have been a refvar target in the caller)
// b is the index;
// Construct pointer to list element (will be a refvar) and store to c.
// Note that b and c can point to the same stack element.

int i;
byte* pp;

int index,bytes,newlen;
int newbytes,oldbytes;
varrec* newptr;

//CPL "DXIXREF1"
//testfreelist("DX1");
index=b-a->lower;			//0-based index

if ((unsigned)index<(unsigned)(a->length)) {	//within current bounds
//CPL "DXIXREF2"
//PRINTF("INSIDE BOUNDS %X %X\n",a,a->vptr);

	c->tagx=trefvar;			//construct refvar to the element
	c->vptr=a->vptr+index;
	return;
}

//testfreelist("DX2");
//CPL "DXIXREF3"
newlen=index+1;
bytes=newlen*varsize;			//total bytes needed to accommodate new index
if (bytes<=allocupper[a->alloc]) {	//Within current allocation
//CPL "DXIXREF4"
//PRINTF("INSIDE ALLOC %X %X\n",a,a->vptr);
	c->tagx=trefvar;			//construct refvar to the element
	c->vptr=a->vptr+index;
	a->length=newlen;		//new length
	a->copy=cc_owner;		//in case started as empty
	return;
}

/* need to extend list */
///PRINTF("EXTEND ALLOC %X %X\n",a,a->vptr);

oldbytes=allocupper[a->alloc];
newptr=pcm_alloc(newlen*varsize);
a->alloc=alloccode;
newbytes=allocupper[alloccode];

pcm_copymem4(newptr,a->vptr,oldbytes);
pcm_clearmem(newptr+oldbytes/varsize,newbytes-oldbytes);	//remaining extra vars to void
pcm_free(a->vptr,oldbytes);
a->vptr=newptr;
a->length=newlen;
a->copy=cc_owner;		//in case started as empty

c->tagx=trefvar;
c->vptr=a->vptr+index;
}

void  inline_n dx_syscall (void) {	//DO_SYSCALL		!DX_SYSCALL
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

sysfn=getopnda;
nparams=getopndb;

fresult=0;
if (nparams & 0x80) {
	nparams &=0x7f;
	fresult=1;
}

x=y=NULL;
switch (nparams) {
break; case 1:  x=sptr;
break; case 2:  x=sptr+1;
	    y=sptr;
}
vresult.tag=tvoid;

//PRINTF("SYSCALL: %s NPARAMS:=%d\n",syscallnames[sysfn],nparams);

switch(sysfn) {
break; case startprintfn: 
	pc_startprint(x);

break; case startprintconfn: 
	vresult.tagx=ti32;			//borrow vresult to set up 0 param
	vresult.value=0;
	pc_startprint(&vresult);
	vresult.tagx=tvoid;

break; case strstartprintfn: 
	pc_strstartprint();

break; case printfn: 
	pc_print(x,y);

break; case endprintfn: 
	pc_endprint();

break; case strendprintfn: 
	pc_strendprint(&vresult);

break; case printlnfn: 
	pc_println();

break; case printnogapfn: 
	pc_printnogap();

break; case setformatfn: 
	pc_setformat(x);

break; case readlnfn: 
	pc_readln(x);

break; case sreadfn: 
	pc_sread(x,&vresult);

break; case ismainfn: 
	pch_ismain(x,&vresult);

break; default:

	PRINTF("%s\n",syscallnames[sysfn]);
	pcerror("SYSCALL");
}

switch (nparams) {
break; case 1: 
	PC_CFREE(x);
	++sptr;
break; case 2: 
	PC_CFREE(x);
	PC_CFREE(y);
	sptr+=2;
}

if (fresult) {
	*(--sptr)=vresult;
}
}

void  dx_pushixref_packi(varrec* p,int index,varrec* dest) {		//DX_PUSHIXREF
//p is a refpack type pointing to a packed object
//return pointer to p^[index] in dest
//a might be a refpack, refbit, packslice, etc. but assume it is a user type
//This might include ref/slice usertypes, but ignore for now. When needed, use use ttbasetype
byte* ptr;
int atag;

ptr=p->ptr;
atag=p->elemtype;

switch (ttbasetype[atag]) {
break; case tarray: 
// index-:=ttlower[atag];
// if ((unsigned)index>=(unsigned)(ttlength[atag])) then
//  pcerror("ixref/rpack/ax bounds");
// fi
// dest->tagx:=trefpack;
// dest->elemtype:=ttelemtype[atag];
// dest->ptr:=ptr+index*ttsize[ttelemtype[atag]];
	pcerror("packi[sect]");
break; case tstring: 
	pcerror("packi[sect]");
break; case tbits: 
	pcerror("packi[sect]");
break; case ti32:case tu32: 
	if (index<0 || index>=32) {
		pcerror("packi-i32 bounds");
	}
doi32:
	dest->tagx=trefbit;
	dest->elemtype=tu1;
	dest->ptr=ptr+(index>>3);
	dest->bitslower=0;
	dest->bitoffset=index & 7;
break; case ti16:case tu16: 
	if (index<0 || index>=16) {
		pcerror("packi-i16 bounds");
	}
	goto doi32;
break; case ti64:case tu64: 
	pcerror("packi[sect]");
break; case tcarray: 
	pcerror("packi[sect]");
break; case tset: 
	pcerror("packi[sect]");
break; case tstruct: 
	pcerror("packi[sect]");
break; default:
	pcustype("packi[sect]",atag);
}
}

void  dx_pushixref_packn(varrec* p,int i,int j,varrec* dest) {		//DX_PUSHIXREF_PACKN
//a is a refpack type pointing to a packed object
//return pointer to slice a^[i..j] in dest
//a might be a refpack, refbit, packslice, etc. but assume it is a user type
byte* ptr;
int atag;

ptr=p->ptr;
atag=p->elemtype;

switch (ttbasetype[atag]) {
break; case tstring: 
	pcerror("packn[sect]");
break; case tarray: 
	pcerror("packn[sect]");
break; case tbits: 
	pcerror("packn[sect]");
break; case trange: 
	pcerror("packn[sect]");
break; case ti32: 
	pcerror("packn[sect]");
break; case ti64:case tu64: 
	pcerror("packn[sect]");
break; case tcarray: 
	pcerror("packn[sect]");
break; case tset: 
	pcerror("packn[sect]");
break; case tstruct: 
	pcerror("packn[sect]");
break; default:
	pcustype("packn[sect]",atag);
}
}

void  dx_pushixref_ai(varrec* a,int index,varrec* dest) {		//DX_PUSHIXREF_AI
// a points to var containining an array of T (a will have been a refvar target in the caller)
// Construct pointer to array element (will be a ref) and store to dest.

int bytes,newlen,etag,esize;
int newbytes,oldbytes;
byte* newptr;

index-=a->shortlower;			//0-based index
etag=a->elemtype;
esize=ttsize[etag];

if ((unsigned)index<(unsigned)(a->length)) {	//within current bounds
//PRINTF("INSIDE BOUNDS %X %X\n",a,a->vptr);

	dest->tagx=trefpack;				//construct ref to the element
	dest->elemtype=etag;
	dest->ptr=a->ptr+index*esize;
	return;
}

newlen=index+1;				//index is 0-based at this point
bytes=newlen*esize;				//total bytes needed to accommodate new index
if (bytes<=allocupper[a->alloc]) {		//Within current allocation
//PRINTF("INSIDE ALLOC %X %X\n",a,a->vptr);
	dest->tagx=trefpack;					//construct refvar to the element
	dest->ptr=a->ptr+index*esize;
	dest->elemtype=etag;
	a->length=newlen;				//new length
	return;
}

/* need to extend list */
//PRINTF("EXTEND ALLOC %X %X\n",a,a->vptr);
oldbytes=allocupper[a->alloc];
newptr=pcm_alloc(newlen*esize);
a->alloc=alloccode;
newbytes=allocupper[alloccode];
pcm_copymem4(newptr,a->vptr,oldbytes);
pcm_clearmem((byte*)newptr+oldbytes,newbytes-oldbytes);	//remaining extra vars to void
pcm_free(a->vptr,oldbytes);
a->ptr=newptr;
a->length=newlen;

dest->tagx=trefpack;
dest->ptr=a->ptr+index*esize;
dest->elemtype=etag;
}

void  dx_pushixref_bi(varrec* a,int index,varrec* dest) {		//DX_PUSHIXREF_BI
// a points to var containining a bitarray of tu1/2/4 (a will have been a refvar target in the caller)
// Construct pointer to array element (will be a refbit) and store to dest.

int bytes,newlen,etag,esize;
int newbytes,oldbytes;
byte* newptr;

index-=a->shortlower;			//0-based index
etag=a->elemtype;
esize=ttsize[etag];

if ((unsigned)index<(unsigned)(a->length)) {	//within current bounds
//PRINTF("INSIDE BOUNDS %X %X\n",a,a->vptr);

	dest->tagx=trefbit;				//construct ref to the element
	dest->elemtype=etag;
	switch (etag) {
	break; case tu1: 
		dest->ptr=a->ptr+(index>>3);
		dest->bitoffset=index&7;
	break; case tu4: 
		dest->ptr=a->ptr+(index>>1);
		dest->bitoffset=(index&1)*4;
	break; default:
		pcustype("ixref_bi",etag);
	}
	return;
}

PCERROR("IXREF/BIT OUT OF RANGE");

newlen=index+1;				//index is 0-based at this point
bytes=newlen*esize;				//total bytes needed to accommodate new index
if (bytes<=allocupper[a->alloc]) {		//Within current allocation
//PRINTF("INSIDE ALLOC %X %X\n",a,a->vptr);
	dest->tagx=trefpack;					//construct refvar to the element
	dest->ptr=a->ptr+index*esize;
	dest->elemtype=etag;
	a->length=newlen;				//new length
	return;
}

/* need to extend list */
//PRINTF("EXTEND ALLOC %X %X\n",a,a->vptr);
oldbytes=allocupper[a->alloc];
newptr=pcm_alloc(newlen*esize);
a->alloc=alloccode;
newbytes=allocupper[alloccode];
pcm_copymem4(newptr,a->vptr,oldbytes);
pcm_clearmem((byte*)newptr+oldbytes,newbytes-oldbytes);	//remaining extra vars to void
pcm_free(a->vptr,oldbytes);
a->ptr=newptr;
a->length=newlen;

dest->tagx=trefpack;
dest->ptr=a->ptr+index*esize;
dest->elemtype=etag;
}

int dx_varinvar(varrec* x,varrec* y) {		//DX_VARINVAR
//test whether x is in y; return index of x in y, or 0
//a,b are freed if necessary
int i,xt,yt,n;
varrec* p;

xt=x->tag;
yt=y->tag;

switch (xt) {
//when ti32,ti64 then
break; case ti32: 
	n=x->value;
	switch (yt) {
	break; case tset: 
doi64inset:
		if ((unsigned)n>=(unsigned)y->length) {	//out of bounds; so not in set
			PC_CFREE(y);
			return 0;
		}
		n=testelem(y->ptr,n);
		PC_CFREE(y);
		return n;
	break; case tlist: 
		n=y->length;
		p=y->vptr;
		FOR (i,1,n) {
			if (p->tag==ti32 && x->value==p->value) {
				PC_CFREE(y);
				return i;
			}
			++p;
		}
		PC_CFREE(y);
		return 0;
	break; case trange: 
		return n>=y->value && n<=y->value2;
	}

break; case tstring: 
	switch (yt) {
	break; case tstring: 
//PCERROR("DX STR IN STR");
		n=pc_strinstr(x,y);
		PC_CFREE(x);
		PC_CFREE(y);
		return n;
	break; case tlist: 
		n=y->length;
		p=y->vptr;
		FOR (i,1,n) {
			if (p->tag==tstring) {
				if (pc_eqstr(x,p)) {
					PC_CFREE(x);
					PC_CFREE(y);
					return i;
				} 
			}
			++p;
		}
		return 0;
	}
break; default:
	if (yt==tset && xt==ti64) {
		n=x->value;
		goto doi64inset;
	}

	switch(yt) {
	break; case tlist: 		//x can be anything
		n=y->length;
		p=y->vptr;
		FOR (i,1,n) {
//CPL "VIV X=%s P=%s N=%D\n",ttname[xt],ttname[p->tag],n
			if (pc_equal(x,p)==1) {
				PC_CFREE(x);
				PC_CFREE(y);
				return i;
			}
			++p;
		}
		PC_CFREE(x);
		PC_CFREE(y);
		return 0;
	}
}
pcmxtypes("varinvar:",xt,yt);
return 0;
}

void  dx_iorset(varrec* x,varrec* y,varrec* dest) {		//DX_IORSET
//x,y are on the stack, and usually dest coincides with x
//add/ior set x to y
int xlen,ylen;
//int n;
//int *p;

xlen=x->length;
ylen=y->length;

//CPL "DXIORSET XLEN:%D YLEN:%D\N",xlen,ylen

if (ylen==0) {		//x/[] + []; return X unchanged
} else if (xlen==0) {		//[] + y; set x to y
	*x=*y;			//y won't be used again; so x takes over copy bits
} else {				//x + y
	PC_CDUPL(x);			//make independent copy if needed (note: can be inefficient if resize needed anyway)
	pc_iresizeset(x,ylen);		//make sure x is at least as big as y
	pc_iorsetbits(x->iptr,y->iptr,ylen);
	PC_CFREE(y);
}
}

void  dx_subset(varrec* x,varrec* y,varrec* dest) {		//DX_SUBSET
//x,y are on the stack, and usually dest coincides with x
//subtract set y from set x
int xlen,ylen;

xlen=x->length;
ylen=y->length;

if (xlen && ylen) {		//x-y
	PC_CDUPL(x);			//make independent copy if needed (note: can be inefficient if resize needed anyway)
	pc_iresizeset(x,ylen);		//make sure x is at least as big as y
	pc_subsetbits(x->iptr,y->iptr,ylen);
}				//else: []-y, x-[], []-[] all have x as result, so leave unchanged
PC_CFREE(y);
}

void  dx_iandset(varrec* x,varrec* y,varrec* dest) {		//DX_IANDSET
//x,y are on the stack, and usually dest coincides with x
//subtract set y from set x
int xlen,ylen;

xlen=x->length;
ylen=y->length;

if (xlen && ylen) {		//x iand y
	PC_CDUPL(x);			//make independent copy if needed (note: can be inefficient if resize needed anyway)
	pc_iresizeset(x,ylen);		//make sure x is at least as big as y
	pc_iandsetbits(x->iptr,y->iptr,ylen);
	PC_CFREE(y);
} else {				//either or both is []: result is []
	if (xlen) {			//means y was []
		PC_CFREE(x);
		x->tagx=tset;			//lose .copy etc
		x->ptr=NULL;
		x->length=0;
	}
}
}

void  dx_ixorset(varrec* x,varrec* y,varrec* dest) {		//DX_IXORSET
//x,y are on the stack, and usually dest coincides with x
//ixor set x to y
int xlen,ylen;

xlen=x->length;
ylen=y->length;

if (ylen==0) {		//x/[] ixor []; return X unchanged
} else if (xlen==0) {		//[] + y; set x to y
	*x=*y;
} else {				//x + y
	PC_CDUPL(x);			//make independent copy if needed (note: can be inefficient if resize needed anyway)
	pc_iresizeset(x,ylen);		//make sure x is at least as big as y
	pc_ixorsetbits(x->iptr,y->iptr,ylen);
	PC_CFREE(y);
}
}

void  dx_calldll_c(int fnindex,int offset,int nparams,int restype,varrec* dest) {		//DX_CALLDLL_C
u32 fnaddr;
i64 retval64;
i32 retval32;
//CPL "DX/CALLDLL"

//CPL "CALLDLL/C FNINDEX=%D NPARAMS=%D RESTYPE=%S \N",fnindex,nparams, ttname[restype]
//PCERROR("CALLDLL/C");


fnaddr=dllproctable[fnindex].address;

if (!fnaddr) {
	puts( dllproctable[fnindex].name);
	pcerror("Calldll null fn:");
}

//CPL "CALLDLL RESTYPE=%s\n",ttname[restype]

switch (restype) {
break; case tvoid:case ti32:case tu32:case trefvar:case trefpack:case trefproc:case trefcstring:case ti64:case trange:case tu64: 
//CPL "NPARAMS %d\n",nparams
	switch (nparams) {
	break; case 0: 
		retval64=((i64(*)(void))fnaddr)
		();
	break; case 1: 
		retval64=((i64(*)(int))fnaddr)
		(dllparams[offset+1]);
	break; case 2: 
		retval64=((i64(*)(int,int))fnaddr)
		(dllparams[offset+1],dllparams[offset+2]);
	break; case 3: 
		retval64=((i64(*)(int,int,int))fnaddr)
		(dllparams[offset+1],dllparams[offset+2],dllparams[offset+3]);
	break; case 4: 
		retval64=((i64(*)(int,int,int,int))fnaddr)
		(dllparams[offset+1],dllparams[offset+2],dllparams[offset+3],dllparams[offset+4]);
	break; default:
		pcerror("calldll/i64 too many params");
	}
//CPL "RETURN1"

	if (restype!=tvoid) {		//when tvoid, no return value location provided
//CPL "RETURNED2 %x\n",dest
		dest->tagx=restype;
		dest->dvalue=retval64;
	}

break; case tr64: 				//tr64 return value
//CPL "NPARAMS %d\n",nparams
	switch (nparams) {
	break; case 0: 
		dest->xvalue=((r64(*)(void))fnaddr)
		();
	break; case 1: 
		dest->xvalue=((r64(*)(int))fnaddr)
		(dllparams[offset+1]);
	break; case 2: 
		dest->xvalue=((r64(*)(int,int))fnaddr)
		(dllparams[offset+1],dllparams[offset+2]);
// when 3 then
//  dest->xvalue:=((r64(*)(int,int,int))fnaddr)
//		(dllparams[offset+1],dllparams[offset+2],dllparams[offset+3]);
// when 4 then
//  dest->xvalue:=((r64(*)(int,int,int,int))fnaddr)
//		(dllparams[offset+1],dllparams[offset+2],dllparams[offset+3],dllparams[offset+4]);
	break; default:
		pcerror("calldll/r64 too many params");
	}

	if (restype!=tvoid) {		//when tvoid, no return value location provided
		dest->tagx=restype;
	}
break; default:					//assume 4: error category (not defined)
	PCERROR("CALLDLL RETTYPE?");
}

//CPL "RETURNED FROM DLL CALL"

}

int dx_carraylen(int ctag,int elemtag) {		//DX_CARRAYLEN
int n;

n=ttlength[ctag];
if (n) {
	return n;
}
switch (elemtag) {
break; case tu1:case tu2:case tu4: 
	return cxbits/ttbitwidth[elemtag];
break; default:
	return cxbytes/ttsize[elemtag];
}
}

void  dx_pushix_cxi(varrec* a,int index,varrec* dest) {		//DX_PUSHIX_CXI
//push carray[int] value

if (pc_cxisbits(a)) {
	pc_bitix((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,0,index-pc_cxlower(a),dest);
} else {
	pc_packix((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,index-pc_cxlower(a),dest);
}
}

void  dx_pushix_cxn(varrec* a,int i,int j,varrec* dest) {		//DX_PUSHIX_CXN
//push carray[range] slice
if (pc_cxisbits(a)) {
	pc_bitslice((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,0,i-pc_cxlower(a),j-pc_cxlower(a),sptr);
} else {
	pc_packslice((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,i-pc_cxlower(a),j-pc_cxlower(a),sptr);
}
}

void  dx_pushsect_cx(varrec* a,int doright,int n,varrec* dest) {		//DX_PUSHSECT_CX
//push carray[n:] or [:n] slice
int i,j,length;

length=pc_cxlength(a);
i=getsectslice(doright,n,length,&j);
if (pc_cxisbits(a)) {
	pc_bitslice((byte*)&(a->cdata),length,a->celemtype,0,i,j,sptr);
} else {
	pc_packslice((byte*)&(a->cdata),length,a->celemtype,i,j,sptr);
}
}

void  dx_pushixref_cxi(varrec* a,int index,varrec* dest) {		//DX_PUSHIXREF_CXI
if (pc_cxisbits(a)) {
	pc_refbitix((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,0,index-pc_cxlower(a),dest);
} else {
	pc_refpackix((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,index-pc_cxlower(a),dest);
}
}

void  dx_pushixref_cxn(varrec* a,int i,int j,varrec* dest) {		//DX_PUSHIXREF_CXN
if (pc_cxisbits(a)) {
	pc_refbitslice((byte*)&(a->cdata),pc_cxlength(a),a->celemtype,0, i-pc_cxlower(a),j-pc_cxlower(a),dest);
} else {
	pc_refpackslice((byte*)&(a->cdata),pc_cxlength(a),a->celemtype, i-pc_cxlower(a),j-pc_cxlower(a),dest);
}
}

varrec* dx_keyindict(varrec* dict,varrec* key){
//search for key in dict
//return var pointer to value data, or nil if not found (caller can convert to none)
varrec *p;
int i,n,keyvalue,slen;

p=dict->vptr;			//p points to key:value pairs
n=dict->length;		//n is number of pairs

switch (key->tag) {
break; case ti32: 			//fast scan for int keys
	keyvalue=key->value;

	TO (i,n) {
		if (p->tag==ti32 && keyvalue==p->value) {
			return p+1;
		}
		p+=2;
	}

break; case tstring: 
	slen=key->length;
	TO (i,n) {
//  if (p->tag=tstring && pc_eqstr(key,p)) then
		if (p->tag==tstring && p->length==slen) {
			if (strncmp(key->strptr,p->strptr,slen)==0) {
				return p+1;
			}
		}
		p+=2;
	}

break; default:				//general search

	TO(i,n) {
		if (pc_equal(p,key)) {	//match on key
			return p+1;			//return pointer to value
		}
		p+=2;				//next key
	}
}
return 0;			//not found
}
