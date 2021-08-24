#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #include <frbc2ci.h>

#define FR2C_STATUS_NO -1

#define FR2C_TY_I 0
#define FR2C_TY_L 1
#define FR2C_TY_F 2
#define FR2C_TY_D 3
#define FR2C_TY_P 4
#define FR2C_TY_NL 5
#define FR2C_TY_UNL 6

typedef unsigned char byte;

typedef unsigned short u16;
typedef signed short s16;
typedef unsigned int u32;
typedef signed int s32;
typedef unsigned long long u64;
typedef signed long long s64;

typedef float f32;
typedef double f64;

typedef s32 snlint;
typedef u32 unlint;
typedef u32 nlint;


typedef union FR2CI_Value_u FR2CI_Value;
typedef struct FR2CI_Context_s FR2CI_Context;

typedef struct FR2CI_Trace_s FR2CI_Trace;
typedef struct FR2CI_Opcode_s FR2CI_Opcode;
typedef struct FR2CI_TailOpcode_s FR2CI_TailOpcode;
typedef struct FR2CI_Frame_s FR2CI_Frame;

typedef struct FR2CI_ArgOpcode_s FR2CI_ArgOpcode;
typedef struct FR2CI_CleanupOpcode_s FR2CI_CleanupOpcode;
typedef struct FR2CI_CallInfo_s FR2CI_CallInfo;

typedef struct FR2CI_Image_s FR2CI_Image;

union FR2CI_Value_u {
s32 i;
s64 l;
u32 ui;
u64 ul;
f32 f;
f64 d;
void *p;
nlint pi;
unlint upi;
// fr2ci_variant va;
struct { int i, j; }ij;
};

struct FR2CI_Opcode_s {
void (*run)(FR2CI_Frame *frm, FR2CI_Opcode *op);
int d, s, t;
FR2CI_Value imm;
};

struct FR2CI_TailOpcode_s {
FR2CI_Trace *(*run)(FR2CI_Frame *frm, FR2CI_TailOpcode *op);
int d, s, t;
FR2CI_Value imm;
FR2CI_Trace *trace;
FR2CI_Trace *nexttrace;
FR2CI_Trace *jmptrace;
byte *jmpip;
};

struct FR2CI_Frame_s {
FR2CI_Value *reg;				//registers
FR2CI_Value *arg;				//arguments
FR2CI_Value *loc;				//locals
FR2CI_Trace *rettrace;			//return trace
FR2CI_Context *ctx;				//owning context
// FR2CI_CleanupOpcode *clop;		//cleanup opcode
FR2CI_CallInfo *cinf;			//caller's call-info
// FR2CI_Function *func;			//current function
// byte *artag;
int n_reg;
int n_arg;
int n_loc;
};

struct FR2CI_Context_s {
// FR2CI_Image *image[256];
// int n_image;
//FR2CI_LoaderCtx *loader;

// FR2CI_FrameSlab *slab_frame;
FR2CI_Frame *free_frame;

FR2CI_Frame *framestack[1024];
int framestackpos;
FR2CI_Frame *frame;

FR2CI_Trace *nexttrace;		//trace to begin execution
FR2CI_Value retval;
int status;
FR2CI_Trace *trap_trace;		//trace which has thrown
FR2CI_Opcode *trap_opcode;		//trap opcode
FR2CI_TailOpcode *trap_tailop;		//trap opcode

// FR2CI_CleanupOpcode *free_clop;

void *free_pool[64];

int stat_n_op;
int stat_n_tr;
};

struct FR2CI_CallInfo_s {
// FR2CI_Function *func;
FR2CI_ArgOpcode **oparg;
FR2CI_TailOpcode *vacallop;		//vararg call opcode
short *artag;					//argument type tags
int n_oparg;
int n_arg;
};

int FR2CI_CleanupFrame(FR2CI_Context *ctx, FR2CI_Frame *frm)
{
	return(0);
}

int FR2CI_FreeFrame(FR2CI_Context *ctx, FR2CI_Frame *frm)
{
	return(0);
}

#define OP_NI(OPN, YYN, SN, TN, OP, TY) \
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(FR2CI_Frame *frm, \
		FR2CI_TailOpcode *op) \
	{ return((frm->SN[op->s].TY OP frm->TN[op->t].TY)? \
		(op->jmptrace):(op->nexttrace)); }

#define OP_NATHDRSEQ	\
		FR2CI_Context *ctx;									\
		FR2CI_Frame *frm2;									\
		ctx=frm->ctx;										\
		fcn=op->imm.p;										\

#define OP_NATRETSEQ	\
		if(ctx->framestackpos>0)							\
		{													\
			ctx->framestackpos--;							\
			frm2=ctx->framestack[ctx->framestackpos];		\
			ctx->frame=frm2;								\
			FR2CI_CleanupFrame(ctx, frm);					\
			FR2CI_FreeFrame(ctx, frm);						\
			return(frm2->rettrace);						\
		}													\
		FR2CI_CleanupFrame(ctx, frm);						\
		FR2CI_FreeFrame(ctx, frm);							\
		ctx->frame=NULL;									\
		ctx->status=FR2C_STATUS_NO;							\
		return(NULL);										\

#define OP_NATCALL0(OPN, YYN, RTY, RTYT)					\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)();										\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn();								\
		OP_NATRETSEQ										\
	}														\

#define OP_NATCALL1(OPN, YYN, RTY, RTYT, ATY0, ATYT0)		\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)(ATYT0);									\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn(frm->arg[0].ATY0);				\
		OP_NATRETSEQ										\
	}														\

#define OP_NATCALL2(OPN, YYN, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1)										\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)(ATYT0, ATYT1);							\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn(frm->arg[0].ATY0,				\
			frm->arg[1].ATY1);								\
		OP_NATRETSEQ										\
	}														\

#define OP_NATCALL3(OPN, YYN, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2)							\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)(ATYT0, ATYT1, ATYT2);					\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn(frm->arg[0].ATY0,				\
			frm->arg[1].ATY1, frm->arg[2].ATY2);			\
		OP_NATRETSEQ										\
	}														\

#define OP_NATCALL4(OPN, YYN, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3)				\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)(ATYT0, ATYT1, ATYT2, ATYT3);			\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn(frm->arg[0].ATY0,				\
			frm->arg[1].ATY1, frm->arg[2].ATY2,				\
			frm->arg[3].ATY3);								\
		OP_NATRETSEQ										\
	}														\

#define OP_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4)	\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)(ATYT0, ATYT1, ATYT2, ATYT3, ATYT4);		\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn(frm->arg[0].ATY0,				\
			frm->arg[1].ATY1, frm->arg[2].ATY2,				\
			frm->arg[3].ATY3, frm->arg[4].ATY4);			\
		OP_NATRETSEQ										\
	}														\

#define OP_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4, \
		ATY5, ATYT5)										\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)(ATYT0, ATYT1, ATYT2, ATYT3, ATYT4, 		\
			ATYT5);											\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn(frm->arg[0].ATY0,				\
			frm->arg[1].ATY1, frm->arg[2].ATY2,				\
			frm->arg[3].ATY3, frm->arg[4].ATY4,				\
			frm->arg[5].ATY5);								\
		OP_NATRETSEQ										\
	}														\

#define OP_NATCALL7(OPN, YYN, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4, \
		ATY5, ATYT5, ATY6, ATYT6)							\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)(ATYT0, ATYT1, ATYT2, ATYT3, ATYT4, 		\
			ATYT5, ATYT6);									\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn(frm->arg[0].ATY0,				\
			frm->arg[1].ATY1, frm->arg[2].ATY2,				\
			frm->arg[3].ATY3, frm->arg[4].ATY4,				\
			frm->arg[5].ATY5, frm->arg[6].ATY6);			\
		OP_NATRETSEQ										\
	}														\

#define OP_NATCALL8(OPN, YYN, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4, \
		ATY5, ATYT5, ATY6, ATYT6, ATY7, ATYT7)				\
	FR2CI_Trace *FR2CI_Op_##OPN##_##YYN(					\
		FR2CI_Frame *frm, FR2CI_TailOpcode *op)				\
	{														\
		RTYT (*fcn)(ATYT0, ATYT1, ATYT2, ATYT3, ATYT4, 		\
			ATYT5, ATYT6, ATYT7);							\
		OP_NATHDRSEQ										\
		ctx->retval.RTY=fcn(frm->arg[0].ATY0,				\
			frm->arg[1].ATY1, frm->arg[2].ATY2,				\
			frm->arg[3].ATY3, frm->arg[4].ATY4,				\
			frm->arg[5].ATY5, frm->arg[6].ATY6,				\
			frm->arg[7].ATY7);								\
		OP_NATRETSEQ										\
	}														\

OP_NATCALL0(NatCall, I_V, i, int)
OP_NATCALL0(NatCall, L_V, l, long long)
OP_NATCALL0(NatCall, F_V, f, float)
OP_NATCALL0(NatCall, D_V, d, double)
OP_NATCALL0(NatCall, P_V, p, void *)

#define OPB_NATCALL1(OPN, YYN, RTY, RTYT)					\
	OP_NATCALL1(NatCall, YYN##I, RTY, RTYT, i, int)			\
	OP_NATCALL1(NatCall, YYN##L, RTY, RTYT, l, long long)	\
	OP_NATCALL1(NatCall, YYN##F, RTY, RTYT, f, float)		\
	OP_NATCALL1(NatCall, YYN##D, RTY, RTYT, d, double)		\
	OP_NATCALL1(NatCall, YYN##P, RTY, RTYT, p, void *)		\

OPB_NATCALL1(NatCall, I_, i, int)
OPB_NATCALL1(NatCall, L_, l, long long)
OPB_NATCALL1(NatCall, F_, f, float)
OPB_NATCALL1(NatCall, D_, d, double)
OPB_NATCALL1(NatCall, P_, p, void *)


#define OPB2_NATCALL2(OPN, YYN, RTY, RTYT, ATY0, ATYT0)		\
	OP_NATCALL2(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0, i, int)		\
	OP_NATCALL2(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0, l, long long)	\
	OP_NATCALL2(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0, f, float)		\
	OP_NATCALL2(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0, d, double)	\
	OP_NATCALL2(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0, p, void*)		\

#define OPB_NATCALL2(OPN, YYN, RTY, RTYT)					\
	OPB2_NATCALL2(NatCall, YYN##I, RTY, RTYT, i, int)			\
	OPB2_NATCALL2(NatCall, YYN##L, RTY, RTYT, l, long long)	\
	OPB2_NATCALL2(NatCall, YYN##F, RTY, RTYT, f, float)		\
	OPB2_NATCALL2(NatCall, YYN##D, RTY, RTYT, d, double)		\
	OPB2_NATCALL2(NatCall, YYN##P, RTY, RTYT, p, void*)		\

OPB_NATCALL2(NatCall, I_, i, int)
OPB_NATCALL2(NatCall, L_, l, long long)
OPB_NATCALL2(NatCall, F_, f, float)
OPB_NATCALL2(NatCall, D_, d, double)
OPB_NATCALL2(NatCall, P_, p, void*)


#define OPB3_NATCALL3(OPN, YYN, RTY, RTYT, ATY0, ATYT0, ATY1, ATYT1)	\
	OP_NATCALL3(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, i, int)									\
	OP_NATCALL3(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, l, long long)								\
	OP_NATCALL3(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, f, float)									\
	OP_NATCALL3(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, d, double)								\
	OP_NATCALL3(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, p, void*)									\

#define OPB2_NATCALL3(OPN, YYN, RTY, RTYT, ATY0, ATYT0)		\
	OPB3_NATCALL3(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0, i, int)		\
	OPB3_NATCALL3(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0, l, long long)	\
	OPB3_NATCALL3(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0, f, float)		\
	OPB3_NATCALL3(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0, d, double)	\
	OPB3_NATCALL3(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0, p, void*)		\

#define OPB_NATCALL3(OPN, YYN, RTY, RTYT)					\
	OPB2_NATCALL3(NatCall, YYN##I, RTY, RTYT, i, int)			\
	OPB2_NATCALL3(NatCall, YYN##L, RTY, RTYT, l, long long)	\
	OPB2_NATCALL3(NatCall, YYN##F, RTY, RTYT, f, float)		\
	OPB2_NATCALL3(NatCall, YYN##D, RTY, RTYT, d, double)		\
	OPB2_NATCALL3(NatCall, YYN##P, RTY, RTYT, p, void*)		\

OPB_NATCALL3(NatCall, I_, i, int)
OPB_NATCALL3(NatCall, L_, l, long long)
OPB_NATCALL3(NatCall, F_, f, float)
OPB_NATCALL3(NatCall, D_, d, double)
OPB_NATCALL3(NatCall, P_, p, void*)


#define OPB4_NATCALL4(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2)								\
	OP_NATCALL4(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, i, int)						\
	OP_NATCALL4(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, l, long long)				\
	OP_NATCALL4(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, f, float)						\
	OP_NATCALL4(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, d, double)					\
	OP_NATCALL4(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, p, void*)						\

#define OPB3_NATCALL4(OPN, YYN, RTY, RTYT, ATY0, ATYT0, ATY1, ATYT1)	\
	OPB4_NATCALL4(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, i, int)									\
	OPB4_NATCALL4(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, l, long long)								\
	OPB4_NATCALL4(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, f, float)									\
	OPB4_NATCALL4(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, d, double)								\
	OPB4_NATCALL4(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, p, void*)									\

#define OPB2_NATCALL4(OPN, YYN, RTY, RTYT, ATY0, ATYT0)		\
	OPB3_NATCALL4(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0, i, int)		\
	OPB3_NATCALL4(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0, l, long long)	\
	OPB3_NATCALL4(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0, f, float)		\
	OPB3_NATCALL4(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0, d, double)	\
	OPB3_NATCALL4(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0, p, void*)		\

#define OPB_NATCALL4(OPN, YYN, RTY, RTYT)						\
	OPB2_NATCALL4(NatCall, YYN##I, RTY, RTYT, i, int)			\
	OPB2_NATCALL4(NatCall, YYN##L, RTY, RTYT, l, long long)	\
	OPB2_NATCALL4(NatCall, YYN##F, RTY, RTYT, f, float)			\
	OPB2_NATCALL4(NatCall, YYN##D, RTY, RTYT, d, double)		\
	OPB2_NATCALL4(NatCall, YYN##P, RTY, RTYT, p, void*)			\

OPB_NATCALL4(NatCall, I_, i, int)
OPB_NATCALL4(NatCall, L_, l, long long)
OPB_NATCALL4(NatCall, F_, f, float)
OPB_NATCALL4(NatCall, D_, d, double)
OPB_NATCALL4(NatCall, P_, p, void*)


#if 0	//{(

#define OPB5_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3)					\
	OP_NATCALL5(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, i, int)			\
	OP_NATCALL5(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, l, long long)	\
	OP_NATCALL5(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, f, float)		\
	OP_NATCALL5(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, d, double)		\
	OP_NATCALL5(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, p, void*)		\

#define OPB4_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2)								\
	OPB5_NATCALL5(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, i, int)						\
	OPB5_NATCALL5(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, l, long long)				\
	OPB5_NATCALL5(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, f, float)						\
	OPB5_NATCALL5(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, d, double)					\
	OPB5_NATCALL5(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, p, void*)						\

#define OPB3_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0, ATY1, ATYT1)	\
	OPB4_NATCALL5(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, i, int)									\
	OPB4_NATCALL5(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, l, long long)								\
	OPB4_NATCALL5(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, f, float)									\
	OPB4_NATCALL5(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, d, double)								\
	OPB4_NATCALL5(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, p, void*)									\

#define OPB2_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0)			\
	OPB3_NATCALL5(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0, i, int)			\
	OPB3_NATCALL5(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0, l, long long)	\
	OPB3_NATCALL5(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0, f, float)		\
	OPB3_NATCALL5(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0, d, double)		\
	OPB3_NATCALL5(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0, p, void*)		\

#define OPB_NATCALL5(OPN, YYN, RTY, RTYT)						\
	OPB2_NATCALL5(NatCall, YYN##I, RTY, RTYT, i, int)			\
	OPB2_NATCALL5(NatCall, YYN##L, RTY, RTYT, l, long long)	\
	OPB2_NATCALL5(NatCall, YYN##F, RTY, RTYT, f, float)			\
	OPB2_NATCALL5(NatCall, YYN##D, RTY, RTYT, d, double)		\
	OPB2_NATCALL5(NatCall, YYN##P, RTY, RTYT, p, void*)			\

// OPB_NATCALL5(NatCall, I_, i, int)
// OPB_NATCALL5(NatCall, L_, l, long long)
// OPB_NATCALL5(NatCall, F_, f, float)
// OPB_NATCALL5(NatCall, D_, d, double)
// OPB_NATCALL5(NatCall, P_, p, void*)


#define OPB6_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4)		\
	OP_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4,		\
		i, int)													\
	OP_NATCALL6(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4,		\
		l, long long)											\
	OP_NATCALL6(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4,		\
		f, float)												\
	OP_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4,		\
		d, double)												\
	OP_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4,		\
		p, void*)												\

#define OPB5_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3)					\
	OPB6_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, i, int)			\
	OPB6_NATCALL6(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, l, long long)	\
	OPB6_NATCALL6(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, f, float)		\
	OPB6_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, d, double)		\
	OPB6_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, p, void*)		\

#define OPB4_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2)								\
	OPB5_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, i, int)						\
	OPB5_NATCALL6(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, l, long long)				\
	OPB5_NATCALL6(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, f, float)						\
	OPB5_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, d, double)					\
	OPB5_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, p, void*)						\

#define OPB3_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0, ATY1, ATYT1)	\
	OPB4_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, i, int)									\
	OPB4_NATCALL6(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, l, long long)								\
	OPB4_NATCALL6(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, f, float)									\
	OPB4_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, d, double)								\
	OPB4_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, p, void*)									\

#define OPB2_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0)			\
	OPB3_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0, i, int)			\
	OPB3_NATCALL6(NatCall, YYN##L, RTY, RTYT, ATY0, ATYT0, l, long long)	\
	OPB3_NATCALL6(NatCall, YYN##F, RTY, RTYT, ATY0, ATYT0, f, float)		\
	OPB3_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0, d, double)		\
	OPB3_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0, p, void*)		\

#define OPB_NATCALL6(OPN, YYN, RTY, RTYT)						\
	OPB2_NATCALL6(NatCall, YYN##I, RTY, RTYT, i, int)			\
	OPB2_NATCALL6(NatCall, YYN##L, RTY, RTYT, l, long long)	\
	OPB2_NATCALL6(NatCall, YYN##F, RTY, RTYT, f, float)			\
	OPB2_NATCALL6(NatCall, YYN##D, RTY, RTYT, d, double)		\
	OPB2_NATCALL6(NatCall, YYN##P, RTY, RTYT, p, void*)			\

// OPB_NATCALL6(NatCall, I_, i, int)
// OPB_NATCALL6(NatCall, L_, l, long long)
// OPB_NATCALL6(NatCall, F_, f, float)
// OPB_NATCALL6(NatCall, D_, d, double)
// OPB_NATCALL6(NatCall, P_, p, void*)

#endif	//)}


#if 1	//{(

#define OPB5_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3)					\
	OP_NATCALL5(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, i, int)			\
	OP_NATCALL5(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, d, double)		\
	OP_NATCALL5(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, p, void*)		\

#define OPB4_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2)								\
	OPB5_NATCALL5(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, i, int)						\
	OPB5_NATCALL5(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, d, double)					\
	OPB5_NATCALL5(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, p, void*)						\

#define OPB3_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0, ATY1, ATYT1)	\
	OPB4_NATCALL5(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, i, int)									\
	OPB4_NATCALL5(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, d, double)								\
	OPB4_NATCALL5(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, p, void*)									\

#define OPB2_NATCALL5(OPN, YYN, RTY, RTYT, ATY0, ATYT0)			\
	OPB3_NATCALL5(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0, i, int)			\
	OPB3_NATCALL5(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0, d, double)		\
	OPB3_NATCALL5(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0, p, void*)		\

#define OPB_NATCALL5(OPN, YYN, RTY, RTYT)						\
	OPB2_NATCALL5(NatCall, YYN##I, RTY, RTYT, i, int)			\
	OPB2_NATCALL5(NatCall, YYN##D, RTY, RTYT, d, double)		\
	OPB2_NATCALL5(NatCall, YYN##P, RTY, RTYT, p, void*)			\

OPB_NATCALL5(NatCall, I_, i, int)
OPB_NATCALL5(NatCall, L_, l, long long)
OPB_NATCALL5(NatCall, F_, f, float)
OPB_NATCALL5(NatCall, D_, d, double)
OPB_NATCALL5(NatCall, P_, p, void*)


#define OPB6_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4)		\
	OP_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4,		\
		i, int)													\
	OP_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4,		\
		d, double)												\
	OP_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, ATY4, ATYT4,		\
		p, void*)												\

#define OPB5_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3)					\
	OPB6_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, i, int)			\
	OPB6_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, d, double)		\
	OPB6_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, ATY3, ATYT3, p, void*)		\

#define OPB4_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0,			\
		ATY1, ATYT1, ATY2, ATYT2)								\
	OPB5_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, i, int)						\
	OPB5_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, d, double)					\
	OPB5_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, ATY2, ATYT2, p, void*)						\

#define OPB3_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0, ATY1, ATYT1)	\
	OPB4_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, i, int)									\
	OPB4_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, d, double)								\
	OPB4_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0,		\
		ATY1, ATYT1, p, void*)									\

#define OPB2_NATCALL6(OPN, YYN, RTY, RTYT, ATY0, ATYT0)			\
	OPB3_NATCALL6(NatCall, YYN##I, RTY, RTYT, ATY0, ATYT0, i, int)			\
	OPB3_NATCALL6(NatCall, YYN##D, RTY, RTYT, ATY0, ATYT0, d, double)		\
	OPB3_NATCALL6(NatCall, YYN##P, RTY, RTYT, ATY0, ATYT0, p, void*)		\

#define OPB_NATCALL6(OPN, YYN, RTY, RTYT)						\
	OPB2_NATCALL6(NatCall, YYN##I, RTY, RTYT, i, int)			\
	OPB2_NATCALL6(NatCall, YYN##D, RTY, RTYT, d, double)		\
	OPB2_NATCALL6(NatCall, YYN##P, RTY, RTYT, p, void*)			\

OPB_NATCALL6(NatCall, I_, i, int)
OPB_NATCALL6(NatCall, D_, d, double)
OPB_NATCALL6(NatCall, P_, p, void*)

#endif	//)}


#define OP_SWNI(OPN, YYN, IDX) \
	case IDX: tmp->run=FR2CI_Op_##OPN##_##YYN; break;

#define OPB0_SWNI(OPN, YYN)					\
	OP_SWNI(NatCall, I_V, 1*5+0)			\
	OP_SWNI(NatCall, L_V, 1*5+1)			\
	OP_SWNI(NatCall, F_V, 1*5+2)			\
	OP_SWNI(NatCall, D_V, 1*5+3)			\
	OP_SWNI(NatCall, P_V, 1*5+4)			\

#define OPB1_SWNI(OPN, YYN, IDX)			\
	OP_SWNI(NatCall, YYN##I, (IDX)*5+0)		\
	OP_SWNI(NatCall, YYN##L, (IDX)*5+1)		\
	OP_SWNI(NatCall, YYN##F, (IDX)*5+2)		\
	OP_SWNI(NatCall, YYN##D, (IDX)*5+3)		\
	OP_SWNI(NatCall, YYN##P, (IDX)*5+4)		\

#define OPB2_SWNI(OPN, YYN, IDX)			\
	OPB1_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB1_SWNI(NatCall, YYN##L, (IDX)*5+1)	\
	OPB1_SWNI(NatCall, YYN##F, (IDX)*5+2)	\
	OPB1_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB1_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB3_SWNI(OPN, YYN, IDX)			\
	OPB2_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB2_SWNI(NatCall, YYN##L, (IDX)*5+1)	\
	OPB2_SWNI(NatCall, YYN##F, (IDX)*5+2)	\
	OPB2_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB2_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB4_SWNI(OPN, YYN, IDX)			\
	OPB3_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB3_SWNI(NatCall, YYN##L, (IDX)*5+1)	\
	OPB3_SWNI(NatCall, YYN##F, (IDX)*5+2)	\
	OPB3_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB3_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#if 0	//{(

#define OPB5_SWNI(OPN, YYN, IDX)			\
	OPB4_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB4_SWNI(NatCall, YYN##L, (IDX)*5+1)	\
	OPB4_SWNI(NatCall, YYN##F, (IDX)*5+2)	\
	OPB4_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB4_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB6_SWNI(OPN, YYN, IDX)			\
	OPB5_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB5_SWNI(NatCall, YYN##L, (IDX)*5+1)	\
	OPB5_SWNI(NatCall, YYN##F, (IDX)*5+2)	\
	OPB5_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB5_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB7_SWNI(OPN, YYN, IDX)			\
	OPB6_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB6_SWNI(NatCall, YYN##L, (IDX)*5+1)	\
	OPB6_SWNI(NatCall, YYN##F, (IDX)*5+2)	\
	OPB6_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB6_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB8_SWNI(OPN, YYN, IDX)			\
	OPB7_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB7_SWNI(NatCall, YYN##L, (IDX)*5+1)	\
	OPB7_SWNI(NatCall, YYN##F, (IDX)*5+2)	\
	OPB7_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB7_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#endif	//)}

#if 1	//{(
#define OPB1IDP_SWNI(OPN, YYN, IDX)				\
	OP_SWNI(NatCall, YYN##I, (IDX)*5+0)			\
	OP_SWNI(NatCall, YYN##D, (IDX)*5+3)			\
	OP_SWNI(NatCall, YYN##P, (IDX)*5+4)			\

#define OPB2IDP_SWNI(OPN, YYN, IDX)				\
	OPB1IDP_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB1IDP_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB1IDP_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB3IDP_SWNI(OPN, YYN, IDX)				\
	OPB2IDP_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB2IDP_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB2IDP_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB4IDP_SWNI(OPN, YYN, IDX)				\
	OPB3IDP_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB3IDP_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB3IDP_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB5_SWNI(OPN, YYN, IDX)				\
	OPB4IDP_SWNI(NatCall, YYN##I, (IDX)*5+0)	\
	OPB4IDP_SWNI(NatCall, YYN##D, (IDX)*5+3)	\
	OPB4IDP_SWNI(NatCall, YYN##P, (IDX)*5+4)	\

#define OPB6_SWNI(OPN, YYN, IDX)				\
	OPB5_SWNI(NatCall, YYN##I, (IDX)*5+0)		\
	OPB5_SWNI(NatCall, YYN##D, (IDX)*5+3)		\
	OPB5_SWNI(NatCall, YYN##P, (IDX)*5+4)		\

#endif	//)}

//{(
#define OPB_SWNI(OPN)					\
	switch(idx) {						\
		OPB0_SWNI(OPN, _)				\
		OPB1_SWNI(OPN, I_, 1*5+0)		\
		OPB1_SWNI(OPN, L_, 1*5+1)		\
		OPB1_SWNI(OPN, F_, 1*5+2)		\
		OPB1_SWNI(OPN, D_, 1*5+3)		\
		OPB1_SWNI(OPN, P_, 1*5+4)		\
		OPB2_SWNI(OPN, I_, 1*5+0)		\
		OPB2_SWNI(OPN, L_, 1*5+1)		\
		OPB2_SWNI(OPN, F_, 1*5+2)		\
		OPB2_SWNI(OPN, D_, 1*5+3)		\
		OPB2_SWNI(OPN, P_, 1*5+4)		\
		OPB3_SWNI(OPN, I_, 1*5+0)		\
		OPB3_SWNI(OPN, L_, 1*5+1)		\
		OPB3_SWNI(OPN, F_, 1*5+2)		\
		OPB3_SWNI(OPN, D_, 1*5+3)		\
		OPB3_SWNI(OPN, P_, 1*5+4)		\
		OPB4_SWNI(OPN, I_, 1*5+0)		\
		OPB4_SWNI(OPN, L_, 1*5+1)		\
		OPB4_SWNI(OPN, F_, 1*5+2)		\
		OPB4_SWNI(OPN, D_, 1*5+3)		\
		OPB4_SWNI(OPN, P_, 1*5+4)		\
		OPB5_SWNI(OPN, I_, 1*5+0)		\
		OPB5_SWNI(OPN, D_, 1*5+3)		\
		OPB5_SWNI(OPN, P_, 1*5+4)		\
		OPB6_SWNI(OPN, I_, 1*5+0)		\
		OPB6_SWNI(OPN, D_, 1*5+3)		\
		OPB6_SWNI(OPN, P_, 1*5+4)		\
		}

//)}

//		OPB5_SWNI(OPN, I_, 1*5+0)		\
//		OPB5_SWNI(OPN, D_, 1*5+3)		\
//		OPB5_SWNI(OPN, P_, 1*5+4)		\
//										\
//		OPB6_SWNI(OPN, I_, 1*5+0)		\
//		OPB6_SWNI(OPN, D_, 1*5+3)		\
//		OPB6_SWNI(OPN, P_, 1*5+4)		\
//										\

char *FR2CI_ParseSigFindNext(
	FR2CI_Image *img, char *sig)
{
	char *s;
	s=sig;
	
	while(*s)
	{
		if(*s=='P')
			{ s++; continue; }
		if(*s=='W')
			{ s++; continue; }
		if(*s=='Q')
			{ s++; continue; }
		if((*s=='A') && (s[1]>='0') && (s[1]<='9'))
		{
			s++;
			while(*s)
			{
				if((*s>='0') && (*s<='9'))
					{ s++; continue; }
				if(*s==',')
					{ s++; continue; }
				break;
			}
			if(*s==';')s++;
			continue;
		}
		break;
	}
	
	if((*s=='X') || (*s=='L') || (*s=='U'))
	{
		s++;
		
		if((*s>='0') && (*s<='9'))
		{
			while(*s && (*s>='0') && (*s<='9'))
				s++;
			if(*s==';')s++;
			return(s);
		}

		while(*s && (*s!=';'))
			s++;
		if(*s==';')s++;
		return(s);
	}
	
	if((*s=='C') && (s[1]>='a') && (s[1]<='z'))
		{ s+=2; return(s); }
	if((*s=='D') && (s[1]>='a') && (s[1]<='z'))
		{ s+=2; return(s); }
	if((*s=='G') && (s[1]>='a') && (s[1]<='z'))
		{ s+=2; return(s); }

	if((*s>='a') && (*s<='z'))
		{ s++; return(s); }
	return(s);
}

int FR2CI_ParseSigBaseType5(
	FR2CI_Image *img, char *sig)
{
	int j;

	if(!sig || !(*sig))
		return(FR2C_TY_I);

	if((*sig=='P') || (*sig=='Q') ||
			(*sig=='L') || (*sig=='A'))
		return(FR2C_TY_P);

	switch(*sig)
	{
	case 'a':	case 'b':
	case 'c':	case 'h':
	case 's':	case 't':
	case 'i':	case 'j':
		return(FR2C_TY_I);
	case 'l':	case 'm':
		j=(sizeof(long)==4)?FR2C_TY_I:FR2C_TY_L;
		return(j);
	case 'x':	case 'y':
		return(FR2C_TY_L);
	case 'f':
		return(FR2C_TY_F);
	case 'd':
		return(FR2C_TY_D);
	default:
		return(FR2C_TY_I);
	}
}

int FR2CI_ParseSigIsVararg(
	FR2CI_Image *img, char *sig)
{
	char *s;
	int i;

	s=sig; i=0;
	if(*s=='(')s++;
	while(*s && (*s!=')'))
	{
		if(*s=='z')
			{ i=1; break; }
		if((s[0]=='C') && (s[1]=='z'))
			{ i=1; break; }
		s=FR2CI_ParseSigFindNext(img, s);
	}
	
	return(i);
}

#if 0
FR2CI_Trace *FR2CI_Op_Natcall_Vararg(
	FR2CI_Frame *frm, FR2CI_TailOpcode *op)
{
	FR2CI_TailOpcode *tmp;
	FR2CI_CallInfo *cinf;
	int i, j, k, idx, idx2;
	
	tmp=frm->cinf->vacallop;
	if(tmp)
	{
		if(op->t)
		{
			for(i=0; i<cinf->n_arg; i++)
			{
				j=cinf->artag[i]&15;
		
				if(j==FR2C_TY_F)
				{
					frm->arg[i].d=frm->arg[i].f;
//					j=FR2C_TY_D; op->t=1;
				}
			}
		}
		return(tmp->run(frm, op));
	}

	cinf=frm->cinf;
//	tmp=FR2CI_AllocTailOpcode(cinf->func->image);
	tmp->imm.p=op->imm.p;
	op->t=0;

	idx=0; idx2=1;
	for(i=0; i<cinf->n_arg; i++)
	{
		j=cinf->artag[i]&15;
		
		if(j==FR2C_TY_F)
		{
			j=FR2C_TY_D; op->t=1;
			frm->arg[i].d=frm->arg[i].f;
		}
		if(j>5)
		{
			switch(j)
			{
			case FR2C_TY_NL: case FR2C_TY_UNL:
				j=(sizeof(long)==4)?FR2C_TY_I:FR2C_TY_L;
				break;
				
			default:
				j=FR2C_TY_I; break;
			}
		}
		
		idx=(idx*5)+j;
		idx2=idx2*5;
	}
//	i=FR2CI_ParseSigBaseType5(img, s);
	j=FR2C_TY_I;
	idx=idx+(idx2*(1*5+j));

	OPB_SWNI(NatCall);

	cinf->vacallop=tmp;

	return(tmp->run(frm, op));
//	RTYT (*fcn)();
//	OP_NATHDRSEQ
//	ctx->retval.RTY=fcn();
//	OP_NATRETSEQ
}

FR2CI_TailOpcode *FR2CI_MakeOpNatCall(
	FR2CI_Image *img, void *fcn, char *sig)
{
	FR2CI_TailOpcode *tmp;
	char *s;
	int i, j, k, idx, idx2;

//	tmp=FR2CI_AllocTailOpcode(img);
//	tmp->s=s;
	tmp->imm.p=fcn;

	s=sig; idx=0; idx2=1;
	if(*s=='(')s++;
	while(*s && (*s!=')'))
	{
		i=FR2CI_ParseSigBaseType5(img, s);
		idx=(idx*5)+i;
		idx2=idx2*5;
		s=FR2CI_ParseSigFindNext(img, s);
	}
	if(*s==')')s++;
	i=FR2CI_ParseSigBaseType5(img, s);
//	idx=(idx*5)+i;
	idx=idx+(idx2*(1*5+i));

	OPB_SWNI(NatCall);

	if(FR2CI_ParseSigIsVararg(img, sig))
	{
		tmp->run=FR2CI_Op_Natcall_Vararg;
	}

	if(!tmp->run)
	{
//		FR2CI_FreeTailOpcode(img, tmp);
		return(NULL);
	}
	
	return(tmp);
}

FR2CI_Trace *FR2CI_GetFunctionNativeCall(
	FR2CI_Image *img, FR2CI_Function *func,
	char *name, char *sig)
{
	FR2CI_TailOpcode *top;
	FR2CI_Trace *trace;
	void *ptr;
	
	ptr=FR2CI_FFI_LookupLabelOS(name);
	if(!ptr)
		return(NULL);

	func->natfcn=ptr;
	
	top=FR2CI_MakeOpNatCall(img, ptr, sig);
	trace=FR2CI_CreateTrace(img, NULL, top, 0);

	func->ops=NULL;
	func->tops=fr2ci_malloc(1*sizeof(FR2CI_TailOpcode *));
	func->traces=fr2ci_malloc(1*sizeof(FR2CI_Trace *));
	func->n_ops=0;
	func->n_tops=1;
	func->n_traces=1;

	func->tops[0]=top;
	func->traces[0]=trace;
	func->entry=trace;

	return(trace);
}

void *FR2CI_FFI_LookupLabel(char *name)
{
	void *ptr;

	ptr=FR2CI_FFI_LookupLabelOS(name);
	return(ptr);
}

int FR2CI_FFI_LookupBlacklist(char *name)
{
	return(0);
}

void FR2CI_FFI_AddExeSection(char *name, s64 va, s64 sz, int fl)
{
}

void FR2CI_FFI_AddProxyPtr(char *name, void *ptr)
{
}

void *FR2CI_FFI_FetchSym(char *name)
{
	return(NULL);
}
#endif

int main()
{
}






