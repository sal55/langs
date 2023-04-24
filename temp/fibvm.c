//VM switch/stack-based interpreter framework, populated with just enough handlers to run my Fib benchmark
// in pccode[]+pcdata/b[] (bytecodes and inline operands are separate arrays)
// Displays fib(1) to fib(36

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
typedef long long int i64;

enum {
	jnotready,
	jload_mi8,
	jload_mi16,
	jload_mi32,
	jload_mi64,
	jload_mu8,
	jload_mu16,
	jload_mu32,
	jload_mmem,
	jload_fi8,
	jload_fi16,
	jload_fi32,
	jload_fi64,
	jload_fu8,
	jload_fu16,
	jload_fu32,
	jload_fmem,
	jstore_mi8,
	jstore_mi16,
	jstore_mi32,
	jstore_mi64,
	jstore_mmem,
	jstore_fi8,
	jstore_fi16,
	jstore_fi32,
	jstore_fi64,
	jstore_fmem,
	jloadimm_i64,
	jloadimm_r64,
	jloadimm_r32,
	jloadimm_str,
	jloadref_m,
	jloadref_f,
	jloadref_lab,
	jloadref_p,
	jloadref_ext,
	jiload_i8,
	jiload_i16,
	jiload_i32,
	jiload_i64,
	jiload_u8,
	jiload_u16,
	jiload_u32,
	jiload_mem,
	jiloadx_i8,
	jiloadx_i16,
	jiloadx_i32,
	jiloadx_i64,
	jiloadx_u8,
	jiloadx_u16,
	jiloadx_u32,
	jiloadx_mem,
	jistore_i8,
	jistore_i16,
	jistore_i32,
	jistore_i64,
	jistore_mem,
	jistorex_i8,
	jistorex_i16,
	jistorex_i32,
	jistorex_i64,
	jistorex_mem,
	jswapmem_i8,
	jswapmem_i16,
	jswapmem_i32,
	jswapmem_i64,
	jswapmem_mem,
	jclear_i8,
	jclear_i16,
	jclear_i32,
	jclear_i64,
	jclear_mem,
	jjumpeq_i64,
	jjumpeq_r64,
	jjumpeq_r32,
	jjumpeq_mem,
	jjumpne_i64,
	jjumpne_r64,
	jjumpne_r32,
	jjumpne_mem,
	jjumplt_i64,
	jjumplt_u64,
	jjumplt_r64,
	jjumplt_r32,
	jjumple_i64,
	jjumple_u64,
	jjumple_r64,
	jjumple_r32,
	jjumpge_i64,
	jjumpge_u64,
	jjumpge_r64,
	jjumpge_r32,
	jjumpgt_i64,
	jjumpgt_u64,
	jjumpgt_r64,
	jjumpgt_r32,
	jadd_i64,
	jadd_r64,
	jadd_r32,
	jsub_i64,
	jsub_r64,
	jsub_r32,
	jmul_i64,
	jmul_r64,
	jmul_r32,
	jneg_i64,
	jneg_r64,
	jneg_r32,
	jabs_i64,
	jabs_r64,
	jabs_r32,
	jsqr_i64,
	jsqr_r64,
	jsqr_r32,
	jsign_i64,
	jsign_r64,
	jsign_r32,
	jnegto_i64,
	jnegto_r64,
	jnegto_r32,
	jabsto_i64,
	jabsto_r64,
	jabsto_r32,
	jdivf_r64,
	jdivf_r32,
	jdiv_i64,
	jdiv_u64,
	jrem_i64,
	jrem_u64,
	jdivrem_i64,
	jdivrem_u64,
	jshr_i64,
	jshr_u64,
	jeq_i64,
	jeq_i32,
	jne_i64,
	jne_i32,
	jmin_i64,
	jmin_u64,
	jmin_r64,
	jmin_r32,
	jmax_i64,
	jmax_u64,
	jmax_r64,
	jmax_r32,
	jlt_i64,
	jlt_u64,
	jlt_r64,
	jlt_r32,
	jle_i64,
	jle_u64,
	jle_r64,
	jle_r32,
	jge_i64,
	jge_u64,
	jge_r64,
	jge_r32,
	jgt_i64,
	jgt_u64,
	jgt_r64,
	jgt_r32,
	jpower_i64,
	jpower_u64,
	jpower_r64,
	jaddto_i8,
	jaddto_i16,
	jaddto_i32,
	jaddto_i64,
	jaddto_r64,
	jaddto_r32,
	jsubto_i8,
	jsubto_i16,
	jsubto_i32,
	jsubto_i64,
	jsubto_r64,
	jsubto_r32,
	jmulto_i8,
	jmulto_i16,
	jmulto_i32,
	jmulto_i64,
	jmulto_r64,
	jmulto_r32,
	jdivto_i8,
	jdivto_i16,
	jdivto_i32,
	jdivto_i64,
	jdivto_u8,
	jdivto_u16,
	jdivto_u32,
	jdivto_u64,
	jdivto_r64,
	jdivto_r32,
	jminto_i8,
	jminto_i16,
	jminto_i32,
	jminto_i64,
	jminto_u8,
	jminto_u16,
	jminto_u32,
	jminto_u64,
	jminto_r64,
	jminto_r32,
	jmaxto_i8,
	jmaxto_i16,
	jmaxto_i32,
	jmaxto_i64,
	jmaxto_u8,
	jmaxto_u16,
	jmaxto_u32,
	jmaxto_u64,
	jmaxto_r64,
	jmaxto_r32,
	jbitnotto_i8,
	jbitnotto_i16,
	jbitnotto_i32,
	jbitnotto_i64,
	jnotto_i8,
	jnotto_i16,
	jnotto_i32,
	jnotto_i64,
	jnotnotto_i8,
	jnotnotto_i16,
	jnotnotto_i32,
	jnotnotto_i64,
	jincrto_i8,
	jincrto_i16,
	jincrto_i32,
	jincrto_i64,
	jincrload_i8,
	jincrload_i16,
	jincrload_i32,
	jincrload_i64,
	jloadincr_i8,
	jloadincr_i16,
	jloadincr_i32,
	jloadincr_i64,
	jbitandto_i8,
	jbitandto_i16,
	jbitandto_i32,
	jbitandto_i64,
	jbitorto_i8,
	jbitorto_i16,
	jbitorto_i32,
	jbitorto_i64,
	jbitxorto_i8,
	jbitxorto_i16,
	jbitxorto_i32,
	jbitxorto_i64,
	jshlto_i8,
	jshlto_i16,
	jshlto_i32,
	jshlto_i64,
	jshrto_i8,
	jshrto_i16,
	jshrto_i32,
	jshrto_i64,
	jshrto_u8,
	jshrto_u16,
	jshrto_u32,
	jshrto_u64,
	jforup_mm,
	jforup_mf,
	jforup_fm,
	jforup_ff,
	jforup_mi64,
	jforup_fi64,
	jfordown_mm,
	jfordown_mf,
	jfordown_fm,
	jfordown_ff,
	jfordown_mi64,
	jfordown_fi64,
	jto_m,
	jto_f,
	jfloat_r64_i64,
	jfloat_r64_u64,
	jfloat_r32_i64,
	jfloat_r32_u64,
	jfix_i64_r64,
	jfix_u64_r64,
	jfix_i64_r32,
	jfix_u64_r32,
	jfwiden,
	jfnarrow,
	jtruncate_i8,
	jtruncate_i16,
	jtruncate_i32,
	jtruncate_u8,
	jtruncate_u16,
	jtruncate_u32,
	jsqrt,
	jsin,
	jcos,
	jtan,
	jasin,
	jacos,
	jatan,
	jlog,
	jlog10,
	jexp,
	jround,
	jfloor,
	jceil,
	jfract,
	jatan2,
	jbitand,
	jbitor,
	jbitxor,
	jshl,
	jprocent,
	jsubent,
	jaddpxto,
	jsubpxto,
	jaddptrx,
	jsubptrx,
	jsubptr,
	jbitnot,
	jnot,
	jnotnot,
	jswitch,
	jswlabel,
	jloadbit,
	jstorebit,
	jloadbf,
	jstorebf,
	junload,
	jdouble,
	jdupl,
	jduplswap,
	jswapopnds,
	jcallp,
	jcallf,
	jicallp,
	jicallf,
	jcalldllp,
	jcalldllf,
	jsetret,
	jreturn_p,
	jreturn_f,
	jreturn_m,
	jjump,
	jijump,
	jjumpt,
	jjumpf,
	jstartmx,
	jresetmx,
	jendmx,
	jcallargs,
	jstop,
	jassem,
	jprinti64,
	jprintu64,
	jprintr64,
	jprintr32,
	jprintstr,
	jprinthex,
	jprintsp,
	jtest_m,
	jtest_f,
};

char* jcodenames[]= {"jnotready","jload_mi8","jload_mi16","jload_mi32","jload_mi64","jload_mu8","jload_mu16","jload_mu32","jload_mmem","jload_fi8","jload_fi16","jload_fi32","jload_fi64","jload_fu8","jload_fu16","jload_fu32","jload_fmem","jstore_mi8","jstore_mi16","jstore_mi32","jstore_mi64","jstore_mmem","jstore_fi8","jstore_fi16","jstore_fi32","jstore_fi64","jstore_fmem","jloadimm_i64","jloadimm_r64","jloadimm_r32","jloadimm_str","jloadref_m","jloadref_f","jloadref_lab","jloadref_p","jloadref_ext","jiload_i8","jiload_i16","jiload_i32","jiload_i64","jiload_u8","jiload_u16","jiload_u32","jiload_mem","jiloadx_i8","jiloadx_i16","jiloadx_i32","jiloadx_i64","jiloadx_u8","jiloadx_u16","jiloadx_u32","jiloadx_mem","jistore_i8","jistore_i16","jistore_i32","jistore_i64","jistore_mem","jistorex_i8","jistorex_i16","jistorex_i32","jistorex_i64","jistorex_mem","jswapmem_i8","jswapmem_i16","jswapmem_i32","jswapmem_i64","jswapmem_mem","jclear_i8","jclear_i16","jclear_i32","jclear_i64","jclear_mem","jjumpeq_i64","jjumpeq_r64","jjumpeq_r32","jjumpeq_mem","jjumpne_i64","jjumpne_r64","jjumpne_r32","jjumpne_mem","jjumplt_i64","jjumplt_u64","jjumplt_r64","jjumplt_r32","jjumple_i64","jjumple_u64","jjumple_r64","jjumple_r32","jjumpge_i64","jjumpge_u64","jjumpge_r64","jjumpge_r32","jjumpgt_i64","jjumpgt_u64","jjumpgt_r64","jjumpgt_r32","jadd_i64","jadd_r64","jadd_r32","jsub_i64","jsub_r64","jsub_r32","jmul_i64","jmul_r64","jmul_r32","jneg_i64","jneg_r64","jneg_r32","jabs_i64","jabs_r64","jabs_r32","jsqr_i64","jsqr_r64","jsqr_r32","jsign_i64","jsign_r64","jsign_r32","jnegto_i64","jnegto_r64","jnegto_r32","jabsto_i64","jabsto_r64","jabsto_r32","jdivf_r64","jdivf_r32","jdiv_i64","jdiv_u64","jrem_i64","jrem_u64","jdivrem_i64","jdivrem_u64","jshr_i64","jshr_u64","jeq_i64","jeq_i32","jne_i64","jne_i32","jmin_i64","jmin_u64","jmin_r64","jmin_r32","jmax_i64","jmax_u64","jmax_r64","jmax_r32","jlt_i64","jlt_u64","jlt_r64","jlt_r32","jle_i64","jle_u64","jle_r64","jle_r32","jge_i64","jge_u64","jge_r64","jge_r32","jgt_i64","jgt_u64","jgt_r64","jgt_r32","jpower_i64","jpower_u64","jpower_r64","jaddto_i8","jaddto_i16","jaddto_i32","jaddto_i64","jaddto_r64","jaddto_r32","jsubto_i8","jsubto_i16","jsubto_i32","jsubto_i64","jsubto_r64","jsubto_r32","jmulto_i8","jmulto_i16","jmulto_i32","jmulto_i64","jmulto_r64","jmulto_r32","jdivto_i8","jdivto_i16","jdivto_i32","jdivto_i64","jdivto_u8","jdivto_u16","jdivto_u32","jdivto_u64","jdivto_r64","jdivto_r32","jminto_i8","jminto_i16","jminto_i32","jminto_i64","jminto_u8","jminto_u16","jminto_u32","jminto_u64","jminto_r64","jminto_r32","jmaxto_i8","jmaxto_i16","jmaxto_i32","jmaxto_i64","jmaxto_u8","jmaxto_u16","jmaxto_u32","jmaxto_u64","jmaxto_r64","jmaxto_r32","jbitnotto_i8","jbitnotto_i16","jbitnotto_i32","jbitnotto_i64","jnotto_i8","jnotto_i16","jnotto_i32","jnotto_i64","jnotnotto_i8","jnotnotto_i16","jnotnotto_i32","jnotnotto_i64","jincrto_i8","jincrto_i16","jincrto_i32","jincrto_i64","jincrload_i8","jincrload_i16","jincrload_i32","jincrload_i64","jloadincr_i8","jloadincr_i16","jloadincr_i32","jloadincr_i64","jbitandto_i8","jbitandto_i16","jbitandto_i32","jbitandto_i64","jbitorto_i8","jbitorto_i16","jbitorto_i32","jbitorto_i64","jbitxorto_i8","jbitxorto_i16","jbitxorto_i32","jbitxorto_i64","jshlto_i8","jshlto_i16","jshlto_i32","jshlto_i64","jshrto_i8","jshrto_i16","jshrto_i32","jshrto_i64","jshrto_u8","jshrto_u16","jshrto_u32","jshrto_u64","jforup_mm","jforup_mf","jforup_fm","jforup_ff","jforup_mi64","jforup_fi64","jfordown_mm","jfordown_mf","jfordown_fm","jfordown_ff","jfordown_mi64","jfordown_fi64","jto_m","jto_f","jfloat_r64_i64","jfloat_r64_u64","jfloat_r32_i64","jfloat_r32_u64","jfix_i64_r64","jfix_u64_r64","jfix_i64_r32","jfix_u64_r32","jfwiden","jfnarrow","jtruncate_i8","jtruncate_i16","jtruncate_i32","jtruncate_u8","jtruncate_u16","jtruncate_u32","jsqrt","jsin","jcos","jtan","jasin","jacos","jatan","jlog","jlog10","jexp","jround","jfloor","jceil","jfract","jatan2","jbitand","jbitor","jbitxor","jshl","jprocent","jsubent","jaddpxto","jsubpxto","jaddptrx","jsubptrx","jsubptr","jbitnot","jnot","jnotnot","jswitch","jswlabel","jloadbit","jstorebit","jloadbf","jstorebf","junload","jdouble","jdupl","jduplswap","jswapopnds","jcallp","jcallf","jicallp","jicallf","jcalldllp","jcalldllf","jsetret","jreturn_p","jreturn_f","jreturn_m","jjump","jijump","jjumpt","jjumpf","jstartmx","jresetmx","jendmx","jcallargs","jstop","jassem","jprinti64","jprintu64","jprintr64","jprintr32","jprintstr","jprinthex","jprintsp","jtest_m","jtest_f"};

i64 pccode[] = {0,
	jprocent,
	jload_fi64,
	jloadimm_i64,
	jjumpge_i64,
	jloadimm_i64,
	jjump,
	jload_fi64,
	jloadimm_i64,
	jsub_i64,
	jcallf,
	jload_fi64,
	jloadimm_i64,
	jsub_i64,
	jcallf,
	jadd_i64,
	jjump,
	jreturn_f,
	jprocent,
	jloadimm_i64,
	jstore_fi64,
	jjump,
	jload_fi64,
	jcallf,
	jprinti64,
	jloadref_f,
	jincrto_i64,
	jload_fi64,
	jloadimm_i64,
	jjumple_i64,
	jloadimm_i64,
	jstop,
};

i64 pcdata[] = {0,
	1,	// jprocent
	-2,	// jload_fi64
	3,	// jloadimm_i64
	7,	// jjumpge_i64
	1,	// jloadimm_i64
	16,	// jjump
	-2,	// jload_fi64
	1,	// jloadimm_i64
	0,	// jsub_i64
	1,	// jcallf
	-2,	// jload_fi64
	2,	// jloadimm_i64
	0,	// jsub_i64
	1,	// jcallf
	0,	// jadd_i64
	17,	// jjump
	4295032832,	// jreturn_f
	18,	// jprocent
	1,	// jloadimm_i64
	0,	// jstore_fi64
	27,	// jjump
	0,	// jload_fi64
	1,	// jcallf
	0,	// jprinti64
	0,	// jloadref_f
	1,	// jincrto_i64
	0,	// jload_fi64
	36,	// jloadimm_i64
//	20,	// jloadimm_i64
	22,	// jjumple_i64
	0,	// jloadimm_i64
	0,	// jstop
};

i64 pcdatab[] = {0,
	65536,	// jprocent
	0,	// jload_fi64
	0,	// jloadimm_i64
	0,	// jjumpge_i64
	0,	// jloadimm_i64
	0,	// jjump
	0,	// jload_fi64
	0,	// jloadimm_i64
	0,	// jsub_i64
	1,	// jcallf
	0,	// jload_fi64
	0,	// jloadimm_i64
	0,	// jsub_i64
	1,	// jcallf
	0,	// jadd_i64
	0,	// jjump
	0,	// jreturn_f
	2,	// jprocent
	0,	// jloadimm_i64
	0,	// jstore_fi64
	0,	// jjump
	0,	// jload_fi64
	1,	// jcallf
	0,	// jprinti64
	0,	// jloadref_f
	0,	// jincrto_i64
	0,	// jload_fi64
	0,	// jloadimm_i64
	0,	// jjumple_i64
	0,	// jloadimm_i64
	0,	// jstop
};

#define getnlocals(x) ((x) & 0xFFFFLL)
#define getnparams(x) ((x>>16) & 0xFFFF)
#define getnretvalues(x) ((x>>32) & 0xFFFF)
#define getnargs(x) (x>>32)

void exec(i64 pc, i64 sp) {
	i64 stack[1000];
	i64 fp=0, a,b,c,x,n;
	#define dc (pc-1)
	i64* pi64;

	while (1) {
	switch (pccode[pc++]) {
	case jnotready:
		goto unimpl;
		break;

	case jload_mi8:
		goto unimpl;
		break;

	case jload_mi16:
		goto unimpl;
		break;

	case jload_mi32:
		goto unimpl;
		break;

	case jload_mi64:
		goto unimpl;
		break;

	case jload_mu8:
		goto unimpl;
		break;

	case jload_mu16:
		goto unimpl;
		break;

	case jload_mu32:
		goto unimpl;
		break;

	case jload_mmem:
		goto unimpl;
		break;

	case jload_fi8:
		goto unimpl;
		break;

	case jload_fi16:
		goto unimpl;
		break;

	case jload_fi32:
		goto unimpl;
		break;

	case jload_fi64:
		a=pcdata[dc];
		stack[++sp]=stack[fp+a];
		break;

	case jload_fu8:
		goto unimpl;
		break;

	case jload_fu16:
		goto unimpl;
		break;

	case jload_fu32:
		goto unimpl;
		break;

	case jload_fmem:
		goto unimpl;
		break;

	case jstore_mi8:
		goto unimpl;
		break;

	case jstore_mi16:
		goto unimpl;
		break;

	case jstore_mi32:
		goto unimpl;
		break;

	case jstore_mi64:
		goto unimpl;
		break;

	case jstore_mmem:
		goto unimpl;
		break;

	case jstore_fi8:
		goto unimpl;
		break;

	case jstore_fi16:
		goto unimpl;
		break;

	case jstore_fi32:
		goto unimpl;
		break;

	case jstore_fi64:
		a=pcdata[dc];
		stack[fp+a]=stack[sp--];
		break;

	case jstore_fmem:
		goto unimpl;
		break;

	case jloadimm_i64:
		stack[++sp]=pcdata[dc];
		break;

	case jloadimm_r64:
		goto unimpl;
		break;

	case jloadimm_r32:
		goto unimpl;
		break;

	case jloadimm_str:
		goto unimpl;
		break;

	case jloadref_m:
		stack[++sp]=pcdata[dc];
		break;

	case jloadref_f:
		stack[++sp]=(i64)&stack[fp+pcdata[dc]];
		break;

	case jloadref_lab:
		goto unimpl;
		break;

	case jloadref_p:
		goto unimpl;
		break;

	case jloadref_ext:
		goto unimpl;
		break;

	case jiload_i8:
		goto unimpl;
		break;

	case jiload_i16:
		goto unimpl;
		break;

	case jiload_i32:
		goto unimpl;
		break;

	case jiload_i64:
		goto unimpl;
		break;

	case jiload_u8:
		goto unimpl;
		break;

	case jiload_u16:
		goto unimpl;
		break;

	case jiload_u32:
		goto unimpl;
		break;

	case jiload_mem:
		goto unimpl;
		break;

	case jiloadx_i8:
		goto unimpl;
		break;

	case jiloadx_i16:
		goto unimpl;
		break;

	case jiloadx_i32:
		goto unimpl;
		break;

	case jiloadx_i64:
		goto unimpl;
		break;

	case jiloadx_u8:
		goto unimpl;
		break;

	case jiloadx_u16:
		goto unimpl;
		break;

	case jiloadx_u32:
		goto unimpl;
		break;

	case jiloadx_mem:
		goto unimpl;
		break;

	case jistore_i8:
		goto unimpl;
		break;

	case jistore_i16:
		goto unimpl;
		break;

	case jistore_i32:
		goto unimpl;
		break;

	case jistore_i64:
		goto unimpl;
		break;

	case jistore_mem:
		goto unimpl;
		break;

	case jistorex_i8:
		goto unimpl;
		break;

	case jistorex_i16:
		goto unimpl;
		break;

	case jistorex_i32:
		goto unimpl;
		break;

	case jistorex_i64:
		goto unimpl;
		break;

	case jistorex_mem:
		goto unimpl;
		break;

	case jswapmem_i8:
		goto unimpl;
		break;

	case jswapmem_i16:
		goto unimpl;
		break;

	case jswapmem_i32:
		goto unimpl;
		break;

	case jswapmem_i64:
		goto unimpl;
		break;

	case jswapmem_mem:
		goto unimpl;
		break;

	case jclear_i8:
		goto unimpl;
		break;

	case jclear_i16:
		goto unimpl;
		break;

	case jclear_i32:
		goto unimpl;
		break;

	case jclear_i64:
		goto unimpl;
		break;

	case jclear_mem:
		goto unimpl;
		break;

	case jjumpeq_i64:
		goto unimpl;
		break;

	case jjumpeq_r64:
		goto unimpl;
		break;

	case jjumpeq_r32:
		goto unimpl;
		break;

	case jjumpeq_mem:
		goto unimpl;
		break;

	case jjumpne_i64:
		goto unimpl;
		break;

	case jjumpne_r64:
		goto unimpl;
		break;

	case jjumpne_r32:
		goto unimpl;
		break;

	case jjumpne_mem:
		goto unimpl;
		break;

	case jjumplt_i64:
		goto unimpl;
		break;

	case jjumplt_u64:
		goto unimpl;
		break;

	case jjumplt_r64:
		goto unimpl;
		break;

	case jjumplt_r32:
		goto unimpl;
		break;

	case jjumple_i64:
		sp-=2;
		if (stack[sp+1]<=stack[sp+2]) pc=pcdata[dc];
		break;

	case jjumple_u64:
		goto unimpl;
		break;

	case jjumple_r64:
		goto unimpl;
		break;

	case jjumple_r32:
		goto unimpl;
		break;

	case jjumpge_i64:
		sp-=2;
		if (stack[sp+1]>=stack[sp+2]) pc=pcdata[dc];
		break;

	case jjumpge_u64:
		goto unimpl;
		break;

	case jjumpge_r64:
		goto unimpl;
		break;

	case jjumpge_r32:
		goto unimpl;
		break;

	case jjumpgt_i64:
		goto unimpl;
		break;

	case jjumpgt_u64:
		goto unimpl;
		break;

	case jjumpgt_r64:
		goto unimpl;
		break;

	case jjumpgt_r32:
		goto unimpl;
		break;

	case jadd_i64:
		--sp;
		stack[sp]+=stack[sp+1];
		break;

	case jadd_r64:
		goto unimpl;
		break;

	case jadd_r32:
		goto unimpl;
		break;

	case jsub_i64:
		--sp;
		stack[sp]-=stack[sp+1];
		break;

	case jsub_r64:
		goto unimpl;
		break;

	case jsub_r32:
		goto unimpl;
		break;

	case jmul_i64:
		goto unimpl;
		break;

	case jmul_r64:
		goto unimpl;
		break;

	case jmul_r32:
		goto unimpl;
		break;

	case jneg_i64:
		goto unimpl;
		break;

	case jneg_r64:
		goto unimpl;
		break;

	case jneg_r32:
		goto unimpl;
		break;

	case jabs_i64:
		goto unimpl;
		break;

	case jabs_r64:
		goto unimpl;
		break;

	case jabs_r32:
		goto unimpl;
		break;

	case jsqr_i64:
		goto unimpl;
		break;

	case jsqr_r64:
		goto unimpl;
		break;

	case jsqr_r32:
		goto unimpl;
		break;

	case jsign_i64:
		goto unimpl;
		break;

	case jsign_r64:
		goto unimpl;
		break;

	case jsign_r32:
		goto unimpl;
		break;

	case jnegto_i64:
		goto unimpl;
		break;

	case jnegto_r64:
		goto unimpl;
		break;

	case jnegto_r32:
		goto unimpl;
		break;

	case jabsto_i64:
		goto unimpl;
		break;

	case jabsto_r64:
		goto unimpl;
		break;

	case jabsto_r32:
		goto unimpl;
		break;

	case jdivf_r64:
		goto unimpl;
		break;

	case jdivf_r32:
		goto unimpl;
		break;

	case jdiv_i64:
		goto unimpl;
		break;

	case jdiv_u64:
		goto unimpl;
		break;

	case jrem_i64:
		goto unimpl;
		break;

	case jrem_u64:
		goto unimpl;
		break;

	case jdivrem_i64:
		goto unimpl;
		break;

	case jdivrem_u64:
		goto unimpl;
		break;

	case jshr_i64:
		goto unimpl;
		break;

	case jshr_u64:
		goto unimpl;
		break;

	case jeq_i64:
		goto unimpl;
		break;

	case jeq_i32:
		goto unimpl;
		break;

	case jne_i64:
		goto unimpl;
		break;

	case jne_i32:
		goto unimpl;
		break;

	case jmin_i64:
		goto unimpl;
		break;

	case jmin_u64:
		goto unimpl;
		break;

	case jmin_r64:
		goto unimpl;
		break;

	case jmin_r32:
		goto unimpl;
		break;

	case jmax_i64:
		goto unimpl;
		break;

	case jmax_u64:
		goto unimpl;
		break;

	case jmax_r64:
		goto unimpl;
		break;

	case jmax_r32:
		goto unimpl;
		break;

	case jlt_i64:
		goto unimpl;
		break;

	case jlt_u64:
		goto unimpl;
		break;

	case jlt_r64:
		goto unimpl;
		break;

	case jlt_r32:
		goto unimpl;
		break;

	case jle_i64:
		goto unimpl;
		break;

	case jle_u64:
		goto unimpl;
		break;

	case jle_r64:
		goto unimpl;
		break;

	case jle_r32:
		goto unimpl;
		break;

	case jge_i64:
		goto unimpl;
		break;

	case jge_u64:
		goto unimpl;
		break;

	case jge_r64:
		goto unimpl;
		break;

	case jge_r32:
		goto unimpl;
		break;

	case jgt_i64:
		goto unimpl;
		break;

	case jgt_u64:
		goto unimpl;
		break;

	case jgt_r64:
		goto unimpl;
		break;

	case jgt_r32:
		goto unimpl;
		break;

	case jpower_i64:
		goto unimpl;
		break;

	case jpower_u64:
		goto unimpl;
		break;

	case jpower_r64:
		goto unimpl;
		break;

	case jaddto_i8:
		goto unimpl;
		break;

	case jaddto_i16:
		goto unimpl;
		break;

	case jaddto_i32:
		goto unimpl;
		break;

	case jaddto_i64:
		goto unimpl;
		break;

	case jaddto_r64:
		goto unimpl;
		break;

	case jaddto_r32:
		goto unimpl;
		break;

	case jsubto_i8:
		goto unimpl;
		break;

	case jsubto_i16:
		goto unimpl;
		break;

	case jsubto_i32:
		goto unimpl;
		break;

	case jsubto_i64:
		goto unimpl;
		break;

	case jsubto_r64:
		goto unimpl;
		break;

	case jsubto_r32:
		goto unimpl;
		break;

	case jmulto_i8:
		goto unimpl;
		break;

	case jmulto_i16:
		goto unimpl;
		break;

	case jmulto_i32:
		goto unimpl;
		break;

	case jmulto_i64:
		goto unimpl;
		break;

	case jmulto_r64:
		goto unimpl;
		break;

	case jmulto_r32:
		goto unimpl;
		break;

	case jdivto_i8:
		goto unimpl;
		break;

	case jdivto_i16:
		goto unimpl;
		break;

	case jdivto_i32:
		goto unimpl;
		break;

	case jdivto_i64:
		goto unimpl;
		break;

	case jdivto_u8:
		goto unimpl;
		break;

	case jdivto_u16:
		goto unimpl;
		break;

	case jdivto_u32:
		goto unimpl;
		break;

	case jdivto_u64:
		goto unimpl;
		break;

	case jdivto_r64:
		goto unimpl;
		break;

	case jdivto_r32:
		goto unimpl;
		break;

	case jminto_i8:
		goto unimpl;
		break;

	case jminto_i16:
		goto unimpl;
		break;

	case jminto_i32:
		goto unimpl;
		break;

	case jminto_i64:
		goto unimpl;
		break;

	case jminto_u8:
		goto unimpl;
		break;

	case jminto_u16:
		goto unimpl;
		break;

	case jminto_u32:
		goto unimpl;
		break;

	case jminto_u64:
		goto unimpl;
		break;

	case jminto_r64:
		goto unimpl;
		break;

	case jminto_r32:
		goto unimpl;
		break;

	case jmaxto_i8:
		goto unimpl;
		break;

	case jmaxto_i16:
		goto unimpl;
		break;

	case jmaxto_i32:
		goto unimpl;
		break;

	case jmaxto_i64:
		goto unimpl;
		break;

	case jmaxto_u8:
		goto unimpl;
		break;

	case jmaxto_u16:
		goto unimpl;
		break;

	case jmaxto_u32:
		goto unimpl;
		break;

	case jmaxto_u64:
		goto unimpl;
		break;

	case jmaxto_r64:
		goto unimpl;
		break;

	case jmaxto_r32:
		goto unimpl;
		break;

	case jbitnotto_i8:
		goto unimpl;
		break;

	case jbitnotto_i16:
		goto unimpl;
		break;

	case jbitnotto_i32:
		goto unimpl;
		break;

	case jbitnotto_i64:
		goto unimpl;
		break;

	case jnotto_i8:
		goto unimpl;
		break;

	case jnotto_i16:
		goto unimpl;
		break;

	case jnotto_i32:
		goto unimpl;
		break;

	case jnotto_i64:
		goto unimpl;
		break;

	case jnotnotto_i8:
		goto unimpl;
		break;

	case jnotnotto_i16:
		goto unimpl;
		break;

	case jnotnotto_i32:
		goto unimpl;
		break;

	case jnotnotto_i64:
		goto unimpl;
		break;

	case jincrto_i8:
		goto unimpl;
		break;

	case jincrto_i16:
		goto unimpl;
		break;

	case jincrto_i32:
		goto unimpl;
		break;

	case jincrto_i64:
		pi64=(void*)(stack[sp--]);
		(*pi64)+=pcdata[dc];
		break;

	case jincrload_i8:
		goto unimpl;
		break;

	case jincrload_i16:
		goto unimpl;
		break;

	case jincrload_i32:
		goto unimpl;
		break;

	case jincrload_i64:
		goto unimpl;
		break;

	case jloadincr_i8:
		goto unimpl;
		break;

	case jloadincr_i16:
		goto unimpl;
		break;

	case jloadincr_i32:
		goto unimpl;
		break;

	case jloadincr_i64:
		goto unimpl;
		break;

	case jbitandto_i8:
		goto unimpl;
		break;

	case jbitandto_i16:
		goto unimpl;
		break;

	case jbitandto_i32:
		goto unimpl;
		break;

	case jbitandto_i64:
		goto unimpl;
		break;

	case jbitorto_i8:
		goto unimpl;
		break;

	case jbitorto_i16:
		goto unimpl;
		break;

	case jbitorto_i32:
		goto unimpl;
		break;

	case jbitorto_i64:
		goto unimpl;
		break;

	case jbitxorto_i8:
		goto unimpl;
		break;

	case jbitxorto_i16:
		goto unimpl;
		break;

	case jbitxorto_i32:
		goto unimpl;
		break;

	case jbitxorto_i64:
		goto unimpl;
		break;

	case jshlto_i8:
		goto unimpl;
		break;

	case jshlto_i16:
		goto unimpl;
		break;

	case jshlto_i32:
		goto unimpl;
		break;

	case jshlto_i64:
		goto unimpl;
		break;

	case jshrto_i8:
		goto unimpl;
		break;

	case jshrto_i16:
		goto unimpl;
		break;

	case jshrto_i32:
		goto unimpl;
		break;

	case jshrto_i64:
		goto unimpl;
		break;

	case jshrto_u8:
		goto unimpl;
		break;

	case jshrto_u16:
		goto unimpl;
		break;

	case jshrto_u32:
		goto unimpl;
		break;

	case jshrto_u64:
		goto unimpl;
		break;

	case jforup_mm:
		goto unimpl;
		break;

	case jforup_mf:
		goto unimpl;
		break;

	case jforup_fm:
		goto unimpl;
		break;

	case jforup_ff:
		goto unimpl;
		break;

	case jforup_mi64:
		goto unimpl;
		break;

	case jforup_fi64:
		goto unimpl;
		break;

	case jfordown_mm:
		goto unimpl;
		break;

	case jfordown_mf:
		goto unimpl;
		break;

	case jfordown_fm:
		goto unimpl;
		break;

	case jfordown_ff:
		goto unimpl;
		break;

	case jfordown_mi64:
		goto unimpl;
		break;

	case jfordown_fi64:
		goto unimpl;
		break;

	case jto_m:
		goto unimpl;
		break;

	case jto_f:
		goto unimpl;
		break;

	case jfloat_r64_i64:
		goto unimpl;
		break;

	case jfloat_r64_u64:
		goto unimpl;
		break;

	case jfloat_r32_i64:
		goto unimpl;
		break;

	case jfloat_r32_u64:
		goto unimpl;
		break;

	case jfix_i64_r64:
		goto unimpl;
		break;

	case jfix_u64_r64:
		goto unimpl;
		break;

	case jfix_i64_r32:
		goto unimpl;
		break;

	case jfix_u64_r32:
		goto unimpl;
		break;

	case jfwiden:
		goto unimpl;
		break;

	case jfnarrow:
		goto unimpl;
		break;

	case jtruncate_i8:
		goto unimpl;
		break;

	case jtruncate_i16:
		goto unimpl;
		break;

	case jtruncate_i32:
		goto unimpl;
		break;

	case jtruncate_u8:
		goto unimpl;
		break;

	case jtruncate_u16:
		goto unimpl;
		break;

	case jtruncate_u32:
		goto unimpl;
		break;

	case jsqrt:
		goto unimpl;
		break;

	case jsin:
		goto unimpl;
		break;

	case jcos:
		goto unimpl;
		break;

	case jtan:
		goto unimpl;
		break;

	case jasin:
		goto unimpl;
		break;

	case jacos:
		goto unimpl;
		break;

	case jatan:
		goto unimpl;
		break;

	case jlog:
		goto unimpl;
		break;

	case jlog10:
		goto unimpl;
		break;

	case jexp:
		goto unimpl;
		break;

	case jround:
		goto unimpl;
		break;

	case jfloor:
		goto unimpl;
		break;

	case jceil:
		goto unimpl;
		break;

	case jfract:
		goto unimpl;
		break;

	case jatan2:
		goto unimpl;
		break;

	case jbitand:
		goto unimpl;
		break;

	case jbitor:
		goto unimpl;
		break;

	case jbitxor:
		goto unimpl;
		break;

	case jshl:
		goto unimpl;
		break;

	case jprocent:
		stack[++sp]=fp;
		fp=sp;
		sp+=getnlocals(pcdatab[dc]);
		break;

	case jsubent:
		goto unimpl;
		break;

	case jaddpxto:
		goto unimpl;
		break;

	case jsubpxto:
		goto unimpl;
		break;

	case jaddptrx:
		goto unimpl;
		break;

	case jsubptrx:
		goto unimpl;
		break;

	case jsubptr:
		goto unimpl;
		break;

	case jbitnot:
		goto unimpl;
		break;

	case jnot:
		goto unimpl;
		break;

	case jnotnot:
		goto unimpl;
		break;

	case jswitch:
		goto unimpl;
		break;

	case jswlabel:
		goto unimpl;
		break;

	case jloadbit:
		goto unimpl;
		break;

	case jstorebit:
		goto unimpl;
		break;

	case jloadbf:
		goto unimpl;
		break;

	case jstorebf:
		goto unimpl;
		break;

	case junload:
		goto unimpl;
		break;

	case jdouble:
		goto unimpl;
		break;

	case jdupl:
		goto unimpl;
		break;

	case jduplswap:
		goto unimpl;
		break;

	case jswapopnds:
		goto unimpl;
		break;

	case jcallp:
		goto unimpl;
		break;

	case jcallf:
		stack[++sp]=pc;
		pc=pcdata[dc];
		break;

	case jicallp:
		goto unimpl;
		break;

	case jicallf:
		goto unimpl;
		break;

	case jcalldllp:
		goto unimpl;
		break;

	case jcalldllf:
		goto unimpl;
		break;

	case jsetret:
		goto unimpl;
		break;

	case jreturn_p:
		goto unimpl;
		break;

	case jreturn_f:
		x=stack[sp];
		n=getnparams(pcdata[dc]);
		sp-=getnlocals(pcdata[dc]);
		fp=stack[--sp];
		pc=stack[--sp];
		sp-=n;
		stack[sp]=x;
		break;

	case jreturn_m:
		goto unimpl;
		break;

	case jjump:
		pc=pcdata[dc];
		break;

	case jijump:
		goto unimpl;
		break;

	case jjumpt:
		goto unimpl;
		break;

	case jjumpf:
		goto unimpl;
		break;

	case jstartmx:
		goto unimpl;
		break;

	case jresetmx:
		goto unimpl;
		break;

	case jendmx:
		goto unimpl;
		break;

	case jcallargs:
		goto unimpl;
		break;

	case jstop:
		puts("Stopped");
		return;

	case jassem:
		goto unimpl;
		break;

	case jprinti64:
//		printf("%lld\n", stack[sp--]);
		break;

	case jprintu64:
		goto unimpl;
		break;

	case jprintr64:
		goto unimpl;
		break;

	case jprintr32:
		goto unimpl;
		break;

	case jprintstr:
		goto unimpl;
		break;

	case jprinthex:
		goto unimpl;
		break;

	case jprintsp:
		goto unimpl;
		break;

	case jtest_m:
		goto unimpl;
		break;

	case jtest_f:
		goto unimpl;
		break;

	default:
unimpl:;
		--pc;
		puts("");
		printf("Unimpl: %s  pc=%lld sp=%lld fp=%lld\n", jcodenames[pccode[pc]], pc, sp, fp);
		return;
	}	// switch
	}   // while
}


int main(void) {
	i64 tt=clock();

	puts("Starting:");
	exec(18, 0);             // 18 is the entry point in the bytecode
	printf("Finished; %lld\n", clock()-tt);
}
