// Tiny Pascal Interpreter
//#define FORCE_SWITCH         // uncomment to always use normal switch for dis

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <assert.h>

typedef int boolean;
#define true          (1)
#define false         (0)

#define odd(x) ((x) & 1)
#define chr(n) ((char)(n))
#define ord(c) ((int)(unsigned char)(c))

/* This version based on the Pascal version of Pascal-S by
  Jan van de Snepscheut, in the book, "What Computing is All About". */

/* line 295 (counting from 1 starting at program PascalS) is
                                  gen1(mulc, ttab[t].size); gen0(add)
  whereas the version printed in the book accidentally reads
                                  gen1(mulc, ttab[t].size)
  the present version also implements boolean negation

  the procedure funcdeclaration in the version printed in the book is
  erroneous. The first line on page 376 in the book should read
                                  if lev>1 then dx:=-1
  the last line of the procedure should read
        gen1(eexit,itab[f].resultadr-dx); lev:=lev-1; dx:=odx
*/

#define AMAX 1000       /* maximum address */

typedef enum {
    add, sub, neg, mul, divd, remd, div2, rem2, eqli, neqi, lssi, leqi, gtri,
    geqi, dupl, swap, andb, orb, load, stor, halt, wri, wrc, wrl, rdi, rdc,
    rdl, eofi, eol, ldc, ldla, ldl, ldg, stl, stg, move, copy, addc, subc, mulc, jump,
    jumpz, call, adjs, sets, ret, last_opcode
} opcode;

static int *code;
static FILE *infile;

/*procedure compile;*/

#define IMAX 300        /* length of identifier table */
#define TMAX 100        /* length of type table */
#define LMAX  10        /* maximum level */
#define AL    32        /* length of identifiers */

/* standard functions */
typedef enum {fabs=0, fsqr, fodd, fchr, ford, fwrite1, fwriteln, fread1, freadln, feof1, feoln} standard_functions;

/* standard types _MUST_ start at 1 - used as array index... */
typedef enum {intip=1, booltip, chartip} standard_types;

typedef enum {
    ident, number, sstring, plus, minus, star, lbrack, rbrack, colon, eql,
    neq, lss, leq, gtr, geq, lparen, rparen, comma, semicolon, period, opencmt,
    becomes, beginsym, endsym, ifsym, thensym, elsesym, whilesym, dosym,
    casesym, repeatsym, untilsym, forsym, tosym, downtosym, notsym, divsym,
    modsym, andsym, orsym, constsym, varsym, typesym, arraysym, ofsym,
    recordsym, progsym, funcsym, procsym, haltsym, lastsym
} symbol;
typedef enum {konst, varbl, field, tipe, funkt, last_idkind} idkind;
typedef enum {simple, arrays, records, last_tpkind} tpkind;
typedef char alfa[AL+1];// leave room for '\0'

static struct {
    int column;             /* character count */
    int linelen;            /* line length */
    int linenum;            /* current line number */
    char line[4096];        /* present input line */
} input;

static struct {
    symbol sym;             /* last symbol read */
    alfa id;                /* last identifier read */
    int num;                /* last number read */
    char curstr[256];       /* last string read */
    int slen;               /* length of last string */
} lex;

static int cx;                 /* code index */
static int cx_max;
static int lev;                /* procedure nesting level */
static int dx;                 /* offset in stack */
static boolean labeled;        /* next instruction has label */
static int namelist[LMAX+2];
static int ix, tx;             /* indices in tables */

static struct {
    alfa name;
    int link;
    int tip;
    idkind kind;
    union {
      int value;
      struct {int vlevel, vadr; boolean refpar;} s_varbl;
      int offset;
      struct {int flevel, fadr, lastpar, resultadr; boolean inside;} s_funkt;
    } u;
} itab[IMAX+1]            /* identifier table */;

static struct {
    int size;
    tpkind kind;
    union {
        struct {int low, high, elemtip;} s_arrays;
        int fields;
    } u;
} ttab[TMAX]                /* type table */;

static void error(int n) {
    int i;

    for( i = 1; i <= input.linelen; i ++)
        printf("%c", input.line[i-1]);
    printf("\n");
    for( i = 1; i <= input.column-2; i ++)
        printf(" ");
    printf("^\n");
    printf("error %1d detected (%d,%d)\n", n, input.linenum, input.column);
    exit(0);
}

static char getch(void) {
    int c;

    if (input.column==input.linelen)  {
        char *p;
        if (fgets(input.line, sizeof(input.line), infile) == NULL)
            error(100);
        p = memchr(input.line, '\n', sizeof(input.line));
        input.linelen = p - input.line + 1;
        input.column = 0;
        input.linenum++;
    }
    c = input.line[input.column];
    input.column++;
    return c;
}

static void putback(void) {
    input.column--;
}

static void getsym(void) {
    static struct key {
        char *word;
        int len;
    } keytab[] = {
        {"begin",     5}, {"end",        3}, {"if",         2}, {"then",       4},
        {"else",      4}, {"while",      5}, {"do",         2}, {"case",       4},
        {"repeat",    6}, {"until",      5}, {"for",        3}, {"to",         2},
        {"downto",    6}, {"not",        3}, {"div",        3}, {"mod",        3},
        {"and",       3}, {"or",         2}, {"const",      5}, {"var",        3},
        {"type",      4}, {"array",      5}, {"of",         2}, {"record",     6},
        {"program",   7}, {"function",   8}, {"procedure",  9}, {"halt",       4},
    };
    static symbol lextab[] = {
        lastsym, /* ! */   lastsym, /* " */   lastsym, /* # */   lastsym, /* $ */
        lastsym, /* % */   lastsym, /* & */   sstring, /* ' */   lparen , /* ( */
        rparen , /* ) */   star   , /* * */   plus   , /* + */   comma  , /* , */
        minus  , /* - */   period , /* . */   lastsym, /* / */   number , /* 0 */
        number , /* 1 */   number , /* 2 */   number , /* 3 */   number , /* 4 */
        number , /* 5 */   number , /* 6 */   number , /* 7 */   number , /* 8 */
        number , /* 9 */   colon  , /* : */   semicolon, /* ; */ lss    , /* < */
        eql    , /* = */   gtr    , /* > */   lastsym, /* ? */   lastsym, /* @ */
        ident  , /* A */   ident  , /* B */   ident  , /* C */   ident  , /* D */
        ident  , /* E */   ident  , /* F */   ident  , /* G */   ident  , /* H */
        ident  , /* I */   ident  , /* J */   ident  , /* K */   ident  , /* L */
        ident  , /* M */   ident  , /* N */   ident  , /* O */   ident  , /* P */
        ident  , /* Q */   ident  , /* R */   ident  , /* S */   ident  , /* T */
        ident  , /* U */   ident  , /* V */   ident  , /* W */   ident  , /* X */
        ident  , /* Y */   ident  , /* Z */   lbrack , /* [ */   lastsym, /* \ */
        rbrack , /* ] */   lastsym, /* ^ */   lastsym, /* _ */   lastsym, /* ` */
        ident  , /* a */   ident  , /* b */   ident  , /* c */   ident  , /* d */
        ident  , /* e */   ident  , /* f */   ident  , /* g */   ident  , /* h */
        ident  , /* i */   ident  , /* j */   ident  , /* k */   ident  , /* l */
        ident  , /* m */   ident  , /* n */   ident  , /* o */   ident  , /* p */
        ident  , /* q */   ident  , /* r */   ident  , /* s */   ident  , /* t */
        ident  , /* u */   ident  , /* v */   ident  , /* w */   ident  , /* x */
        ident  , /* y */   ident  , /* z */   opencmt, /* { */   lastsym, /* | */
        lastsym, /* } */   lastsym, /* ~ */
    };
    char input_ch;

    while (isspace(input_ch=getch()))
        ;

    lex.sym = input_ch > 32 && input_ch < 127 ? lextab[input_ch - 33] : lastsym;

    switch (lex.sym) {
        case ident: {
            symbol s;
            int id_len = 0;
            do {
                if (id_len != AL)  {
                    lex.id[id_len]= input_ch;
                    id_len++;
                }
                input_ch=getch();
            } while (isalnum(input_ch) || input_ch == '_');
            lex.id[id_len] = '\0';
            putback();

            for (s = beginsym; s < lastsym; s = (symbol)((int)s + 1)) {
                if (keytab[s - beginsym].len == id_len && memcmp(keytab[s - beginsym].word, lex.id, id_len) == 0) {
                    lex.sym = s;
                    break;
                }
            }
            break;
        }
        case number:
            lex.num = 0;
            do {
                lex.num = lex.num*10 + (ord(input_ch)-ord('0'));
                input_ch=getch();
            } while (isdigit(input_ch));
            putback();
            break;
        case colon:     // ':'
            if (getch()=='=')  {
                lex.sym = becomes;
            } else putback(); // else lex.sym = colon;
            break;
        case gtr:       // '>'
            if (getch()=='=')  {
                lex.sym = geq;
            } else putback(); // else lex.sym = gtr;
            break;
        case lss:       // '<'
            input_ch = getch();
            if (input_ch=='=')  {
                lex.sym = leq;
            } else if (input_ch=='>')  {
                lex.sym = neq;
            } else putback(); // else lex.sym = lss;
            break;
        case period:    // '.'
            if (getch()=='.')  {
                lex.sym = colon;
            } else putback();// else lex.sym = period;
            break;
        case sstring: { //'\''
            boolean strend = false;
            lex.slen = 0;
            do {
                if (input.column==input.linelen)
                    error(101);
                input_ch=getch();
                if (input_ch=='\'')  {
                    input_ch=getch();
                    if (input_ch=='\'')  {
                        lex.curstr[lex.slen]= input_ch;
                        lex.slen++;
                    } else {
                        strend = true;
                        putback();
                    }
                } else {
                    lex.curstr[lex.slen]= input_ch;
                    lex.slen++;
                }
            } while (!strend);
            if (lex.slen==0)
                error(102);
            }
            break;
        case plus: break;     // '+'
        case minus: break;     // '-'
        case star: break;      // '*'
        case lparen: break;   // '('
        case rparen: break;   // ')'
        case lbrack: break;   // '['
        case rbrack: break;   // ']'
        case eql: break;      // '='
        case comma: break;    // ','
        case semicolon: break; // ';'
        case opencmt:   // '{'
            while (getch() != '}')
                ;
            getsym();
            break;
        default:
            error(103);
            break;
    }
}

static void check(symbol s) {
    if (lex.sym!=s)
        error(ord(s));
}

static void skip(symbol s) {
    check(s);
    getsym();
}

//static void enter(alfa const id, idkind k, int t) {
static void enter(char const *id, idkind k, int t) {
    int j;
    if (ix==IMAX)
        error(104);
    ix++;
    snprintf(itab[0].name, sizeof(itab[0].name), "%s", id);
    j = namelist[lev+1];
    while (strcmp(itab[j].name, id) != 0)
        j = itab[j].link;
    if (j!=0)
        error(105);
    snprintf(itab[ix].name, sizeof(itab[ix].name), "%s", id);
    itab[ix].link = namelist[lev+1];
    itab[ix].tip = t;
    itab[ix].kind = k;
    namelist[lev+1]= ix;
}

static int position(void) {
    int i, j;
    snprintf(itab[0].name, sizeof(itab[0].name), "%s", lex.id);
    i = lev;
    do {
        j = namelist[i+1];
        while (strcmp(itab[j].name, lex.id) != 0)
            j = itab[j].link;
        i--;
    } while (!((i<-1) || (j!=0)));
    if (j==0)
        error(106);
    return j;
}

static void gen(int op, int adr) {
    if (cx >= cx_max) {
        cx_max += cx_max;
        if (cx_max == 0)
            cx_max = 50000000;
        code = realloc(code, sizeof(*code) * cx_max);
    }
    switch (op) {
        case dupl: case eofi: case eol: case ldc: case ldla: case ldl: case ldg:
            dx--;
            break;
        case neg: case div2: case rem2: case swap: case load: case halt: case wrl: case rdl:
        case addc: case subc: case mulc: case jump: case call: case sets: case ret:;
            break;
        case add: case sub: case mul: case divd: case remd: case eqli: case neqi: case lssi: case leqi: case gtri:
        case geqi: case andb: case orb: case wrc: case rdi: case rdc: case stl: case stg: case jumpz:
            dx++;
            break;
        case stor: case wri: case move:
            dx+=2;
            break;
        case copy:
            dx = dx-adr+1;
            break;
        case adjs:
            dx = dx+adr;
            break;
        default:
            ;
    }
    if (! (((op == addc || op == subc || op == adjs) && (adr==0)) || ((op==mulc) && (adr==1)))) {
        if (labeled)  {
            code[cx]= op;
            cx++;
            code[cx]= adr;
            cx++;
            labeled = false;
        } else if ((code[cx-2]==ldc) && (op==add))
            code[cx-2] = addc;
        else if ((code[cx-2]==ldc) && (op==sub))
            code[cx-2] = subc;
        else if ((code[cx-2]==ldc) && (op==mul))
            code[cx-2] = mulc;
        else if ((code[cx-2]==ldc) && (op==neg))
            code[cx-1] = -code[cx-1];
        else if ((code[cx-2]==ldc) && (code[cx-1]==2) && (op==divd)) {
            code[cx-2] = div2;
            cx--;
        } else if ((code[cx-2]==ldc) && (code[cx-1]==2) && (op==remd)) {
            code[cx-2] = rem2;
            cx--;
        } else if ((code[cx-2]==ldc) && (op==stor))
            code[cx-2] = stg;
        else if ((code[cx-2]==ldc) && (op==load))
            code[cx-2] = ldg;
        else if ((code[cx-2]==ldla) && (op==stor))
            code[cx-2] = stl;
        else if ((code[cx-2]==ldla) && (op==load))
            code[cx-2] = ldl;
        else {
            code[cx]= op;
            cx++;
            code[cx]= adr;
            cx++;
        }
    }
}

static void gen0(opcode op) {
    gen(op, 0);
}

static void gen1(opcode op, int adr) {
    gen(op, adr);
}

static int codelabel(void) {
    labeled = true;
    return cx;
}

static void address(int lv, int ad) {
    if (lv==0)
        gen1(ldc, ad);
    else if (lv==lev)
        gen1(ldla, ad-dx);
    else {
        gen1(ldl, -dx);
        while (lv+1!=lev) {
            gen0(load);
            lv++;
        }
        gen1(addc, ad);
    }
}

static void addressvar(int ref) {
    address(itab[ref].u.s_varbl.vlevel, itab[ref].u.s_varbl.vadr);
    if (itab[ref].u.s_varbl.refpar)
        gen0(load);
}

static void mustbe(int x, int y) {
    if (x!=y) {
        if ((ttab[x-1].kind==arrays) && (ttab[y-1].kind==arrays) &&
         (ttab[x-1].u.s_arrays.low==ttab[y-1].u.s_arrays.low) && (ttab[x-1].u.s_arrays.high==ttab[y-1].u.s_arrays.high))
            mustbe(ttab[x-1].u.s_arrays.elemtip, ttab[y-1].u.s_arrays.elemtip);
        else
            error(107);
    }
}

static void expression(int* x);

static int selector(int* ref) {
    int j, t, x;
    t = itab[*ref].tip;
    getsym();
    if (lex.sym == period || lex.sym == lbrack)  {
        addressvar(*ref);
        *ref = 0;
        while (lex.sym == period || lex.sym == lbrack)
            switch (lex.sym) {
                case period :
                    if (ttab[t-1].kind!=records)
                        error(108);
                    getsym();
                    check(ident);
                    j = ttab[t-1].u.fields;
                    snprintf(itab[0].name, sizeof(itab[0].name), "%s", lex.id);
                    while (strcmp(itab[j].name, lex.id) != 0)
                        j = itab[j].link;
                    if (j==0)
                        error(109);
                    gen1(addc, itab[j].u.offset);
                    t = itab[j].tip;
                    getsym();
                    break;
                case lbrack :
                    do {
                        if (ttab[t-1].kind!=arrays)
                            error(110);
                        getsym();
                        expression(&x);
                        mustbe(intip, x);
                        gen1(addc, -ttab[t-1].u.s_arrays.low);
                        t = ttab[t-1].u.s_arrays.elemtip;
                        gen1(mulc, ttab[t-1].size);
                        gen0(add);
                    } while (lex.sym==comma);
                    skip(rbrack);
                    break;
                default:
                    ;
            }
    }
    return t;
}

static int varpar(void) {
    int j, t;
    check(ident);
    j = position();
    t = selector(&j);
    if (j!=0)
        addressvar(j);
    return t;
}

static void standfct(int n) {
    int x, l;
    switch (n) {
        case fabs:
            skip(lparen);
            expression(&x);
            mustbe(intip, x);
            gen0(dupl);
            gen1(ldc, 0);
            gen0(lssi);
            l = codelabel();
            gen1(jumpz, 0);
            gen0(neg);
            code[l+1] = codelabel();
            skip(rparen);
            break;
        case fsqr:
            skip(lparen);
            expression(&x);
            mustbe(intip, x);
            gen0(dupl);
            gen0(mul);
            skip(rparen);
            break;
        case fodd:
            skip(lparen);
            expression(&x);
            mustbe(intip, x);
            gen0(rem2);
            skip(rparen);
            break;
        case fchr:
            skip(lparen);
            expression(&x);
            mustbe(intip, x);
            skip(rparen);
            break;
        case ford:
            skip(lparen);
            expression(&x);
            if (x != booltip)
                mustbe(chartip, x);
            skip(rparen);
            break;
        case fwrite1: case fwriteln:
            if (n==fwrite1)
                check(lparen);
            if (lex.sym==lparen)  {
                do {
                    getsym();
                    if (lex.sym==sstring)  {
                        for( x = 1; x <= lex.slen; x ++) {
                            gen1(ldc, ord(lex.curstr[x-1]));
                            gen0(wrc);
                        }
                        getsym();
                    } else {
                        expression(&x);
                        if (lex.sym==colon)  {
                            mustbe(intip, x);
                            getsym();
                            expression(&x);
                            mustbe(intip,x);
                            gen0(wri);
                        } else if (x==intip)  {
                            gen1(ldc, 8);
                            gen0(wri);
                        } else if (x==chartip)
                            gen0(wrc);
                        else
                            error(111);
                    }
                } while (lex.sym==comma);
                skip(rparen);
            }
            if (n==fwriteln)
                gen0(wrl);
            break;
     case fread1: case freadln:
            if (n==fread1)
                check(lparen);
            if (lex.sym==lparen)  {
                do {
                    getsym();
                    x = varpar();
                    if (x==intip)
                        gen0(rdi);
                    else if (x==chartip)
                        gen0(rdc);
                    else
                        error(112);
                } while (lex.sym==comma);
                skip(rparen);
            }
            if (n==freadln)
                gen0(rdl);
            break;
        case feof1: gen0(eofi); break;
        case feoln: gen0(eol); break;
    }
}

static void funcall(int i) {
    int d, p, x;
    getsym();
    if (itab[i].u.s_funkt.flevel<0)
        standfct(itab[i].u.s_funkt.fadr);
    else {
        if (itab[i].tip!=0)
            gen1(ldc, 0);
        p = i;
        d = dx;
        if (lex.sym==lparen)  {
            do { getsym();
                if (p==itab[i].u.s_funkt.lastpar)
                    error(113);
                p++;
                if (itab[p].u.s_varbl.refpar)
                    x = varpar();
                else {
                    expression(&x);
                    if (ttab[x-1].kind!=simple)
                        gen1(copy, ttab[x-1].size);
                }
                mustbe(itab[p].tip, x);
            } while (lex.sym==comma);
            skip(rparen);
        }
        if (p!=itab[i].u.s_funkt.lastpar)
            error(114);
        if (itab[i].u.s_funkt.flevel!=0)
            address(itab[i].u.s_funkt.flevel, 0);
        gen1(call, itab[i].u.s_funkt.fadr);
        dx = d;
    }
}

static void factor(int* t) {
    int i;

    switch (lex.sym) {
        case ident:
            i = position();
            *t = itab[i].tip;
            switch (itab[i].kind) {
                case konst:
                    getsym();
                    gen1(ldc, itab[i].u.value);
                    break;
                case varbl:
                    *t = selector(&i);
                    if (i!=0)
                        addressvar(i);
                    if (ttab[*t-1].kind==simple)
                        gen0(load);
                    break;
                case funkt:
                    if (*t==0)
                        error(115);
                    else
                        funcall(i);
                    break;
                case tipe :
                    error(116);
                    break;
                default:
                    ;
            }
            break;
        case number:
            gen1(ldc, lex.num);
            *t = intip;
            getsym();
            break;
        case sstring:
            if (lex.slen!=1)
                error(117);
            else {
                gen1(ldc, ord(lex.curstr[0]));
                *t = chartip;
                getsym();
            }
            break;
        case lparen:
            getsym();
            expression(t);
            skip(rparen);
            break;
        case notsym:
            getsym();
            factor(t);
            mustbe(booltip, *t);
            gen0(neg);
            gen1(addc, 1);
            break;
        default:
            error(117);
            break;
    }
}

static void term(int* x) {
    int y;

    factor(x);
    while (lex.sym == andsym || lex.sym == star || lex.sym == divsym || lex.sym == modsym)  {
        if (lex.sym==andsym)
            mustbe(booltip, *x);
        else
            mustbe(intip, *x);
        switch (lex.sym) {
            case star  : getsym(); factor(&y); gen0(mul); break;
            case divsym: getsym(); factor(&y); gen0(divd); break;
            case modsym: getsym(); factor(&y); gen0(remd); break;
            case andsym: getsym(); factor(&y); gen0(andb); break;
            default:
                ;
        }
        mustbe(*x, y);
    }
}

static void simplexpression(int* x) {
    int y;

    if (lex.sym==plus)  {
        getsym();
        term(x);
        mustbe(intip, *x);
    } else if (lex.sym==minus)  {
        getsym();
        term(x);
        mustbe(intip, *x);
        gen0(neg);
    } else
        term(x);
    while (lex.sym == orsym || lex.sym == plus || lex.sym == minus)  {
        if (lex.sym==orsym)
            mustbe(booltip, *x);
        else
            mustbe(intip, *x);
        switch (lex.sym) {
            case plus : getsym(); term(&y); gen0(add); break;
            case minus: getsym(); term(&y); gen0(sub); break;
            case orsym: getsym(); term(&y); gen0(orb); break;
            default:
                ;
        }
        mustbe(*x, y);
    }
}

static void expression(int* x) {
    symbol op;
    int y;
    simplexpression(x);
    if (lex.sym == eql || lex.sym == neq || lex.sym == lss || lex.sym == leq || lex.sym == gtr || lex.sym == geq)  {
        if (ttab[*x-1].kind!=simple)
            error(118);
        op = lex.sym;
        getsym();
        simplexpression(&y);
        mustbe(*x, y);
        switch (op) {
            case eql: gen0(eqli); break;
            case neq: gen0(neqi); break;
            case lss: gen0(lssi); break;
            case leq: gen0(leqi); break;
            case gtr: gen0(gtri); break;
            case geq: gen0(geqi); break;
            default:
                ;
        }
        *x = booltip;
    }
}

static void statement(void) {
    int i, j, m, n, t, x;
    int l[255];
    switch (lex.sym) {
        case ident:
            i = position();
            switch (itab[i].kind) {
                case varbl:
                    t = selector(&i);
                    skip(becomes);
                    expression(&x);
                    mustbe(t, x);
                    if (i==0)
                        gen0(swap);
                    else
                        addressvar(i);
                    if (ttab[t-1].kind==simple)
                        gen0(stor);
                    else
                        gen1(move, ttab[t-1].size);
                    break;
                case funkt:
                    if (itab[i].tip==0)
                        funcall(i);
                    else {
                        if (! itab[i].u.s_funkt.inside)
                            error(119);
                        getsym();
                        skip(becomes);
                        expression(&x);
                        mustbe(itab[i].tip, x);
                        address(itab[i].u.s_funkt.flevel+1, itab[i].u.s_funkt.resultadr);
                        gen0(stor);
                    }
                    break;
                case konst: case field: case tipe:
                    error(120);
                    break;
                default:
                    ;
            }
            break;
        case ifsym:
            getsym();
            expression(&t);
            mustbe(booltip, t);
            skip(thensym);
            i = codelabel();
            gen1(jumpz, 0);
            statement();
            if (lex.sym==elsesym)  {
                getsym();
                j = codelabel();
                gen1(jump, 0);
                code[i+1]= codelabel();
                i = j;
                statement();
            }
            code[i+1]= codelabel();
            break;
        case casesym:
            getsym();
            expression(&t);
            mustbe(intip, t);
            skip(ofsym);
            j = m = 0;
            do {
                if (j != 0) {
                    code[j+1] = codelabel();
                }
                n = m;
                do {
                    if (n != m)
                        getsym();
                    i = position();
                    if (itab[i].kind != konst)
                        error(131);
                    gen0(dupl);
                    gen1(ldc, itab[i].u.value);
                    gen0(neqi);
                    n++;
                    l[n] = codelabel();
                    gen1(jumpz, 0);
                    getsym();
                } while (lex.sym == comma);
                if (lex.sym != colon)
                    error(120);
                j = codelabel();
                gen1(jump, 0);
                do {
                    code[l[n-1]+1] = codelabel();
                    n--;
                } while (n != m);
                getsym();
                statement();
                m++;
                l[m-1] = codelabel();
                gen1(jump, 0);
                if (lex.sym == semicolon)
                  getsym();
            } while (lex.sym != endsym);
            code[j+1] = codelabel();
            do {
                code[l[m-1]+1] = codelabel();
                m--;
            } while (m != 0);
            gen1(adjs, 1);
            getsym();
            break;
        case whilesym:  // while expression do statement;
            getsym();
            i = codelabel();
            expression(&t);
            mustbe(booltip, t);
            skip(dosym);
            j = codelabel();
            gen1(jumpz, 0);
            statement();
            gen1(jump, i);
            code[j+1]= codelabel();
            break;
        case forsym: {   // for integer_var := expr to expr do statement;
            int d;
            getsym();
            check(ident);
            i = position();
            t = itab[i].tip;
            if (itab[i].kind != varbl)
                error(130);
            if (ttab[t].kind != simple)
                error(131);
            getsym();
            skip(becomes);
            expression(&x);
            mustbe(t, x);
            addressvar(i);
            gen0(stor);
            if (lex.sym == downtosym)
                d = -1;
            else {
                check(tosym);
                d = 1;
            }
            getsym();
            expression(&x);
            mustbe(t, x);
            gen0(dupl);
            addressvar(i);
            gen0(load);
            if (d == 1)
                gen0(geqi);
            else
                gen0(leqi);
            j = codelabel();
            gen1(jumpz, 0);
            skip(dosym);
            statement();
            gen0(dupl);
            addressvar(i);
            gen0(load);
            gen0(neqi);
            x = codelabel();
            gen1(jumpz, 0);
            addressvar(i);
            gen0(load);
            gen1(addc, d);
            addressvar(i);
            gen0(stor);
            gen1(jump, j + 1);
            code[j+1]= codelabel();
            code[x+1]= codelabel();
            gen1(adjs, 1);
            break;
            }
        case repeatsym: // repeat statements(s) until expression;
            i = codelabel();
            do {
                getsym();
                statement();
            } while (lex.sym==semicolon);
            skip(untilsym);
            expression(&t);
            mustbe(booltip, t);
            gen1(jumpz, i);
            break;
        case beginsym:
            do {
                getsym();
                statement();
            } while (lex.sym==semicolon);
            skip(endsym);
            break;
        case haltsym:
            getsym();
            gen0(halt);
            break;
        default:
            ;
    }
}

static void block(int l);

static void constant(int* c, int* t) {
    int i, s;
    if ((lex.sym==sstring) && (lex.slen==1))  {
        *c = ord(lex.curstr[0]);
        *t = chartip;
    } else {
        if (lex.sym==plus)   {
            getsym();
            s = 1;
        } else if (lex.sym==minus)  {
            getsym();
            s = -1;
        } else
            s = 0;
        if (lex.sym==ident)  {
            i = position();
            if (itab[i].kind!=konst)
                error(121);
            *c = itab[i].u.value;
            *t = itab[i].tip;
        } else if (lex.sym==number)  {
            *c = lex.num;
            *t = intip;
        } else
            error(122);
        if (s!=0)  {
            mustbe(*t, intip);
            *c = *c*s;
        }
    }
    getsym();
}

static void constdeclaration(void) {
    alfa a;
    int t, c;
    snprintf(a, sizeof(a), "%s", lex.id);
    getsym();
    skip(eql);
    constant(&c, &t);
    skip(semicolon);
    enter(a, konst, t);
    itab[ix].u.value = c;
}

static void typ(int* t);

static void arraytyp(int* t) {
    int x;
    ttab[*t-1].kind = arrays;
    getsym();
    constant(&ttab[*t-1].u.s_arrays.low, &x);
    mustbe(intip, x);
    skip(colon);
    constant(&ttab[*t-1].u.s_arrays.high, &x);
    mustbe(intip, x);
    if (ttab[*t-1].u.s_arrays.low>ttab[*t-1].u.s_arrays.high)
        error(123);
    if (lex.sym==comma) {   // multi-dim arrays
        // next two lines are experimental
        tx++;                                   // why?
        ttab[*t-1].u.s_arrays.elemtip = tx;     // why?
        arraytyp(&ttab[*t-1].u.s_arrays.elemtip);
    } else {
        skip(rbrack);
        skip(ofsym);
        typ(&ttab[*t-1].u.s_arrays.elemtip);
    }
    ttab[*t-1].size = (ttab[*t-1].u.s_arrays.high-ttab[*t-1].u.s_arrays.low+1)*ttab[ttab[*t-1].u.s_arrays.elemtip-1].size;
}

static void typ(int* t) {
    int i, j, sz, ft;
    if (lex.sym==ident)  {
        i = position();
        if (itab[i].kind!=tipe)
            error(124);
        *t = itab[i].tip;
        getsym();
    } else {
        if (tx==TMAX)
            error(125);
        tx++;
        *t = tx;
        if (lex.sym==arraysym)  {
            getsym();
            check(lbrack);
            arraytyp(t);
        } else {
            skip(recordsym);
            if (lev==LMAX)
                error(126);
            lev++;
            namelist[lev+1]= 0;
            check(ident);
            sz = 0;
            do {
                enter(lex.id, field, 0);
                i = ix;
                getsym();
                while (lex.sym==comma)  {
                    getsym();
                    check(ident);
                    enter(lex.id, field, 0);
                    getsym();
                }
                j = ix;
                skip(colon);
                typ(&ft);
                do {
                    itab[i].tip = ft;
                    itab[i].u.offset = sz;
                    sz = sz+ttab[ft-1].size;
                    i++;
                } while (!(i>j));
                if (lex.sym==semicolon)
                    getsym();
                else
                    check(endsym);
            } while (lex.sym==ident);
            ttab[*t-1].size = sz;
            ttab[*t-1].kind = records;
            ttab[*t-1].u.fields = namelist[lev+1];
            lev--;
            skip(endsym);
        }
    }
}

static void typedeclaration(void) {
    alfa a;
    int t;
    snprintf(a, sizeof(a), "%s", lex.id);
    getsym();
    skip(eql);
    typ(&t);
    skip(semicolon);
    enter(a, tipe, t);
}

static void vardeclaration(void) {
    int p, q, t;
    enter(lex.id, varbl, 0);
    p = ix;
    getsym();
    while (lex.sym==comma)  {
        getsym();
        check(ident);
        enter(lex.id, varbl, 0);
        getsym();
    }
    q = ix;
    skip(colon);
    typ(&t);
    skip(semicolon);
    do {
        itab[p].u.s_varbl.vlevel = lev;
        dx = dx-ttab[t-1].size;
        itab[p].tip = t;
        itab[p].u.s_varbl.vadr = dx;
        itab[p].u.s_varbl.refpar = false;
        p++;
    } while (!(p>q));
}

static void paramlist(int* p, int* ps) {
    boolean r;
    int t;
    if (lex.sym==varsym)  {
        r = true;
        getsym();
    } else
        r = false;
    check(ident);
    *p = ix;
    enter(lex.id, varbl, 0);
    getsym();
    while (lex.sym==comma)  {
        getsym();
        check(ident);
        enter(lex.id, varbl, 0);
        getsym();
    }
    skip(colon);
    check(ident);
    typ(&t);
    while (*p<ix)  {
        *p = *p+1;
        itab[*p].tip = t;
        itab[*p].u.s_varbl.refpar = r;
        if (r)
            *ps = *ps+1;
        else
            *ps = *ps+ttab[t-1].size;
    }
}

static void funcdeclaration(boolean isf) {
    int f, p, ps, odx;
    getsym();
    check(ident);
    enter(lex.id, funkt, 0);
    getsym();
    f = ix;
    itab[f].u.s_funkt.flevel = lev;
    itab[f].u.s_funkt.fadr = codelabel();
    gen1(jump, 0);
    if (lev==LMAX)
        error(127);
    lev++;
    namelist[lev+1]= 0;
    ps = 1;
    odx = dx;
    if (lex.sym==lparen)  {
        do {
            getsym();
            paramlist(&p, &ps);
        } while (lex.sym==semicolon);
        skip(rparen);
    }
    if (lev>1)
        dx = -1;
    else
        dx = 0;
    itab[f].u.s_funkt.resultadr = ps;
    p = f;
    while (p<ix)  {
        p++;
        if (itab[p].u.s_varbl.refpar)
            ps--;
        else
            ps = ps-ttab[itab[p].tip-1].size;
        itab[p].u.s_varbl.vlevel = lev;
        itab[p].u.s_varbl.vadr = ps;
    }
    itab[f].tip=0;
    if (isf)  {
        skip(colon);
        check(ident);
        typ(&itab[f].tip);
        if (ttab[itab[f].tip-1].kind!=simple)
            error(128);
    }
    skip(semicolon);
    itab[f].u.s_funkt.lastpar = ix;
    itab[f].u.s_funkt.inside = true;
    block(itab[f].u.s_funkt.fadr);
    itab[f].u.s_funkt.inside = false;
    gen1(ret, itab[f].u.s_funkt.resultadr-dx);
    lev--;
    dx = odx;
    skip(semicolon);
}

static void block(int l) {
    int d, odx, oix;
    odx = dx;
    oix = ix;
    while (lex.sym==constsym)  {
        getsym();
        check(ident);
        do {
            constdeclaration();
        } while (lex.sym==ident);
    }
    while (lex.sym==typesym)  {
        getsym();
        check(ident);
        do {
            typedeclaration();
        } while (lex.sym==ident);
    }
    while (lex.sym==varsym)  {
        getsym();
        check(ident);
        do {
            vardeclaration();
        } while (lex.sym==ident);
    }
    while (lex.sym == funcsym || lex.sym == procsym)
        funcdeclaration(lex.sym==funcsym);
    if (l+2==codelabel())
        cx -= 2;
    else
        code[l+1]= codelabel();
    if (lev==0)
        gen1(sets, dx);
    else {
        d = dx-odx;
        dx = odx;
        gen1(adjs, d);
    }
    statement();
    if (lev!=0)
        gen1(adjs, odx-dx);
    ix = oix;
}

static void listcode(void) {
    int i;
return;
    for( i = 0; i <= cx-1; i += 2) {
        printf("%6d :    ", i);
        switch (code[i]) {
            case add  : printf("add\n"); break;
            case sub  : printf("sub\n"); break;
            case neg  : printf("neg\n"); break;
            case mul  : printf("mul\n"); break;
            case divd : printf("divd\n"); break;
            case remd : printf("remd\n"); break;
            case div2 : printf("div2\n"); break;
            case rem2 : printf("rem2\n"); break;
            case eqli : printf("eqli\n"); break;
            case neqi : printf("neqi\n"); break;
            case lssi : printf("lssi\n"); break;
            case leqi : printf("leqi\n"); break;
            case gtri : printf("gtri\n"); break;
            case geqi : printf("geqi\n"); break;
            case dupl : printf("dupl\n"); break;
            case swap : printf("swap\n"); break;
            case andb : printf("andb\n"); break;
            case orb  : printf("orb\n"); break;
            case load : printf("load\n"); break;
            case stor : printf("stor\n"); break;
            case halt : printf("halt\n"); break;
            case wri  : printf("wri\n"); break;
            case wrc  : printf("wrc\n"); break;
            case wrl  : printf("wrl\n"); break;
            case rdi  : printf("rdi\n"); break;
            case rdc  : printf("rdc\n"); break;
            case rdl  : printf("rdl\n"); break;
            case eofi : printf("eof\n"); break;
            case eol  : printf("eol\n"); break;
            case ldc  : printf("ldc   %6d\n", code[i+1]); break;
            case ldla : printf("ldla  %6d\n", code[i+1]); break;
            case ldl  : printf("ldl   %6d\n", code[i+1]); break;
            case ldg  : printf("ldg   %6d\n", code[i+1]); break;
            case stl  : printf("stl   %6d\n", code[i+1]); break;
            case stg  : printf("stg   %6d\n", code[i+1]); break;
            case move : printf("move  %6d\n", code[i+1]); break;
            case copy : printf("copy  %6d\n", code[i+1]); break;
            case addc : printf("addc  %6d\n", code[i+1]); break;
            case subc : printf("subc  %6d\n", code[i+1]); break;
            case mulc : printf("mulc  %6d\n", code[i+1]); break;
            case jump : printf("jump  %6d\n", code[i+1]); break;
            case jumpz: printf("jumpz %6d\n", code[i+1]); break;
            case call : printf("call  %6d\n", code[i+1]); break;
            case adjs : printf("adjs  %6d\n", code[i+1]); break;
            case sets : printf("sets  %6d\n", code[i+1]); break;
            case ret  : printf("ret   %6d\n", code[i+1]); break;
            default:
                ;
        }
    }
}

static void compile(void) {
    ttab[intip-1].size = 1;
    ttab[intip-1].kind = simple;
    ttab[chartip-1].size = 1;
    ttab[chartip-1].kind = simple;
    ttab[booltip-1].size = 1;
    ttab[booltip-1].kind = simple;
    tx = 3;
    namelist[0]= 0;
    lev = -1;
    ix = 0;
    enter("false", konst, booltip); itab[ix].u.value = ord(false);
    enter("true", konst, booltip); itab[ix].u.value = ord(true);
    enter("maxint", konst, intip); itab[ix].u.value = INT_MAX;
    enter("integer", tipe, intip);
    enter("char", tipe, chartip);
    enter("boolean", tipe, booltip);
    enter("abs", funkt, intip);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = fabs; itab[ix].u.s_funkt.inside = false;
    enter("sqr", funkt, intip);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = fsqr; itab[ix].u.s_funkt.inside = false;
    enter("odd", funkt, booltip);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = fodd; itab[ix].u.s_funkt.inside = false;
    enter("chr", funkt, chartip);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = fchr; itab[ix].u.s_funkt.inside = false;
    enter("ord", funkt, intip);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = ford; itab[ix].u.s_funkt.inside = false;
    enter("write", funkt, 0);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = fwrite1;
    enter("writeln", funkt, 0);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = fwriteln;
    enter("read", funkt, 0);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = fread1;

    enter("readln", funkt, 0);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = freadln;

    enter("eof", funkt, booltip);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = feof1; itab[ix].u.s_funkt.inside = false;

    enter("eoln", funkt, booltip);
    itab[ix].u.s_funkt.flevel = -1; itab[ix].u.s_funkt.fadr = feoln; itab[ix].u.s_funkt.inside = false;

    namelist[1]= 0;
    lev = 0;
    input.column = 0;
    input.linelen = 0;
    input.linenum = 0;
    getsym();
    labeled = true;
    cx = 0;
    dx = AMAX+1;
    // don't require program foo(input,output); business...
    if (lex.sym == progsym) {
        skip(progsym);
        skip(ident);
        // accept program foo; -- (input,output); is not required...
        if (lex.sym == semicolon) {
            getsym();
        } else {
            check(lparen);
            do {
                getsym();
                check(ident);
                if ((strcmp(lex.id, "input") != 0) && (strcmp(lex.id, "output") != 0))
                    error(129);
                getsym();
            } while (lex.sym==comma);
            skip(rparen);
            skip(semicolon);
        }
    }
    gen1(jump, 0);
    block(0);
    gen0(halt);
    check(period);
}

static void interpret(void) {
    int *pc, sp;
    static int s[AMAX + 1];

    #if !defined(__GNUC__) || defined(FORCE_SWITCH)
      #define START start: switch (*pc++)
      #define CASE case
      #define NEXT goto start
      #define DEFAULT default
      puts("switch");
    #else
    /*************************************************************************
      Dependency: the ording of this table depends on the Code_t enum.
     *************************************************************************/
    static void *label[] = {
        &&add, &&sub, &&neg, &&mul, &&divd, &&remd, &&div2, &&rem2, &&eqli, &&neqi,
        &&lssi, &&leqi, &&gtri, &&geqi, &&dupl, &&swap, &&andb, &&orb, &&load, &&stor,
        &&halt, &&wri, &&wrc, &&wrl, &&rdi, &&rdc, &&rdl, &&eofi, &&eol, &&ldc, &&ldla,
        &&ldl, &&ldg, &&stl, &&stg, &&move, &&copy, &&addc, &&subc, &&mulc, &&jump,
        &&jumpz, &&call, &&adjs, &&sets, &&ret, &&DEFAULT
    };

      assert(label[ret]         == &&ret);
      assert(label[ret+1]       == &&DEFAULT);

      #define START goto *label[*pc++];
      #define CASE
      #define NEXT goto *label[*pc++]
      #define DEFAULT DEFAULT
      puts("goto");
    #endif

    sp = 0; // only to keep the compiler happy
    pc = code;

    START {
      CASE add  : s[sp+1] += s[sp]; sp++; pc++; NEXT;
      CASE sub  : s[sp+1] -= s[sp]; sp++; pc++; NEXT;
      CASE neg  : s[sp]= -s[sp]; pc++; NEXT;
      CASE mul  : s[sp+1] *= s[sp]; sp++; pc++; NEXT;
      CASE divd : s[sp+1] /= s[sp]; sp++; pc++; NEXT;
      CASE remd : s[sp+1] %= s[sp]; sp++; pc++; NEXT;
      CASE div2 : s[sp] /= 2; pc++; NEXT;
      CASE rem2 : s[sp] %= 2; pc++; NEXT;
      CASE eqli : s[sp+1]= ord(s[sp+1]==s[sp]); sp++; pc++; NEXT;
      CASE neqi : s[sp+1]= ord(s[sp+1]!=s[sp]); sp++; pc++; NEXT;
      CASE lssi : s[sp+1]= ord(s[sp+1]<s[sp]); sp++; pc++; NEXT;
      CASE leqi : s[sp+1]= ord(s[sp+1]<=s[sp]); sp++; pc++; NEXT;
      CASE gtri : s[sp+1]= ord(s[sp+1]>s[sp]); sp++; pc++; NEXT;
      CASE geqi : s[sp+1]= ord(s[sp+1]>=s[sp]); sp++; pc++; NEXT;
      CASE dupl : sp--; s[sp]= s[sp+1]; pc++; NEXT;
      CASE swap : { int k = s[sp]; s[sp]= s[sp+1]; s[sp+1]= k; } pc++; NEXT;
      CASE andb : if (s[sp]==0)  s[sp+1]= 0; sp++; pc++; NEXT;
      CASE orb  : if (s[sp]==1)  s[sp+1]= 1; sp++; pc++; NEXT;

      CASE load : s[sp]= s[s[sp]]; pc++; NEXT;
      CASE stor : s[s[sp]]= s[sp+1]; sp+=2; pc++; NEXT;

      CASE wri  : printf("%*d", s[sp], s[sp+1]); sp+=2; pc++; NEXT;
      CASE wrc  : printf("%c", chr(s[sp])); sp++; pc++; NEXT;
      CASE wrl  : printf("\n"); pc++; NEXT;
      CASE rdi  : { char _buf[32]; if (fgets(_buf,sizeof(_buf),stdin)) sscanf(_buf,"%d",&s[s[sp]]); sp++; pc++; NEXT; }
      CASE rdc  : { char _buf[32]; char c=0; if (fgets(_buf,sizeof(_buf),stdin)) c=_buf[0]; s[s[sp]]=ord(c); sp++; } pc++; NEXT;
      CASE rdl  : { int c; while ((c = getc(stdin)) != EOF && c != '\n'); } pc++; NEXT;
      CASE eofi : sp--; s[sp] = feof(stdin); pc++; NEXT;
      CASE eol  : { int c = getc(stdin); ungetc(c, stdin); sp--; s[sp]= c == '\n'; } pc++; NEXT;

      CASE ldc:   sp--; s[sp] = *pc++; NEXT;
      CASE ldla:  sp--; s[sp] = sp + 1 + *pc++; NEXT;
      CASE ldl:   sp--; s[sp] = s[sp + 1 + *pc++]; NEXT;
      CASE ldg:   sp--; s[sp] = s[*pc++]; NEXT;
      CASE stl:   s[sp + *pc++] = s[sp]; sp++; NEXT;
      CASE stg:   s[*pc++] = s[sp]; sp++; NEXT;

      CASE move : {
       int j,k,n;
       k = s[sp];
       j = s[sp+1];
       sp+=2;
       n = *pc++;
       // do { n--; s[k+n]= s[j+n]; } while (n!=0);
       memcpy(&s[k], &s[j], (n) * sizeof(s[0]));
       NEXT;
       }
     CASE copy : {
       int j,n;
       j = s[sp];
       n = *pc++;
        sp = sp-n+1;
       // do { n--; s[sp+n]= s[j+n]; } while (n!=0);
       memcpy(&s[sp], &s[j], (n) * sizeof(s[0]));
       NEXT;
       }

      CASE addc:  s[sp] += *pc++; NEXT;
      CASE subc:  s[sp] -= *pc++; NEXT;
      CASE mulc:  s[sp] *= *pc++; NEXT;
      CASE jump:  pc = code + *pc; NEXT;
      CASE jumpz: if (s[sp] == 0) pc = code + *pc; else pc++; sp++; NEXT;
      CASE adjs:  sp += *pc++; NEXT;
      CASE sets:  sp = *pc++; NEXT;
      CASE call:
        sp--;
        s[sp] = pc - code + 1;
        pc = code + *pc;
        NEXT;
      CASE ret: {
        int pop_stack = *pc;
        pc = code + s[sp];
        sp += pop_stack;
        NEXT;
        }

      CASE halt:    ;
      DEFAULT:      ;
    }
}

int main(int argc, char *argv[]) {
    int i;
    char fn[1024];
    boolean quiet, compile_only;

    quiet = compile_only = false;
    for (i = 1; i < argc && argv[i][0] == '-';) {
        switch (tolower(argv[i][1])) {
            case 'q':
                quiet = true;
                i++;
                break;
            case 'c':
                compile_only = true;
                i++;
                break;
        }
    }

    if (argc <= i) {
        printf("no filename entered\n");
        exit(1);
    }

    snprintf(fn, sizeof(fn), "%s", argv[i]);

    infile = fopen(fn, "r");
    if (infile == NULL)
        snprintf(fn + strlen(fn), sizeof(fn) - strlen(fn), ".pas");

    infile = fopen(fn, "r");
    if (infile == NULL) {
        fputs("Can't open ", stderr); fputs(argv[1], stderr);
        exit(0);
    }

    compile();

    if (!quiet)
        listcode();

    if (!compile_only)
        interpret();

    fclose(infile);
    return EXIT_SUCCESS;
}
