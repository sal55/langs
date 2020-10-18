// evaluate simple expressions inside strings
// variable must be single-letters only
// see main() for example

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double eval(char* expr);
double evalexpr(void);
double evaladd(void);
double evalmul(void);
double evalterm(void);
void nexttk(void);
void error(char* mess);

int token;
char* lxptr;
double tkvalue;
enum {add_tk, sub_tk, mul_tk, div_tk,lbrack_tk, rbrack_tk,
      number_tk, sin_tk, end_tk};

double variables[26];

double eval(char* expr) {
    double x;
    lxptr=expr;

    nexttk();
    x=evalexpr();
    if (token!=end_tk) error("Bad ending");
    return x;
}

double evalexpr(void) {
    return evaladd();
}

double evaladd(void) {
    double x,y;
    int opc;

    x=evalmul();

    while (token==add_tk || token==sub_tk) {
        opc=token;
        nexttk();
        y=evalmul();
        if (opc==add_tk) {
            x+=y;
        } else {
            x-=y;
        }
    }

    return x;
}

double evalmul(void) {
    double x,y;
    int opc;

    x=evalterm();

    while (token==mul_tk || token==div_tk) {
        opc=token;
        nexttk();
        y=evalterm();
        if (opc==mul_tk) {
            x*=y;
        } else {
            x/=y;
        }
    }

    return x;
}

double evalterm(void) {
    double x;

    switch (token) {
    case sub_tk:
        nexttk();
        return -evalterm();
    case lbrack_tk:
        nexttk();
        x=evalexpr();
        if (token!=rbrack_tk) error("')' expected");
        nexttk();
        return x;
    case sin_tk:
        nexttk();
        return sin(evalterm());
    case number_tk:
        x=tkvalue;
        nexttk();
        return x;
    default:
        error("Term");
    }
    return 0.0;
}

void nexttk(void) {
    int c;
    char* pstart, pend;

    switch (c=*lxptr++) {
    case ' ': case '\t': nexttk(); break;
    case '+': token=add_tk; break;
    case '-': token=sub_tk; break;
    case '*': token=mul_tk; break;
    case '/': token=div_tk; break;
    case '(': token=lbrack_tk; break;
    case ')': token=rbrack_tk; break;
    case 'A':case 'B':case 'C':case 'D':case 'E':case 'F':case 'G':
    case 'H':case 'I':case 'J':case 'K':case 'L':case 'M':case 'N':
    case 'O':case 'P':case 'Q':case 'R':case 'S':case 'T':case 'U':
    case 'V':case 'W':case 'X':case 'Y':case 'Z':
        tkvalue=variables[(c-'A')];
        token=rbrack_tk; break;

    case 'a':case 'b':case 'c':case 'd':case 'e':case 'f':case 'g':
    case 'h':case 'i':case 'j':case 'k':case 'l':case 'm':case 'n':
    case 'o':case 'p':case 'q':case 'r':case 's':case 't':case 'u':
    case 'v':case 'w':case 'x':case 'y':case 'z':
        if (c=='s' && *lxptr=='i' && *(lxptr+1)=='n') {
            lxptr+=2;
            token=sin_tk;
        } else {
            tkvalue=variables[c-'a'];
            token=number_tk;
        }
        break;

    case '.':case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        pstart=lxptr-1;
        tkvalue=strtod(pstart,&lxptr);
        token=number_tk;
        break;

    case 0:
        token=end_tk;
        break;

    default:
        error("Syntax");
    }
}

void error(char* mess) {
    printf("Error: %s\n",mess);
    exit(1);
}


int main(void) {
    char* expr;

    variables['x'-'a']=0.523;
    variables['y'-'a']=34.6;
    variables['z'-'a']=99.0;

    expr="sin(x)*2+y";
    printf("%s = %f\n", expr, eval(expr));

}
