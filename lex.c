#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef long long int i64;

char* source = "abc = (xyz + 123);";
char* sptr;

enum {tkident, tknumber, tkstring, tkequals, tksemi,
     tkadd, tkminus, tkmul, tklparen, tkrparen, tkeof, tkerror};

char* tokennames[] = {
"name", "number", "string", "equals", "semi", "add", "minus", "mul",
"lparen", "rparen", "eof", "error"};

i64 tk_value;
char* tk_name;
char* tk_strvalue;

int NextChar() {
    if (*sptr==0) return 0;
    return *sptr++;
}

char* heapstring(char* p, int len) {
    char* s = malloc(len+1);
    memcpy(s, p, len);
    *(s+len)=0;
    return s;
}

int ReadIdent() {
    char* pstart = sptr-1;
    int c;

    while (1) {
        c=NextChar();
        if (!(isalpha(c) || isdigit(c) || c=='_')) break;
    }
    tk_name = heapstring(pstart, sptr-pstart-1);
    --sptr;
    return tkident;
}

int ReadNumber(int c) {
    tk_value = c-'0';
    
    while (1) {
        c=NextChar();
        if (!isdigit(c)) break;
        tk_value = tk_value*10+c-'0';
    }
    --sptr;
    return tknumber;
}

int ReadString() {
    char* pstart = sptr;
    int c;

    while (1) {
        c=NextChar();
        if (c==0) return tkerror;
        if (c=='"') break;
    }
    tk_strvalue = heapstring(pstart, sptr-pstart-1);

    return tkstring;
}

int NextToken() {
    int c;

    do
        c=NextChar();
    while (c==' ' || c=='\t');

    switch (c) {
    case 0:   return tkeof;
    case '(': return tklparen;
    case ')': return tkrparen;
    case '+': return tkadd;
    case '-': return tkminus;
    case '*': return tkmul;
    case '=': return tkequals;
    case ';': return tksemi;
    }

    if (isalpha(c)) return ReadIdent();
    else if (isdigit(c)) return ReadNumber(c);
    else if (c=='"') return ReadString();
    else return tkerror;
}

int main(void) {
    int ntokens=0, token;
    sptr = source;

    do {
        token = NextToken();
        ++ntokens;

        printf("Token: %s ", tokennames[token]);
        if (token==tkident) printf(": %s\n", tk_name);
        else if (token==tknumber) printf(": %lld\n", tk_value);
        else if (token==tkstring) printf(": \"%s\"\n", tk_strvalue);
        else puts("");

    } while (token!=tkeof);

    printf("%d tokens\n\n", ntokens);
}
