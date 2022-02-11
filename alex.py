# Lexer for (incomplete) C source code (but will cope with anything else for the purposes
# of measuring rough performance)

# Change the 'infile' assignment near the top to select the input file
# (Or recode it to use argv)

# (I've incorporated one of the import files to make it easier to run, but messier
# at the start)

import time

infile="sqlite3.c"

skip_sym = 0
error_sym = 1
dot_sym = 2
comma_sym = 3
semi_sym = 4
question_sym = 5
colon_sym = 6
assign_sym = 7
lbrack_sym = 8
rbrack_sym = 9
lsq_sym = 10
rsq_sym = 11
lcurly_sym = 12
rcurly_sym = 13
addr_sym = 14
deref_sym = 15
ellipsis_sym = 16
op_sym = 17
opto_sym = 18
eol_sym = 19
eof_sym = 20
hash_sym = 21
incr_sym = 22
name_sym = 23
intconst_sym = 24
realconst_sym = 25
charconst_sym = 26
wcharconst_sym = 27
stringconst_sym = 28
wstringconst_sym = 29
kdummy_sym = 30

symbolnames=("skip_sym",
    "error_sym",
    "dot_sym",
    "comma_sym",
    "semi_sym",
    "question_sym",
    "colon_sym",
    "assign_sym",
    "lbrack_sym",
    "rbrack_sym",
    "lsq_sym",
    "rsq_sym",
    "lcurly_sym",
    "rcurly_sym",
    "addr_sym",
    "deref_sym",
    "ellipsis_sym",
    "op_sym",
    "opto_sym",
    "eol_sym",
    "eof_sym",
    "hash_sym",
    "incr_sym",
    "name_sym",
    "intconst_sym",
    "realconst_sym",
    "charconst_sym",
    "wcharconst_sym",
    "stringconst_sym",
    "wstringconst_sym",
    "kdummy_sym")

j_eq = 1
j_ne = 2
j_lt = 3
j_le = 4
j_gt = 5
j_ge = 6
j_add = 7
j_sub = 8
j_mul = 9
j_div = 10
j_rem = 11
j_iand = 12
j_ior = 13
j_ixor = 14
j_shl = 15
j_shr = 16
j_andand = 17
j_oror = 18
j_neg = 19
j_abs = 20
j_not = 21
j_inot = 22
j_preincr = 23
j_predecr = 24
j_addto = 25
j_subto = 26
j_multo = 27
j_divto = 28
j_remto = 29
j_iandto = 30
j_iorto = 31
j_ixorto = 32
j_shlto = 33
j_shrto = 34
j_dummy = 35


jtagnames=("",
    "j_eq",
    "j_ne",
    "j_lt",
    "j_le",
    "j_gt",
    "j_ge",
    "j_add",
    "j_sub",
    "j_mul",
    "j_div",
    "j_rem",
    "j_iand",
    "j_ior",
    "j_ixor",
    "j_shl",
    "j_shr",
    "j_andand",
    "j_oror",
    "j_neg",
    "j_abs",
    "j_not",
    "j_inot",
    "j_preincr",
    "j_predecr",
    "j_addto",
    "j_subto",
    "j_multo",
    "j_divto",
    "j_remto",
    "j_iandto",
    "j_iorto",
    "j_ixorto",
    "j_shlto",
    "j_shrto",
    "j_dummy")

cr=chr(13)
lf=chr(10)
etx=chr(26)

def readstrfile(file):
    try:
        f=open(file,"r")
    except IOError:
        return 0
    try:
        data=f.read()
    except IOError:
        return 0
    f.close()
    return data

def initdata():
    global simples
    simples={"#":hash_sym,
        "(":lbrack_sym,
        ")":rbrack_sym,
        "[":lsq_sym,
        "]":lsq_sym,
        "{":lcurly_sym,
        "}":lcurly_sym,
        ",":comma_sym,
        ":":colon_sym,
        ";":semi_sym,
        "?":question_sym}

def start():
    global psource,lxsptr,lxvalue,lxsymbol,lxsubcode,lxlineno,table

    initdata()
    t=time.process_time()
    psource=readstrfile(infile)
    if psource==0:
        print ("Can't load file",infile)
        exit(0)

    psource+=etx
#       print ("PSOURCE",psource)
#   print ("PSOURCE",type(psource))
#   print ("PSOURCE",len(psource))
    print ("-"*80)

    nchars=0
    nlines=0
    ntokens=0

    table={}

    nn=1

    for i in range(nn):
            lxsptr=0

            lxfileno=1
            lxlineno=1
            lxsymbol=0

            while lxsymbol!=eof_sym:
                readtoken(psource)
#               printsymbol()
                ntokens+=1

            nlines+=lxlineno
            nchars+=len(psource)

    t=time.process_time()-t

    print ("File",infile)
    print ("Nlines",nlines)
    print ("NTokens",ntokens)
    print ("Nchars",nchars)
    print ("Time",t)

    print (int(ntokens/t),' Tokens per second')
    print (int(nlines/t),"  Lines per second")
    print (int(nchars/t),"  Chars per second")

#   print ("ALLNAMES",ALLNAMES)

#   showtable()

def readtoken(psource):
    global lxsptr,lxvalue,lxsymbol,lxsubcode,lxlineno,table,simples
    lxsubcode=0

    while (1):
        c=psource[lxsptr]
        lxsptr+=1
    
        if c.isalpha() or c in "$_":
            i=lxsptr-1
            while 1:
                d=psource[lxsptr]
                if not (d.isalnum() or d in "$_"): break
                lxsptr+=1

            lxvalue=psource[i:lxsptr]
            lxsymbol=name_sym
            return

        elif c.isdigit():
            d=psource[lxsptr]
            if d in "xX":
                readhexdigits(psource)
            elif d in "bB":
                readbindigits(psource)
            else:
                readdecdigits(psource,c)
            return

#       elif c in simples:
#           lxsymbol=simples[c]
#           return

        elif c=="#":
            lxsymbol=hash_sym
            return

        elif c=="(":
            lxsymbol=lbrack_sym
            return

        elif c==")":
            lxsymbol=rbrack_sym
            return

        elif c=="{":
            lxsymbol=lcurly_sym
            return

        elif c=="}":
            lxsymbol=rcurly_sym
            return

        elif c=="[":
            lxsymbol=lsq_sym
            return

        elif c=="]":
            lxsymbol=rsq_sym
            return

        elif c==",":
            lxsymbol=comma_sym
            return

        elif c==":":
            lxsymbol=colon_sym
            return

        elif c==";":
            lxsymbol=semi_sym
            return

        elif c=="?":
            lxsymbol=question_sym
            return




        elif c in " \t":
            pass

        elif c=="\r":
            if psource[lxsptr]=="\n":
                lxsptr+=1
            lxlineno+=1
            lxsymbol=eol_sym
            return

        elif c=="\n":
            lxlineno+=1
            lxsymbol=eol_sym
            return

        elif c=="'":
            readstring(psource,c)
            return

        elif c=='"':
            readstring(psource,c)
            return

        elif c=="+":
            d=psource[lxsptr]
            if d=="+":
                lxsptr+=1
                lxsymbol=incr_sym
                lxsubcode=j_preincr
            elif d=="=":
                lxsptr+=1
                lxsymbol=opto_sym
                lxsubcode=j_addto
            else:
                lxsymbol=op_sym
                lxsubcode=j_add
            return

        elif c=="-":
            d=psource[lxsptr]
            if d=="-":
                lxsptr+=1
                lxsymbol=incr_sym
                lxsubcode=j_predecr
            elif d=="=":
                lxsptr+=1
                lxsymbol=opto_sym
                lxsubcode=j_subto
            else:
                lxsymbol=op_sym
                lxsubcode=j_sub
            return

        elif c=="*":
            if psource[lxsptr]=="=":
                lxsptr+=1
                lxsymbol=opto_sym
                lxsubcode=j_multo
            else:
                lxsymbol=op_sym
                lxsubcode=j_mul
            return

        elif c=="/":
            d=psource[lxsptr]
            if d=="/":
                readlinecomment(psource)
            elif d=="*":
                readblockcomment(psource)
            elif d=="=":
                lxsymbol=opto_sym
                lxsubcode=j_divto
            else:
                lxsymbol=op_sym
                lxsubcode=j_div
            return

        elif c=="%":
            if psource[lxsptr]=="=":
                lxsptr+=1
                lxsymbol=opto_sym
                lxsubcode=j_remto
            else:
                lxsymbol=op_sym
                lxsubcode=j_rem
            return

        elif c=="<":
            lxsymbol=op_sym
            d=psource[lxsptr]
            if d=="<":
                lxsptr+=1
                if psource[lxsptr]=="=":
                    lxsptr+=1
                    lxsymbol=opto_sym
                    lxsubcode=j_shlto
                else:
                    lxsubcode=j_shl
            elif d=="=":
                lxsptr+=1
                lxsubcode=j_le
            else:
                lxsubcode=j_lt
            return

        elif c==">":
            lxsymbol=op_sym
            d=psource[lxsptr]
            if d==">":
                lxsptr+=1
                if psource[lxsptr]=="=":
                    lxsptr+=1
                    lxsymbol=opto_sym
                    lxsubcode=j_shrto
                else:
                    lxsubcode=j_shl
            elif d=="=":
                lxsptr+=1
                lxsubcode=j_ge
            else:
                lxsubcode=j_gt
            return


        elif c=="^":
            if psource[lxsptr]=="=":
                lxsptr+=1
                lxsymbol=opto_sym
                lxsubcode=j_ixorto
            else:
                lxsymbol=op_sym
                lxsubcode=j_ixor
            return

        elif c=="|":
            d=psource[lxsptr]
            if d=="|":
                lxsptr+=1
                lxsymbol=op_sym
                lxsubcode=j_oror
            elif d=="=":
                lxsymbol=opto_sym
                lxsubcode=j_iorto
            else:
                lxsymbol=op_sym
                lxsubcode=j_ior
            return

        elif c=="&":
            d=psource[lxsptr]
            if d=="&":
                lxsptr+=1
                lxsymbol=op_sym
                lxsubcode=j_andand
            elif d=="=":
                lxsymbol=opto_sym
                lxsubcode=j_iandto
            else:
                lxsymbol=op_sym
                lxsubcode=j_iand
            return

        elif c=="~":
            lxsymbol=op_sym
            lxsubcode=j_inot
            return

        elif c=="=":
            if psource[lxsptr]=="=":
                lxsptr+=1
                lxsymbol=op_sym
                lxsubcode=j_eq
            else:
                lxsymbol=assign_sym
            return

        elif c=="!":
            if psource[lxsptr]=="=":
                lxsptr+=1
                lxsymbol=op_sym
                lxsubcode=j_ne
            else:
                lxsymbol=op_sym
                lxsubcode=j_not
            return
        elif c==".":
            d=psource[lxsptr]
            if d in "eE0123456789":
                readdecdigits(psource,d)
            else:
                lxsymbol=dot_sym
            return

        elif c==etx :
            lxsymbol=eof_sym
            return

        else:
            lxsymbol=error_sym
            return


def printsymbol():
    global psource,lxsptr,lxvalue,lxsymbol,lxsubcode
    name=symbolnames[lxsymbol][:-4]
    if lxsymbol==op_sym or lxsymbol==opto_sym:
        print (name,jtagnames[lxsubcode])
    elif lxsymbol==intconst_sym or lxsymbol==realconst_sym:
        print (name,lxvalue)
    elif lxsymbol==stringconst_sym:
        print (name,"<",lxvalue,">")
    elif lxsymbol==name_sym:
        print (name,lxvalue)
    else:
        print (name)

def readdecdigits(psource,c):
    global lxsptr,lxvalue,lxsymbol,lxsubcode

    if c==".":
        readfraction(psource,intstr)
        return

    i=lxsptr-1
    c=psource[lxsptr]
    while c>="0" and c<="9":
        lxsptr+=1
        c=psource[lxsptr]

    intstr=psource[i:lxsptr]

    if c==".":
        lxsptr+=1
        if psource[lxsptr]!=".":
            readfraction(psource,intstr)
            return
        lxsptr-=1
    elif c in "eE":
        lxsptr+=1
        x=readexpon(psource)
        lxsymbol=realconst_sym
        lxvalue=0
        return

    elif c in "lL":
        lxsptr+=1
        d=psource[lxsptr]
        if d in "lL":
            lxsptr+=1
        elif d in "uU":
            lxsptr+=1

    elif c in "uU":
        lxsptr+=1
        d=psource[lxsptr]
        if d in "lL":
            lxsptr+=1
        elif d in "uU":
            lxsptr+=1

    lxsymbol=intconst_sym
    readdecimal(psource,intstr)

def readfraction(psource,intstr):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    c=psource[lxsptr]
#   if c in "0123456789":
    if c.isdigit():
        f=lxsptr
        lxsptr+=1
        c=psource[lxsptr]
        while c.isdigit():
            lxsptr+=1
            c=psource[lxsptr]

        fractstr=psource[f:lxsptr]

        if c in "eE":
            lxsptr+=1
            expon=readexpon(psource)
        else:
            expon=0

        lxsymbol=realconst_sym
        lxvalue=readreal(intstr,fractstr,expon)

def readhexdigits(psource):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    lxsptr+=1
    i=lxsptr
    c=psource[lxsptr]
    while c in "0123456789ABCDEFabcdef":
        lxsptr+=1
        c=psource[lxsptr]

    intstr=psource[i:lxsptr]
    lxsymbol=intconst_sym
    readhex(intstr)

def readbindigits(psource):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    lxsptr+=1
    i=lxsptr
    c=psource[lxsptr]
    while c in "01":
        lxsptr+=1
        c=psource[lxsptr]

    intstr=psource[i:lxsptr]
    lxsymbol=intconst_sym
    readbin(intstr)


def readstring(psource,termchar):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    start=lxsptr
    s=""

    while 1:
        c=psource[lxsptr]
        lxsptr+=1
        if c=="\\":
            c=psource[lxsptr]
            if c.isupper():
                c.lower()
            lxsptr+=1
            if c=="w":
                s+="\r\n"
            elif c in "cr":
                s+="\r"
            elif c in "ln":
                s+="\n"
            elif c=="t":
                s+="\t"
            elif c=="v":
                s+=chr(11)
            elif c=="a":
                s+=chr(7)
            elif c=="b":
                s+=chr(8)
            elif c=='"Q':
                s+=chr('"')
            elif c=="e":
                s+=chr(26)
            elif c in "z0":
                s+=chr(0)
            elif c=="\\":
                s+="\\"
            elif c=="\'":
                s+="\'"

                s+=chr(0)
            else:
                lxsymbol=error_sym
                return
        elif c in '"\'':
            if c==termchar:
                if psource[lxsptr]==c:
                    lxsptr+=1
                else:
                    break
        elif ord(c) in (13,10,26):
            lxsptr-=1
            lxsymbol=error_sym
            return
        else:
            s+=c
    lxvalue=s
    if termchar=='"':
        lxsymbol=stringconst_sym
    else:
        lxsymbol=charconst_sym

def readlinecomment(psource):
    global lxsptr,lxvalue,lxsymbol,lxsubcode,lxlineno
    lxsptr+=1
    while 1:
        c=psource[lxsptr]
        lxsptr+=1
        if c==cr:
            if psource[lxsptr]==lf:
                lxsptr+=1
            break
        elif c==lf:
            break
        elif c==etx:
            lxsptr-=1
            break
    lxlineno+=1


def readblockcomment(psource):
    global lxsptr,lxvalue,lxsymbol,lxsubcode,lxlineno
    lxsptr+=1
    while 1:
        c=psource[lxsptr]
        lxsptr+=1
        if c==cr:
            if psource[lxsptr]==lf:
                lxsptr+=1
            lxlineno+=1
        elif c==lf:
            lxlineno+=1
        elif c==etx:
            lxsymbol=error_sym
            return
        elif c=="*":
            if psource[lxsptr]=="/":
                lxsptr+=1
                break

def readdecimal(psource,s):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    length=len(s)
    lxvalue=0
    if length<=20:
        for c in s:
            lxvalue=lxvalue*10+ord(c)-48
    lxsymbol=intconst_sym


def readhex(s):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    length=len(s)
    lxvalue=0
    if length<=16:
        for c in s:
            lxvalue=lxvalue*16
            if c.isdigit():
                lxvalue+=ord(c)-ord("0")
            elif c in "ABCDEF":
                lxvalue+=ord(c)-ord("A")+10
            else:
                lxvalue+=ord(c)-ord("a")+10
        lxsymbol=intconst_sym
    else:
        lxsymbol=error_sym

def readbin(s):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    length=len(s)
    lxvalue=0
    if length<=64:
        for c in s:
            lxvalue=lxvalue*2+ord(c)-48
        lxsymbol=intconst_sym
    else:
        lxsymbol=error_sym

def readreal(p,length,expon):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    return 0.0

def readexpon(psource):
    global lxsptr,lxvalue,lxsymbol,lxsubcode
    expon=0
    c=psource[lxsptr]
    while c.isdigit():
        expon=expon*10+ord(c)-48
        lxsptr+=1
        c=psource[lxsptr]
    return expon

def showtable():
#   for d in table:
#       print (d,table[d])
    print (len(table),"unique entries")

print ("starting")

start()
