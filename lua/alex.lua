-- Simple Lexer for 'Q' syntax
-- Based on ASCII codes (65 (ie. string.byte('A') here) instead of 'A')

lxsource=""
lxlineno=1
lxvalue=""
lxhash=0
lxindex=0
lxsymbol=0

etx=26
etxstr=string.char(etx)
cr=13
lf=10

errorsym = 1
dotsym = 2
commasym = 3
semisym = 4
colonsym = 5
assignsym = 6
addsym = 7
subsym = 8
mulsym = 9
divsym = 10
idivsym = 11
eqsym = 12
nesym = 13
ltsym = 14
lesym = 15
gesym = 16
gtsym = 17
shlsym = 18
shrsym = 19
lbracksym = 20
rbracksym = 21
lsqsym = 22
rsqsym = 23
lcurlysym = 24
rcurlysym = 25
addrsym = 26
ptrsym = 27
ellipsissym = 28
rangesym = 29
barsym = 30
questionsym = 31
atsym = 32
eolsym = 33
eofsym = 34
hashsym = 35
incrsym = 36
decrsym = 37
namesym = 38
intconstsym = 39
charconstsym = 40
stringconstsym = 41

symbolnames = {"errorsym","dotsym","commasym","semisym","colonsym","assignsym",
    "addsym","subsym","mulsym","divsym","idivsym","eqsym","nesym","ltsym",
    "lesym","gesym","gtsym","shlsym","shrsym","lbracksym","rbracksym","lsqsym",
    "rsqsym","lcurlysym","rcurlysym","addrsym","ptrsym","ellipsissym","rangesym",
    "barsym","questionsym","atsym","eolsym","eofsym","hashsym","incrsym",
    "decrsym","namesym","intconstsym","charconstsym","stringconstsym"}

function sx(s,i)
    return string.byte(string.sub(s,i,i))
end

function printsymbol()
    io.write(lxlineno)
    io.write(" ")

    io.write(symbolnames[lxsymbol])
    if lxvalue ~= "" then
        io.write(" ",lxvalue)
    end
--  if lxsymbol==namesym then
--      io.write(" ", lxhash)
--  end

    io.write("\n")
end

function lxerror(mess)
    print("Error on line " .. lxlineno..": "..mess)
    os.exit()
end

function readnumber(base)
    lxvalue=0

    while 1 do
        repeat
            c=sx(lxsource,lxindex)
            lxindex=lxindex+1
        until c~=string.byte('_') and c~=string.byte("'")

        if c>=string.byte('0') and c<=string.byte('9') then
            d=c-string.byte('0')
        elseif c>=string.byte('A') and c<=string.byte('F') then
            d=c-string.byte('a')+10
        elseif c>=string.byte('a') and c<=string.byte('f') then
            d=c-string.byte('a')+10
        else
            lxindex=lxindex-1
            break
        end
        if d>=base then
            if c~=string.byte('e') and c~=string.byte('E') then
                lxerror("Bad Digit")
            else
                lxindex=lxindex-1
            end
            break
        end
        lxvalue=lxvalue*base+d
    end
    lxsymbol=intconstsym
end

function readstring(termchar)
    if termchar==string.byte('"') then
        lxsymbol=stringconstsym
    else
        lxsymbol=charconstsym
    end
    lxvalue=""

    while 1 do
        c=sx(lxsource, lxindex)
        lxindex=lxindex+1

        if c==string.byte("\\") then
            c=sx(lxsource, lxindex)
            if c>=string.byte('A') and c<=string.byte('Z') then
                c=c+32
            end
            lxindex=lxindex+1
            if c==string.byte('c') or c==string.byte('r') then
                c=cr
            elseif c==string.byte('l') or c==string.byte('n') then
                c=lf
            elseif c==string.byte('t') then
                c=9
            elseif c==string.byte('a') then
                c=7
            elseif c==string.byte('b') then
                c=8
            elseif c==string.byte('"') then
                c=string.byte('"')
            elseif c==string.byte("'") then
                c=string.byte("'")
            elseif c==string.byte("\\") then
                c=string.byte("\\")
            elseif c==string.byte("w") then
                lxvalue=lxvalue..string.char(cr)
                c=lf
            else
                c=string.byte('?')
            end
        elseif c==termchar then
            if sx(lxsource,lxindex)==c then
                lxindex=lxindex+1
            else
                break
            end
        elseif c==cr or c==lf or c==etx then
            lxindex=lxindex-1
            lxerror("String not terminated")
        end
        lxvalue=lxvalue..string.char(c)
    end
end

function readrawstring()
    lxsymbol=stringconstsym
    lxvalue=""

    while 1 do
        c=sx(lxsource, lxindex)
        lxindex=lxindex+1

        if c==string.byte('"') then
            break
        elseif c==cr or c==lf or c==etx then
            lxerror("String not terminated")
        end
        lxvalue=lxvalue..string.char(c)
    end
end

function readtoken()
    lxvalue=""

    while 1 do
        c=sx(lxsource,lxindex)

        if c>=string.byte('A') and c<=string.byte('Z') or
                c>=string.byte('a') and c<=string.byte('z') or
                c==string.byte('$') or c==string.byte('_') then

            lxhash=0
            if c>=string.byte('A') and c<=string.byte('Z') then
                lxvalue=string.char(c+32)
            else
                lxvalue=string.char(c)
            end

            while 1 do
                lxindex = lxindex+1
                c=sx(lxsource,lxindex)
                if c>=string.byte('A') and c<=string.byte('Z') then
                    lxvalue = lxvalue..string.char(c+32)
--                  lxhash = lxhash*15+c
                elseif  c>=string.byte('a') and c<=string.byte('z') or
                        c>=string.byte('0') and c<=string.byte('9') or
                        c==string.byte('$') or c==string.byte('_') then
                    lxvalue = lxvalue..string.char(c)
--                  lxhash = lxhash*15+c
                else
                    break
                end

            end

            if c==string.byte('"') and (lxvalue=="f" or lxvalue=="F") then
                lxindex=lxindex+1
                readrawstring()
                return
            end

            lxsymbol=namesym
--          lxhash=lxhash*31
            return

        elseif c>=string.byte('0') and c<=string.byte('9') then
            lxindex=lxindex+1
            d=sx(lxsource,lxindex)

            if d==string.byte('x') or d==string.byte('X') then
                lxindex=lxindex+1
                if c==string.byte('0') then
                    readnumber(16)
                elseif c==string.byte('2') then
                    readnumber(2)
                else
                    lxerror("Bad base")
                end
            else
                lxindex=lxindex-1
                readnumber(10)
            end
            return

        elseif c==32 or c==9 then
            lxindex=lxindex+1
        elseif c==string.byte('!') or c==string.byte('#') then
            if c==string.byte('!') then
                lxsymbol=eolsym
            else
                lxsymbol=eolsym
            end

            while 1 do
                lxindex=lxindex+1
                c=sx(lxsource,lxindex)
                if c==cr then
                elseif c==lf then
                    lxlineno=lxlineno+1
                    lxindex=lxindex+1
                    break
                elseif c==etx then
                    break
                end
            end
            lxsymbol=eolsym
            return

        elseif c==string.byte('{') then
            lxindex=lxindex+1
            lxsymbol=lcurlysym
            return

        elseif c==string.byte('}') then
            lxindex=lxindex+1
            lxsymbol=rcurlysym
            return
        elseif c==string.byte('(') then
            lxindex=lxindex+1
            lxsymbol=lbracksym
            return
        elseif c==string.byte(')') then
            lxindex=lxindex+1
            lxsymbol=rbracksym
            return
        elseif c==string.byte('[') then
            lxindex=lxindex+1
            lxsymbol=lsqsym
            return
        elseif c==string.byte(']') then
            lxindex=lxindex+1
            lxsymbol=rsqsym
            return
        elseif c==string.byte('@') then
            lxindex=lxindex+1
            lxsymbol=atsym
            return
        elseif c==string.byte('^') then
            lxindex=lxindex+1
            lxsymbol=ptrsym
            return
        elseif c==string.byte('|') then
            lxindex=lxindex+1
            lxsymbol=barsym
            return
        elseif c==string.byte(',') then
            lxindex=lxindex+1
            lxsymbol=commasym
            return
        elseif c==string.byte(':') then
            lxindex=lxindex+1
            if sx(lxsource,lxindex)==string.byte('=') then
                lxindex=lxindex+1
                lxsymbol=assignsym
            else
                lxsymbol=colonsym
            end
            return
        elseif c==string.byte(';') then
            lxindex=lxindex+1
            lxsymbol=semisym
            return
        elseif c==string.byte('/') then
            lxindex=lxindex+1
            lxsymbol=divsym
            return
        elseif c==string.byte('%') then
            lxindex=lxindex+1
            lxsymbol=idivsym
            return
        elseif c==string.byte('=') then
            lxindex=lxindex+1
            lxsymbol=eqsym
            return
        elseif c==string.byte('&') then
            lxindex=lxindex+1
            lxsymbol=addrsym
            return
        elseif c==string.byte('?') then
            lxindex=lxindex+1
            lxsymbol=questionsym
            return
        elseif c==string.byte('\\') then
            lxindex=lxindex+1

        elseif c==string.byte("'") or c==string.byte('"') then
            lxindex=lxindex+1
            readstring(c)
            return

        elseif c==string.byte('.') then
            lxindex=lxindex+1
            c=sx(lxsource,lxindex)
            if c==string.byte('.') then
                lxindex=lxindex+1
                if sx(lxsource,lxindex)==string.byte('.') then
                    lxindex=lxindex+1
                    lxsymbol=ellipsissym
                else
                    lxsymbol=rangesym
                end
            else
                lxsymbol=dotsym
            end
            return

        elseif c==string.byte('+') then
            lxindex=lxindex+1
            if sx(lxsource,lxindex)==string.byte('+') then
                lxindex=lxindex+1
                lxsymbol=incrsym
            else
                lxsymbol=addsym
            end
            return

        elseif c==string.byte('-') then
            lxindex=lxindex+1
            if sx(lxsource,lxindex)==string.byte('-') then
                lxindex=lxindex+1
                lxsymbol=decrsym
            else
                lxsymbol=subsym
            end
            return

        elseif c==string.byte('*') then
            lxindex=lxindex+1
            if sx(lxsource,lxindex)==string.byte('*') then
                lxindex=lxindex+1
                lxsymbol=powersym
            else
                lxsymbol=mulsym
            end
            return

        elseif c==string.byte('<') then
            lxindex=lxindex+1
            c=sx(lxsource,lxindex)
            if c==string.byte('=') then
                lxindex=lxindex+1
                lxsymbol=lesym
            elseif c==string.byte('>') then
                lxindex=lxindex+1
                lxsymbol=nesym
            elseif c==string.byte('<') then
                lxindex=lxindex+1
                lxsymbol=shlsym
            else
                lxsymbol=ltsym
            end
            return

        elseif c==string.byte('>') then
            lxindex=lxindex+1
            c=sx(lxsource,lxindex)
            if c==string.byte('=') then
                lxindex=lxindex+1
                lxsymbol=gesym
                lxsymbol=nesym
            elseif c==string.byte('>') then
                lxindex=lxindex+1
                lxsymbol=shrsym
            else
                lxsymbol=gtsym
            end
            return

        elseif c==cr then
            lxindex=lxindex+1
        elseif c==lf then
            lxlineno=lxlineno+1
            lxindex=lxindex+1
            lxsymbol=eolsym
            return

        elseif c==etx then
            lxsymbol=eofsym
            break
        else
            lxerror("Unknown token")
        end
    end
end

function read_file(path)
    local file = io.open(path, "rb")
    if not file then return nil end
    local content = file:read "*a"
    file:close()
    return content
end

function readsource(file)
    lxsource=read_file(file)
    if lxsource==nil then
        lxerror("Can't open file:"..file)
    end
    lxindex=1
    lxlineno=1
    lxsource = lxsource..etxstr
end

function start()
    if arg[1]==nil then
        infile="input"
    else
        infile=arg[1]
    end

    print("Tokenising: "..infile.."...")
    readsource(infile)

    ntokens=0
    t=os.clock()

    repeat
        readtoken()
--          printsymbol()
        ntokens=ntokens+1
    until lxsymbol==eofsym

    t=os.clock()-t

    nlines=lxlineno
    nchars=string.len(lxsource)-1

    print("")
    print("Lines:",nlines)
    print("Tokens:",ntokens)
    print("Chars:",nchars)
    print("")
    print(lxlineno/t,"Lines per second")
    print(ntokens/t,"Tokens per second")
    print(nchars/t,"Chars per second")

    print("")

    print("Time:",t)

end

start()
