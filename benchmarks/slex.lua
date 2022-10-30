-- Simple Lexer for 'Q' syntax
-- Based on string codes ("A" instead of 65)

lxsource=""
lxlineno=1
lxvalue=""
lxhash=0
lxindex=0
lxsymbol=0

etx=26
etxstr=string.char(etx)
crstr=string.char(13)
lfstr=string.char(10)

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

-- function sx(s,i)
-- 	return string.byte(string.sub(s,i,i))
-- end

function ss(s,i)
	return string.sub(s,i,i)
end

function printsymbol()
	io.write(lxlineno)
	io.write(" ")

	io.write(symbolnames[lxsymbol])
	if lxvalue ~= "" then
		io.write(" ",lxvalue)
	end
	if lxsymbol==namesym then
		io.write(" ", lxhash)
	end

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
			c=ss(lxsource,lxindex)
			lxindex=lxindex+1
		until c~=('_') and c~=("'")

		if c>=('0') and c<=('9') then
			d=string.byte(c)-string.byte('0')
		elseif c>=('A') and c<=('F') then
			d=string.byte(c)-string.byte('A')+10
		elseif c>=('a') and c<=('f') then
			d=string.byte(c)-string.byte('a')+10
		else
			lxindex=lxindex-1
			break
		end
		if d>=base then
			if c~=('e') and c~=('E') then
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
	if termchar==('"') then
		lxsymbol=stringconstsym
	else
		lxsymbol=charconstsym
	end
	lxvalue=""

	while 1 do
		c=ss(lxsource, lxindex)
		lxindex=lxindex+1

		if c==("\\") then
			c=ss(lxsource, lxindex)
			if c>=('A') and c<=('Z') then
				c=string.char(string.byte(c)+32)
			end
			lxindex=lxindex+1
			if c==('c') or c==('r') then
				c=crstr
			elseif c==('l') or c==('n') then
				c=lfstr
			elseif c==('t') then
				c=string.char(9)
			elseif c==('a') then
				c=string.char(7)
			elseif c==('b') then
				c=string.char(8)
			elseif c==('"') then
				c=('"')
			elseif c==("'") then
				c=("'")
			elseif c==("\\") then
				c=("\\")
			elseif c==("w") then
				lxvalue=lxvalue..crstr
				c=lfstr
			else
				c=('?')
			end
		elseif c==termchar then
			if ss(lxsource,lxindex)==c then
				lxindex=lxindex+1
			else
				break
			end
		elseif c==crstr or c==lfstr or c==etxstr then
			lxindex=lxindex-1
			lxerror("String not terminated")
		end
		lxvalue=lxvalue..(c)
	end
end

function readrawstring()
	lxsymbol=stringconstsym
	lxvalue=""

	while 1 do
		c=ss(lxsource, lxindex)
		lxindex=lxindex+1

		if c==('"') then
			break
		elseif c==crstr or c==lfstr or c==etxstr then
			lxerror("String not terminated")
		end
		lxvalue=lxvalue..(c)
	end
end

function readtoken()
	lxvalue=""

	while 1 do
		c=ss(lxsource,lxindex)

		if c>=('A') and c<=('Z') or
				c>=('a') and c<=('z') or
				c==('$') or c==('_') then

			lxhash=0
			if c>=('A') and c<=('Z') then
				lxvalue=string.char(string.byte(c)+32)
			else
				lxvalue=c
			end

			while 1 do
				lxindex = lxindex+1
				c=ss(lxsource,lxindex)
				if c>=('A') and c<=('Z') then
					lxvalue = lxvalue..string.char(string.byte(c)+32)
-- 					lxhash = lxhash*15+string.byte(c)
				elseif	c>=('a') and c<=('z') or
						c>=('0') and c<=('9') or
						c==('$') or c==('_') then
					lxvalue = lxvalue..c
-- 					lxhash = lxhash*15+string.byte(c)
				else
					break
				end

			end

			if c==('"') and (lxvalue=="f" or lxvalue=="F") then
				lxindex=lxindex+1
				readrawstring()
				return
			end

			lxsymbol=namesym
-- 			lxhash=lxhash*31
			return

		elseif c>=('0') and c<=('9') then
			lxindex=lxindex+1
			d=ss(lxsource,lxindex)

			if d==('x') or d==('X') then
				lxindex=lxindex+1
				if c==('0') then
					readnumber(16)
				elseif c==('2') then
					readnumber(2)
				else
					lxerror("Bad base")
				end
			else
				lxindex=lxindex-1
				readnumber(10)
			end
			return

		elseif c==" " or c=="\t" then
			lxindex=lxindex+1
		elseif c==('!') or c==('#') then
			if c==('!') then
				lxsymbol=eolsym
			else
				lxsymbol=eolsym
			end

			while 1 do
				lxindex=lxindex+1
				c=ss(lxsource,lxindex)
				if c==crstr then
				elseif c==lfstr then
					lxlineno=lxlineno+1
					lxindex=lxindex+1
					break
				elseif c==etxstr then
					break
				end
			end
			lxsymbol=eolsym
			return

-- 		elseif c==('#') then
-- 			lxindex=lxindex+1
-- 			lxsymbol=hashsym
-- 			return

		elseif c==('{') then
			lxindex=lxindex+1
			lxsymbol=lcurlysym
			return

		elseif c==('}') then
			lxindex=lxindex+1
			lxsymbol=rcurlysym
			return
		elseif c==('(') then
			lxindex=lxindex+1
			lxsymbol=lbracksym
			return
		elseif c==(')') then
			lxindex=lxindex+1
			lxsymbol=rbracksym
			return
		elseif c==('[') then
			lxindex=lxindex+1
			lxsymbol=lsqsym
			return
		elseif c==(']') then
			lxindex=lxindex+1
			lxsymbol=rsqsym
			return
		elseif c==('@') then
			lxindex=lxindex+1
			lxsymbol=atsym
			return
		elseif c==('^') then
			lxindex=lxindex+1
			lxsymbol=ptrsym
			return
		elseif c==('|') then
			lxindex=lxindex+1
			lxsymbol=barsym
			return
		elseif c==(',') then
			lxindex=lxindex+1
			lxsymbol=commasym
			return
		elseif c==(':') then
			lxindex=lxindex+1
			if ss(lxsource,lxindex)==('=') then
				lxindex=lxindex+1
				lxsymbol=assignsym
			else
				lxsymbol=colonsym
			end
			return
		elseif c==(';') then
			lxindex=lxindex+1
			lxsymbol=semisym
			return
		elseif c==('/') then
			lxindex=lxindex+1
			lxsymbol=divsym
			return
		elseif c==('%') then
			lxindex=lxindex+1
			lxsymbol=idivsym
			return
		elseif c==('=') then
			lxindex=lxindex+1
			lxsymbol=eqsym
			return
		elseif c==('&') then
			lxindex=lxindex+1
			lxsymbol=addrsym
			return
		elseif c==('?') then
			lxindex=lxindex+1
			lxsymbol=questionsym
			return
		elseif c==('\\') then
			lxindex=lxindex+1

		elseif c==("'") or c==('"') then
			lxindex=lxindex+1
			readstring(c)
			return

		elseif c==('.') then
			lxindex=lxindex+1
			c=ss(lxsource,lxindex)
			if c==('.') then
				lxindex=lxindex+1
				if ss(lxsource,lxindex)==('.') then
					lxindex=lxindex+1
					lxsymbol=ellipsissym
				else
					lxsymbol=rangesym
				end
			else
				lxsymbol=dotsym
			end
			return

		elseif c==('+') then
			lxindex=lxindex+1
			if ss(lxsource,lxindex)==('+') then
				lxindex=lxindex+1
				lxsymbol=incrsym
			else
				lxsymbol=addsym
			end
			return

		elseif c==('-') then
			lxindex=lxindex+1
			if ss(lxsource,lxindex)==('-') then
				lxindex=lxindex+1
				lxsymbol=decrsym
			else
				lxsymbol=subsym
			end
			return

		elseif c==('*') then
			lxindex=lxindex+1
			if ss(lxsource,lxindex)==('*') then
				lxindex=lxindex+1
				lxsymbol=powersym
			else
				lxsymbol=mulsym
			end
			return

		elseif c==('<') then
			lxindex=lxindex+1
			c=ss(lxsource,lxindex)
			if c==('=') then
				lxindex=lxindex+1
				lxsymbol=lesym
			elseif c==('>') then
				lxindex=lxindex+1
				lxsymbol=nesym
			elseif c==('<') then
				lxindex=lxindex+1
				lxsymbol=shlsym
			else
				lxsymbol=ltsym
			end
			return

		elseif c==('>') then
			lxindex=lxindex+1
			c=ss(lxsource,lxindex)
			if c==('=') then
				lxindex=lxindex+1
				lxsymbol=gesym
				lxsymbol=nesym
			elseif c==('>') then
				lxindex=lxindex+1
				lxsymbol=shrsym
			else
				lxsymbol=gtsym
			end
			return

		elseif c==crstr then
			lxindex=lxindex+1
		elseif c==lfstr then
			lxlineno=lxlineno+1
			lxindex=lxindex+1
			lxsymbol=eolsym
			return

		elseif c==etxstr then
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
--   		printsymbol()
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
