import sys
import time

lxsource=""
lxlineno=1
lxvalue=""
lxhash=0
lxindex=0
lxsymbol=0

etx=26
etxstr=chr(etx)
cr=13
lf=10

dummysym = 0
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
powersym = 42

symbolnames = ("dummy","errorsym","dotsym","commasym","semisym","colonsym","assignsym",
	"addsym","subsym","mulsym","divsym","idivsym","eqsym","nesym","ltsym",
	"lesym","gesym","gtsym","shlsym","shrsym","lbracksym","rbracksym","lsqsym",
	"rsqsym","lcurlysym","rcurlysym","addrsym","ptrsym","ellipsissym","rangesym",
	"barsym","questionsym","atsym","eolsym","eofsym","hashsym","incrsym",
	"decrsym","namesym","intconstsym","charconstsym","stringconstsym","powersym")

def printsymbol():
	global lxsource, lxlineno, lxvalue, lxindex, lxsymbol
#	print("SYM=",lxsymbol, len(symbolnames))
	print(lxlineno, symbolnames[lxsymbol])
	if lxvalue != "":
		print(" ",lxvalue)
	#end
#end

def lxerror(mess):
	global lxsource, lxlineno, lxvalue, lxindex, lxsymbol
	print("Error on line " + str(lxlineno)+": "+mess)
	exit()
#end

def readnumber(base):
	global lxsource, lxlineno, lxvalue, lxindex, lxsymbol
	lxvalue=0

	while 1:
		while 1:
			c=ord(lxsource[lxindex])
			lxindex=lxindex+1
			if c!=ord('_') and c!=ord("'"): break

		if c>=ord('0') and c<=ord('9'):
			d=c-ord('0')
		elif c>=ord('A') and c<=ord('F'):
			d=c-(ord('A')-10)
		elif c>=ord('a') and c<=ord('f'):
			d=c-(ord('a')-10)
		else:
			lxindex=lxindex-1
			break
		#end
		if d>=base:
			if c!=ord('e') and c!=ord('E'):
				lxerror("Bad Digit")
			else:
				lxindex=lxindex-1
			#end
			break
		#end
		lxvalue=lxvalue*base+d
	#end
	lxsymbol=intconstsym
#end

def readstring(termchar):
	global lxsource, lxlineno, lxvalue, lxindex, lxsymbol
	if termchar==ord('"'):
		lxsymbol=stringconstsym
	else:
		lxsymbol=charconstsym
	#end
	lxvalue=""

	while 1:
		c=ord(lxsource[lxindex])
		lxindex=lxindex+1

		if c==ord("\\"):
			c=ord(lxsource[lxindex])
			if c>=ord('A') and c<=ord('Z'):
				c=c+32
			#end
			lxindex=lxindex+1
			if c==ord('c') or c==ord('r'):
				c=cr
			elif c==ord('l') or c==ord('n'):
				c=lf
			elif c==ord('t'):
				c=9
			elif c==ord('a'):
				c=7
			elif c==ord('b'):
				c=8
			elif c==ord('"'):
				c=ord('"')
			elif c==ord("'"):
				c=ord("'")
			elif c==ord("\\"):
				c=ord("\\")
			elif c==ord("w"):
				lxvalue=lxvalue+chr(cr)
				c=lf
			else:
				c=ord('?')
			#end
		elif c==termchar:
			if ord(lxsource[lxindex])==c:
				lxindex=lxindex+1
			else:
				break
			#end
		elif c==cr or c==lf or c==etx:
			lxindex=lxindex-1
			lxerror("String not terminated")
		#end
		lxvalue=lxvalue+chr(c)
	#end
#end

def readrawstring():
	global lxsource, lxlineno, lxvalue, lxindex, lxsymbol
	lxsymbol=stringconstsym
	lxvalue=""

	while 1:
		c=ord(lxsource[lxindex])
		lxindex=lxindex+1

		if c==ord('"'):
			break
		elif c==cr or c==lf or c==etx:
			lxerror("String not terminated")
		#end
		lxvalue=lxvalue+chr(c)
	#end
#end

def readtoken():
	global lxsource, lxlineno, lxvalue, lxindex, lxsymbol
	lxvalue=""

	while 1:
		c=ord(lxsource[lxindex])

		if c>=ord('A') and c<=ord('Z') or\
				c>=ord('a') and c<=ord('z') or\
				c==ord('$') or c==ord('_'):

			lxhash=0
			if c>=ord('A') and c<=ord('Z'):
				lxvalue=chr(c+32)
			else:
				lxvalue=chr(c)
			#end

			while 1:
				lxindex = lxindex+1
				c=ord(lxsource[lxindex])
				if c>=ord('A') and c<=ord('Z'):
					lxvalue = lxvalue+chr(c+32)
# 					lxhash = lxhash*15+c
				elif	c>=ord('a') and c<=ord('z') or\
						c>=ord('0') and c<=ord('9') or\
						c==ord('$') or c==ord('_'):
					lxvalue = lxvalue+chr(c)
# 					lxhash = lxhash*15+c
				else:
					break
				#end

			#end

			if c==ord('"') and (lxvalue=="f" or lxvalue=="F"):
				lxindex=lxindex+1
				readrawstring()
				return
			#end

			lxsymbol=namesym
# 			lxhash=lxhash*31
			return

		elif c>=ord('0') and c<=ord('9'):
			lxindex=lxindex+1
			d=ord(lxsource[lxindex])

			if d==ord('x') or d==ord('X'):
				lxindex=lxindex+1
				if c==ord('0'):
					readnumber(16)
				elif c==ord('2'):
					readnumber(2)
				else:
					lxerror("Bad base")
				#end
			else:
				lxindex=lxindex-1
				readnumber(10)
			#end
			return

		elif c==32 or c==9:
			lxindex=lxindex+1
		elif c==ord('!') or c==ord('#'):
			if c==ord('!'):
				lxsymbol=eolsym
			else:
				lxsymbol=eolsym
			#end

			while 1:
				lxindex=lxindex+1
				c=ord(lxsource[lxindex])
				if c==cr:
					pass
				elif c==lf:
					lxlineno=lxlineno+1
					lxindex=lxindex+1
					break
				elif c==etx:
					break
				#end
			#end
			lxsymbol=eolsym
			return

		elif c==ord('{'):
			lxindex=lxindex+1
			lxsymbol=lcurlysym
			return

		elif c==ord('}'):
			lxindex=lxindex+1
			lxsymbol=rcurlysym
			return
		elif c==ord('('):
			lxindex=lxindex+1
			lxsymbol=lbracksym
			return
		elif c==ord(')'):
			lxindex=lxindex+1
			lxsymbol=rbracksym
			return
		elif c==ord('['):
			lxindex=lxindex+1
			lxsymbol=lsqsym
			return
		elif c==ord(']'):
			lxindex=lxindex+1
			lxsymbol=rsqsym
			return
		elif c==ord('@'):
			lxindex=lxindex+1
			lxsymbol=atsym
			return
		elif c==ord('^'):
			lxindex=lxindex+1
			lxsymbol=ptrsym
			return
		elif c==ord('|'):
			lxindex=lxindex+1
			lxsymbol=barsym
			return
		elif c==ord(','):
			lxindex=lxindex+1
			lxsymbol=commasym
			return
		elif c==ord(':'):
			lxindex=lxindex+1
			if ord(lxsource[lxindex])==ord('='):
				lxindex=lxindex+1
				lxsymbol=assignsym
			else:
				lxsymbol=colonsym
			#end
			return
		elif c==ord(';'):
			lxindex=lxindex+1
			lxsymbol=semisym
			return
		elif c==ord('/'):
			lxindex=lxindex+1
			lxsymbol=divsym
			return
		elif c==ord('%'):
			lxindex=lxindex+1
			lxsymbol=idivsym
			return
		elif c==ord('='):
			lxindex=lxindex+1
			lxsymbol=eqsym
			return
		elif c==ord('&'):
			lxindex=lxindex+1
			lxsymbol=addrsym
			return
		elif c==ord('?'):
			lxindex=lxindex+1
			lxsymbol=questionsym
			return
		elif c==ord('\\'):
			lxindex=lxindex+1

		elif c==ord("'") or c==ord('"'):
			lxindex=lxindex+1
			readstring(c)
			return

		elif c==ord('.'):
			lxindex=lxindex+1
			c=ord(lxsource[lxindex])
			if c==ord('.'):
				lxindex=lxindex+1
				if ord(lxsource[lxindex])==ord('.'):
					lxindex=lxindex+1
					lxsymbol=ellipsissym
				else:
					lxsymbol=rangesym
				#end
			else:
				lxsymbol=dotsym
			#end
			return

		elif c==ord('+'):
			lxindex=lxindex+1
			if ord(lxsource[lxindex])==ord('+'):
				lxindex=lxindex+1
				lxsymbol=incrsym
			else:
				lxsymbol=addsym
			#end
			return

		elif c==ord('-'):
			lxindex=lxindex+1
			if ord(lxsource[lxindex])==ord('-'):
				lxindex=lxindex+1
				lxsymbol=decrsym
			else:
				lxsymbol=subsym
			#end
			return

		elif c==ord('*'):
			lxindex=lxindex+1
			if ord(lxsource[lxindex])==ord('*'):
				lxindex=lxindex+1
				lxsymbol=powersym
			else:
				lxsymbol=mulsym
			#end
			return

		elif c==ord('<'):
			lxindex=lxindex+1
			c=ord(lxsource[lxindex])
			if c==ord('='):
				lxindex=lxindex+1
				lxsymbol=lesym
			elif c==ord('>'):
				lxindex=lxindex+1
				lxsymbol=nesym
			elif c==ord('<'):
				lxindex=lxindex+1
				lxsymbol=shlsym
			else:
				lxsymbol=ltsym
			#end
			return

		elif c==ord('>'):
			lxindex=lxindex+1
			c=ord(lxsource[lxindex])
			if c==ord('='):
				lxindex=lxindex+1
				lxsymbol=gesym
				lxsymbol=nesym
			elif c==ord('>'):
				lxindex=lxindex+1
				lxsymbol=shrsym
			else:
				lxsymbol=gtsym
			#end
			return

		elif c==cr:
			lxindex=lxindex+1
		elif c==lf:
			lxlineno=lxlineno+1
			lxindex=lxindex+1
			lxsymbol=eolsym
			return

		elif c==etx:
			lxsymbol=eofsym
			break
		else:
			lxerror("Unknown token")
		#end
	#end
#end

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

def readsource(file):
	global lxsource, lxlineno, lxvalue, lxindex, lxsymbol
	lxsource=readstrfile(file)
	if lxsource==0:
		lxerror("Can't open file:"+file)
	#end
	lxindex=0
	lxlineno=1
	lxsource = lxsource+etxstr
#end

def start():
	global lxsource, lxlineno, lxvalue, lxindex, lxsymbol
	if len(sys.argv)>=2:
		infile=sys.argv[1]
	else:
		infile="input"

	print("Tokenising: "+infile+"...")
	readsource(infile)

	ntokens=0
#	t=time.clock()
	t=time.process_time()
	lxsymbol=0

	while lxsymbol!=eofsym:
		readtoken()
#		printsymbol()
		ntokens=ntokens+1

#	t=time.clock()-t
	t=time.process_time()-t

	nlines=lxlineno
	nchars=len(lxsource)-1

	if t:
		print("")
		print("Lines:",nlines)
		print("Tokens:",ntokens)
		print("Chars:",nchars)
		print("")
		print(lxlineno/t,"Lines per second")
# 	print(ntokens/t,"Tokens per second")
# 	print(nchars/t,"Chars per second")

	print("")

	print("Time:",t)

#end

start()
