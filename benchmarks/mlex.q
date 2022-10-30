!Simple Lexer for Q source code
!For benchmark purposes only

tabledata() symbolnames=
    (errorsym,          $),
    (dotsym,            $),
    (commasym,          $),
    (semisym,           $),
    (colonsym,          $),
    (assignsym,         $),
    (addsym,            $),
    (subsym,            $),
    (mulsym,            $),
    (divsym,            $),
    (idivsym,           $),

    (eqsym,             $),
    (nesym,             $),
    (ltsym,             $),
    (lesym,             $),
    (gesym,             $),
    (gtsym,             $),
    (shlsym,            $),
    (shrsym,            $),
    (powersym,          $),

    (lbracksym,         $),
    (rbracksym,         $),
    (lsqsym,            $),
    (rsqsym,            $),
    (lcurlysym,         $),
    (rcurlysym,         $),
    (addrsym,           $),
    (ptrsym,            $),
    (ellipsissym,       $),
    (rangesym,          $),
    (barsym,            $),
    (questionsym,       $),
    (atsym,             $),

    (eolsym,            $),
    (eofsym,            $),
    (hashsym,           $),
    (incrsym,           $),
    (decrsym,           $),
    (namesym,           $),

    (intconstsym,       $),
    (charconstsym,      $),
    (stringconstsym,    $),
end

var lxlineno
var lxhash
var lxsptr

var lxsymbol
var lxvalue

const cr    = 13
const lf    = 10
const etx   = 26

proc start=
    if ncmdparams>=1 then
        infile:=cmdparams[1]
    else
        infile:="input"
    fi

    psource:=readbinfile(infile)

    if psource=0 then
        println "Can't open",infile
        stop
    fi

    psource append:=etx
    nchars:=nlines:=ntokens:=0

    lxsptr:=makeref(&psource[1],byte)
    lxlineno:=1
    t:=ticks()

    repeat
        readtoken()
        ++ntokens
!       printsymbol()
    until lxsymbol=eofsym

    t:=ticks()-t
    tsecs:=t/1000.0


    nlines+:=lxlineno
    nchars+:=psource.len

    println
    println "Lines:",nlines
    println "Tokens:",ntokens
    println "Chars:",nchars-1

    println

    println nlines/tsecs,"Lines per second"
    println ntokens/tsecs,"Tokens per second"
    println real(nchars)/tsecs,"Chars per second"

    println
    println tsecs," Seconds"
    println
end

proc readtoken=
    lxvalue::=""

!    docase c:=lxsptr^
    doswitch c:=lxsptr^
    when 'a'..'z','$','_' then
        lxvalue::=chr(c)
doname:
!       hsum:=0

        doswitch c:=(++lxsptr)^
        when 'A'..'Z' then
            lxvalue+:=c+' '
!           hsum:=hsum<<4-hsum+c+' '
        when 'a'..'z','0'..'9','_','$' then
            lxvalue+:=c
!           hsum:=hsum<<4-hsum+c
        else
            exit
        end doswitch

        if c='"' and lxvalue in "fF" then
            ++lxsptr
            readrawstring()
            return
        fi

        lxsymbol:=namesym
!       lxhash:=hsum*31
        return

    when 'A'..'Z' then
        lxvalue::=chr(c)
        goto doname

    when '0'..'9' then
        ++lxsptr
        case lxsptr^
        when 'x','X' then
            ++lxsptr
            case c
            when '0' then
                readnumber(16)
            when '2' then
                readnumber(2)
            else
                lxerror("Bad base")
            esac
        else
            --lxsptr
            readnumber(10)
        esac
        return

    when '!' then           !comment to eol
        lxsymbol:=eolsym
docomment:
        doswitch c:=(++lxsptr)^
        when 13 then
        when 10,0 then
            ++lxlineno
            ++lxsptr
            exit
        end

        return

    when '#' then       !(can be used as docstring to eol)
        ++lxsptr
        lxsymbol:=hashsym
        goto docomment

    when '{' then
        ++lxsptr
        lxsymbol:=lcurlysym
        return

    when '}' then
        ++lxsptr
        lxsymbol:=rcurlysym
        return

    when '.' then
        ++lxsptr
        if lxsptr^='.' then
            ++lxsptr
            if lxsptr^='.' then
                ++lxsptr
                lxsymbol:=ellipsissym
            else
                lxsymbol:=rangesym
            fi
        else
            lxsymbol:=dotsym
        fi
        return

    when ',' then
        ++lxsptr
        lxsymbol:=commasym
        return

    when ';' then
        ++lxsptr
        lxsymbol:=semisym
        return

    when ':' then
        ++lxsptr
        if lxsptr^='=' then
            ++lxsptr
            lxsymbol:=assignsym
        else
            lxsymbol:=colonsym
        fi
        return

    when '(' then
        ++lxsptr
        lxsymbol:=lbracksym
        return

    when ')' then
        ++lxsptr
        lxsymbol:=rbracksym
        return

    when '[' then
        ++lxsptr
        lxsymbol:=lsqsym
        return

    when ']' then
        ++lxsptr
        lxsymbol:=rsqsym
        return

    when '|' then
        ++lxsptr
        lxsymbol:=barsym
        return

    when '^' then
        ++lxsptr
        lxsymbol:=ptrsym
        return

    when '@' then
        ++lxsptr
        lxsymbol:=atsym
        return

    when '+' then
        ++lxsptr
        if lxsptr^='+' then
            ++lxsptr
            lxsymbol:=incrsym
        else
            lxsymbol:=addsym
        fi
        return

    when '-' then
        ++lxsptr
        if lxsptr^='-' then
            ++lxsptr
            lxsymbol:=decrsym
        else
            lxsymbol:=subsym
        fi
        return

    when '*' then
        ++lxsptr
        if lxsptr^='*' then
            ++lxsptr
            lxsymbol:=powersym
        else
            lxsymbol:=mulsym
        fi
        return
        return

    when '/' then
        ++lxsptr
        lxsymbol:=divsym
        return

    when '%' then
        ++lxsptr
        lxsymbol:=idivsym
        return

    when '=' then
        ++lxsptr
        lxsymbol:=eqsym
        return

    when '<' then
        switch (++lxsptr)^
        when '=' then
            ++lxsptr
            lxsymbol:=lesym
        when '>' then
            ++lxsptr
            lxsymbol:=nesym
        when '<' then
            ++lxsptr
            lxsymbol:=shlsym
        else
            lxsymbol:=ltsym
        endswitch
        return

    when '>' then
        switch (++lxsptr)^
        when '=' then
            ++lxsptr
            lxsymbol:=gesym
        when '>' then
            ++lxsptr
            lxsymbol:=shrsym
        else
            lxsymbol:=gtsym
        endswitch
        return

    when '&' then
        ++lxsptr
        lxsymbol:=addrsym
        return

    when '\'' then
        ++lxsptr
        readstring('\'')
        return

    when '"' then
        ++lxsptr
        readstring('"')
        return

    when '?' then
        ++lxsptr
        lxsymbol:=questionsym
        return

    when '\\' then
        ++lxsptr

    when ' ','\t' then
        ++lxsptr

    when cr then
        ++lxsptr

    when lf then
        ++lxlineno
        ++lxsptr
        lxsymbol:=eolsym
        return

    when etx,0 then
        lxsymbol:=eofsym
        return

    else
        lxerror("Unknown token")

!    end doswitch
    end
end

proc printsymbol=
    print lxlineno,symbolnames[lxsymbol],"  "

    if lxvalue<>"" then
        print lxvalue
    fi
    if lxsymbol=namesym then
        print " ",lxhash
    fi
    println
end

proc readnumber(base)=
    lxvalue:=word(0)

    do
        switch c:=lxsptr++^
        when '_','\'' then
            next
        when '0'..'9' THEN
            d:=c-'0'
        when 'A'..'F' then
            d:=c-('A'-10)
        when 'a'..'f' then
            d:=c-('a'-10)
        else
            --lxsptr
            exit
        end
        if d>=base then
            if c not in ['e','E'] then
				lxerror("Bad digit")
			else		
				--lxsptr
			fi
			exit
		fi
		lxvalue:=lxvalue*word(base)+word(d)
	od

	lxsymbol:=intconstsym
end

proc readstring(termchar)=
	lxsymbol:=(termchar='"'|stringconstsym|charconstsym)
	lxvalue::=""

	do
		switch c:=lxsptr++^
		when '\\' then			!escape char
			c:=lxsptr^
			if c in 'A'..'Z' then c+:=' ' fi

			++lxsptr
			switch c
			when 'c','r' then
				c:=cr
			when 'l','n' then
				c:=lf
			when 't' then
				c:=9
			when 'a' then
				c:=7
			when 'b' then
				c:=8
			when '"' then
				c:='"'
			when '\\' then
				c:='\\'
			when '\'' then
				c:='\''
			when 'w' then
				lxvalue+:=chr(cr)
				c:=lf
			else
				c:='?'
			end
		when '"','\'' then
			if c=termchar then
				if lxsptr^=c then
					++lxsptr
				else
					exit
				fi
			fi
		when cr,lf,etx,0 then
			--lxsptr
			lxerror("String not terminated")
		endswitch
		lxvalue+:=c
	od
end

proc readrawstring=
	lxsymbol:=stringconstsym
	lxvalue::=""

	do
		switch c:=lxsptr++^
		when '"' then
			exit
		when cr,lf,etx,0 then
			--lxsptr
			lxerror("String not terminated")
		endswitch
		lxvalue+:=c
	od
end

proc lxerror(mess)=
	println "Error on line",lxlineno,":",mess
	println
	println
	stop 1
end
