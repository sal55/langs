global proc lexreadtoken=
    int c,hsum
    ref char sptr, lxsvalue
    int commentseen
    
    nextlx.subcode:=0

    doswitch lxstart:=lxsptr; lxsptr++^
    when 'a'..'z','_','$' then
        lxsvalue:=lxsptr-1
    doname:
        hsum:=lxsvalue^

        sptr:=lxsptr

        docase namemap[c:=sptr++^]
        when 1 then
            hsum:=hsum<<4-hsum+c
        when 2 then
            (sptr-1)^:=c+' '
            hsum:=hsum<<4-hsum+c+' '
        else
            lxsptr:=sptr-1
            exit
        end docase

        if c='"' then
            if lxsvalue+1=ref char(lxsptr) then
                case c:=toupper(lxsvalue^)
                when  'F','R' then 
                    readrawstring()         # raw string like F"..."
                    return
                when  'S','B' then          # data string S"...." (zero-term) or B"..." (binary)
                    readarraystring(c)
                    return
                esac
            fi
        fi

        lookup(lxsvalue, lxsptr-lxsvalue, hashw(hsum))

        return

    when 'A'..'Z' then
        lxsvalue:=lxsptr-1
        lxsvalue^+:=32
        goto doname

    when '0'..'9' then
        lxstart:=lxsptr-1
        case lxsptr^
        when ')',cr,',',' ' then        !assume single digit decimal
            nextlx.symbol:=intconstsym
            nextlx.subcode:=tint
            nextlx.value:=lxstart^-'0'
        when 'x','X' then
            case lxstart^
            when '0' then       !0x
                ++lxsptr
                readhex()
            when '2' then
                ++lxsptr
                readbin()
            else
                lxerror("Bad base")
            esac
        else
            --lxsptr
            readdec()
        esac
        return

    when '!' then           !comment to eol
        docase c:=lxsptr++^
        when cr then
            ++lxsptr
            exit
        when lf then
            exit
        when 0 then
            --lxsptr
            exit
        end
        nextlx.symbol:=eolsym
        return

    when '#' then
        nextlx.symbol:=hashsym
        return

    when '\\' then          !line continuation

!two stages:
! 1: read chars until any eol chars (unless further '\' seen)
! 2: read until non-white space
        commentseen:=0
        docase lxsptr++^            !read until end of this line
        when cr then
!           ++nextlx.pos
            ++lxsptr                !skip lf
            exit
        when lf then
!           ++nextlx.pos
            exit
        when 0 then
            nextlx.symbol:=eofsym
            --lxsptr
            return
        when ' ',tab then
        when '!' then
            commentseen:=1
        else
            if not commentseen then
                lxerror("\\ not followed by eol")
            fi
        end docase
!eol seen: now skip 0 or more further eol chars, plus any white space (ie. multiple blank lines)

        docase lxsptr++^
        when cr then
            ++lxsptr                !skip lf
        when lf then
        when ' ',tab then
        else
            --lxsptr
            exit
        end docase

    when '{' then
        nextlx.symbol:=lcurlysym
        return

    when '}' then
        nextlx.symbol:=rcurlysym
        return

    when '.' then
        case lxsptr^
        when '.' then               !.. or ...
            ++lxsptr
            if lxsptr^='.' then
                ++lxsptr
                nextlx.symbol:=ellipsissym
            else
                nextlx.symbol:=rangesym
                nextlx.subcode:=jmakerange      !helps treat as opsym which all have k-code as subcode
            fi
            return
        elsif lxsptr^ in '0'..'9' then          !real const: deal with this after the switch
            --lxsptr
LXERROR(".123 not done")
!           readrealnumber(nil,0,10)
            return
        else
            nextlx.symbol:=dotsym
            return
        esac

    when ',' then
        nextlx.symbol:=commasym
        return

    when ';' then
        nextlx.symbol:=semisym
        return

    when ':' then
        case lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=assignsym
            nextlx.subcode:=jassign     !helps treat as opsym which all have k-code as subcode
        else
            nextlx.symbol:=colonsym
        esac
        return

    when '(' then
        nextlx.symbol:=lbracksym
        return

    when ')' then
        nextlx.symbol:=rbracksym
        return

    when '[' then
        nextlx.symbol:=lsqsym
        return

    when ']' then
        nextlx.symbol:=rsqsym
        return

    when '|' then
!       if lxsptr^='|' then
!           ++lxsptr
!           nextlx.symbol:=dbarsym
!       else
            nextlx.symbol:=barsym
!       fi
        return

    when '^' then
        nextlx.symbol:=ptrsym
        return

    when '@' then
!       if lxsptr^='@' then
!           ++lxsptr
!           nextlx.symbol:=datsym
!       else
            nextlx.symbol:=atsym
!       fi
        return

    when '?' then
        nextlx.symbol:=questionsym
        return

!   when '~' then
!       nextlx.symbol:=curlsym
!       return

    when '+' then
        nextlx.symbol:=addsym
        if lxsptr^='+' then
            ++lxsptr
            nextlx.symbol:=incrsym
            nextlx.subcode:=kincr
            return
        fi
        return

    when '-' then
        nextlx.symbol:=subsym
        case lxsptr^
        when '-' then
            ++lxsptr
            nextlx.symbol:=incrsym
            nextlx.subcode:=kdecr
            return
        when '>' then
            ++lxsptr
            nextlx.symbol:=pipesym
            return
        esac
        return

    when '*' then
        if lxsptr^='*' then
            ++lxsptr
            nextlx.symbol:=powersym
        else
            nextlx.symbol:=mulsym
        fi
        return

    when '/' then
        nextlx.symbol:=divsym
        return

    when '%' then
        nextlx.symbol:=idivsym
        return

    when '=' then
        case lxsptr^
        when '>' then
            nextlx.symbol:=sendtosym
            ++lxsptr
        when '=' then
            ++lxsptr
            nextlx.symbol:=samesym
        else
            nextlx.symbol:=eqsym
            nextlx.subcode:=keq
        esac
        return

    when '<' then
        nextlx.symbol:=cmpsym
        case lxsptr^
        when '=' then
            ++lxsptr
            nextlx.subcode:=kle
        when '>' then
            ++lxsptr
            nextlx.subcode:=kne
        when '<' then
            ++lxsptr
            nextlx.symbol:=shlsym
        else
            nextlx.subcode:=klt
        esac
        return

    when '>' then
        nextlx.symbol:=cmpsym
        case lxsptr^
        when '=' then
            ++lxsptr
            nextlx.symbol:=cmpsym
            nextlx.subcode:=kge
        when '>' then
            ++lxsptr
            nextlx.symbol:=shrsym
        else
            nextlx.symbol:=cmpsym
            nextlx.subcode:=kgt
        esac
        return

    when '&' then
        case lxsptr^
            when '&' then
            ++lxsptr
            nextlx.symbol:=daddrsym
            nextlx.subcode:=jdaddrvv
        when '.' then
            ++lxsptr
            nextlx.symbol:=anddotsym
            nextlx.subcode:=0
        else
            nextlx.symbol:=addrsym
            nextlx.subcode:=jaddrof
        esac
        return

    when '\'' then
        lxreadstring('\'')
        return

    when '"' then
        lxreadstring('"')
        return

    when '`' then
        readrawxname()
        return

    when ' ',tab then

    when cr then
        ++lxsptr                !skip lf
        nextlx.symbol:=eolsym
        return

    when lf then            !only lfs not preceded by cr
        nextlx.symbol:=eolsym
        return

    when 0 then
        if sourcelevel then
            unstacksource()
            RETURN
        else
            nextlx.symbol:=eofsym
            --lxsptr
            return
        fi

    else
        lxerror("Unknown char")
!       nextlx.symbol:=errorsym
        return

    end doswitch

end
