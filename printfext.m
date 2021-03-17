!(Extract: extension to my C compiler to enable auto-%? formats
! Missing: full width and other modifiers that need to be passed through, eg. %08? => %08d, %08lld etc
! Missing: overcome hardcoded limit of 512-char format strings)

if d and eqstring(d.name,"printf") and q and q.tag=j_const and
        q.slength<str.len/2 then
    ss:=q.svalue
    tt:=&.str

    u:=q.nextunit
    while c:=ss++^ do
        if c='%' and ss^ in ['?','='] and u then
            if ss^='=' then
                ++ss
                exprstr:=strexpr(u)
                uu:=exprstr.strptr
                convucstring(uu)
                to exprstr.length do
                    tt++^:=uu++^
                od
                tt++^:='='
            fi
            ++ss

            tt++^:='%'
            case ttbasetype[u.mode]
            when tsint then
                tt++^:='d'
            when tsllong then
                tt++^:='l'
                tt++^:='l'
                tt++^:='d'
            when tuint then
                tt++^:='u'
            when tullong then
                tt++^:='l'
                tt++^:='l'
                tt++^:='u'
            when tfloat, tdouble,tldouble then
                tt++^:='f'
            when tref then
                if tttarget[u.mode]=tschar then
                    tt++^:='s'
                else
                    tt++^:='p'
                fi
            else
                tt++^:='?'
            esac
            u:=u.nextunit
        else
            tt++^:=c
        fi
    od
    tt^:=0
    q.svalue:=pcm_copyheapstring(&.str)
    q.slength:=strlen(&.str)
fi
