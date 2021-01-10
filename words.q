proc start=
    t:="bartsimpson"; words:=("imp", "n","so", "rt", "ba","lisa","s")
    result:=cancon(t,words)
    fprintln "cancon(#,#) = #",t,words, result

end

function cancon(t, words)=
    if t="" then return (".",) fi

    forall i,w in words do
        if startswith(t, w) then
            rest:=rightstr(t,-w.len)
            a:=(w,)
            b:=cancon(rest, words)
            if b then
                return a concat b
            fi
        fi
    od
    return ()
end

function startswith(s, t)=
!return 1 when s starts with t
    if s.len=0 or t.len=0 then return 0 fi
    if t.len>s.len then return 0 fi

    return leftstr(s,t.len)=t
end
