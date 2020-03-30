var q=new(list,0..20631)
var carry  = 36243678541u
var xcng   = 12367890123456u
var xs     = 521288629546311u
var indx   = Q.len

function refill =
    for i in Q do
            h := carry iand 1
            z := (Q[i]<<41)>>1 + (Q[i]<<39)>>1 + carry>>1
            carry :=  Q[i]>>23 + Q[i]>>25 + z>>63
            Q[i] := inot (z<<1+h)
    od
    indx:=1
    return Q[Q.lwb]
end

function kiss =
    return supr()+cng()+xxs()
end

function supr=
    if indx <= Q.upb then
        return Q[indx++]
    else
        return refill()
    fi
end

function xxs =
    xs :=xs ixor xs<<13
    xs :=xs ixor xs>>17
    xs :=xs ixor xs<<43
    return xs
end

function cng =
    xcng:=word(6906969069) * xcng + word(123)
    return xcng
end

proc start=

    for i in Q do
        Q[i] := cng() + xxs()
    od

    to 10 million do
        x:=kiss()
    od

    println "Does x = 4013566000157423768"
    println "     x =",x
end
