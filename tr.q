# Notes: "%" is integer divide
# Command line is copied into read buffer when it starts
# 'read' will read zero-values if it hits end-of-line

enumdata =
    start=0,
    afu,
    usagex,
    root,
    drat,
    hwct,
    fin,
end

macro go(x) = state := x

proc main =
    state := start

    doswitch state
    when start then
        
        read a1:"i", a2:"i", a3:"i"
        usage := ncmdparams not in 1..3

        square := a1 and a2=a3=0
        hw := a1 and a2 and a3=0
        hwc := a1 and a2 and a3

        k := 0
        h := (hw or hwc | a1 | a1%4 + not a1)
        w := (hw or hwc | a2 | h)
        c := (square | a1 | (not hwc | h*w | (a3 > h*w | h*w | a3)))
        d := tostr(c).len

        go((usage | usagex | (square | root | (hw or hwc and a1 and a2 | hwct | afu))))

    when afu then
        println "Bad args"
        go(usagex)

    when usagex then
        println "Usage..."
        go(fin)

    when root then
        h := (c<2 | 1 | (c<5 | 2 | (h + c%h)%2))
        h +:= h*h<c
        w := h
        done := c<2 or h*h < c+2*h and c<= h*h
        good := c<2 or h*h < c+h and c<= h*h

        go((not done | root | (good | hwct | drat)))

    when drat then
        fprintln " square with cutoff # - no joy", c
        go(fin)

    when hwct then
        eol := k%h+1 = w
        endx := k+1 + (h>c | h-c | 0) >= h*w
        nextk := (eol | k-(w-1)*h+1 | k+h)
        printf(( k < c | " %*zu" | ""),  d,  k+1 )
        printf(( c > 0 and eol | "\n" | ""))
        go((endx | (println "-------"; fin) | (k := nextk; hwct)))

    else
        exit
    end
end
