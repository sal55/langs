Opcode | Inline | Stack | Description
--- | --- | --- | ---
kproc        | --        |  (0-0)     | Define proc
kparam       | --        |  (0-0)     | Define parameter
klocal       | --        |  (0-0)     | Define local variable
krettype     | --        |  (0-0)     | Define return type
kend         | --        |  (0-0)     | End of function
kextproc     | --        |  (0-0)     | Declare proc import
kextparam    | --        |  (0-0)     | Param of import
kextvariadic | --        |  (0-0)     | Variadic start int
kextend      | --        |  (0-0)     | Variadic start int
kistatic     | --        |  (0-0)     | Define initialised static variable
kzstatic     | --        |  (0-0)     | 
kdata        | --        |  (0-0)     | Define data for statics
klinkdll     | --        |  (0-0)     | Name of DLL as eg `msvcrt` or `"msvcrt"`
kstartmx     | --        | --         |   Multi-path markers
kresetmx     | --        | --         |  
kendmx       | --        | --         |  
kload        | --        |  (0-1)     | X' := M/L/N^
kloadref     | --        |  (0-1)     | X' := &M/&L
kloadimm     | --        |  (0-1)     | X' := N/S
kstore       | --        |  (1-0)     | M := X
kunload      | --        |  (1-0)     | Decr operand count, unload if zero
kdouble      | --        |  (1-1)     | Count extra instance of X: (X) => (2X)
kdupl        | --        |  (1-2)     | Dupl X: (X) => (X,X)
kswapopnds   | --        |  (2-2)     | Swap X, Y: (X,Y) => (Y,X)
kswapmem     | --        |  (2-0)     | Swap(X^, Y^)
kclear       | --        |  (1-0)     | clear X^ to zeros
kiload       | --        |  (1-1)     | X' := X^
kistore      | --        |  (2-0)     | Y^ := X
kiloadx      | --        |  (2-1)     | X' := (X + Y*scale + offset)^
kistorex     | --        |  (3-0)     | (Y + Z*scale + offset)^ := X
kaddptrx     | --        |  (2-1)     | X' := X + Y*scale + offset
ksubptrx     | --        |  (2-1)     | X' := X - Y*scale + offset
ksubptr      | --        |  (2-1)     | X' := (X - Y)*scale
kcallp       | --        |  (n-0)     | M(...)
kcallf       | --        |  (n-1)     | X' := M(...)
kicallp      | --        |  (n-0)     | X^(...)
kicallf      | --        |  (n+1-1)   | X' := X^(...)
ksetcall     | --        |  (0-0)     | 
ksetarg      | --        |  (1-1)     | 
ksetret      | --        |  (1-1)     | Mark X as return value
kreturn      | --        | --         |  
kstop        | --        |  (1-0)     | Stop execution with return code X
kjump        | --        |  (0-0)     | Goto L
kijump       | --        |  (1-0)     | Goto X
kjumpeq      | --        |  (2-0/1)   | Goto L when X = Y; popone: leave X on stack
kjumpne      | --        |  (2-0/1)   | Goto L when X <> Y
kjumplt      | --        |  (2-0/1)   | Goto L when X < Y
kjumple      | --        |  (2-0/1)   | Goto L when X <= Y
kjumpge      | --        |  (2-0/1)   | Goto L when X >= Y
kjumpgt      | --        |  (2-0/1)   | Goto L when X > Y
kjumpt       | --        |  (1-0)     | Goto L when X is true (X is always int)
kjumpf       | --        |  (1-0)     | Goto L when X is false
kforup       | --        |  (0-0)     | M+:=s; goto L when M<=MN
kfordown     | --        |  (0-0)     | M-:=s; goto L when M>=MN
kto          | --        |  (0-0)     | --M;   goto L when M<>0
kswitch      | --        |  (1-0)     | L=jumptab, L2=elselab, a/b = min/max
kswlabel     | --        |  (0-0)     | Jumptable entry
kendsw       | --        |  (0-0)     | Mark end of jumptable (allow segment change etc)
kloadbit     | --        |   (2-1)    | X' := X.[Y]
kstorebit    | --        |   (3-0)    | Y^.[Z] := X
kloadbf      | --        |   (3-1)    | X' := X.[Y..Z]
kstorebf     | --        |   (4-0)    | X^.[Y..Z] := W
kadd         | --        |  (2-1)     | X' := X + Y (Similar for following)
ksub         | --        |  (2-1)     | 
kmul         | --        |  (2-1)     | 
kdivf        | --        |  (2-1)     | (Floating point divide)
kdiv         | --        |  (2-1)     | (Integer divide)
krem         | --        |  (2-1)     | (Integer remainder)
kdivrem      | --        |  (2-2)     | (X', Y') := (X % Y, X rem Y) (% = int divide)
kbitand      | --        |  (2-1)     | X' := X iand Y (bitwise AND)
kbitor       | --        |  (2-1)     | 
kbitxor      | --        |  (2-1)     | 
kshl         | --        |  (2-1)     | (Shift left)
kshr         | --        |  (2-1)     | (Shift right)
kmin         | --        |  (2-1)     | X' := min(X, Y)
kmax         | --        |  (2-1)     | 
keq          | --        |  (2-1)     | X' := X = Y
kne          | --        |  (2-1)     | 
klt          | --        |  (2-1)     | 
kle          | --        |  (2-1)     | 
kge          | --        |  (2-1)     | 
kgt          | --        |  (2-1)     | 
kpower       | --        |  (2-1)     | X' := X ** Y
katan2       | --        |  (2-1)     | 
kaddto       | --        |  (2-0)     | X^ +:= Y (similar for following)
ksubto       | --        |  (2-0)     | 
kmulto       | --        |  (2-0)     | 
kdivfto      | --        |  (2-0)     | 
kdivto       | --        |  (2-0)     | 
kremto       | --        |  (2-0)     | 
kbitandto    | --        |  (2-0)     | 
kbitorto     | --        |  (2-0)     | 
kbitxorto    | --        |  (2-0)     | 
kshlto       | --        |  (2-0)     | 
kshrto       | --        |  (2-0)     | 
kminto       | --        |  (2-0)     | X^ := min(X^, Y)
kmaxto       | --        |  (2-0)     | 
kaddpxto     | --        |  (2-0)     | X^ +:= Y*s + d; X^ points to T; Y is i64
ksubpxto     | --        |  (2-0)     | X^ -:= Y*s + d; X^ points to T; Y is i64
kneg         | --        |  (1-1)     | X' := -X
kabs         | --        |  (1-1)     | X' := abs(X)
kbitnot      | --        |  (1-1)     | Bitwise invert
knot         | --        |  (1-1)     | X' := (X=0 | 1 | 0) (Logical)
knotnot      | --        |  (1-1)     | X' := (X=0 | 0 | 1)
ksqr         | --        |  (1-1)     | X' := X*X
ksign        | --        |  (1-1)     | X' := -1,0,1 according to X<0, X=0, X>0
ksqrt        | --        |  (1-1)     | 
ksin         | --        |  (1-1)     | X' := sin(X)
kcos         | --        |  (1-1)     | 
ktan         | --        |  (1-1)     | 
kasin        | --        |  (1-1)     | 
kacos        | --        |  (1-1)     | 
katan        | --        |  (1-1)     | 
kln          | --        |  (1-1)     | Natural log
klog         | --        |  (1-1)     | Base-10 log
kexp         | --        |  (1-1)     | 
kround       | --        |  (1-1)     | 
kfloor       | --        |  (1-1)     | 
kceil        | --        |  (1-1)     | 
kfract       | --        |  (1-1)     | 
knegto       | --        |  (1-0)     | X^ := -X^
kabsto       | --        |  (1-0)     | 
kbitnotto    | --        |  (1-0)     | 
knotto       | --        |  (1-0)     | X^ := (X^=0 | 1 | 0)
knotnotto    | --        |  (1-0)     | X^ := (X^=0 | 0 | 1)
kincrto      | --        |  (0-0)     | X^ +:=s; default s is 1
kincrload    | --        |  (0-0)     | X^ +:=s; X' := X^
kloadincr    | --        |  (0-0)     | X' := X^; X^ +:= s
kdecrto      | --        |  (0-0)     | X^ -:=s; default s is 1
kdecrload    | --        |  (0-0)     | X^ -:=s; X' := X^
kloaddecr    | --        |  (0-0)     | X' := X^; X^ -:= s
kfloat       | --        |  (1-1)     | X' := T(X) (convert int to float)
kfix         | --        |  (1-1)     | X' := T(X) (convert float to int)
ktruncate    | --        |  (1-1)     | 
kfwiden      | --        |  (1-1)     | X' := r64(x) (from r32)
kfnarrow     | --        |  (1-1)     | X' := r32(X) (from r64)
ktypepun     | --        |  (0-0)     | X' := T@(X) (T/U must be same size)
kwiden       | --        |  (0-0)     | X' := Widen(X) widen narrow int values
kopnd        | --        |  (0-0)     | auxiliary op
kassem       | --        | --         |   Ignored in discrete PCL code
kprinti64    | --        |  (1-0)     | print X as decimal
kprintu64    | --        |  (1-0)     | 
kprintr64    | --        |  (1-0)     | 
kprintr32    | --        |  (1-0)     | 
kprintstr    | --        |  (1-0)     | print X as string
kprinthex    | --        |  (1-0)     | print X as hex (eg. ptr)
kprintsp     | --        |  (0-0)     | print SP
ktest        | --        |  (0-0)     | 
kdebug       | --        |  (0-0)     | debug 1/0 to turn it on/off
