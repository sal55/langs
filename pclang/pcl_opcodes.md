
Opcode      | Inline    | Stack        | Description
---         | ---       | ---          | ---
**Declare**|            |              |
proc        | [t] m[\*]  |  --          | Define proc/function m (* means exported)
param       | t m       |  --          | Define parameter m 
local       | t m       |  --          | Define local variable m
rettype     | t         |  --          | Define return type
end         | --        |  --          | End of proc/function
extproc     | m         |  --          | Declare imported proc/function m
extparam    | t         |  --          | Param of import
extvariadic | n         |  --          | Variadic start int
extend      | --        |  --          | End of extproc block
istatic     | t m       |  --          | Define initialised static variable m
zstatic     | t m       |  --          | Define unitialised static variable m
data        | t m/s/l/n |  --          | Define data for istatic variable
linkdll     | m/s       |  --          | Name of DLL as eg `msvcrt` or `"msvcrt"`
**Load/Store**|         |              |
load        | [t] m     |  (0 - 1)     | `X' := M`
loadref     | [t] m/l   |  (0 - 1)     | `X' := &M` or `&L`
loadimm     | [t] n/s   |  (0 - 1)     | `X' := N` or `S`
store       | [t] m     |  (1 - 0)     | `M := X`
unload      | t         |  (1 - 0)     | Pop X
double      | [t]       |  (1 - 2)     | Dupl X: `(X) => (X,X)` (see notes)
dupl        | [t]       |  (1 - 2)     | Dupl X: `(X) => (X,X)`
swapopnds   | [t]       |  (2 - 2)     | Swap X, Y: `(X,Y) => (Y,X)`
swapmem     | t         |  (2 - 0)     | `Swap(X^, Y^)`
clear       | t         |  (1 - 0)     | clear `X^` to zeros
iload       | t         |  (1 - 1)     | `X' := X^`
istore      | t         |  (2 - 0)     | `Y^ := X`
iloadx      | t s off   |  (2 - 1)     | `X' := (X + Y*scale + offset)^`
istorex     | t s off   |  (3 - 0)     | `(Y + Z*scale + offset)^ := X`
addptrx     | t s off   |  (2 - 1)     | `X' := X + Y*scale + offset`
subptrx     | t s off   |  (2 - 1)     | `X' := X - Y*scale + offset`
subptr      | t s       |  (2 - 1)     | `X' := (X - Y)*scale`
loadbit     | --        |  (2 - 1)     | `X' := X.[Y]`
storebit    | --        |  (3 - 0)     | `Y^.[Z] := X`
loadbf      | --        |  (3 - 1)     | `X' := X.[Y..Z]`
storebf     | --        |  (4 - 0)     | `X^.[Y..Z] := W`
**Control Flow**|       |              |
callp       | m n [v]   |  (n - 0)     | `M(...)`
callf       | t m n [v] |  (n - 1)     | `X' := M(...)`
icallp      | n [v]     |  (n - 0)     | `X^(...)`
icallf      | t n [v]   |  (n+1 - 1)   | `X' := X^(...)`
return      | --        |  ( - )       |  
stop        | --        |  (1 - 0)     | Stop execution with return code X
jump        | l         |  (0 - 0)     | `Goto L`
ijump       | --        |  (1 - 0)     | `Goto X`
jumpeq      | t l [p1]  |  (2 - 0/1)   | `Goto L when X = Y; popone: leave X on stack`
jumpne      | t l [p1]  |  (2 - 0/1)   | `Goto L when X <> Y; "`
jumplt      | t l [p1]  |  (2 - 0/1)   | `Goto L when X < Y`
jumple      | t l [p1]  |  (2 - 0/1)   | `Goto L when X <= Y`
jumpge      | t l [p1]  |  (2 - 0/1)   | `Goto L when X >= Y`
jumpgt      | t l [p1]  |  (2 - 0/1)   | `Goto L when X > Y`
jumpt       | t l       |  (1 - 0)     | `Goto L when X is true (X is always int)`
jumpf       | t l       |  (1 - 0)     | `Goto L when X is false`
forup       | l m [s]   |  (0 - 0)     | `M+:=s; goto L when A1<=A2` (uses 2 x aux opnds)
fordown     | l m [s]   |  (0 - 0)     | `M-:=s; goto L when A1>=A2` (uses 2 aux)
to          | l         |  (0 - 0)     | `--M;   goto L when A1<>0` (uses 1 aux)
switch      | L min max |  (1 - 0)     | `L=jumptab, L2=elselab (1 aux)
swlabel     | L         |  --          | Jumptable entry
endsw       | --        |  --          | Mark end of jumptable (allow segment change etc)
**Arithmetic**|           |              |
add         | t         |  (2 - 1)     | `X' := X + Y` (Similar for following)`
sub         | t         |  (2 - 1)     | 
mul         | t         |  (2 - 1)     | 
divf        | t         |  (2 - 1)     | (Floating point divide)
div         | t         |  (2 - 1)     | (Integer divide)
rem         | t         |  (2 - 1)     | (Integer remainder)
divrem      | t         |  (2 - 2)     | `(X', Y') := (X % Y, X rem Y)` (`%` = int divide)
bitand      | t         |  (2 - 1)     | `X' := X iand Y` (bitwise AND)
bitor       | t         |  (2 - 1)     | 
bitxor      | t         |  (2 - 1)     | 
shl         | t         |  (2 - 1)     | (Shift left)
shr         | t         |  (2 - 1)     | (Shift right)
min         | t         |  (2 - 1)     | `X' := min(X, Y)`
max         | t         |  (2 - 1)     | 
eq          | t         |  (2 - 1)     | `X' := X = Y`
ne          | t         |  (2 - 1)     | 
lt          | t         |  (2 - 1)     | 
le          | t         |  (2 - 1)     | 
ge          | t         |  (2 - 1)     | 
gt          | t         |  (2 - 1)     | 
power       | t         |  (2 - 1)     | `X' := X ** Y`
atan2       | t         |  (2 - 1)     | 
neg         | t         |  (1 - 1)     | `X' := -X`
abs         | t         |  (1 - 1)     | `X' := abs(X)`
bitnot      | t         |  (1 - 1)     | Bitwise invert
not         | t         |  (1 - 1)     | `X' := (X=0 | 1 | 0)` (Logical)
notnot      | t         |  (1 - 1)     | `X' := (X=0 | 0 | 1)`
sqr         | t         |  (1 - 1)     | `X' := X*X`
sign        | t         |  (1 - 1)     | `X' := -1,0,1` according to `X<0, X=0, X>0`
sqrt        | t         |  (1 - 1)     | 
sin         | t         |  (1 - 1)     | `X' := sin(X)`
cos         | t         |  (1 - 1)     | 
tan         | t         |  (1 - 1)     | 
asin        | t         |  (1 - 1)     | 
acos        | t         |  (1 - 1)     | 
atan        | t         |  (1 - 1)     | 
log         | t         |  (1 - 1)     | Natural log
log10       | t         |  (1 - 1)     | Base-10 log
exp         | t         |  (1 - 1)     | 
round       | t         |  (1 - 1)     | 
floor       | t         |  (1 - 1)     | 
ceil        | t         |  (1 - 1)     | 
fract       | t         |  (1 - 1)     | 
**In-place**|           |              |
addto       | t         |  (2 - 0)     | `X^ +:= Y` (similar for following)
subto       | t         |  (2 - 0)     | 
multo       | t         |  (2 - 0)     | 
divfto      | t         |  (2 - 0)     | 
divto       | t         |  (2 - 0)     | 
remto       | t         |  (2 - 0)     | 
bitandto    | t         |  (2 - 0)     | 
bitorto     | t         |  (2 - 0)     | 
bitxorto    | t         |  (2 - 0)     | 
shlto       | t         |  (2 - 0)     | 
shrto       | t         |  (2 - 0)     | 
minto       | t         |  (2 - 0)     | `X^ := min(X^, Y)`
maxto       | t         |  (2 - 0)     | 
addpxto     | t         |  (2 - 0)     | `X^ +:= Y*s + d`; `X^` points to T; Y is i64
subpxto     | t         |  (2 - 0)     | `X^ -:= Y*s + d`; `X^` points to T; Y is i64
negto       | t         |  (1 - 0)     | `X^ := -X^
absto       | t         |  (1 - 0)     | 
bitnotto    | t         |  (1 - 0)     | 
notto       | t         |  (1 - 0)     | `X^ := (X^=0 | 1 | 0)`
notnotto    | t         |  (1 - 0)     | `X^ := (X^=0 | 0 | 1)`
incrto      | t         |  (0 - 0)     | `X^ +:=s`; default s is 1
incrload    | t         |  (0 - 0)     | `X^ +:=s; X' := X^`
loadincr    | t         |  (0 - 0)     | `X' := X^; X^ +:= s`
decrto      | t         |  (0 - 0)     | `X^ -:=s`; default s is 1
decrload    | t         |  (0 - 0)     | `X^ -:=s; X' := X^`
loaddecr    | t         |  (0 - 0)     | `X' := X^; X^ -:= s`
**Conversion**|         |              |
float       | t         |  (1 - 1)     | `X' := T(X)` (convert int to float)
fix         | t         |  (1 - 1)     | `X' := T(X)` (convert float to int)
truncate    | t         |  (1 - 1)     | `X' := T(X)` (truncate integer to narrow type)
fwiden      | [t]       |  (1 - 1)     | `X' := r64(x)` (from r32)
fnarrow     | [t]       |  (1 - 1)     | `X' := r32(X)` (from r64)
typepun     | --        |  (0 - 0)     | `X' := T@(X)`
widen       | t         |  (0 - 0)     | `X' := Widen(X)` Widen from narrow type t
**Auxiliary**|          |              |
opnd        | [t] m/l/n |  --          | auxiliary op
assem       | [s]       |  --          |   Ignored in discrete PCL code
setcall     | n         |  --          | Init call seq with n args
setarg      | n         |  --          | Mark X as n'th argument of call
setret      | --        |  --          | Mark X as return value
startmx     | --        |  --          | Multi-path markers
resetmx     | --        |  --          |  
endmx       | --        |  --          |  
**Debug**   |           |              |
printi64    | --        |  (1 - 0)     | print X as decimal
printu64    | --        |  (1 - 0)     | 
printr64    | --        |  (1 - 0)     | 
printr32    | --        |  (1 - 0)     | 
printstr    | --        |  (1 - 0)     | print X as string
printhex    | --        |  (1 - 0)     | print X as hex (eg. ptr)
printsp     | --        |  (0 - 0)     | print SP
test        | --        |  (0 - 0)     | 
debug       | --        |  (0 - 0)     | debug 1/0 to turn it on/off
