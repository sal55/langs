const N = 1024

global word32 hp = 0
global word32 sp = 1024
global word32 ATOM = 32760
global word32 PRIM = 32761
global word32 CONS = 32762
global word32 CLOS = 32763
global word32 NIL = 32764
global [1024]real64 cell
global real64 nil
global real64 tru
global real64 err
global real64 env
record $T3 =
    ref byte s
    ref clang function(real64,real64)real64 f
end

global []$T3 prim = (("eval",&f_eval),("quote",&f_quote),("cons",&f_cons),("car",&f_car),("cdr",&f_cdr),("+",&f_add),("-",&f_sub),("*",&f_mul),("/",&f_div),("int",&f_int),("<",&f_lt),("eq?",&f_eq),("or",&f_or),("and",&f_and),("not",&f_not),("cond",&f_cond),("if",&f_if),("let*",&f_leta),("lambda",&f_lambda),("define",&f_define),(0))
global [40]byte buf
global byte see = ' '

global function box(word32 t, i)real64 =
    real64 x

    (ref word64(&x))^ := word64(t)<<48 ior i
    return x
end

global function ord(real64 x)word32 =
    return (ref word64(&x))^
end

global function num(real64 n)real64 =
    return n
end

global function equ(real64 x, y)word32 =
    return (ref word64(&x))^=(ref word64(&y))^
end

global function atom(ref byte s)real64 =
    word32 i

    i := 0
    while i<hp and strcmp(ref byte(cell)+i,s) do
        i +:= strlen(ref byte(cell)+i)+1
    od
    if i=hp and hp+:=strlen(strcpy(ref byte(cell)+i,s))+1>sp<<3 then
        abort()
    fi
    return box(ATOM,i)
end

global function cons(real64 x, y)real64 =
    cell[--sp] := x
    cell[--sp] := y
    if hp>sp<<3 then
        abort()
    fi
    return box(CONS,sp)
end

global function car(real64 p)real64 =
    return ((ref word64(&p))^>>48 iand inot CONS ixor CLOS=CONS|cell[ord(p)+1]|err)
end

global function cdr(real64 p)real64 =
    return ((ref word64(&p))^>>48 iand inot CONS ixor CLOS=CONS|cell[ord(p)]|err)
end

global function pair(real64 v, x, e)real64 =
    return cons(cons(v,x),e)
end

global function closure(real64 v, x, e)real64 =
    return box(CLOS,ord(pair(v,x,(equ(e,env)|nil|e))))
end

global function assoc(real64 v, e)real64 =
    while (ref word64(&e))^>>48=CONS and not equ(v,car(car(e))) do
        e := cdr(e)
    od
    return ((ref word64(&e))^>>48=CONS|cdr(car(e))|err)
end

global function not(real64 x)word32 =
    return (ref word64(&x))^>>48=NIL
end

global function let(real64 x)word32 =
    return (ref word64(&x))^>>48<>NIL and not not(cdr(x))
end

global function eval(real64 x, e)real64 =
    return ((ref word64(&x))^>>48=ATOM|assoc(x,e)|((ref word64(&x))^>>48=CONS|apply(eval(car(x),e),cdr(x),e)|x))
end

global function parse()real64 =
    return (buf^='('|list()|(buf^='''|quote()|atomic()))
end

global function evlis(real64 t, e)real64 =
    return ((ref word64(&t))^>>48=CONS|cons(eval(car(t),e),evlis(cdr(t),e))|((ref word64(&t))^>>48=ATOM|assoc(t,e)|nil))
end

global function f_eval(real64 t, e)real64 =
    return eval(car(evlis(t,e)),e)
end

global function f_quote(real64 t, _)real64 =
    return car(t)
end

global function f_cons(real64 t, e)real64 =
    return (t := evlis(t,e);cons(car(t),car(cdr(t))))
end

global function f_car(real64 t, e)real64 =
    return car(car(evlis(t,e)))
end

global function f_cdr(real64 t, e)real64 =
    return cdr(car(evlis(t,e)))
end

global function f_add(real64 t, e)real64 =
    real64 n

    n := car(t := evlis(t,e))
    while not not(t := cdr(t)) do
        n +:= car(t)
    od
    return num(n)
end

global function f_sub(real64 t, e)real64 =
    real64 n

    n := car(t := evlis(t,e))
    while not not(t := cdr(t)) do
        n -:= car(t)
    od
    return num(n)
end

global function f_mul(real64 t, e)real64 =
    real64 n

    n := car(t := evlis(t,e))
    while not not(t := cdr(t)) do
        n *:= car(t)
    od
    return num(n)
end

global function f_div(real64 t, e)real64 =
    real64 n

    n := car(t := evlis(t,e))
    while not not(t := cdr(t)) do
        n /:= car(t)
    od
    return num(n)
end

global function f_int(real64 t, e)real64 =
    real64 n

    n := car(evlis(t,e))
    return (n<10000000000000000.000000 and n>-10000000000000000.000000|int64(n)|n)
end

global function f_lt(real64 t, e)real64 =
    return (t := evlis(t,e);(car(t)-car(cdr(t))<0|tru|nil))
end

global function f_eq(real64 t, e)real64 =
    return (t := evlis(t,e);(equ(car(t),car(cdr(t)))|tru|nil))
end

global function f_not(real64 t, e)real64 =
    return (not(car(evlis(t,e)))|tru|nil)
end

global function f_or(real64 t, e)real64 =
    real64 x

    x := nil
    while (ref word64(&t))^>>48<>NIL and not(x := eval(car(t),e)) do
        t := cdr(t)
    od
    return x
end

global function f_and(real64 t, e)real64 =
    real64 x

    x := nil
    while (ref word64(&t))^>>48<>NIL and not not(x := eval(car(t),e)) do
        t := cdr(t)
    od
    return x
end

global function f_cond(real64 t, e)real64 =
    while (ref word64(&t))^>>48<>NIL and not(eval(car(car(t)),e)) do
        t := cdr(t)
    od
    return eval(car(cdr(car(t))),e)
end

global function f_if(real64 t, e)real64 =
    return eval(car(cdr((not(eval(car(t),e))|cdr(t)|t))),e)
end

global function f_leta(real64 t, e)real64 =
    while let(t) do
        e := pair(car(car(t)),eval(car(cdr(car(t))),e),e)
        t := cdr(t)
    od
    return eval(car(t),e)
end

global function f_lambda(real64 t, e)real64 =
    return closure(car(t),car(cdr(t)),e)
end

global function f_define(real64 t, e)real64 =
    env := pair(car(t),eval(car(cdr(t)),e),env)
    return car(t)
end

global function bind(real64 v, t, e)real64 =
    return ((ref word64(&v))^>>48=NIL|e|((ref word64(&v))^>>48=CONS|bind(cdr(v),cdr(t),pair(car(v),car(t),e))|pair(v,t,e)))
end

global function reduce(real64 f, t, e)real64 =
    return eval(cdr(car(f)),bind(car(car(f)),evlis(t,e),(not(cdr(f))|env|cdr(f))))
end

global function apply(real64 f, t, e)real64 =
    return ((ref word64(&f))^>>48=PRIM|prim[ord(f)].f(t,e)|((ref word64(&f))^>>48=CLOS|reduce(f,t,e)|err))
end

global proc look() =
    int32 c

    c := getchar()
    see := c
    if c=-1 then
        exit(0)
    fi
end

global function seeing(byte c)word32 =
    return (c=' '|see>0 and see<=c|see=c)
end

global function get()byte =
    byte c

    c := see
    look()
    return c
end

global function scan()byte =
    int32 i

    i := 0
    while seeing(' ') do
        look()
    od
    if seeing('(') or seeing(')') or seeing(''') then
        buf[i++] := get()
    else
        repeat 
            buf[i++] := get()
        until not(i<39 and not seeing('(') and not seeing(')') and not seeing(' '))
    fi
    return (buf[i] := 0;buf^)
end

global function read()real64 =
    return (scan();parse())
end

global function list()real64 =
    real64 x

    return (scan()=')'|nil|(not strcmp(buf,".")|(x := read();scan();x)|(x := parse();cons(x,list()))))
end

global function quote()real64 =
    return cons(atom("quote"),cons(read(),nil))
end

global function atomic()real64 =
    real64 n
    int32 i

    return (sscanf(buf,"%lg%n",&n,&i)>0 and not buf[i]|n|atom(buf))
end

global proc print(real64 x) =
    if (ref word64(&x))^>>48=NIL then
        printf("()")
    else
        if (ref word64(&x))^>>48=ATOM then
            printf("%s",ref byte(cell)+ord(x))
        else
            if (ref word64(&x))^>>48=PRIM then
                printf("<%s>",prim[ord(x)].s)
            else
                if (ref word64(&x))^>>48=CONS then
                    printlist(x)
                else
                    if (ref word64(&x))^>>48=CLOS then
                        printf("{%u}",ord(x))
                    else
                        printf("%.10lg",x)
                    fi
                fi
            fi
        fi
    fi
end

global proc printlist(real64 t) =
    putchar('(')
    do
        print(car(t))
        if not(t := cdr(t)) then
            exit
        fi
        if (ref word64(&t))^>>48<>CONS then
            printf(" . ")
            print(t)
            exit
        fi
        putchar(' ')
    od
    putchar(')')
end

global proc gc() =
    sp := ord(env)
end

global function main()int32 =
    int32 i

    printf("tinylisp")
    nil := box(NIL,0)
    err := atom("ERR")
    tru := atom("#t")
    env := pair(tru,tru,nil)
    i := 0
    while prim[i].s do
        env := pair(atom(prim[i].s),box(PRIM,i),env)
        ++i
    od
    do
        printf("\n%u>",sp-hp/8)
        print(eval(read(),env))
        gc()
    od
    return 0
end

proc start =
    stop main()
end
