    case lx.symbol
    when lsqsym then
        lex()
        ndims:=0
        pushlisttype(0)
        do
            lowerx:=lengthx:=nil
            if lx.symbol=rsqsym or lx.symbol=commasym then      ![]
            else
                x:=readunit()
                if x.tag=jmakerange then            ![a..b] variable
                    lowerx:=x.a
                    upperx:=x.b
                    if lowerx.tag=jintconst and upperx.tag=jintconst then
                        lengthx:=createintunit(upperx.value-lowerx.value+1)
                    else
                        lengthx:=createunit2(jsub,upperx,lowerx)
                        lengthx:=createunit2(jadd,lengthx,createintunit(1))
                    fi
                else
                    case lx.symbol
                    when rsqsym,commasym then       ![n]
                        lengthx:=x
                    when colonsym then              !a:n
                        lowerx:=x
                        lex()
                        if not (lx.symbol=commasym or lx.symbol=rsqsym) then
                            lengthx:=readunit()
                        fi
                    esac
                fi
            fi
            lowerdims[++ndims]:=lowerx
            lengthdims[ndims]:=lengthx
            exit when lx.symbol<>commasym
            lex()
        od
