function run()int64 =
    ref CallFrame frame
    byte instruction
    word64 constant
    byte slot
    ref ObjString name
    word64 value
    ref ObjInstance instance
    ref ObjClass superclass
    word64 b
    word64 a
    word16 offset
    int32 argCount
    ref ObjString method
    ref ObjFunction function
    ref ObjClosure closure
    int32 i
    byte isLocal
    byte index
    word64 result
    ref ObjClass subclass

    frame := &vm.frames[vm.frameCount-1]
    do
        switch instruction := (frame^.ip++)^
        when OP_CONSTANT then
            constant := ((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^]
            pushx(constant)
        when OP_NIL then
            pushx(word64(word64(9222246136947933184) ior 1))
        when OP_TRUE then
            pushx((1|word64(word64(9222246136947933184) ior 3)|word64(word64(9222246136947933184) ior 2)))
        when OP_FALSE then
            pushx((0|word64(word64(9222246136947933184) ior 3)|word64(word64(9222246136947933184) ior 2)))
        when OP_POP then
            popx()
        when OP_GET_LOCAL then
            slot := (frame^.ip++)^
            pushx(frame^.slots[slot])
        when OP_SET_LOCAL then
            slot := (frame^.ip++)^
            frame^.slots[slot] := peek(0)
        when OP_GET_GLOBAL then
            name := ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            if not tableGet(&vm.globals,name,&value) then
                runtimeError("Undefined variable '%s'.",name^.chars)
                return INTERPRET_RUNTIME_ERROR
            fi
            pushx(value)
        when OP_DEFINE_GLOBAL then
            name := ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            tableSet(&vm.globals,name,peek(0))
            popx()
        when OP_SET_GLOBAL then
            name := ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            if tableSet(&vm.globals,name,peek(0)) then
                tableDelete(&vm.globals,name)
                runtimeError("Undefined variable '%s'.",name^.chars)
                return INTERPRET_RUNTIME_ERROR
            fi
        when OP_GET_UPVALUE then
            slot := (frame^.ip++)^
            pushx((((frame^.closure)^.upvalues[slot])^.location)^)
        when OP_SET_UPVALUE then
            slot := (frame^.ip++)^
            (((frame^.closure)^.upvalues[slot])^.location)^ := peek(0)
        when OP_GET_PROPERTY then
            if not isObjType(peek(0),OBJ_INSTANCE) then
                runtimeError("Only instances have properties.")
                return INTERPRET_RUNTIME_ERROR
            fi
            instance := ref ObjInstance(ref Obj(word64(peek(0) iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            name := ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            if tableGet(&instance^.fields,name,&value) then
                popx()
                pushx(value)
            fi
            if not bindMethod(instance^.klass,name) then
                return INTERPRET_RUNTIME_ERROR
            fi
        when OP_SET_PROPERTY then
            if not isObjType(peek(1),OBJ_INSTANCE) then
                runtimeError("Only instances have fields.")
                return INTERPRET_RUNTIME_ERROR
            fi
            instance := ref ObjInstance(ref Obj(word64(peek(1) iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            tableSet(&instance^.fields,ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184)))),peek(0))
            value := popx()
            popx()
            pushx(value)
        when OP_GET_SUPER then
            name := ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            superclass := ref ObjClass(ref Obj(word64(popx() iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            if not bindMethod(superclass,name) then
                return INTERPRET_RUNTIME_ERROR
            fi
        when OP_EQUAL then
            b := popx()
            a := popx()
            pushx((valuesEqual(a,b)|word64(word64(9222246136947933184) ior 3)|word64(word64(9222246136947933184) ior 2)))
        when OP_GREATER then
            repeat 
                if not peek(0) iand word64(9222246136947933184)<>word64(9222246136947933184) or not peek(1) iand word64(9222246136947933184)<>word64(9222246136947933184) then
                    runtimeError("Operands must be numbers.")
                    return INTERPRET_RUNTIME_ERROR
                fi
                b := valueToNum(popx())
                a := valueToNum(popx())
                pushx((a>b|word64(word64(9222246136947933184) ior 3)|word64(word64(9222246136947933184) ior 2)))
            until not(0)
        when OP_LESS then
            repeat 
                if not peek(0) iand word64(9222246136947933184)<>word64(9222246136947933184) or not peek(1) iand word64(9222246136947933184)<>word64(9222246136947933184) then
                    runtimeError("Operands must be numbers.")
                    return INTERPRET_RUNTIME_ERROR
                fi
                b := valueToNum(popx())
                a := valueToNum(popx())
                pushx((a<b|word64(word64(9222246136947933184) ior 3)|word64(word64(9222246136947933184) ior 2)))
            until not(0)
        when OP_ADD then
            if isObjType(peek(0),OBJ_STRING) and isObjType(peek(1),OBJ_STRING) then
                concatenate()
            else
                if peek(0) iand word64(9222246136947933184)<>word64(9222246136947933184) and peek(1) iand word64(9222246136947933184)<>word64(9222246136947933184) then
                    b := valueToNum(popx())
                    a := valueToNum(popx())
                    pushx(numToValue(a+b))
                else
                    runtimeError("Operands must be two numbers or two strings.")
                    return INTERPRET_RUNTIME_ERROR
                fi
            fi
        when OP_SUBTRACT then
            repeat 
                if not peek(0) iand word64(9222246136947933184)<>word64(9222246136947933184) or not peek(1) iand word64(9222246136947933184)<>word64(9222246136947933184) then
                    runtimeError("Operands must be numbers.")
                    return INTERPRET_RUNTIME_ERROR
                fi
                b := valueToNum(popx())
                a := valueToNum(popx())
                pushx(numToValue(a-b))
            until not(0)
        when OP_MULTIPLY then
            repeat 
                if not peek(0) iand word64(9222246136947933184)<>word64(9222246136947933184) or not peek(1) iand word64(9222246136947933184)<>word64(9222246136947933184) then
                    runtimeError("Operands must be numbers.")
                    return INTERPRET_RUNTIME_ERROR
                fi
                b := valueToNum(popx())
                a := valueToNum(popx())
                pushx(numToValue(a*b))
            until not(0)
        when OP_DIVIDE then
            repeat 
                if not peek(0) iand word64(9222246136947933184)<>word64(9222246136947933184) or not peek(1) iand word64(9222246136947933184)<>word64(9222246136947933184) then
                    runtimeError("Operands must be numbers.")
                    return INTERPRET_RUNTIME_ERROR
                fi
                b := valueToNum(popx())
                a := valueToNum(popx())
                pushx(numToValue(a/b))
            until not(0)
        when OP_NOT then
            pushx((isFalsey(popx())|word64(word64(9222246136947933184) ior 3)|word64(word64(9222246136947933184) ior 2)))
        when OP_NEGATE then
            if not peek(0) iand word64(9222246136947933184)<>word64(9222246136947933184) then
                runtimeError("Operand must be a number.")
                return INTERPRET_RUNTIME_ERROR
            fi
            pushx(numToValue(-valueToNum(popx())))
        when OP_PRINT then
            printValue(popx())
            printf("\n")
        when OP_JUMP then
            offset := (frame^.ip+:=2;word16(frame^.ip[-2]<<8 ior frame^.ip[-1]))
            frame^.ip +:= offset
        when OP_JUMP_IF_FALSE then
            offset := (frame^.ip+:=2;word16(frame^.ip[-2]<<8 ior frame^.ip[-1]))
            if isFalsey(peek(0)) then
                frame^.ip +:= offset
            fi
        when OP_LOOP then
            offset := (frame^.ip+:=2;word16(frame^.ip[-2]<<8 ior frame^.ip[-1]))
            frame^.ip -:= offset
        when OP_CALL then
            argCount := (frame^.ip++)^
            if not callValue(peek(argCount),argCount) then
                return INTERPRET_RUNTIME_ERROR
            fi
            frame := &vm.frames[vm.frameCount-1]
        when OP_INVOKE then
            method := ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            argCount := (frame^.ip++)^
            if not invoke(method,argCount) then
                return INTERPRET_RUNTIME_ERROR
            fi
            frame := &vm.frames[vm.frameCount-1]
        when OP_SUPER_INVOKE then
            method := ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            argCount := (frame^.ip++)^
            superclass := ref ObjClass(ref Obj(word64(popx() iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            if not invokeFromClass(superclass,method,argCount) then
                return INTERPRET_RUNTIME_ERROR
            fi
            frame := &vm.frames[vm.frameCount-1]
        when OP_CLOSURE then
            function := ref ObjFunction(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            closure := newClosure(function)
            pushx(word64(word64(-9223372036854775808) ior word64(9222246136947933184) ior word64(closure)))
            i := 0
            while i<closure^.upvalueCount do
                isLocal := (frame^.ip++)^
                index := (frame^.ip++)^
                if isLocal then
                    closure^.upvalues[i] := captureUpvalue(frame^.slots+index)
                else
                    closure^.upvalues[i] := (frame^.closure)^.upvalues[index]
                fi
                i++
            od
        when OP_CLOSE_UPVALUE then
            closeUpvalues(vm.stackTop-1)
            popx()
        when OP_RETURN then
            result := popx()
            closeUpvalues(frame^.slots)
            vm.frameCount--
            if vm.frameCount=0 then
                popx()
                return INTERPRET_OK
            fi
            vm.stackTop := frame^.slots
            pushx(result)
            frame := &vm.frames[vm.frameCount-1]
        when OP_CLASS then
            pushx(word64(word64(-9223372036854775808) ior word64(9222246136947933184) ior word64(newClass(ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))))))
        when OP_INHERIT then
            superclass := peek(1)
            if not isObjType(superclass,OBJ_CLASS) then
                runtimeError("Superclass must be a class.")
                return INTERPRET_RUNTIME_ERROR
            fi
            subclass := ref ObjClass(ref Obj(word64(peek(0) iand inot word64(-9223372036854775808) ior word64(9222246136947933184))))
            tableAddAll(&(ref ObjClass(ref Obj(word64(superclass iand inot word64(-9223372036854775808) ior word64(9222246136947933184)))))^.methods,&subclass^.methods)
            popx()
        when OP_METHOD then
            defineMethod(ref ObjString(ref Obj(word64(((frame^.closure)^.function)^.chunk.constants.values[(frame^.ip++)^] iand inot word64(-9223372036854775808) ior word64(9222246136947933184)))))
        end switch
    od
end
