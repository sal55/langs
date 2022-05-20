import clib

record $T3 =
    ref ref void slots
    int32 slotCount
    int32 first
    int32 last
end

enum (S_required = 0)
enum (S_strongPreferred = 1)
enum (S_preferred = 2)
enum (S_strongDefault = 3)
enum (S_default = 4)
enum (S_weakDefault = 5)
enum (S_weakest = 6)
record constraint =
    ref clang proc() execute
    int32 inputFlag
    int32 strength
    byte whichMethod
    byte methodCount
    byte varCount
    [7]byte methodOuts
    [1]ref VariableStruct variables
end

record $T4 =
    int32 value
    ref ListStruct constraints
    ref ConstraintStruct determinedBy
    int32 mark
    int32 walkStrength
    int32 stay
    [10]byte name
end

global ref ListStruct allVariables = 0
global int32 currentMark = 0
global ref ListStruct hot = 0
global ref ListStruct unsatisfied
global int32 strength
global int32 startTime

global proc Grow(ref ListStruct list) =
    list^.slotCount +:= ((list^.slotCount>2|list^.slotCount|2)<512|(list^.slotCount>2|list^.slotCount|2)|512)
    list^.slots := realloc(list^.slots,list^.slotCount*8)
    if list^.slots=0 then
        Error("out of memory")
    fi
end

global proc MakeRoom(ref ListStruct list) =
    ref ref void srcPtr
    ref ref void destPtr
    ref ref void lastPtr

    srcPtr := &list^.slots[list^.first]
    destPtr := &list^.slots[0]
    lastPtr := &list^.slots[list^.last]
    if list^.last-list^.first+1>=list^.slotCount then
        Grow(list)
    fi
    if list^.first=0 then
        return 
    fi
    while srcPtr<=lastPtr do
        (destPtr++)^ := (srcPtr++)^
    od
    list^.last := list^.last-list^.first
    list^.first := 0
end

global function StrengthString(int32 strength)ref byte =
    static [20]byte temp

    switch strength
    when S_required then
        return "required"
    when S_strongPreferred then
        return "strongPreferred"
    when S_preferred then
        return "preferred"
    when S_strongDefault then
        return "strongDefault"
    when S_default then
        return "default"
    when S_weakDefault then
        return "weakDefault"
    when S_weakest then
        return "weakest"
    else
        sprintf(temp,"strength[%d]",strength)
        return temp
    end switch
end

global function Variable_Create(ref byte name, int32 initialValue)ref VariableStruct =
    ref VariableStruct new

    new := ref VariableStruct(malloc(40))
    if new=0 then
        Error("out of memory")
    fi
    new^.value := initialValue
    new^.constraints := List_Create(2)
    new^.determinedBy := 0
    new^.mark := 0
    new^.walkStrength := S_weakest
    new^.stay := true
    strncpy(new^.name,name,10)
    new^.name[9] := 0
    AddVariable(new)
    return new
end

global function Variable_CreateConstant(ref byte name, int32 value)ref VariableStruct =
    ref VariableStruct new

    new := ref VariableStruct(malloc(40))
    if new=0 then
        Error("out of memory")
    fi
    new^.value := value
    new^.constraints := List_Create(0)
    new^.determinedBy := 0
    new^.mark := 0
    new^.walkStrength := S_required
    new^.stay := true
    strncpy(new^.name,name,10)
    new^.name[9] := 0
    AddVariable(new)
    return new
end

global proc Variable_Destroy(ref VariableStruct v) =
    if v^.constraints=0 then
        Error("bad VariableStruct; already freed?")
    fi
    List_Destroy(v^.constraints)
    v^.constraints := 0
    free(v)
end

global proc Variable_Print(ref VariableStruct v) =
    printf("%s(%s,%ld)",v^.name,StrengthString(v^.walkStrength),v^.value)
end

global function Constraint_Create(int32 variableCount, strength)ref ConstraintStruct =
    ref ConstraintStruct new
    int32 i

    new := ref ConstraintStruct(malloc(24+variableCount-1*8))
    if new=0 then
        Error("out of memory")
    fi
    new^.execute := &Noop
    new^.inputFlag := false
    new^.strength := strength
    new^.whichMethod := -1
    new^.methodCount := 0
    i := 0
    while i<7 do
        new^.methodOuts[i] := 0
        i++
    od
    new^.varCount := variableCount
    i := 0
    while i<new^.varCount do
        new^.variables[i] := 0
        i++
    od
    return new
end

global proc Constraint_Destroy(ref ConstraintStruct c) =
    if c^.execute=0 then
        Error("bad ConstraintStruct; already freed?")
    fi
    c^.execute := 0
    free(c)
end

global proc Constraint_Print(ref ConstraintStruct c) =
    int32 i
    int32 outIndex

    if not c^.whichMethod<>-1 then
        printf("Unsatisfied(")
        i := 0
        while i<c^.varCount do
            Variable_Print(c^.variables[i])
            printf(" ")
            i++
        od
        printf(")")
    else
        outIndex := c^.methodOuts[c^.whichMethod]
        printf("Satisfied(")
        i := 0
        while i<c^.varCount do
            if i<>outIndex then
                Variable_Print(c^.variables[i])
                printf(" ")
            fi
            i++
        od
        printf("-> ")
        Variable_Print(c^.variables[outIndex])
        printf(")")
    fi
    printf("\n")
end

global proc ExecutePlan(ref ListStruct list) =
    List_Do(list,&Execute)
end

global function StayC(ref VariableStruct v, int32 strength)ref ConstraintStruct =
    ref ConstraintStruct new

    new := Constraint_Create(1,strength)
    new^.variables[0] := v
    new^.methodCount := 1
    new^.methodOuts[0] := 0
    AddConstraint(new)
    return new
end

global function EditC(ref VariableStruct v, int32 strength)ref ConstraintStruct =
    ref ConstraintStruct new

    new := Constraint_Create(1,strength)
    new^.inputFlag := true
    new^.variables[0] := v
    new^.methodCount := 1
    new^.methodOuts[0] := 0
    AddConstraint(new)
    return new
end

global function EqualsC(ref VariableStruct a, b, int32 strength)ref ConstraintStruct =
    ref ConstraintStruct new

    new := Constraint_Create(2,strength)
    new^.execute := &EqualsC_Execute
    new^.variables[0] := a
    new^.variables[1] := b
    new^.methodCount := 2
    new^.methodOuts[0] := 0
    new^.methodOuts[1] := 1
    AddConstraint(new)
    return new
end

global function ScaleOffsetC(ref VariableStruct src, scale, offset, dest, int32 strength)ref ConstraintStruct =
    ref ConstraintStruct new

    new := Constraint_Create(4,strength)
    new^.execute := &ScaleOffsetC_Execute
    new^.variables[0] := src
    new^.variables[1] := scale
    new^.variables[2] := offset
    new^.variables[3] := dest
    new^.methodCount := 2
    new^.methodOuts[0] := 3
    new^.methodOuts[1] := 0
    AddConstraint(new)
    return new
end

global proc InitDeltaBlue() =
    ref VariableStruct v

    if allVariables=0 then
        allVariables := List_Create(128)
    fi
    v := ref VariableStruct(List_RemoveFirst(allVariables))
    while v<>0 do
        FreeVariable(v)
        v := ref VariableStruct(List_RemoveFirst(allVariables))
    od
    List_RemoveAll(allVariables)
    currentMark := 0
end

global proc AddVariable(ref VariableStruct v) =
    List_Add(allVariables,v)
end

global proc DestroyVariable(ref VariableStruct v) =
    ref ConstraintStruct c

    c := ref ConstraintStruct(List_RemoveFirst(v^.constraints))
    while c<>0 do
        DestroyConstraint(c)
        c := ref ConstraintStruct(List_RemoveFirst(v^.constraints))
    od
    List_Remove(allVariables,v)
    Variable_Destroy(v)
end

global proc AddConstraint(ref ConstraintStruct c) =
    int32 i

    i := c^.varCount-1
    while i>=0 do
        List_Add((c^.variables[i])^.constraints,ref void(c))
        i--
    od
    c^.whichMethod := -1
    IncrementalAdd(c)
end

global proc DestroyConstraint(ref ConstraintStruct c) =
    int32 i

    if c^.whichMethod<>-1 then
        IncrementalRemove(c)
    fi
    i := c^.varCount-1
    while i>=0 do
        List_Remove((c^.variables[i])^.constraints,ref void(c))
        i--
    od
    Constraint_Destroy(c)
end

global function ExtractPlan()ref ListStruct =
    if hot=0 then
        hot := List_Create(128)
    fi
    List_RemoveAll(hot)
    List_Do(allVariables,&CollectSatisfiedInputs)
    return MakePlan()
end

global function ExtractPlanFromConstraint(ref ConstraintStruct c)ref ListStruct =
    if hot=0 then
        hot := List_Create(128)
    fi
    List_RemoveAll(hot)
    AddIfSatisfiedInput(c)
    return MakePlan()
end

global function ExtractPlanFromConstraints(ref ListStruct constraints)ref ListStruct =
    if hot=0 then
        hot := List_Create(128)
    fi
    List_RemoveAll(hot)
    List_Do(constraints,&AddIfSatisfiedInput)
    return MakePlan()
end

global function List_Create(int32 initialCount)ref ListStruct =
    ref ListStruct newList

    newList := ref ListStruct(malloc(24))
    if newList=0 then
        Error("out of memory")
    fi
    newList^.slots := ref ref void(malloc(initialCount*8))
    if newList^.slots=0 then
        Error("out of memory")
    fi
    newList^.slotCount := initialCount
    newList^.first := 0
    newList^.last := -1
    return newList
end

global proc List_Destroy(ref ListStruct list) =
    if list^.slots=0 then
        Error("bad ListStruct; already freed?")
    fi
    free(list^.slots)
    list^.slots := 0
    list^.slotCount := 0
    list^.first := 0
    list^.last := -1
    free(list)
end

global proc List_Do(ref ListStruct list, ref clang proc() proc) =
    ref ref void nextPtr
    ref ref void lastPtr

    nextPtr := &list^.slots[list^.first]
    lastPtr := &list^.slots[list^.last]
    while nextPtr<=lastPtr do
        proc^((nextPtr++)^)
    od
end

global function List_Size(ref ListStruct list)int32 =
    return list^.last-list^.first+1
end

global function List_At(ref ListStruct list, int32 index)ref void =
    if index<0 or index>list^.last-list^.first+1 then
        Error("List access out of bounds")
    fi
    return list^.slots[index+list^.first]
end

global proc List_Add(ref ListStruct list, ref void element) =
    if list^.last>=list^.slotCount-1 then
        MakeRoom(list)
    fi
    list^.slots[++list^.last] := element
end

global proc List_Append(ref ListStruct list1, list2) =
    ref ref void nextPtr
    ref ref void lastPtr

    nextPtr := &list2^.slots[list2^.first]
    lastPtr := &list2^.slots[list2^.last]
    while nextPtr<=lastPtr do
        List_Add(list1,(nextPtr++)^)
    od
end

global proc List_Remove(ref ListStruct list, ref void element) =
    ref ref void srcPtr
    ref ref void destPtr
    ref ref void lastPtr

    srcPtr := &list^.slots[list^.first]
    destPtr := &list^.slots[0]
    lastPtr := &list^.slots[list^.last]
    list^.last := list^.last-list^.first
    list^.first := 0
    while srcPtr<=lastPtr do
        if srcPtr^=element then
            list^.last--
        else
            (destPtr++)^ := srcPtr^
        fi
        srcPtr++
    od
end

global function List_RemoveFirst(ref ListStruct list)ref void =
    ref void element

    if list^.last<list^.first then
        return 0
    fi
    element := list^.slots[list^.first++]
    return element
end

global proc List_RemoveAll(ref ListStruct list) =
    list^.first := 0
    list^.last := -1
end

global proc Errorxxx(ref byte errorString) =
    printf("error: %s.\n",errorString)
    exit(-1)
end

global proc Execute(ref ConstraintStruct c) =
    c^.execute(c)
end

global proc Noop(ref ConstraintStruct c) =
end

global proc FreeVariable(ref VariableStruct v) =
    ref ConstraintStruct c
    int32 i

    c := ref ConstraintStruct(List_RemoveFirst(v^.constraints))
    while c<>0 do
        i := c^.varCount-1
        while i>=0 do
            List_Remove((c^.variables[i])^.constraints,ref void(c))
            i--
        od
        Constraint_Destroy(c)
        c := ref ConstraintStruct(List_RemoveFirst(v^.constraints))
    od
    Variable_Destroy(v)
end

global proc AddIfSatisfiedInput(ref ConstraintStruct c) =
    if c^.inputFlag and c^.whichMethod<>-1 then
        List_Add(hot,c)
    fi
end

global proc CollectSatisfiedInputs(ref VariableStruct v) =
    List_Do(v^.constraints,&AddIfSatisfiedInput)
end

global function MakePlan()ref ListStruct =
    ref ListStruct plan
    ref ConstraintStruct nextC
    ref VariableStruct out

    NewMark()
    plan := List_Create(128)
    nextC := ref ConstraintStruct(List_RemoveFirst(hot))
    while nextC<>0 do
        out := nextC^.variables[nextC^.methodOuts[nextC^.whichMethod]]
        if out^.mark<>currentMark and InputsKnown(nextC) then
            List_Add(plan,nextC)
            out^.mark := currentMark
            nextC := NextDownstreamConstraint(hot,out)
        else
            nextC := ref ConstraintStruct(List_RemoveFirst(hot))
        fi
    od
    return plan
end

global proc IncrementalAdd(ref ConstraintStruct c) =
    ref ConstraintStruct overridden

    NewMark()
    overridden := Satisfy(c)
    while overridden<>0 do
        overridden := Satisfy(overridden)
    od
end

global proc AddAtStrength(ref ConstraintStruct c) =
    if c^.strength=strength then
        IncrementalAdd(c)
    fi
end

global proc IncrementalRemove(ref ConstraintStruct c) =
    ref VariableStruct out
    int32 i

    out := c^.variables[c^.methodOuts[c^.whichMethod]]
    c^.whichMethod := -1
    i := c^.varCount-1
    while i>=0 do
        List_Remove((c^.variables[i])^.constraints,ref void(c))
        i--
    od
    unsatisfied := List_Create(8)
    RemovePropagateFrom(out)
    strength := S_required
    while strength<=S_weakest do
        List_Do(unsatisfied,&AddAtStrength)
        strength++
    od
    List_Destroy(unsatisfied)
end

global function AddPropagate(ref ConstraintStruct c)int32 =
    ref ListStruct todo
    ref ConstraintStruct nextC
    ref VariableStruct out

    todo := List_Create(8)
    nextC := c
    while nextC<>0 do
        out := nextC^.variables[nextC^.methodOuts[nextC^.whichMethod]]
        if out^.mark=currentMark then
            IncrementalRemove(c)
            return false
        fi
        Recalculate(nextC)
        nextC := NextDownstreamConstraint(todo,out)
    od
    List_Destroy(todo)
    return true
end

global proc CollectUnsatisfied(ref ConstraintStruct c) =
    if not c^.whichMethod<>-1 then
        List_Add(unsatisfied,c)
    fi
end

global proc RemovePropagateFrom(ref VariableStruct v) =
    ref ConstraintStruct nextC
    ref ListStruct todo

    v^.determinedBy := 0
    v^.walkStrength := S_weakest
    v^.stay := true
    todo := List_Create(8)
    while true do
        List_Do(v^.constraints,&CollectUnsatisfied)
        nextC := NextDownstreamConstraint(todo,v)
        if nextC=0 then
            exit
        else
            Recalculate(nextC)
            v := nextC^.variables[nextC^.methodOuts[nextC^.whichMethod]]
        fi
    od
    List_Destroy(todo)
end

global function Satisfy(ref ConstraintStruct c)ref ConstraintStruct =
    int32 outIndex
    int32 i
    ref ConstraintStruct overridden
    ref VariableStruct out

    c^.whichMethod := ChooseMethod(c)
    if c^.whichMethod<>-1 then
        outIndex := c^.methodOuts[c^.whichMethod]
        i := c^.varCount-1
        while i>=0 do
            if i<>outIndex then
                (c^.variables[i])^.mark := currentMark
            fi
            i--
        od
        out := c^.variables[outIndex]
        overridden := ref ConstraintStruct(out^.determinedBy)
        if overridden<>0 then
            overridden^.whichMethod := -1
        fi
        out^.determinedBy := c
        if not AddPropagate(c) then
            Error("Cycle encountered")
            return 0
        fi
        out^.mark := currentMark
        return overridden
    else
        if c^.strength=S_required then
            Error("Could not satisfy a required constraint")
        fi
        return 0
    fi
end

global function ChooseMethod(ref ConstraintStruct c)int32 =
    int32 best
    int32 m
    int32 bestOutStrength
    ref VariableStruct mOut

    best := -1
    bestOutStrength := c^.strength
    m := c^.methodCount-1
    while m>=0 do
        mOut := c^.variables[c^.methodOuts[m]]
        if mOut^.mark<>currentMark and mOut^.walkStrength>bestOutStrength then
            best := m
            bestOutStrength := mOut^.walkStrength
        fi
        m--
    od
    return best
end

global proc Recalculate(ref ConstraintStruct c) =
    ref VariableStruct out

    out := c^.variables[c^.methodOuts[c^.whichMethod]]
    out^.walkStrength := OutputWalkStrength(c)
    out^.stay := ConstantOutput(c)
    if out^.stay then
        c^.execute(c)
    fi
end

global function OutputWalkStrength(ref ConstraintStruct c)int32 =
    int32 outIndex
    int32 m
    int32 mOutIndex
    int32 minStrength

    minStrength := c^.strength
    outIndex := c^.methodOuts[c^.whichMethod]
    m := c^.methodCount-1
    while m>=0 do
        mOutIndex := c^.methodOuts[m]
        if mOutIndex<>outIndex and (c^.variables[mOutIndex])^.walkStrength>minStrength then
            minStrength := (c^.variables[mOutIndex])^.walkStrength
        fi
        m--
    od
    return minStrength
end

global function ConstantOutput(ref ConstraintStruct c)int32 =
    int32 outIndex
    int32 i

    if c^.inputFlag then
        return false
    fi
    outIndex := c^.methodOuts[c^.whichMethod]
    i := c^.varCount-1
    while i>=0 do
        if i<>outIndex then
            if not (c^.variables[i])^.stay then
                return false
            fi
        fi
        i--
    od
    return true
end

global function InputsKnown(ref ConstraintStruct c)int32 =
    int32 outIndex
    int32 i
    ref VariableStruct in

    outIndex := c^.methodOuts[c^.whichMethod]
    i := c^.varCount-1
    while i>=0 do
        if i<>outIndex then
            in := c^.variables[i]
            if in^.mark<>currentMark and not in^.stay and in^.determinedBy<>0 then
                return false
            fi
        fi
        i--
    od
    return true
end

global proc NewMark() =
    currentMark++
end

global function NextDownstreamConstraint(ref ListStruct todo, ref VariableStruct variable)ref ConstraintStruct =
    ref ListStruct allC
    ref ref ConstraintStruct nextPtr
    ref ref ConstraintStruct lastPtr
    ref ConstraintStruct determiningC
    ref ConstraintStruct first

    allC := variable^.constraints
    nextPtr := ref ref ConstraintStruct(&allC^.slots[allC^.first])
    lastPtr := ref ref ConstraintStruct(&allC^.slots[allC^.last])
    determiningC := variable^.determinedBy
    first := 0
    while nextPtr<=lastPtr do
        if nextPtr^<>determiningC and (nextPtr^)^.whichMethod<>-1 then
            if first=0 then
                first := nextPtr^
            else
                List_Add(todo,nextPtr^)
            fi
        fi
        nextPtr++
    od
    if first=0 then
        first := ref ConstraintStruct(List_RemoveFirst(todo))
    fi
    return first
end

global proc EqualsC_Execute(ref ConstraintStruct c) =
    switch c^.whichMethod
    when 0 then
        (c^.variables[0])^.value := (c^.variables[1])^.value
    when 1 then
        (c^.variables[1])^.value := (c^.variables[0])^.value
    end switch
end

global proc AddC_Execute(ref ConstraintStruct c) =
    switch c^.whichMethod
    when 0 then
        (c^.variables[2])^.value := (c^.variables[0])^.value+(c^.variables[1])^.value
    when 1 then
        (c^.variables[1])^.value := (c^.variables[2])^.value-(c^.variables[0])^.value
    when 2 then
        (c^.variables[0])^.value := (c^.variables[2])^.value-(c^.variables[1])^.value
    end switch
end

global function AddC(ref VariableStruct a, b, sum, int32 strength)ref ConstraintStruct =
    ref ConstraintStruct new

    new := Constraint_Create(3,strength)
    new^.execute := &AddC_Execute
    new^.variables[0] := a
    new^.variables[1] := b
    new^.variables[2] := sum
    new^.methodCount := 3
    new^.methodOuts[0] := 2
    new^.methodOuts[1] := 1
    new^.methodOuts[2] := 0
    AddConstraint(new)
    return new
end

global proc ScaleOffsetC_Execute(ref ConstraintStruct c) =
    switch c^.whichMethod
    when 0 then
        (c^.variables[3])^.value := (c^.variables[0])^.value*(c^.variables[1])^.value+(c^.variables[2])^.value
    when 1 then
        (c^.variables[0])^.value := (c^.variables[3])^.value-(c^.variables[2])^.value/(c^.variables[1])^.value
    end switch
end

global function Milliseconds()int32 =
    int32 millisecondsPerClock

    millisecondsPerClock := CLOCKS_PER_SEC/1000
    return clock()/millisecondsPerClock
end

global proc Start() =
    startTime := Milliseconds()
end

global proc Finish(ref int32 milliseconds) =
    milliseconds^ := Milliseconds()-startTime
end

global proc ChainTest(int32 n) =
    int32 msecs
    int32 i
    [20]byte name
    ref VariableStruct prev
    ref VariableStruct v
    ref VariableStruct first
    ref VariableStruct last
    ref ConstraintStruct editC
    ref ListStruct plan

    InitDeltaBlue()
    prev := first := last := 0
    i := 0
    while i<n do
        sprintf(name,"v%ld",i)
        v := Variable_Create(name,0)
        if prev<>0 then
            EqualsC(prev,v,S_required)
        fi
        if i=0 then
            first := v
        fi
        if i=n-1 then
            last := v
        fi
        prev := v
        i++
    od
    StayC(last,S_default)
    editC := EditC(first,S_strongDefault)
    plan := ExtractPlanFromConstraint(editC)
    i := 0
    while i<100 do
        first^.value := i
        ExecutePlan(plan)
        if last^.value<>i then
            Error("ChainTest failed!")
        fi
        i++
    od
    List_Destroy(plan)
    DestroyConstraint(editC)
end

global proc Change(ref VariableStruct v, int32 newValue) =
    ref ConstraintStruct editC
    int32 i
    int32 msecs
    ref ListStruct plan

    editC := EditC(v,S_strongDefault)
    plan := ExtractPlanFromConstraint(editC)
    v^.value := newValue
    i := 0
    while i<10 do
        ExecutePlan(plan)
        i++
    od
    List_Destroy(plan)
    DestroyConstraint(editC)
end

global proc ProjectionTest(int32 n) =
    ref VariableStruct src
    ref VariableStruct scale
    ref VariableStruct offset
    ref VariableStruct dest
    int32 msecs
    int32 i
    [20]byte name
    ref ListStruct dests

    InitDeltaBlue()
    scale := Variable_Create("scale",10)
    offset := Variable_Create("offset",1000)
    dests := List_Create(n)
    i := 1
    while i<=n do
        sprintf(name,"src%ld",i)
        src := Variable_Create(name,i)
        sprintf(name,"dest%ld",i)
        dest := Variable_Create(name,i)
        List_Add(dests,dest)
        StayC(src,S_default)
        ScaleOffsetC(src,scale,offset,dest,S_required)
        i++
    od
    Change(src,17)
    if dest^.value<>1170 then
        Error("Projection Test 1 failed!")
    fi
    Change(dest,1050)
    if src^.value<>5 then
        Error("Projection Test 2 failed!")
    fi
    Change(scale,5)
    i := 1
    while i<List_Size(dests) do
        if (ref VariableStruct(List_At(dests,i-1)))^.value<>i*5+1000 then
            Error("Projection Test 3 failed!")
        fi
        ++i
    od
    Change(offset,2000)
    i := 1
    while i<List_Size(dests) do
        if (ref VariableStruct(List_At(dests,i-1)))^.value<>i*5+2000 then
            Error("Projection Test 4 failed!")
        fi
        ++i
    od
    List_Destroy(dests)
end

global function main(int32 argc, ref ref byte argv)int32 =
    int32 iterations
    [100]byte options_array
    ref byte options
    int32 n
    int32 j
    int32 msecs

    iterations := 1000
    options_array := (0)
    options := options_array
    if argc>1 then
        iterations := atoi(argv[1])
    fi
    if iterations<1 then
        iterations := 1000
    fi
    if argc>2 then
        options := argv[2]
    fi
    n := 100
    Start()
    j := 0
    while j<iterations do
        ChainTest(n)
        ProjectionTest(n)
        ++j
    od
    Finish(&msecs)
    printf("DeltaBlue\tC\t<S:%s>\t%dx\t%gms\n",options,iterations,real64(msecs)/iterations)
    return 0
end
