### 'M' Reserved Words

````
    abs          acos         and          asin         asm          
    assem        atan         atan2        binclude     bitwidth     
    bool         bool64       bool8        bounds       by           
    byte         bytes        c64          c8           case         
    cast         ceil         char         char64       char8        
    clamp        clear        const        cos          divrem       
    do           docase       doswitch     doswitchu    downto       
    else         elsecase     elseswitch   elsif        end          
    enumdata     esac         eval         even         exit         
    exp          export       false        fi           floor        
    fmod         for          fprint       fprintln     fract        
    fun          func         function     global       goto         
    i16          i32          i64          i8           iand         
    ichar        if           import       importdll    in           
    include      infinity     inot         inrev        int          
    int16        int32        int64        int8         ior          
    istrue       ixor         label        len          let          
    linkdll      log          log10        lsb          lsbit        
    lsw          lwb          macro        max          min          
    module       msb          msbit        msw          nextloop     
    nil          not          notin        od           odd          
    or           pi           print        println      proc         
    r32          r64          range        read         readln       
    real         real32       real64       recase       record       
    redoloop     ref          rem          repeat       return       
    round        sign         sin          sinclude     slice        
    sliceptr     sprint       sqr          sqrt         static       
    stop         struct       swap         switch       tabledata    
    tan          then         threadedproc to           true         
    type         typestr      u16          u32          u64          
    u8           union        unless       until        upb          
    var          void         when         while        word         
    word16       word32       word64       word8        xor          
    project
````

Includes:
* Built-in types
* Choice of type names (eg `i64` and `int64`)
* Choice of block endings (eg. `fi esac od` can be used in place of `end` or `end if` etc)
* Operators that tend to be written as symbols in some languages (eg. `iand` in place of `&`)
* Mathematical functions
* Statements usually handled by libraries (eg. `print`)
* 

Any reserved word can be used an identifier via a backtick prefix:
````
   int `int
   `int := 0
````
