    mlang proc     pcl_start()
    mlang proc     pcl_end(i64 fixup=0)
    mlang proc     pcl_free(i64 fixup)
    mlang proc     pcl_gen(i64 opcode,pcl p=nil)
    mlang proc     pcl_gent(i64 opcode,t,pcl p=nil)
    mlang proc     pcl_genx(i64 opcode,x,pcl p=nil)
    mlang proc     pcl_genxy(i64 opcode,x,y,pcl p=nil)
    mlang function pcl_genint(i64 a,mode=9) => pcl
    mlang function pcl_genint128(i128 a,i64 mode=10) => pcl
    mlang function pcl_genreal(r64 x) => pcl
    mlang function pcl_genreal32(r64 x) => pcl
    mlang function pcl_genstring(ichar s) => pcl
    mlang function pcl_genlabel(i64 a) => pcl
    mlang function pcl_genmem(psymbol d) => pcl
    mlang function pcl_genmemaddr(psymbol d) => pcl
    mlang proc     pcl_gencomment(ichar s)
    mlang function pcl_genname(ichar s) => pcl
    mlang function pcl_gennameaddr(ichar s) => pcl
    mlang function pcl_makesymbol(ichar s) => psymbol
    mlang function pcl_getopcode() => i64
    mlang proc     pcl_setopcode(i64 opc)
    mlang proc     pcl_settype(i64 t,size=0)
    mlang proc     pcl_setxy(i64 x,y)
    mlang proc     pcl_setscale(i64 scale)
    mlang proc     pcl_setoffset(i64 offset)
    mlang proc     pcl_addoffset(i64 offset)
    mlang proc     pcl_setincr(i64 n)
    mlang proc     pcl_setnargs(i64 n)
    mlang proc     pcl_setnmult(i64 n)
    mlang proc     pcl_setrettypes(ref []i64 types,i64 n)
    mlang proc     pcl_setexported(i64 x)
    mlang proc     pcl_setnvariadics(i64 n)
    mlang proc     pcl_setalign(i64 n)
    mlang proc     pcl_setrtsproc()
    mlang proc     pcl_setoldtype(i64 t)
    mlang proc     pcl_setpos(i64 pos)
    mlang function pcl_lasterror() => ichar
    mlang function pcl_writepclfile(ichar filename) => i64
    mlang function pcl_readpclfile(ichar filename,rtsfile=nil) => i64
    mlang function pcl_genmcl(i64 optim=0) => i64
    mlang function pcl_writeasmfile(ichar filename,i64 optim=0) => i64
    mlang function pcl_getasmstring(i64 optim=0) => ichar
    mlang function pcl_writeexefile(ichar filename,i64 optim=0) => i64
    mlang function pcl_writedllfile(ichar filename,i64 optim=0) => i64
    mlang function pcl_readrts(ichar filename) => i64
    mlang proc     pcl_endprog(i64 fixup=1,dorts=1)
    mlang proc     pcl_showss(ichar filename,i64 fexe)
    mlang proc     pcl_writeclangfile(ichar filename)
