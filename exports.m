
    func pcm_alloc(i64 n) => ref void
    proc pcm_free(ref void p,i64 n)
    proc pcm_freeac(ref void p,i64 alloc)
    proc pcm_clearmem(ref void p,i64 n)
    proc pcm_init()
    func pcm_getac(i64 size) => i64
    func pcm_newblock(i64 itemsize) => ref void
    func pcm_round(i64 n) => i64
    func pcm_allocz(i64 n) => ref void
    func pcm_copyheapstring(ichar s) => ichar
    func pcm_copyheapstringn(ichar s,i64 n) => ichar
    func pcm_copyheapblock(ichar s,i64 length) => ichar
    func allocmem(i64 n) => ref void
    proc abortprogram(ichar s)
    func getfilesize(filehandle handlex) => i64
    proc readrandom(filehandle handlex,ref u8 mem,i64 offset,size)
    func writerandom(filehandle handlex,ref u8 mem,i64 offset,size) => i64
    func setfilepos(filehandle file,i64 offset) => i64
    func getfilepos(filehandle file) => i64
    func readfile(ichar filename) => ref u8
    func writefile(ichar filename,ref u8 data,i64 size) => i64
    func checkfile(ichar file) => i64
    proc readlinen(filehandle handlex,ichar buffer,i64 size)
    proc iconvlcn(ichar s,i64 n)
    proc iconvucn(ichar s,i64 n)
    func convlcstring(ichar s) => ichar
    func convucstring(ichar s) => ichar
    func changeext(ichar s,newext) => ichar
    func extractext(ichar s,i64 period=0) => ichar
    func extractpath(ichar s) => ichar
    func extractfile(ichar s) => ichar
    func extractbasefile(ichar s) => ichar
    func addext(ichar s,newext) => ichar
    func pcm_alloc32() => ref void
    proc pcm_free32(ref void p)
    func pcm_alloc64() => ref void
    proc pcm_free64(ref void p)
    func pcm_alloc16() => ref void
    proc pcm_free16(ref void p)
    proc outbyte(filehandle f,i64 x)
    proc outword16(filehandle f,u64 x)
    proc outword32(filehandle f,u64 x)
    proc outword64(filehandle f,u64 x)
    proc outstring(filehandle f,ichar s)
    proc outblock(filehandle f,ref void p,i64 n)
    func myeof(filehandle f) => i64
    proc strbuffer_add(ref strbuffer dest,ichar s,i64 n=-1)
    proc gs_init(ref strbuffer dest)
    proc gs_free(ref strbuffer dest)
    proc gs_str(ref strbuffer dest,ichar s)
    proc gs_char(ref strbuffer dest,i64 c)
    proc gs_strn(ref strbuffer dest,ichar s,i64 length)
    proc gs_strvar(ref strbuffer dest,s)
    proc gs_strint(ref strbuffer dest,i64 a)
    proc gs_strln(ref strbuffer dest,ichar s)
    proc gs_strsp(ref strbuffer dest,ichar s)
    proc gs_line(ref strbuffer dest)
    func gs_getcol(ref strbuffer dest) => i64
    proc gs_leftstr(ref strbuffer dest,ichar s,i64 w,padch=32)
    proc gs_leftint(ref strbuffer dest,i64 a,w,padch=32)
    proc gs_padto(ref strbuffer dest,i64 col,ch=32)
    proc gs_println(ref strbuffer dest,filehandle f=nil)
    func nextcmdparamnew(i64 &paramno,ichar &name,ichar &value,ichar defext=nil) => i64
    proc ipadstr(ichar s,i64 width,ichar padchar=" ")
    func padstr(ichar s,i64 width,ichar padchar=" ") => ichar
    func chr(i64 c) => ichar
    func cmpstring(ichar s,t) => i64
    func cmpstringn(ichar s,t,i64 n) => i64
    func eqstring(ichar s,t) => i64
    func cmpbytes(ref void p,q,i64 n) => i64
    func eqbytes(ref void p,q,i64 n) => i64
    proc mseed(u64 a,b=0)
    func mrandom() => u64
    func mrandomp() => i64
    func mrandomint(i64 n) => i64
    func mrandomrange(i64 a,b) => i64
    func mrandomreal() => r64
    func mrandomreal1() => r64
    func checkpackfile() => ref u8
    func readline() => ichar
    func findfunction(ichar name) => ref void
    func roundtoblock(i64 n,align) => i64
    proc os_init()
    func os_execwait(ichar cmdline,i64 newconsole=0,ichar workdir=nil) => i64
    func os_execcmd(ichar cmdline,i64 newconsole=0) => i64
    func os_getch() => i64
    func os_kbhit() => i64
    func os_getdllinst(ichar name) => u64
    func os_getdllprocaddr(i64 hinst,ichar name) => ref void
    proc os_initwindows()
    proc os_gxregisterclass(ichar classname)
    proc os_setmesshandler(ref void addr)
    func os_getchx() => i64
    func os_getos() => ichar
    func os_gethostsize() => i64
    func os_shellexec(ichar opc,file) => i64
    proc os_sleep(i64 a)
    func os_getstdin() => filehandle
    func os_getstdout() => filehandle
    func os_gethostname() => ichar
    func os_getmpath() => ichar
    func os_clock() => i64
    func os_ticks() => i64
    func os_iswindows() => i64
    proc os_getsystime(ref rsystemtime tm)
    proc os_peek()
    func os_allocexecmem(i64 n) => ref u8
    func os_calldllfunction(ref proc() fnaddr,i64 retcode,nargs,ref []i64 args,ref []u8 argcodes) => u64
    proc main()
    proc m$print_startfile(ref void dev)
    proc m$print_startstr(ichar s)
    proc m$print_startptr(ref ichar p)
    proc m$print_startcon()
    proc m$print_setfmt(ichar format)
    proc m$print_end()
    proc m$print_ptr(u64 a,ichar fmtstyle=nil)
    proc m$print_ptr_nf(u64 a)
    proc m$print_i64(i64 a,ichar fmtstyle=nil)
    proc m$print_i64_nf(i64 a)
    proc m$print_bool(i64 a,ichar fmtstyle=nil)
    proc m$print_u64(u64 a,ichar fmtstyle=nil)
    proc m$print_r64(r64 x,ichar fmtstyle=nil)
    proc m$print_r32(r32 x,ichar fmtstyle=nil)
    proc m$print_str(ichar s,fmtstyle=nil)
    proc m$print_strn(ichar s,i64 length,ichar fmtstyle=nil)
    proc m$print_str_nf(ichar s)
    proc m$print_strsl(slice[]c8 &s,ichar fmtstyle=nil)
    proc m$print_newline()
    proc m$print_nogap()
    proc m$print_space()
    proc printstr(ichar s)
    proc printstr_n(ichar s,i64 n)
    proc printstrn_app(ichar s,i64 length,filehandle f=nil)
    func strint(i64 a,ichar fmtstyle=nil) => ichar
    proc getstrint(i64 a,ichar dest)
    func strword(u64 a,ichar fmtstyle=nil) => ichar
    func strreal(r64 a,ichar fmtstyle=nil) => ichar
    func getstr(ichar s,ref fmtrec fmt) => ichar
    func strtoint(ichar s,i64 length=-1,u64 base=10) => i64
    proc readstr(ichar dest,i64 fmt=0,destlen=0)
    proc rereadln()
    proc reread()
    func valint(ichar s,i64 fmt=0) => i64
    func valreal(ichar s) => r64
