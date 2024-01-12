
int cmdstartindex

proc main=
	ichar source
	int i,nnames,t,tstart, stopcode
	unit p

!CPL =UNITREC.BYTES
!CPL =klastpcl
!STOP

	initdata()

	getinputoptions()

!	if fverbose then
!		println dispatchnames[dispatchtype],"Opt:",foptimise
!	fi

	readqabundle()
	loadsyslib()

	compile_sp(inputfile)

	if fallsp then
		if fshowast1 and runcode=parse_cc then showast(nil, "AST1") fi
		if fshowast2 and runcode>parse_cc then showast(nil, "AST2") fi
		if (fshowpcl1 or fshowpcl2) and runcode=gencode_cc then showpcl(nil, 1) fi
		if fshowpcl3 and runcode=fixup_cc then showpcl(nil, 3) fi
	fi

!run the stack of sps (laters sps will be run as they are compiled)

	writeqafile()

!	for i to nsubprogs do
!		stopcode:=runprogram(subprogs[i])
!	od
	stopcode:=runqprogram(subprogs[nsubprogs])

	showlogfile()

	stop stopcode
end

proc getinputoptions=
	int paramno,pmtype
	ichar name,value
	ichar appstr, appname
	ref function:ichar fnaddr

!fnaddr will be nil unless a built-in app exists
	fnaddr:=findfunction("getbuiltin_app")

	paramno:=1

	while pmtype:=nextcmdparamnew(paramno,name,value,"q") do
		case pmtype
		when pm_option then
			convlcstring(name)
			for sw to optionnames.len do
				if eqstring(name,optionnames[sw]) then
					do_option(sw,value)
					exit
				fi
			else
				println "Unknown option:",name
				stop 99
			od
		when pm_sourcefile then
			if fnaddr then				!treat as data
				--paramno
				exit
			fi
			inputfile:=pcm_copyheapstring(name)
			exit				!leave any other files to app being run
		esac
	od

	if fnaddr then
		appstr:=fnaddr()
!		dobuiltin_app(appstr, appname)
LOADERROR("DO BUILT-IN")

!		dobuiltin_app(appstr)
	elsif not inputfile then
		println "Q5.2 Interpreter"
		println "Usage:"
		println "	",,sysparams[1],"filename[.q]"
		stop
	fi

	if dispatchtype in [debug_dispatch,fdebug_dispatch] then
		hasbytecodes:=1
	else
		hasbytecodes:=0
	fi

	cmdstartindex:=paramno

	setcli(cast(&cmdparams[cmdstartindex]),ncmdparams-cmdstartindex+1)
end

proc do_option(int sw, ichar value)=
	ref byte p

	p:=optionvars[sw]
	if p then
		p^:=optionvalues[sw]
		return
	fi

	case sw
	when asmopt_sw then
		foptimise:=1
		dispatchtype:=asm_dispatch
	esac

end

