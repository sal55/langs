!extract *.* files from mc.ma (etc) into fred/*.*

const mafile = "mc.ma"

proc main=
	filehandle f, g:=nil
	[1000]char str
	[300]char filespec
	char c

	f:=fopen(mafile, "rb")
	if f=nil then
		println "Can't find",mafile
		stop
	fi

	readln @f			# skip === ma === header

	while not myeof(f) do
		readln @f

		read c:'c'

		if c='=' then
			read c,c
			readstr(str, 'n')

			if g then
				fclose(g)
			fi
			strcpy(filespec,"fred/")
			strcat(filespec,str)
			if eqstring(str,"end") then
				exit
			fi

			g:=fopen(filespec,"wb")
			if g then
				println "Extracting",str,"to",filespec
			else
				println "Can't write",filespec
				stop
			fi

		else
			reread()
			readstr(str,'L')
			if g then
				println @g,str
			fi

		fi
	od

	fclose(f)
end
