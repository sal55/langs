import clib
import mlib

const maxmodule=250
const maxnamelen=32

const infile="mm.ma"

proc start=
	filehandle f
	[maxnamelen]char name
	ref byte s

	static [maxmodule,maxnamelen]char names
	static [maxmodule]int sizes
	static [maxmodule]int offsets
	static [maxmodule]byte issupport
	int nfiles, index

	f:=fopen(infile,"rb")
	if f=nil then abortprogram("Can't open "+infile) fi

	readln @f
	readstr(&.name,,maxnamelen)
	read nfiles

	if nfiles>maxmodule then
		abortprogram("Too many modules")
	fi

	for i to nfiles do
		readln @f, index
		readstr(&.names[i],'n',maxnamelen)
		read sizes[i], offsets[i], issupport[i]
	od

	fclose(f) 

	s:=readfile(infile)
	if s=nil then abortprogram("Error reading while file") fi

	for i to nfiles do
		extractfile(s,&.names[i],sizes[i],offsets[i])
	od

end

proc extractfile(ref byte s, ichar name, int size, offset)=
	[300]char path

	strcpy(&.path, "./sources/")
	strcat(&.path, name)

	println "Writing:",&.path

	if not writefile(&.path, s+offset, size) then
		println &.path
		abortprogram("Error writing file")
	fi
end
