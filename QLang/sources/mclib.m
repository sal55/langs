export type filehandle=ref void

importdll $cstd=
	func malloc		(word64)ref void
	func realloc	(ref void, word)ref void
	proc free		(ref void)
	proc memset		(ref void, int32, word)
	proc memcpy		(ref void, ref void, word)
	proc memmove		(ref void, ref void, word)
	func clock		:int32
	func ftell		(filehandle)int32
	func fseek		(filehandle, int32, int32)int32
	func fread		(ref void, word, word, filehandle)word
	func fwrite		(ref void, word, word, filehandle)word
	func getc		(filehandle)int32
	func ungetc		(int32, filehandle)int32
	func fopen		(ichar a, b="rb")filehandle
	func fclose		(filehandle)int32
	func fgets		(ichar, int, filehandle)ichar
	func remove		(ichar)int32
	func rename		(ichar, ichar)int32
	func getchar	:int32
	proc putchar	(int32)
	proc setbuf		(filehandle, ref byte)

	func strlen		(ichar)int
	func strcpy		(ichar, ichar)ichar
	func strcmp		(ichar, ichar)int32
	func strncmp	(ichar, ichar, word)int32
	func strncpy	(ichar, ichar, word)word
	func memcmp		(ref void, ref void, word)int32
	func strcat		(ichar, ichar)ichar
	func tolower	(int32)int32
	func toupper	(int32)int32
	func isalpha	(int32)int32
	func isupper	(int32)int32
	func islower	(int32)int32
	func isalnum	(int32)int32
	func isspace	(int32)int32
	func strstr		(ichar, ichar)ichar
	func atol		(ichar)int
	func atoi		(ichar)int32
	func strtod		(ichar,ref ref char)real64
	func _strdup	(ichar)ichar

	func puts		(ichar)int32
	func printf		(ichar, ...)int32

	func sprintf	(ichar, ichar, ...)int32

	func sscanf		(ichar, ichar, ...)int32
	func scanf		(ichar, ...)int32

	func rand		:int32
	proc srand		(word32)
	func system		(ichar)int32

	func fgetc		(filehandle)int32
	func fputc		(int32,  filehandle)int32
	func fprintf	(filehandle, ichar, ...)int32
	func fputs		(ichar,  filehandle)int32
	func feof		(filehandle)int32
	func getch		:int32
	func _getch		:int32
	func kbhit		:int32
	func _mkdir		(ichar)int32
	func mkdir		(ichar)int32
	func strchr		(ichar,int32)ichar

	func _setmode	(int32,int32)int32

	proc _exit		(int32)
	proc "exit"		(int32)
!	proc `exit		(int32)
	func pow		(real,real)real

	func `sin 		(real)real
	func `cos		(real)real
	func `tan		(real)real
	func `asin		(real)real
	func `acos		(real)real
	func `atan 		(real)real
	func `log		(real)real
	func `log10		(real)real
	func `exp		(real)real
	func `floor		(real)real
	func `ceil		(real)real

	proc  qsort   	(ref void, word64, word64, ref proc)

end

export macro strdup=_strdup

importdll $cstdextra=
	func __getmainargs	(ref int32, ref void, ref void, int, ref void)int32
end

export const c_eof		=-1
export const seek_set	= 0
export const seek_curr	= 1
export const seek_end	= 2
