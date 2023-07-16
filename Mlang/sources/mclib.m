export type filehandle=ref void

importdll $cstd=
	function  malloc		(word64)ref void
	function  realloc	(ref void, word)ref void
	proc free		(ref void)
	proc memset		(ref void, int32, word)
	proc memcpy		(ref void, ref void, word)
	proc memmove		(ref void, ref void, word)
	function  clock		:int32
	function  ftell		(filehandle)int32
	function  fseek		(filehandle, int32, int32)int32
	function  fread		(ref void, word, word, filehandle)word
	function  fwrite		(ref void, word, word, filehandle)word
	function  getc		(filehandle)int32
	function  ungetc		(int32, filehandle)int32
	function  fopen		(ichar a, b="rb")filehandle
	function  fclose		(filehandle)int32
	function  fgets		(ichar, int, filehandle)ichar
	function  remove		(ichar)int32
	function  rename		(ichar, ichar)int32
	function  getchar	:int32
	proc putchar	(int32)
	proc setbuf		(filehandle, ref byte)

	function  strlen		(ichar)int
	function  strcpy		(ichar, ichar)ichar
	function  strcmp		(ichar, ichar)int32
	function  strncmp	(ichar, ichar, word)int32
	function  strncpy	(ichar, ichar, word)word
	function  memcmp		(ref void, ref void, word)int32
	function  strcat		(ichar, ichar)ichar
	function  tolower	(int32)int32
	function  toupper	(int32)int32
	function  isalpha	(int32)int32
	function  isupper	(int32)int32
	function  islower	(int32)int32
	function  isalnum	(int32)int32
	function  isspace	(int32)int32
	function  strstr		(ichar, ichar)ichar
	function  atol		(ichar)int
	function  atoi		(ichar)int32
!	function  strtod		(ichar,ref ref char)real64
	function  strtod		(ichar,ref ref char)real64
	function  _strdup	(ichar)ichar

	function  puts		(ichar)int32
	function  printf		(ichar, ...)int32

	function  sprintf	(ichar, ichar, ...)int32

	function  sscanf		(ichar, ichar, ...)int32
	function  scanf		(ichar, ...)int32

	function  rand		:int32
	proc srand		(word32)
	function  system		(ichar)int32

	function  fgetc		(filehandle)int32
	function  fputc		(int32,  filehandle)int32
	function  fprintf	(filehandle, ichar, ...)int32
	function  fputs		(ichar,  filehandle)int32
	function  feof		(filehandle)int32
	function  getch		:int32
	function  _getch		:int32
	function  kbhit		:int32
	function  _mkdir		(ichar)int32
	function  mkdir		(ichar)int32
	function  strchr		(ichar,int32)ichar

	function  _setmode	(int32,int32)int32

	proc _exit		(int32)
	proc "exit"		(int32)
!	proc `exit		(int32)
	function  pow		(real,real)real

	function  `sin 		(real)real
	function  `cos		(real)real
	function  `tan		(real)real
	function  `asin		(real)real
	function  `acos		(real)real
	function  `atan 		(real)real
	function  `log		(real)real
	function  `log10		(real)real
	function  `exp		(real)real
	function  `floor		(real)real
	function  `ceil		(real)real

	proc  qsort   	(ref void, word64, word64, ref proc)

end

export macro strdup=_strdup

importdll $cstdextra=
	function  __getmainargs(ref int32, ref void, ref void, int, ref void)int32
end

export const c_eof		=-1
export const seek_set	= 0
export const seek_curr	= 1
export const seek_end	= 2
