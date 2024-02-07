;	Module:jump
%include "mccasm.inc"

;Offsets in buffer:
%define kreturn	0
%define kstack	8
%define kframe	16


	global $mccsetjmp
	segment .text
$mccsetjmp:

;on entry to setjmp:
;Dstack		points to return address
;D10		points to address of buffer to store restore info
;Caller will have subtracted 32 from Dstack, and will add it again on return

; Store current state

	mov [D10+kstack],Dstack
	mov [D10+kframe],Dframe
	mov D0,[Dstack]			; return address
	mov [D10+kreturn],D0

	mov	A0,0
	ret

	global $mcclongjmp

$mcclongjmp:

;on entry to longjmp:
;Dstack		points to return address
;D10		points to address of buffer containing store restore info
;D11		has return value to use
;Caller will have subtracted 32 from Dstack, and will add it again on return

; Restore state as it was on call to setjmp


	mov Dstack,[D10+kstack]		; restore stack value
	mov Dframe,[D10+kframe]		; restore frame ptr

	mov D0,[D10+kreturn]		; stored return address
	mov [Dstack+0],D0			; replace return address, as it will return elsewhere
	mov A0,A11					; return value (from 'setjmp', as it will be)

	ret

;Float routines for unsigned
;Input passed in D10
;Output in XMM15

	global m$ufloat_r64u32
m$ufloat_r64u32:
	mov D10,D10					; clear top half (already done if value just moved there)
	cvtsi2sd XMM15,D10
	ret

	global m$ufloat_r32u32
m$ufloat_r32u32:
	mov D10,D10
	cvtsi2ss XMM15,D10
	ret

	global m$ufloat_r64u64
m$ufloat_r64u64:
	cmp D10,0
	jl fl1
;number is positive, so can treat like i64
	cvtsi2sd XMM15,D10
	ret
fl1:						;negative value
	and D10,[mask63]		;clear top bit (subtract 2**63)
	cvtsi2sd XMM15,D10
	addsd XMM15,[offset64]	;(add 2**63 back to result)
	ret

	global m$ufloat_r32u64
m$ufloat_r32u64:
	cmp D10,0
	jl fl2
;number is positive, so can treat like i64
	cvtsi2ss XMM15,D10
	ret
fl2:						;negative value
	and D10,[mask63]		;clear top bit (subtract 2**63)
	cvtsi2ss XMM15,D10
	addss XMM15,[offset32]	;(add 2**63 back to result)
	ret

	segment .data
mask63:
	dq 0x7fffffffffffffff
offset64:
	dq 9223372036854775808.0		; 2**63 as r64
offset32:
	dd 9223372036854775808.0		; 2**63 as r32

