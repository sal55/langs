;	bcc support library

;Offsets in buffer:
kreturn	= 0
kstack	= 8
kframe	= 16

	segment code
$mccsetjmp::

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

$mcclongjmp::

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

m$ufloat_r64u32::
	mov D10,D10					; clear top half (already done if value just moved there)
	cvtsi2sd XMM15,D10
	ret

m$ufloat_r32u32::
	mov D10,D10
	cvtsi2ss XMM15,D10
	ret

m$ufloat_r64u64::
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

m$ufloat_r32u64::
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

	segment idata
mask63:
	dq 0x7fffffffffffffff
offset64:
	dq 9223372036854775808.0		! 2**63 as r64
offset32:
	dd 9223372036854775808.0		; 2**63 as r32

	segment code
__rdtsc::
!	rdtsc
	mov eax,eax
	shl rdx,32
	or rax,rdx
	ret

	segment zdata
callbackstack:
	resb 576			!8-level stack
;	resb 5'120'000

ncallbacks:
	resb 4

segment code

m$pushcallback::
	inc dword [ncallbacks]
	mov A4,[ncallbacks]
	shl A4,6					!8x8 bytes is size per entry
	lea D4,[A4+callbackstack]

	mov [D4],rbx
	mov [D4+8],rsi
	mov [D4+16],rdi
	mov [D4+24],r12
	mov [D4+32],r13
	mov [D4+40],r14
	mov [D4+48],r15
	ret

m$popcallback::
	mov A4,[ncallbacks]
	shl A4,6					!8x8 bytes is size per entry
	lea D4,[A4+callbackstack]
	mov rbx,[D4]
	mov rsi,[D4+8]
	mov rdi,[D4+16]
	mov r12,[D4+24]
	mov r13,[D4+32]
	mov r14,[D4+40]
	mov r15,[D4+48]
	dec dword [ncallbacks]
	ret


