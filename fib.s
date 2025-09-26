# GAS VERSION
    .intel_syntax noprefix

    .global fib
    .global main

    .text
# Proc fib
fib:
#?>>
    .set fib.$T1, -8
    push      rdi
#?]]
    push      rbp
    mov       rbp,	rsp
    sub       rsp,	40
    mov       edi,	ecx
#---------------
    cmp       edi,	3
    jge       L2
    mov       eax,	1
    jmp       L1
L2:
    lea       eax,	[rdi-1]
    mov       ecx,	eax
    call      fib
    mov       [rbp + fib.$T1],	eax
    lea       eax,	[rdi-2]
    mov       ecx,	eax
    call      fib
    mov       r10d,	[rbp + fib.$T1]
    add       r10d,	eax
    mov       eax,	r10d
L3:
L1:
#---------------
    add       rsp,	40
    pop       rbp
    pop       rdi
    ret       
# End 
# Proc main
main:
#?>>
    push      rdi
#?]]
    sub       rsp,	48
#---------------
    mov       edi,	1
    jmp       L7
L8:
    mov       ecx,	edi
    call      fib
    lea       rcx,	[rip+L9]
    mov       edx,	edi
    mov       r8d,	eax
    call      printf
    inc       edi
L7:
    cmp       edi,	36
    jle       L8
    lea       rcx,	[rip+L10]
    call      puts
    xor       ecx,	ecx
    call      exit
#---------------
    add       rsp,	48
    pop       rdi
    ret       
# End 
#String Table
    .data
    .align    8
L10:
    .byte     0
L9:
    .ascii    "%d %d"
    .byte     10
    .byte     0



