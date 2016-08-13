	.file	"factorial.c"
	.text
	.globl	fact
	.type	fact, @function
fact:
.LFB0:
	.cfi_startproc
	pushq	%rbp ;put frame pointer on the stack
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp ;move the stack pointer to the frame pointer
	subq	$16, %rsp ;move the stack pointer down.
	movl	%edi, -4(%rbp) ;argument (int arg) is put 4 places away from rbp.
	cmpl	$0, -4(%rbp) ;compare 0 and arg (arg == 0).
	jne	.L2 ;jump to else
	movl	$1, %eax ;return 1 (eax is a return register)
	jmp	.L3 ;skip over else
.L2:
	movl	-4(%rbp), %eax ;put (int arg) into %eax
	subl	$1, %eax ;arg - 1
	movl	%eax, %edi ;passes to the next argument
	call	fact ;recursion!!!!
	imull	-4(%rbp), %eax ;hmmm...
.L3:
	leave ;movq %rbp, %rsp then popq %rbp
	.cfi_def_cfa 7, 8
	ret ;popq %rip
	.cfi_endproc
nonRecFact:
        movl $1, %eax
.while
        cmpl $0, %edi
        je .ret
        imull %edi, %eax
        subl $1, %edi
        jmp .while
.ret:
        ret
adder:
        movl (%edi, %esi,), %eax
        leal (%edi, %esi,), %eax
        ret ; a (%b, %c, d) == b + c * d (constant) + a (constant = 1 ...)
        ; int * b; b[c] + a (d is sizeof(*b))
.LFE0:
	.size	fact, .-fact
	.ident	"GCC: (Ubuntu 5.3.1-14ubuntu2.1) 5.3.1 20160413"
	.section	.note.GNU-stack,"",@progbits
