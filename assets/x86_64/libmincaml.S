// Must conform to System V AMD64 ABI

#if defined(__CYGWIN__) || defined(__MACH__)
#define U(x) _##x
#else
#define U(x) x
#endif
#if defined(__MACH__)
#define ALIGNSTACK0 andq $-16, %rsp
#define ALIGNSTACK1 andq $-16, %rsp; pushq %rax; pushq %rax; pushq %rax
#define ALIGNSTACK2 andq $-16, %rsp; pushq %rax; pushq %rax
#define ALIGNSTACK3 andq $-16, %rsp; pushq %rax
#else
#define ALIGNSTACK0
#define ALIGNSTACK1
#define ALIGNSTACK2
#define ALIGNSTACK3
#endif
.text
.globl min_caml_print_newline
min_caml_print_newline:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK1
	pushq	$10
	call	U(putchar)
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_print_int
min_caml_print_int:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK2
	movq	%rax, %rsi
	movq	format_int@GOTPCREL(%rip), %rdi
	movb	$0, %al
	callq	U(printf)
	movq	%rbp, %rsp
	popq	%rbp
	retq
.data
format_int:
	.asciz	"%d"
format_float:
	.asciz	"%lf"
.balign 8
float_0:
	.long	0x0
	.long	0x0
float_1:
	.long	0x0
	.long	0x3ff00000
.balign 16
.globl min_caml_fnegd
min_caml_fnegd:
	.long	0
	.long	0x80000000
	.long	0
	.long	0
.globl min_caml_hp
min_caml_hp:
	.long	0x0
