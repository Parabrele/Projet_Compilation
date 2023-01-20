	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
	movq $S_3, %rax
	movq %rax, %rsi
	call print_string
E_main:
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
print_string:
	movq $S_2, %rdi
	call printf
	ret
	.data
S_2:
	.string "%s"
S_3:
	.string "ah"
S_1:
	.string " "
