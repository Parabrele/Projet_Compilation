	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
	movq $24, %rdi
	pushq %rbp
	movq %rsp, %rbp
	movq $0xfffffffffffffff0, %rax
	andq %rax, %rsp
	call malloc
	movq %rbp, %rsp
	popq %rbp
	movq %rax, %rdi
	movq $0, %rsi
	movq $3, %rdx
	pushq %rbp
	movq %rsp, %rbp
	movq $0xfffffffffffffff0, %rax
	andq %rax, %rsp
	call memset
	movq %rbp, %rsp
	popq %rbp
	movq %rdi, %rax
	pushq %rax
	movq $5, %rax
	pushq %rax
	movq -8(%rbp), %rax
	addq $0, %rax
	popq %rsi
	movq %rsi, 0(%rax)
	addq $8, %rsp
E_main:
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
S_1:
	.string " "
