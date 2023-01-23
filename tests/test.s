	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_fact:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rax
	pushq %rax
	movq $1, %rax
	cmpq %rax, 0(%rsp)
	setle %al
	movzbq %al, %rax
	popq %rcx
	cmpq $0, %rax
	je L_1
	movq $1, %rax
	movq %rax, 24(%rbp)
	jmp E_fact
	jmp L_2
L_1:
L_2:
	subq $8, %rsp
	movq $1, %rax
	pushq %rax
	movq 16(%rbp), %rax
	subq 0(%rsp), %rax
	popq %rcx
	pushq %rax
	call F_fact
	addq $8, %rsp
	popq %rax
	movq %rax, 24(%rbp)
	jmp E_fact
E_fact:
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
	movq $0, %rax
	pushq %rax
	jmp L_3
L_4:
	subq $8, %rsp
	movq -8(%rbp), %rax
	pushq %rax
	call F_fact
	addq $8, %rsp
	popq %rsi
	call print_int
	movq $S_4, %rax
	movq %rax, %rsi
	call print_string
	leaq -8(%rbp), %rax
	incq 0(%rax)
L_3:
	movq -8(%rbp), %rax
	pushq %rax
	movq $10, %rax
	cmpq %rax, 0(%rsp)
	setle %al
	movzbq %al, %rax
	popq %rcx
	cmpq $1, %rax
	je L_4
	addq $8, %rsp
E_main:
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
print_string:
	pushq %rbp
	movq %rsp, %rbp
	movq $0xfffffffffffffff0, %rax
	andq %rax, %rsp
	movq $S_2, %rdi
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
print_int:
	pushq %rbp
	movq %rsp, %rbp
	movq $0xfffffffffffffff0, %rax
	andq %rax, %rsp
	movq $S_3, %rdi
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
S_2:
	.string "%s"
S_4:
	.string "\n"
S_3:
	.string "%ld"
S_1:
	.string " "
