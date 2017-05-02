	.text
	.file	"main.ll"
	.section	.text.startup,"ax",@progbits
	.align	16, 0x90
	.type	__cxx_global_var_init,@function
__cxx_global_var_init:                  # @__cxx_global_var_init
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp0:
	.cfi_def_cfa_offset 16
.Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp2:
	.cfi_def_cfa_register %rbp
	movl	$_ZStL8__ioinit, %edi
	callq	_ZNSt8ios_base4InitC1Ev
	movl	$_ZNSt8ios_base4InitD1Ev, %edi
	movl	$_ZStL8__ioinit, %esi
	movl	$__dso_handle, %edx
	callq	__cxa_atexit
	popq	%rbp
	retq
.Lfunc_end0:
	.size	__cxx_global_var_init, .Lfunc_end0-__cxx_global_var_init
	.cfi_endproc

	.text
	.globl	_Z7displayv
	.align	16, 0x90
	.type	_Z7displayv,@function
_Z7displayv:                            # @_Z7displayv
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp3:
	.cfi_def_cfa_offset 16
.Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp5:
	.cfi_def_cfa_register %rbp
	movl	$16640, %edi            # imm = 0x4100
	callq	glClear
	callq	glutSwapBuffers
	popq	%rbp
	retq
.Lfunc_end1:
	.size	_Z7displayv, .Lfunc_end1-_Z7displayv
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4630826316843712512     # double 40
.LCPI2_1:
	.quad	4607182418800017408     # double 1
.LCPI2_2:
	.quad	4621819117588971520     # double 10
.LCPI2_3:
	.quad	4617315517961601024     # double 5
	.section	.rodata.cst4,"aM",@progbits,4
	.align	4
.LCPI2_4:
	.long	3212836864              # float -1
.LCPI2_5:
	.long	1114636288              # float 60
.LCPI2_6:
	.long	1065353216              # float 1
.LCPI2_7:
	.long	3248488448              # float -20
	.text
	.globl	_Z4initv
	.align	16, 0x90
	.type	_Z4initv,@function
_Z4initv:                               # @_Z4initv
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp6:
	.cfi_def_cfa_offset 16
.Ltmp7:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp8:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$2929, %edi             # imm = 0xB71
	callq	glEnable
	movl	$5889, %edi             # imm = 0x1701
	callq	glMatrixMode
	movsd	.LCPI2_0(%rip), %xmm0   # xmm0 = mem[0],zero
	movsd	.LCPI2_1(%rip), %xmm1   # xmm1 = mem[0],zero
	movsd	.LCPI2_2(%rip), %xmm3   # xmm3 = mem[0],zero
	movapd	%xmm1, %xmm2
	callq	gluPerspective
	movl	$5888, %edi             # imm = 0x1700
	callq	glMatrixMode
	movq	$0, (%rsp)
	movsd	.LCPI2_3(%rip), %xmm2   # xmm2 = mem[0],zero
	xorpd	%xmm0, %xmm0
	xorpd	%xmm1, %xmm1
	xorpd	%xmm3, %xmm3
	xorps	%xmm4, %xmm4
	xorps	%xmm5, %xmm5
	xorps	%xmm6, %xmm6
	movsd	.LCPI2_1(%rip), %xmm7   # xmm7 = mem[0],zero
	callq	gluLookAt
	movss	.LCPI2_4(%rip), %xmm2   # xmm2 = mem[0],zero,zero,zero
	xorpd	%xmm0, %xmm0
	xorpd	%xmm1, %xmm1
	callq	glTranslatef
	movss	.LCPI2_5(%rip), %xmm0   # xmm0 = mem[0],zero,zero,zero
	movss	.LCPI2_6(%rip), %xmm1   # xmm1 = mem[0],zero,zero,zero
	xorps	%xmm2, %xmm2
	xorpd	%xmm3, %xmm3
	callq	glRotatef
	movss	.LCPI2_7(%rip), %xmm0   # xmm0 = mem[0],zero,zero,zero
	xorps	%xmm1, %xmm1
	xorps	%xmm2, %xmm2
	movss	.LCPI2_6(%rip), %xmm3   # xmm3 = mem[0],zero,zero,zero
	callq	glRotatef
	addq	$16, %rsp
	popq	%rbp
	retq
.Lfunc_end2:
	.size	_Z4initv, .Lfunc_end2-_Z4initv
	.cfi_endproc

	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp9:
	.cfi_def_cfa_offset 16
.Ltmp10:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp11:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$0, -4(%rbp)
	movl	%edi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	-8(%rbp), %rdi
	callq	glutInit
	movl	$18, %edi
	callq	glutInitDisplayMode
	movl	$.L.str, %edi
	callq	glutCreateWindow
	movl	$_Z7displayv, %edi
	callq	glutDisplayFunc
	callq	_Z4initv
	callq	glutMainLoop
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbp
	retq
.Lfunc_end3:
	.size	main, .Lfunc_end3-main
	.cfi_endproc

	.section	.text.startup,"ax",@progbits
	.align	16, 0x90
	.type	_GLOBAL__sub_I_main.cpp,@function
_GLOBAL__sub_I_main.cpp:                # @_GLOBAL__sub_I_main.cpp
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp12:
	.cfi_def_cfa_offset 16
.Ltmp13:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp14:
	.cfi_def_cfa_register %rbp
	callq	__cxx_global_var_init
	popq	%rbp
	retq
.Lfunc_end4:
	.size	_GLOBAL__sub_I_main.cpp, .Lfunc_end4-_GLOBAL__sub_I_main.cpp
	.cfi_endproc

	.type	_ZStL8__ioinit,@object  # @_ZStL8__ioinit
	.local	_ZStL8__ioinit
	.comm	_ZStL8__ioinit,1,1
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"testgl"
	.size	.L.str, 7

	.section	.init_array,"aw",@init_array
	.align	8
	.quad	_GLOBAL__sub_I_main.cpp

	.ident	"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"
	.section	".note.GNU-stack","",@progbits
