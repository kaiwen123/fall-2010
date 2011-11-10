	.data
	.align	2
	.globl	class_nameTab
	.globl	Main_protObj
	.globl	Int_protObj
	.globl	String_protObj
	.globl	bool_const0
	.globl	bool_const1
	.globl	_int_tag
	.globl	_bool_tag
	.globl	_string_tag
_int_tag:
	.word	3
_bool_tag:
	.word	4
_string_tag:
	.word	5
	.globl	_MemMgr_INITIALIZER
_MemMgr_INITIALIZER:
	.word	_NoGC_Init
	.globl	_MemMgr_COLLECTOR
_MemMgr_COLLECTOR:
	.word	_NoGC_Collect
	.globl	_MemMgr_TEST
_MemMgr_TEST:
	.word	0
	.word	-1
str_const25:
	.word	5
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"Cons"
	.byte	0	
	.align	2
	.word	-1
str_const24:
	.word	5
	.word	6
	.word	String_dispTab
	.word	int_const2
	.ascii	"Stack"
	.byte	0	
	.align	2
	.word	-1
str_const23:
	.word	5
	.word	6
	.word	String_dispTab
	.word	int_const3
	.ascii	"String"
	.byte	0	
	.align	2
	.word	-1
str_const22:
	.word	5
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"Bool"
	.byte	0	
	.align	2
	.word	-1
str_const21:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const4
	.ascii	"Int"
	.byte	0	
	.align	2
	.word	-1
str_const20:
	.word	5
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"Main"
	.byte	0	
	.align	2
	.word	-1
str_const19:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const5
	.ascii	"IO"
	.byte	0	
	.align	2
	.word	-1
str_const18:
	.word	5
	.word	6
	.word	String_dispTab
	.word	int_const3
	.ascii	"Object"
	.byte	0	
	.align	2
	.word	-1
str_const17:
	.word	5
	.word	8
	.word	String_dispTab
	.word	int_const6
	.ascii	"<basic class>"
	.byte	0	
	.align	2
	.word	-1
str_const16:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"q"
	.byte	0	
	.align	2
	.word	-1
str_const15:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"!"
	.byte	0	
	.align	2
	.word	-1
str_const14:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"+"
	.byte	0	
	.align	2
	.word	-1
str_const13:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"*"
	.byte	0	
	.align	2
	.word	-1
str_const12:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"b"
	.byte	0	
	.align	2
	.word	-1
str_const11:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"a"
	.byte	0	
	.align	2
	.word	-1
str_const10:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"j"
	.byte	0	
	.align	2
	.word	-1
str_const9:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"i"
	.byte	0	
	.align	2
	.word	-1
str_const8:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const0
	.byte	0	
	.align	2
	.word	-1
str_const7:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"p"
	.byte	0	
	.align	2
	.word	-1
str_const6:
	.word	5
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"real"
	.byte	0	
	.align	2
	.word	-1
str_const5:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const4
	.ascii	"int"
	.byte	0	
	.align	2
	.word	-1
str_const4:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const5
	.ascii	">>"
	.byte	0	
	.align	2
	.word	-1
str_const3:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	" "
	.byte	0	
	.align	2
	.word	-1
str_const2:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const7
	.ascii	"\n"
	.byte	0	
	.align	2
	.word	-1
str_const1:
	.word	5
	.word	6
	.word	String_dispTab
	.word	int_const8
	.ascii	"type.cl"
	.byte	0	
	.align	2
	.word	-1
str_const0:
	.word	5
	.word	8
	.word	String_dispTab
	.word	int_const9
	.ascii	" Stack Empty! "
	.byte	0	
	.align	2
	.word	-1
int_const9:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	14
	.word	-1
int_const8:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	7
	.word	-1
int_const7:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	1
	.word	-1
int_const6:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	13
	.word	-1
int_const5:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	2
	.word	-1
int_const4:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	3
	.word	-1
int_const3:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	6
	.word	-1
int_const2:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	5
	.word	-1
int_const1:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	4
	.word	-1
int_const0:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
bool_const0:
	.word	4
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
bool_const1:
	.word	4
	.word	4
	.word	Bool_dispTab
	.word	1
class_nameTab:
	.word	str_const18
	.word	str_const19
	.word	str_const20
	.word	str_const21
	.word	str_const22
	.word	str_const23
	.word	str_const24
	.word	str_const25
class_objTab:
	.word	Object_protObj
	.word	Object_init
	.word	IO_protObj
	.word	IO_init
	.word	Main_protObj
	.word	Main_init
	.word	Int_protObj
	.word	Int_init
	.word	Bool_protObj
	.word	Bool_init
	.word	String_protObj
	.word	String_init
	.word	Stack_protObj
	.word	Stack_init
	.word	Cons_protObj
	.word	Cons_init
Object_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
Stack_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	Stack.isNil
	.word	Stack.head
	.word	Stack.tail
	.word	Stack.cons
Cons_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	Cons.isNil
	.word	Cons.head
	.word	Cons.tail
	.word	Stack.cons
	.word	Cons.init
String_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	String.length
	.word	String.concat
	.word	String.substr
Bool_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
Int_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
IO_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
Main_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
	.word	Main.print
	.word	Main.main
	.word	-1
Object_protObj:
	.word	0
	.word	3
	.word	Object_dispTab
	.word	-1
Stack_protObj:
	.word	6
	.word	3
	.word	Stack_dispTab
	.word	-1
Cons_protObj:
	.word	7
	.word	5
	.word	Cons_dispTab
	.word	str_const8
	.word	0
	.word	-1
String_protObj:
	.word	5
	.word	5
	.word	String_dispTab
	.word	int_const0
	.word	0
	.word	-1
Bool_protObj:
	.word	4
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
Int_protObj:
	.word	3
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
IO_protObj:
	.word	1
	.word	3
	.word	IO_dispTab
	.word	-1
Main_protObj:
	.word	2
	.word	4
	.word	Main_dispTab
	.word	0
	.globl	heap_start
heap_start:
	.word	0
	.text
	.globl	Main_init
	.globl	Int_init
	.globl	String_init
	.globl	Bool_init
	.globl	Main.main
Object_init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Stack_init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	jal	Object_init
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Cons_init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	jal	Stack_init
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
String_init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	jal	Object_init
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Bool_init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	jal	Object_init
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Int_init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	jal	Object_init
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
IO_init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	jal	Object_init
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Main_init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	jal	IO_init
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Stack.isNil:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	la	$a0 bool_const1
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Stack.head:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	move	$a0 $s0
	bne	$a0 $zero label0
	la	$a0 str_const1
	li	$t1 27
	jal	_dispatch_abort
label0:
	lw	$t1 8($a0)
	lw	$t1 0($t1)
	jalr		$t1
	la	$a0 str_const0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Stack.tail:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	move	$a0 $s0
	bne	$a0 $zero label1
	la	$a0 str_const1
	li	$t1 30
	jal	_dispatch_abort
label1:
	lw	$t1 8($a0)
	lw	$t1 0($t1)
	jalr		$t1
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Stack.cons:
	addiu	$sp $sp -16
	sw	$fp 16($sp)
	sw	$s0 12($sp)
	sw	$ra 8($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 16($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 Cons_protObj
	jal	Object.copy
	jal	Cons_init
	bne	$a0 $zero label2
	la	$a0 str_const1
	li	$t1 33
	jal	_dispatch_abort
label2:
	lw	$t1 8($a0)
	lw	$t1 28($t1)
	jalr		$t1
	lw	$fp 16($sp)
	lw	$s0 12($sp)
	lw	$ra 8($sp)
	addiu	$sp $sp 20
	jr	$ra	
Cons.isNil:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	la	$a0 bool_const0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Cons.head:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 12($s0)
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Cons.tail:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 16($s0)
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Cons.init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 16($fp)
	sw	$a0 12($s0)
	lw	$a0 12($fp)
	sw	$a0 16($s0)
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 20
	jr	$ra	
Main.print:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 12($fp)
	bne	$a0 $zero label5
	la	$a0 str_const1
	li	$t1 76
	jal	_dispatch_abort
label5:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	lw	$t1 12($a0)
	beqz	$t1 label3
	la	$a0 str_const2
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label6
	la	$a0 str_const1
	li	$t1 76
	jal	_dispatch_abort
label6:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	b	label4
label3:
	lw	$a0 12($fp)
	bne	$a0 $zero label7
	la	$a0 str_const1
	li	$t1 78
	jal	_dispatch_abort
label7:
	lw	$t1 8($a0)
	lw	$t1 16($t1)
	jalr		$t1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label8
	la	$a0 str_const1
	li	$t1 78
	jal	_dispatch_abort
label8:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	la	$a0 str_const3
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label9
	la	$a0 str_const1
	li	$t1 79
	jal	_dispatch_abort
label9:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	lw	$a0 12($fp)
	bne	$a0 $zero label10
	la	$a0 str_const1
	li	$t1 80
	jal	_dispatch_abort
label10:
	lw	$t1 8($a0)
	lw	$t1 20($t1)
	jalr		$t1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label11
	la	$a0 str_const1
	li	$t1 80
	jal	_dispatch_abort
label11:
	lw	$t1 8($a0)
	lw	$t1 28($t1)
	jalr		$t1
label4:
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 16
	jr	$ra	
Main.main:
	addiu	$sp $sp -48
	sw	$fp 48($sp)
	sw	$s0 44($sp)
	sw	$ra 40($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	la	$a0 bool_const1
	sw	$a0 0($fp)
	la	$a0 str_const8
	sw	$a0 4($fp)
	la	$a0 str_const4
	sw	$a0 8($fp)
	la	$a0 str_const5
	sw	$a0 12($fp)
	la	$a0 str_const6
	sw	$a0 16($fp)
	la	$a0 Stack_protObj
	jal	Object.copy
	jal	Stack_init
	sw	$a0 20($fp)
label12:
	lw	$a0 0($fp)
	lw	$t1 12($a0)
	beq	$t1 0 label13
	lw	$a0 8($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label14
	la	$a0 str_const1
	li	$t1 94
	jal	_dispatch_abort
label14:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	move	$a0 $s0
	bne	$a0 $zero label15
	la	$a0 str_const1
	li	$t1 95
	jal	_dispatch_abort
label15:
	lw	$t1 8($a0)
	lw	$t1 20($t1)
	jalr		$t1
	sw	$a0 4($fp)
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const7
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label18
	la	$a1 bool_const0
	jal	equality_test
label18:
	lw	$t1 12($a0)
	beqz	$t1 label16
	lw	$a0 20($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label19
	la	$a0 str_const1
	li	$t1 97
	jal	_dispatch_abort
label19:
	lw	$t1 8($a0)
	lw	$t1 28($t1)
	jalr		$t1
	b	label17
label16:
	la	$a0 str_const8
label17:
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const9
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label22
	la	$a1 bool_const0
	jal	equality_test
label22:
	lw	$t1 12($a0)
	beqz	$t1 label20
	lw	$a0 12($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 20($fp)
	bne	$a0 $zero label23
	la	$a0 str_const1
	li	$t1 98
	jal	_dispatch_abort
label23:
	lw	$t1 8($a0)
	lw	$t1 24($t1)
	jalr		$t1
	sw	$a0 20($fp)
	b	label21
label20:
	la	$a0 str_const8
label21:
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const10
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label26
	la	$a1 bool_const0
	jal	equality_test
label26:
	lw	$t1 12($a0)
	beqz	$t1 label24
	lw	$a0 12($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 20($fp)
	bne	$a0 $zero label27
	la	$a0 str_const1
	li	$t1 99
	jal	_dispatch_abort
label27:
	lw	$t1 8($a0)
	lw	$t1 24($t1)
	jalr		$t1
	sw	$a0 20($fp)
	b	label25
label24:
	la	$a0 str_const8
label25:
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const11
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label30
	la	$a1 bool_const0
	jal	equality_test
label30:
	lw	$t1 12($a0)
	beqz	$t1 label28
	lw	$a0 16($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 20($fp)
	bne	$a0 $zero label31
	la	$a0 str_const1
	li	$t1 100
	jal	_dispatch_abort
label31:
	lw	$t1 8($a0)
	lw	$t1 24($t1)
	jalr		$t1
	sw	$a0 20($fp)
	b	label29
label28:
	la	$a0 str_const8
label29:
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const12
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label34
	la	$a1 bool_const0
	jal	equality_test
label34:
	lw	$t1 12($a0)
	beqz	$t1 label32
	lw	$a0 16($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 20($fp)
	bne	$a0 $zero label35
	la	$a0 str_const1
	li	$t1 101
	jal	_dispatch_abort
label35:
	lw	$t1 8($a0)
	lw	$t1 24($t1)
	jalr		$t1
	sw	$a0 20($fp)
	b	label33
label32:
	la	$a0 str_const8
label33:
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const13
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label38
	la	$a1 bool_const0
	jal	equality_test
label38:
	lw	$t1 12($a0)
	beqz	$t1 label36
	la	$a0 str_const8
	sw	$a0 24($fp)
	la	$a0 str_const8
	sw	$a0 28($fp)
	lw	$a0 20($fp)
	bne	$a0 $zero label39
	la	$a0 str_const1
	li	$t1 107
	jal	_dispatch_abort
label39:
	lw	$t1 8($a0)
	lw	$t1 16($t1)
	jalr		$t1
	sw	$a0 24($fp)
	lw	$a0 20($fp)
	bne	$a0 $zero label40
	la	$a0 str_const1
	li	$t1 108
	jal	_dispatch_abort
label40:
	lw	$t1 8($a0)
	lw	$t1 20($t1)
	jalr		$t1
	sw	$a0 20($fp)
	lw	$a0 20($fp)
	bne	$a0 $zero label41
	la	$a0 str_const1
	li	$t1 109
	jal	_dispatch_abort
label41:
	lw	$t1 8($a0)
	lw	$t1 16($t1)
	jalr		$t1
	sw	$a0 28($fp)
	lw	$a0 20($fp)
	bne	$a0 $zero label42
	la	$a0 str_const1
	li	$t1 110
	jal	_dispatch_abort
label42:
	lw	$t1 8($a0)
	lw	$t1 20($t1)
	jalr		$t1
	sw	$a0 20($fp)
	lw	$a0 24($fp)
	sw	$a0 32($fp)
	lw	$a0 12($fp)
	lw	$t1 32($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label45
	la	$a1 bool_const0
	jal	equality_test
label45:
	lw	$t1 12($a0)
	beqz	$t1 label43
	lw	$a0 28($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 20($fp)
	bne	$a0 $zero label46
	la	$a0 str_const1
	li	$t1 113
	jal	_dispatch_abort
label46:
	lw	$t1 8($a0)
	lw	$t1 24($t1)
	jalr		$t1
	sw	$a0 20($fp)
	b	label44
label43:
	lw	$a0 16($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 20($fp)
	bne	$a0 $zero label47
	la	$a0 str_const1
	li	$t1 115
	jal	_dispatch_abort
label47:
	lw	$t1 8($a0)
	lw	$t1 24($t1)
	jalr		$t1
	sw	$a0 20($fp)
label44:
	b	label37
label36:
	la	$a0 int_const0
label37:
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const14
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label50
	la	$a1 bool_const0
	jal	equality_test
label50:
	lw	$t1 12($a0)
	beqz	$t1 label48
	la	$a0 str_const8
	sw	$a0 24($fp)
	la	$a0 str_const8
	sw	$a0 28($fp)
	lw	$a0 20($fp)
	bne	$a0 $zero label51
	la	$a0 str_const1
	li	$t1 125
	jal	_dispatch_abort
label51:
	lw	$t1 8($a0)
	lw	$t1 16($t1)
	jalr		$t1
	sw	$a0 24($fp)
	lw	$a0 20($fp)
	bne	$a0 $zero label52
	la	$a0 str_const1
	li	$t1 126
	jal	_dispatch_abort
label52:
	lw	$t1 8($a0)
	lw	$t1 20($t1)
	jalr		$t1
	sw	$a0 20($fp)
	lw	$a0 20($fp)
	bne	$a0 $zero label53
	la	$a0 str_const1
	li	$t1 127
	jal	_dispatch_abort
label53:
	lw	$t1 8($a0)
	lw	$t1 16($t1)
	jalr		$t1
	sw	$a0 28($fp)
	lw	$a0 20($fp)
	bne	$a0 $zero label54
	la	$a0 str_const1
	li	$t1 128
	jal	_dispatch_abort
label54:
	lw	$t1 8($a0)
	lw	$t1 20($t1)
	jalr		$t1
	sw	$a0 20($fp)
	lw	$a0 24($fp)
	sw	$a0 32($fp)
	lw	$a0 12($fp)
	lw	$t1 32($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label57
	la	$a1 bool_const0
	jal	equality_test
label57:
	lw	$t1 12($a0)
	beqz	$t1 label55
	lw	$a0 28($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 20($fp)
	bne	$a0 $zero label58
	la	$a0 str_const1
	li	$t1 131
	jal	_dispatch_abort
label58:
	lw	$t1 8($a0)
	lw	$t1 24($t1)
	jalr		$t1
	sw	$a0 20($fp)
	b	label56
label55:
	lw	$a0 16($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 20($fp)
	bne	$a0 $zero label59
	la	$a0 str_const1
	li	$t1 133
	jal	_dispatch_abort
label59:
	lw	$t1 8($a0)
	lw	$t1 24($t1)
	jalr		$t1
	sw	$a0 20($fp)
label56:
	b	label49
label48:
	la	$a0 int_const0
label49:
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const15
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label62
	la	$a1 bool_const0
	jal	equality_test
label62:
	lw	$t1 12($a0)
	beqz	$t1 label60
	lw	$a0 20($fp)
	bne	$a0 $zero label63
	la	$a0 str_const1
	li	$t1 139
	jal	_dispatch_abort
label63:
	lw	$t1 8($a0)
	lw	$t1 16($t1)
	jalr		$t1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label64
	la	$a0 str_const1
	li	$t1 139
	jal	_dispatch_abort
label64:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	la	$a0 str_const2
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label65
	la	$a0 str_const1
	li	$t1 140
	jal	_dispatch_abort
label65:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	b	label61
label60:
	la	$a0 int_const0
label61:
	lw	$a0 4($fp)
	sw	$a0 24($fp)
	la	$a0 str_const16
	lw	$t1 24($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label68
	la	$a1 bool_const0
	jal	equality_test
label68:
	lw	$t1 12($a0)
	beqz	$t1 label66
	la	$a0 bool_const0
	sw	$a0 0($fp)
	b	label67
label66:
	la	$a0 int_const0
label67:
	b	label12
label13:
	move	$a0 $zero
	lw	$fp 48($sp)
	lw	$s0 44($sp)
	lw	$ra 40($sp)
	addiu	$sp $sp 48
	jr	$ra	
