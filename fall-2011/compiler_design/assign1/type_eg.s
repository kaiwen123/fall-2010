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
	.word	5
_bool_tag:
	.word	6
_string_tag:
	.word	7
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
str_const27:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const0
	.byte	0	
	.align	2
	.word	-1
str_const26:
	.word	7
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"String"
	.byte	0	
	.align	2
	.word	-1
str_const25:
	.word	7
	.word	6
	.word	String_dispTab
	.word	int_const2
	.ascii	"Bool"
	.byte	0	
	.align	2
	.word	-1
str_const24:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const3
	.ascii	"Int"
	.byte	0	
	.align	2
	.word	-1
str_const23:
	.word	7
	.word	6
	.word	String_dispTab
	.word	int_const2
	.ascii	"Main"
	.byte	0	
	.align	2
	.word	-1
str_const22:
	.word	7
	.word	7
	.word	String_dispTab
	.word	int_const4
	.ascii	"StringStack"
	.byte	0	
	.align	2
	.word	-1
str_const21:
	.word	7
	.word	6
	.word	String_dispTab
	.word	int_const5
	.ascii	"Stack"
	.byte	0	
	.align	2
	.word	-1
str_const20:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const6
	.ascii	"IO"
	.byte	0	
	.align	2
	.word	-1
str_const19:
	.word	7
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"Object"
	.byte	0	
	.align	2
	.word	-1
str_const18:
	.word	7
	.word	8
	.word	String_dispTab
	.word	int_const7
	.ascii	"<basic class>"
	.byte	0	
	.align	2
	.word	-1
str_const17:
	.word	7
	.word	10
	.word	String_dispTab
	.word	int_const8
	.ascii	"Error: Illegal Input\n"
	.byte	0	
	.align	2
	.word	-1
str_const16:
	.word	7
	.word	6
	.word	String_dispTab
	.word	int_const5
	.ascii	" *) \t"
	.byte	0	
	.align	2
	.word	-1
str_const15:
	.word	7
	.word	9
	.word	String_dispTab
	.word	int_const9
	.ascii	"(* Input String = "
	.byte	0	
	.align	2
	.word	-1
str_const14:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"!"
	.byte	0	
	.align	2
	.word	-1
str_const13:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"b"
	.byte	0	
	.align	2
	.word	-1
str_const12:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"a"
	.byte	0	
	.align	2
	.word	-1
str_const11:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"j"
	.byte	0	
	.align	2
	.word	-1
str_const10:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const3
	.ascii	"int"
	.byte	0	
	.align	2
	.word	-1
str_const9:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"i"
	.byte	0	
	.align	2
	.word	-1
str_const8:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"*"
	.byte	0	
	.align	2
	.word	-1
str_const7:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"+"
	.byte	0	
	.align	2
	.word	-1
str_const6:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"q"
	.byte	0	
	.align	2
	.word	-1
str_const5:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const6
	.ascii	">>"
	.byte	0	
	.align	2
	.word	-1
str_const4:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const10
	.ascii	"\n"
	.byte	0	
	.align	2
	.word	-1
str_const3:
	.word	7
	.word	6
	.word	String_dispTab
	.word	int_const11
	.ascii	"type.cl"
	.byte	0	
	.align	2
	.word	-1
str_const2:
	.word	7
	.word	11
	.word	String_dispTab
	.word	int_const12
	.ascii	"Error: Empty Stack Found.\n"
	.byte	0	
	.align	2
	.word	-1
str_const1:
	.word	7
	.word	14
	.word	String_dispTab
	.word	int_const13
	.ascii	"Error: Two Stack Operands Expected.\n"
	.byte	0	
	.align	2
	.word	-1
str_const0:
	.word	7
	.word	6
	.word	String_dispTab
	.word	int_const2
	.ascii	"real"
	.byte	0	
	.align	2
	.word	-1
int_const13:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	36
	.word	-1
int_const12:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	26
	.word	-1
int_const11:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	7
	.word	-1
int_const10:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	1
	.word	-1
int_const9:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	18
	.word	-1
int_const8:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	21
	.word	-1
int_const7:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	13
	.word	-1
int_const6:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	2
	.word	-1
int_const5:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	5
	.word	-1
int_const4:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	11
	.word	-1
int_const3:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	3
	.word	-1
int_const2:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	4
	.word	-1
int_const1:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	6
	.word	-1
int_const0:
	.word	5
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
bool_const0:
	.word	6
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
bool_const1:
	.word	6
	.word	4
	.word	Bool_dispTab
	.word	1
class_nameTab:
	.word	str_const19
	.word	str_const20
	.word	str_const21
	.word	str_const22
	.word	str_const23
	.word	str_const24
	.word	str_const25
	.word	str_const26
class_objTab:
	.word	Object_protObj
	.word	Object_init
	.word	IO_protObj
	.word	IO_init
	.word	Stack_protObj
	.word	Stack_init
	.word	StringStack_protObj
	.word	StringStack_init
	.word	Main_protObj
	.word	Main_init
	.word	Int_protObj
	.word	Int_init
	.word	Bool_protObj
	.word	Bool_init
	.word	String_protObj
	.word	String_init
Object_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
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
	.word	Main.prompt
	.word	Main.main
Stack_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
	.word	Stack.setRest
	.word	Stack.getRest
	.word	Stack.show
	.word	Stack.createTop
	.word	Stack.inferType
StringStack_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
	.word	Stack.setRest
	.word	Stack.getRest
	.word	StringStack.show
	.word	Stack.createTop
	.word	Stack.inferType
	.word	StringStack.setTop
	.word	StringStack.getTop
	.word	StringStack.init
	.word	-1
Object_protObj:
	.word	0
	.word	3
	.word	Object_dispTab
	.word	-1
String_protObj:
	.word	7
	.word	5
	.word	String_dispTab
	.word	int_const0
	.word	0
	.word	-1
Bool_protObj:
	.word	6
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
Int_protObj:
	.word	5
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
	.word	4
	.word	5
	.word	Main_dispTab
	.word	0
	.word	str_const27
	.word	-1
Stack_protObj:
	.word	2
	.word	4
	.word	Stack_dispTab
	.word	0
	.word	-1
StringStack_protObj:
	.word	3
	.word	5
	.word	StringStack_dispTab
	.word	0
	.word	str_const27
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
	addiu	$sp $sp -16
	sw	$fp 16($sp)
	sw	$s0 12($sp)
	sw	$ra 8($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	jal	IO_init
	la	$a0 Stack_protObj
	jal	Object.copy
	jal	Stack_init
	sw	$a0 12($s0)
	move	$a0 $s0
	lw	$fp 16($sp)
	lw	$s0 12($sp)
	lw	$ra 8($sp)
	addiu	$sp $sp 16
	jr	$ra	
Stack_init:
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
StringStack_init:
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
Main.prompt:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	la	$a0 str_const5
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label0
	la	$a0 str_const3
	li	$t1 90
	jal	_dispatch_abort
label0:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	move	$a0 $s0
	bne	$a0 $zero label1
	la	$a0 str_const3
	li	$t1 91
	jal	_dispatch_abort
label1:
	lw	$t1 8($a0)
	lw	$t1 20($t1)
	jalr		$t1
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
Main.main:
	addiu	$sp $sp -16
	sw	$fp 16($sp)
	sw	$s0 12($sp)
	sw	$ra 8($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
label2:
	move	$a0 $s0
	bne	$a0 $zero label6
	la	$a0 str_const3
	li	$t1 97
	jal	_dispatch_abort
label6:
	lw	$t1 8($a0)
	lw	$t1 28($t1)
	jalr		$t1
	sw	$a0 16($s0)
	sw	$a0 0($fp)
	la	$a0 str_const6
	lw	$t1 0($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label5
	la	$a1 bool_const0
	jal	equality_test
label5:
	lw	$t1 12($a0)
	la	$a0 bool_const1
	beqz	$t1 label4
	la	$a0 bool_const0
label4:
	lw	$t1 12($a0)
	beq	$t1 0 label3
	lw	$a0 16($s0)
	sw	$a0 0($fp)
	la	$a0 str_const7
	lw	$t1 0($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label9
	la	$a1 bool_const0
	jal	equality_test
label9:
	lw	$t1 12($a0)
	beqz	$t1 label7
	la	$a0 str_const7
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($s0)
	bne	$a0 $zero label10
	la	$a0 str_const3
	li	$t1 99
	jal	_dispatch_abort
label10:
	lw	$t1 8($a0)
	lw	$t1 44($t1)
	jalr		$t1
	b	label8
label7:
	lw	$a0 16($s0)
	sw	$a0 0($fp)
	la	$a0 str_const8
	lw	$t1 0($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label13
	la	$a1 bool_const0
	jal	equality_test
label13:
	lw	$t1 12($a0)
	beqz	$t1 label11
	la	$a0 str_const8
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($s0)
	bne	$a0 $zero label14
	la	$a0 str_const3
	li	$t1 100
	jal	_dispatch_abort
label14:
	lw	$t1 8($a0)
	lw	$t1 44($t1)
	jalr		$t1
	b	label12
label11:
	lw	$a0 16($s0)
	sw	$a0 0($fp)
	la	$a0 str_const9
	lw	$t1 0($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label17
	la	$a1 bool_const0
	jal	equality_test
label17:
	lw	$t1 12($a0)
	beqz	$t1 label15
	la	$a0 str_const10
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($s0)
	bne	$a0 $zero label18
	la	$a0 str_const3
	li	$t1 101
	jal	_dispatch_abort
label18:
	lw	$t1 8($a0)
	lw	$t1 40($t1)
	jalr		$t1
	b	label16
label15:
	lw	$a0 16($s0)
	sw	$a0 0($fp)
	la	$a0 str_const11
	lw	$t1 0($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label21
	la	$a1 bool_const0
	jal	equality_test
label21:
	lw	$t1 12($a0)
	beqz	$t1 label19
	la	$a0 str_const10
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($s0)
	bne	$a0 $zero label22
	la	$a0 str_const3
	li	$t1 102
	jal	_dispatch_abort
label22:
	lw	$t1 8($a0)
	lw	$t1 40($t1)
	jalr		$t1
	b	label20
label19:
	lw	$a0 16($s0)
	sw	$a0 0($fp)
	la	$a0 str_const12
	lw	$t1 0($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label25
	la	$a1 bool_const0
	jal	equality_test
label25:
	lw	$t1 12($a0)
	beqz	$t1 label23
	la	$a0 str_const0
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($s0)
	bne	$a0 $zero label26
	la	$a0 str_const3
	li	$t1 103
	jal	_dispatch_abort
label26:
	lw	$t1 8($a0)
	lw	$t1 40($t1)
	jalr		$t1
	b	label24
label23:
	lw	$a0 16($s0)
	sw	$a0 0($fp)
	la	$a0 str_const13
	lw	$t1 0($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label29
	la	$a1 bool_const0
	jal	equality_test
label29:
	lw	$t1 12($a0)
	beqz	$t1 label27
	la	$a0 str_const0
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 12($s0)
	bne	$a0 $zero label30
	la	$a0 str_const3
	li	$t1 104
	jal	_dispatch_abort
label30:
	lw	$t1 8($a0)
	lw	$t1 40($t1)
	jalr		$t1
	b	label28
label27:
	lw	$a0 16($s0)
	sw	$a0 0($fp)
	la	$a0 str_const14
	lw	$t1 0($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label33
	la	$a1 bool_const0
	jal	equality_test
label33:
	lw	$t1 12($a0)
	beqz	$t1 label31
	lw	$a0 12($s0)
	bne	$a0 $zero label34
	la	$a0 str_const3
	li	$t1 105
	jal	_dispatch_abort
label34:
	lw	$t1 8($a0)
	lw	$t1 36($t1)
	jalr		$t1
	b	label32
label31:
	la	$a0 str_const15
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label35
	la	$a0 str_const3
	li	$t1 107
	jal	_dispatch_abort
label35:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	lw	$a0 16($s0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label36
	la	$a0 str_const3
	li	$t1 108
	jal	_dispatch_abort
label36:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	la	$a0 str_const16
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label37
	la	$a0 str_const3
	li	$t1 108
	jal	_dispatch_abort
label37:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	la	$a0 str_const17
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label38
	la	$a0 str_const3
	li	$t1 109
	jal	_dispatch_abort
label38:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	move	$a0 $s0
	bne	$a0 $zero label39
	la	$a0 str_const3
	li	$t1 110
	jal	_dispatch_abort
label39:
	lw	$t1 8($a0)
	lw	$t1 0($t1)
	jalr		$t1
	lw	$a0 12($s0)
label32:
label28:
label24:
label20:
label16:
label12:
label8:
	sw	$a0 12($s0)
	b	label2
label3:
	move	$a0 $zero
	lw	$fp 16($sp)
	lw	$s0 12($sp)
	lw	$ra 8($sp)
	addiu	$sp $sp 16
	jr	$ra	
Stack.setRest:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 12($fp)
	sw	$a0 12($s0)
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 16
	jr	$ra	
Stack.getRest:
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
Stack.show:
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
Stack.createTop:
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
	la	$a0 StringStack_protObj
	jal	Object.copy
	jal	StringStack_init
	bne	$a0 $zero label40
	la	$a0 str_const3
	li	$t1 26
	jal	_dispatch_abort
label40:
	lw	$t1 8($a0)
	lw	$t1 56($t1)
	jalr		$t1
	lw	$fp 16($sp)
	lw	$s0 12($sp)
	lw	$ra 8($sp)
	addiu	$sp $sp 20
	jr	$ra	
Stack.inferType:
	addiu	$sp $sp -24
	sw	$fp 24($sp)
	sw	$s0 20($sp)
	sw	$ra 16($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	move	$a0 $s0
	bne	$a0 $zero label42
	la	$a0 str_const3
	li	$t1 47
	jal	_case_abort2
label42:
	lw	$t2 0($a0)
	blt	$t2 3 label43
	bgt	$t2 3 label43
	sw	$a0 0($fp)
	lw	$a0 12($s0)
	bne	$a0 $zero label45
	la	$a0 str_const3
	li	$t1 41
	jal	_case_abort2
label45:
	lw	$t2 0($a0)
	blt	$t2 3 label46
	bgt	$t2 3 label46
	sw	$a0 4($fp)
	lw	$a0 4($fp)
	bne	$a0 $zero label50
	la	$a0 str_const3
	li	$t1 34
	jal	_dispatch_abort
label50:
	lw	$t1 8($a0)
	lw	$t1 52($t1)
	jalr		$t1
	sw	$a0 8($fp)
	lw	$a0 0($fp)
	bne	$a0 $zero label51
	la	$a0 str_const3
	li	$t1 34
	jal	_dispatch_abort
label51:
	lw	$t1 8($a0)
	lw	$t1 52($t1)
	jalr		$t1
	lw	$t1 8($fp)
	move	$t2 $a0
	la	$a0 bool_const1
	beq	$t1 $t2 label49
	la	$a1 bool_const0
	jal	equality_test
label49:
	lw	$t1 12($a0)
	beqz	$t1 label47
	lw	$a0 4($fp)
	bne	$a0 $zero label52
	la	$a0 str_const3
	li	$t1 35
	jal	_dispatch_abort
label52:
	lw	$t1 8($a0)
	lw	$t1 52($t1)
	jalr		$t1
	b	label48
label47:
	la	$a0 str_const0
label48:
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 4($fp)
	bne	$a0 $zero label53
	la	$a0 str_const3
	li	$t1 35
	jal	_dispatch_abort
label53:
	lw	$t1 8($a0)
	lw	$t1 32($t1)
	jalr		$t1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	la	$a0 StringStack_protObj
	jal	Object.copy
	jal	StringStack_init
	bne	$a0 $zero label54
	la	$a0 str_const3
	li	$t1 35
	jal	_dispatch_abort
label54:
	lw	$t1 8($a0)
	lw	$t1 56($t1)
	jalr		$t1
	b	label44
label46:
	blt	$t2 2 label55
	bgt	$t2 3 label55
	sw	$a0 4($fp)
	la	$a0 str_const1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label56
	la	$a0 str_const3
	li	$t1 38
	jal	_dispatch_abort
label56:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	move	$a0 $s0
	b	label44
label55:
	jal	_case_abort
label44:
	b	label41
label43:
	blt	$t2 2 label57
	bgt	$t2 3 label57
	sw	$a0 0($fp)
	la	$a0 str_const2
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label58
	la	$a0 str_const3
	li	$t1 44
	jal	_dispatch_abort
label58:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	move	$a0 $s0
	b	label41
label57:
	jal	_case_abort
label41:
	lw	$fp 24($sp)
	lw	$s0 20($sp)
	lw	$ra 16($sp)
	addiu	$sp $sp 28
	jr	$ra	
StringStack.setTop:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 12($fp)
	sw	$a0 16($s0)
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 16
	jr	$ra	
StringStack.getTop:
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
StringStack.show:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 16($s0)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label59
	la	$a0 str_const3
	li	$t1 65
	jal	_dispatch_abort
label59:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	la	$a0 str_const4
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label60
	la	$a0 str_const3
	li	$t1 66
	jal	_dispatch_abort
label60:
	lw	$t1 8($a0)
	lw	$t1 12($t1)
	jalr		$t1
	move	$a0 $s0
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 12
	jr	$ra	
StringStack.init:
	addiu	$sp $sp -12
	sw	$fp 12($sp)
	sw	$s0 8($sp)
	sw	$ra 4($sp)
	addiu	$fp $sp 4
	move	$s0 $a0
	lw	$a0 12($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$a0 16($fp)
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	move	$a0 $s0
	bne	$a0 $zero label61
	la	$a0 str_const3
	li	$t1 79
	jal	_dispatch_abort
label61:
	lw	$t1 8($a0)
	lw	$t1 48($t1)
	jalr		$t1
	bne	$a0 $zero label62
	la	$a0 str_const3
	li	$t1 79
	jal	_dispatch_abort
label62:
	lw	$t1 8($a0)
	lw	$t1 28($t1)
	jalr		$t1
	lw	$fp 12($sp)
	lw	$s0 8($sp)
	lw	$ra 4($sp)
	addiu	$sp $sp 20
	jr	$ra	
