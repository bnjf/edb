COMMENT /
 
  File name:  c0.asm, C0 startup file of Emulating Debugger (EDB)
  Obviosly, this file is derived from Borland's C0.ASM... Although
  not much of it left here, it should still be considered to be
  (C) Borland intl....

/

	.286
__TINY__	EQU	1

_TEXT	SEGMENT BYTE PUBLIC 'CODE'
_TEXT	ENDS
_DATA	SEGMENT PARA PUBLIC 'DATA'
_DATA	ENDS
_CVTSEG SEGMENT WORD PUBLIC 'DATA'
_CVTSEG ENDS
_SCNSEG SEGMENT WORD PUBLIC 'DATA'
_SCNSEG ENDS
_BSS	SEGMENT WORD PUBLIC 'BSS'
_BSS	ENDS
_BSSEND SEGMENT BYTE PUBLIC 'STACK'
_BSSEND ENDS

DGROUP	GROUP	_TEXT, _DATA, _CVTSEG, _SCNSEG, _BSS, _BSSEND

	ASSUME	CS:_TEXT, DS:DGROUP

	extrn	_main	  :near
;	extrn	__setargv :near
;	extrn	__setenvp :near
	extrn	_exit	  :near
	extrn	__heaplen :word
	extrn	__stklen  :word

	public	__exit, _abort

PSPHigh 	equ	00002h
PSPEnv		equ	0002ch
PSPCmd		equ	00080h

_TEXT	SEGMENT
	ORG	100h
STARTX		PROC	NEAR
;
		mov	cs:_entry_ax, ax
		mov	cs:_entry_bx, bx
		mov	cs:_entry_cx, cx
		mov	cs:_entry_bp, bp
;
		mov	dx, cs		; DX = GROUP Segment address
		mov	cs:DGROUP@, dx
		mov	ah, 30h
		int	21h
		mov	bp, ds:[PSPHigh]; BP = Highest Memory Segment Addr
		mov	bx, ds:[PSPEnv] ; BX = Environment Segment address
		mov	ds, dx
		mov	__version, ax	; Keep major and minor version number
		mov	__psp, es	; Keep Program Segment Prefix address
		mov	__envseg, bx	; Keep Environment Segment address
;
;	Determine the amount of memory that we need to keep
;
		mov	dx, ds
		sub	bp, dx		; BP = remaining size in paragraphs
		mov	di, __stklen	; DI = Requested stack size
		add	di, offset DGROUP: edata@
		jb	InitFailed	; DATA segment can NOT be > 64 Kbytes
		add	di, __heaplen
		jb	InitFailed	; DATA segment can NOT be > 64 Kbytes
		mov	cl, 4
		shr	di, cl		; $$$ Do not destroy CL $$$
		inc	di		; DI = DS size in paragraphs
		cmp	bp, di
		jnb	ExcessOfMemory
;
;	All initialization errors arrive here
;
InitFailed	label	near
		jmp	near ptr _abort
;
;	Return to DOS the amount of memory in excess
;	Set far heap base and pointer
;
ExcessOfMemory	label	near
		mov	bx, di
		add	bx, dx
		mov	ax, __psp
		sub	bx, ax		; BX = Number of paragraphs to keep
		mov	es, ax		; ES = Program Segment Prefix address
		mov	ah, 04Ah
		push	di		; preserve DI
		int	021h		; this call clobbers SI,DI,BP !!!!!!
		pop	di		; restore  DI
;
;	Set the program stack.	Take care to prevent the disastrous
;	interrupt that could happen with a stack that is half switched.
;
		shl	di, cl		; $$$ CX is still equal to 4 $$$

		cli
		mov	ss, dx
		mov	sp, di
		sti

;
;	Reset uninitialized data area
;
		xor	ax, ax
		mov	es, cs:DGROUP@
		mov	di, offset DGROUP: bdata@
		mov	cx, offset DGROUP: edata@
		sub	cx, di
		rep	stosb
;
;	Prepare main arguments
;
;		call	__setargv
;
;	ExitCode = main(argc,argv,envp);
;
		push	word ptr _environ
		push	word ptr __argv
		push	__argc
		call	_main
;
;	Flush and close streams and files
;
		push	ax
		call	_exit

__exit		proc	near
		mov	ds, cs:DGROUP@
;
;	Exit to DOS
;
ExitToDOS	label	near
		mov	bp,sp
		mov	ah,4Ch
		mov	al,[bp+2]
		int	21h			; Exit to DOS
__exit		endp
STARTX		ENDP
;
ErrorDisplay	PROC	NEAR
		mov	ah, 040h
		mov	bx, 2
		int	021h
		ret
ErrorDisplay	ENDP
;
_abort		proc	near
		mov	cx, lgth_abortMSG
		mov	dx, offset DGROUP: abortMSG
MsgExit3	label	near
		mov	ds, cs: DGROUP@
		call	ErrorDisplay
CallExit3	label	near
		mov	ax, 3
		push	ax
		call	__exit
_abort		endp
;
;	The DGROUP@ variable is used to reload DS with DGROUP
;
		public	DGROUP@
		public	_entry_ax, _entry_bx, _entry_cx, _entry_bp
DGROUP@ 	dw	?
_entry_ax	dw	?
_entry_bx	dw	?
_entry_cx	dw	?
_entry_bp	dw	?

_TEXT	ENDS
;
;
_DATA	SEGMENT
		public	_errno, __psp, __envLng, __osmajor, __envseg
		public	__argc, __argv, __envSize, _environ
		public	___heapbase, ___brklvl, ___heaptop


abortMSG	db	'Abnormal program termination', 13, 10
lgth_abortMSG	equ	$ - abortMSG

;
;			Miscellaneous variables
;
__argc		dw	0
__argv		dw	0
_environ	dw	0
__envLng	dw	0
__envseg	dw	0
__envSize	dw	0
__psp		dw	0
__version	label	word
__osmajor	db	0
__osminor	db	0
_errno		dw	0
__8087		dw	0
__StartTime	dw	0,0
;
;	Memory management variables
;
___heapbase	dw	DGROUP:edata@
___brklvl	dw	DGROUP:edata@
___heaptop	dw	DGROUP:edata@

_DATA	ENDS

_CVTSEG SEGMENT
	public	__RealCvtVector
__RealCvtVector label  word
_CVTSEG ENDS

_SCNSEG SEGMENT
	public	__ScanTodVector
__ScanTodVector label  word
_SCNSEG ENDS

_BSS	SEGMENT
bdata@	label	byte
_BSS	ENDS

_BSSEND SEGMENT
edata@	label	byte
_BSSEND ENDS

	END	STARTX
