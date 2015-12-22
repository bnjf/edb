#if 0 

  File name:  perversi.asm, switch execution to native CPU and handle
              returns into emulated environment.
 
  x86 Emulating Debugger (EDB)

  Copyright (C) 1991-1993 Serge Pachkovsky

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 1, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  If you need to contact me, feel free to write to ps@oci.unizh.ch
  or to Serge Pachkovsky, Erligatterweg 61, Zuerich CH 8038, Switzerland

#endif
	.8086
	.MODEL	TINY
;
;	 void
;	 perverted_call( void far *return_addr ) ;
;
	.DATA
	extrn	_ax:word, _bx:word, _cx:word, _dx:word
	extrn	_si:word, _di:word, _bp:word, _sp:word
	extrn	_cs:word, _ds:word, _es:word, _ss:word
	extrn	_ip:word, _flags:word

	.CODE
	public	_start_program
	public	_break_point_interrupt
	public	_huge_write
	public	_setup_vector_monitor
	public	_fill_IDT

	extrn	__IOERROR:near
	extrn	_interrupt_monitor:near
	extrn	_IDT_descriptor:qword

_start_program	proc	near
		mov	ax, _ax
		mov	bx, _bx
		mov	cx, _cx
		mov	dx, _dx
		mov	si, _si
		mov	di, _di
		mov	bp, _bp
		mov	es, _es
		mov	ss, _ss
		mov	sp, _sp
		push	_flags
		push	_cs
		push	_ip
		push	_ds
		pop	ds
		iret
_start_program	endp

;
;	WORD
;	huge_write( int handle, void far *mem, WORD cnt ) ;
;
_huge_write	proc	near
	push	bp
	mov	bp, sp
	push	ds
	mov	bx, [bp+4]
	lds	dx, [bp+6]
	mov	cx, [bp+10]
	mov	ah, 40h
	int	21h
	pop	ds
	jnc	write_Ok
	push	ax
	call	__IOERROR
write_Ok:
	pop	bp
	ret
_huge_write	endp

;
;	void
;	setup_vector_monitor( WORD vec, char *area ) ;
;
;	area will contain:
;	call	cs:[common_loc] 	02eh, 0ffg, 016h, offset common_loc
;	dw	code			dw code
;	dd	old_vector		dd old_vector
;					Total 10 bytes
;
common_loc	dw	[common_code]

common_code	proc	near
	push	ax		;  8
	push	bx		;  6
	push	cx		;  4
	push	dx		;  2
	push	ds		;  0

	mov	ax, cs
	mov	ds, ax
	mov	bx, sp
	mov	ax, word ptr ss:[bx+10]
	sub	ax, 5
	push	ax
	call	_interrupt_monitor
	pop	ax

	pop	ds
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	add	sp, 2
	iret
common_code	endp

_setup_vector_monitor	proc	near
	push	bp
	mov	bp, sp
	push	di
	mov	ax, word ptr [bp+4]
	mov	ah, 35h
	int	21h
	mov	dx, es
	mov	di, word ptr [bp+6]
	push	ds
	pop	es
	cld
	mov	al, 02eh
	stosb
	mov	ax, 016ffh
	stosw
	lea	ax, common_loc
	stosw
	inc	di
	mov	ax, word ptr [bp+4]
	stosw
	mov	ax, bx
	stosw
	mov	ax, dx
	stosw
	mov	dx, word ptr [bp+6]
	mov	ax, word ptr [bp+4]
	mov	ah, 25h
	int	21h
	pop	di
	pop	bp
	ret
_setup_vector_monitor	endp

	.286p

cs_base 	dw	?
temp_cs 	dw	?
temp_addr	dd	?
break_point	dw	?

interrupt_vector	proc	near
	push	cs
	db	0EAh
	dw	interrupt_proc, 0
interrupt_seg	EQU	word ptr $-2
interrupt_proc	label	near
	pop	cs:temp_cs
	push	ax
	push	bx
	push	ds
	xor	ax, ax
	mov	ds, ax
	mov	bx, cs:temp_cs
	sub	bx, cs:cs_base
	shl	bx, 2
	mov	ax, ds:[bx]
	mov	word ptr cs:temp_addr, ax
	mov	ax, ds:[bx+2]
	mov	word ptr cs:temp_addr + 2, ax
	pop	ds
	pop	bx
	pop	ax
	jmp	cs:[temp_addr]
interrupt_vector	endp

save_interrupt_vector	proc	near
	cli
	xor	ax, ax
	mov	ds, ax
	mov	ax, ds:[bx]
	mov	word ptr cs:temp_addr, ax
	mov	ax, ds:[bx+2]
	mov	word ptr cs:temp_addr + 2, ax
	ret
save_interrupt_vector	endp

AT_extended_service	proc	near
	push	ax
	push	bx
	push	ds
	mov	bx, 15h*4
	call	save_interrupt_vector
	pop	ds
	pop	bx
	pop	ax
	pushf
	call	cs:[temp_addr]
	lidt	cs:_IDT_descriptor
	retf	2
AT_extended_service	endp

	.8086

_break_point_interrupt	 proc	 near
	push	ax
	push	ds

	push	cs
	pop	ds

	pop	ax
	mov	_ds, ax
	pop	ax
	mov	_ax, ax
	pop	ax
	dec	ax
	mov	_ip, ax
	pop	ax
	mov	_cs, ax
	pop	ax
	mov	_flags, ax

	mov	_sp, sp
	mov	_bx, bx
	mov	_cx, cx
	mov	_dx, dx
	mov	_bp, bp
	mov	_si, si
	mov	_di, di
	mov	_es, es
	mov	_ss, ss
	jmp	cs:[break_point]
_break_point_interrupt	 endp

_fill_IDT	proc	near
	push	bp
	mov	bp, sp
	push	di

	mov	ax, cs
	mov	es, ax
	mov	interrupt_seg, ax
	sub	ax, 256
	mov	cs:cs_base, ax
	mov	bx, offset interrupt_vector + 256 * 16
	mov	cx, 256
	mov	di, [bp+4]
	cld
prepare_loop:
	xchg	ax, bx
	stosw
	xchg	ax, bx
	stosw
	inc	ax
	sub	bx, 16
	loop	prepare_loop

	mov	di, [bp+4]
	mov	word ptr [di+15h*4], offset AT_extended_service
	mov	word ptr [di+15h*4+2], cs
	mov	word ptr [di+03h*4], offset _break_point_interrupt
	mov	word ptr [di+03h*4+2], cs

	mov	ax, [bp+6]
	mov	cs:break_point, ax

	pop	di
	pop	bp
	ret
_fill_IDT	endp

	public	_copy_RAM

DEST	EQU	word ptr [bp+4]
SRC	EQU	word ptr [bp+6]
SIZEX	EQU	word ptr [bp+8]

_copy_RAM	proc	near
	push	bp
	mov	bp, sp
	push	si
	push	di
	push	ds
	cld
	mov	es, DEST
	mov	ds, SRC
	mov	cx, SIZEX
	xor	dx, dx
	shl	cx, 1
	rcl	dx, 1
	shl	cx, 1
	rcl	dx, 1
	shl	cx, 1
	rcl	dx, 1
	shl	cx, 1
	rcl	dx, 1
	jcxz	no_small_part
		xor	si, si
		xor	di, di
		shr	cx, 1
		rep	movsw
		mov	cl, 4
		shr	si, cl
		mov	ax, ds
		add	ax, si
		mov	ds, ax
		mov	ax, es
		add	ax, si
		mov	es, ax
no_small_part:
	or	dx, dx
	jz	done_moving
		xor	si, si
		xor	di, di
		mov	cx, 8000h
		rep	movsw
		mov	ax, ds
		add	ax, 800h
		mov	ds, ax
		mov	ax, es
		add	ax, 800h
		mov	es, ax
	dec	dx
	jmp	no_small_part
done_moving:
	pop	ds
	pop	di
	pop	si
	pop	bp
	ret
_copy_RAM	endp

	public	_swap_RAM

LOC_A	EQU	dword ptr [bp+4]
LOC_B	EQU	dword ptr [bp+8]
SIZEY	EQU	dword ptr [bp+12]

large_move	proc	near
	push	cx
next_byte:
	mov	ax, es:[di]
	movsw
	mov	ds:[si-2], ax
	loop	next_byte
	pop	cx
	shl	cx, 1
	sub	si, cx
	sub	di, cx
	shr	cx, 1
	shr	cx, 1
	shr	cx, 1
	shr	cx, 1
	mov	ax, ds
	add	ax, cx
	mov	ds, ax
	mov	ax, es
	add	ax, cx
	mov	es, ax
	ret
large_move	endp

_swap_RAM	proc	near
	push	bp
	mov	bp, sp
	push	si
	push	di
	push	ds

	lds	si, LOC_A
	les	di, LOC_B
	cld

next_huge_chunk:
	cmp	word ptr SIZEY + 2, 0
	je	try_small_chunk
	dec	word ptr SIZEY + 2
	mov	cx, 4000h
	call	large_move
	mov	cx, 4000h
	call	large_move
	jmp	next_huge_chunk
try_small_chunk:
	mov	cx, word ptr SIZEY
	and	cx, 0FFF0h
	shr	cx, 1
	push	cx
	call	large_move
	mov	cx, word ptr SIZEY
	pop	ax
	shl	ax, 1
	sub	cx, ax
	jcxz	done_swapping
next_byte_rest:
	mov	al, es:[di]
	movsb
	mov	ds:[si-1], al
	loop	next_byte_rest
done_swapping:
	pop	ds
	pop	di
	pop	si
	pop	bp
	ret
_swap_RAM	endp

	END
