#if 0 

  File name:  cpu8086.h
 
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
/*
 *	Modifications history
 *
 *	03 Dec 91 - Added CPU_type variable and corresponding enum
 */
/*
#define FAST_INSTRUCTION_FETCH
 */
/*
 *	Implementation-dependent definitions & types
 */
typedef unsigned short	WORD ;		/* Should contain exactly 16 bits	*/
typedef signed	 short	SWORD ; 	/* Should contain exactly 16 bits	*/
typedef unsigned long	DWORD ; 	/* Should contain at least 32 bits	*/
typedef signed	 long	SDWORD ;	/* Should contain at least 32 bits	*/
typedef unsigned char	BYTE ;		/* Should contain exactly  8 bits	*/
typedef signed	 char	SBYTE ; 	/* Should contain exactly  8 bits	*/
typedef unsigned long	ADDR20 ;	/* Should contain at least 20 bits	*/

/*
 *	LO_BYTE(x) gets address of least significant byte nibble of word at x
 *	HI_BYTE(x) gets address of most significant byte nibble of word at x
 *	Current definition is Ok on Intel processors and PDP-11
 */
BYTE	*LO_BYTE(WORD*) ;
BYTE	*HI_BYTE(WORD*) ;
WORD	MK_WORD(BYTE,BYTE) ;
#define LO_BYTE(x)	((BYTE*)(x)+0)
#define HI_BYTE(x)	((BYTE*)(x)+1)
#define MK_WORD(lo,hi)	(((WORD)(hi)<<8)|(lo))

/*
 *	CBW(x)	performs sign extend conversion of BYTE to WORD
 */
WORD	CBW(BYTE) ;
#define CBW(x)		((WORD)(SWORD)(SBYTE)x)

/*
 *	FASTCALL should be set to function type modifier which
 *	provides fastest call sequence (such as pascal in TC)
 */
#ifdef	__TURBOC__
	#if	__TURBOC__ >= 0x0400
		#define	FASTCALL        _fastcall
	#else
		#define FASTCALL	pascal
	#endif
		#define	LIBSYM		_Cdecl
#else
		#define	FASTCALL
		#define	LIBSYM
#endif
/*
 *	FAR is the pointer type modifier which enables access to
 *	all system memory on target macine.
 */
#define FAR		far

/*
 *	End of implementation-dependent definitions & types
 */

/*
 *	Bits in FLAGS register
 */
#define FL_OF		0x0800u 	/* Overflow		*/
#define FL_DF		0x0400u 	/* Direction		*/
#define FL_IF		0x0200u 	/* Interrupt		*/
#define FL_TF		0x0100u 	/* Debug trap		*/
#define FL_SF		0x0080u 	/* Sign 		*/
#define FL_ZF		0x0040u 	/* Zero 		*/
#define FL_AF		0x0010u 	/* Auxillary carry	*/
#define FL_PF		0x0004u 	/* Parity		*/
#define FL_CF		0x0001u 	/* Carry		*/
#define FL_RESET_86	0xF002u 	/* Bits always set	*/
#define FL_SET_86	0xFFD7u 	/* Bits always zero	*/
					/*    286 extensions	*/
#define FL_IOPL 	0x3000u 	/* IOPL 		*/
#define FL_IOPL_SHIFT	12
#define FL_NT		0x4000u 	/* Nested task		*/
#define FL_RESET_286	0x0002u 	/* Bits always set	*/
#define FL_SET_286	0x0FD7u 	/* Bits always zero	*/

/*
 *	Bits in MSW register
 */
#define MSW_PE		0x0001u 	/* Protected mode	*/
#define MSW_MP		0x0002u 	/* Monitor math 	*/
#define MSW_EM		0x0004u 	/* Emulate math 	*/
#define MSW_TS		0x0008u 	/* Task switched	*/
#define MSW_RESET	0xFFF0u 	/* Bits always set	*/
#define MSW_SET 	0xFFFFu 	/* Bits always zero	*/

/*
 *	286 protected mode additional types
 */
typedef struct	{
	DWORD		base ;
	WORD		selector ;
	WORD		limit ;
	WORD		flags ;
	BYTE		rights ;
	} DESC ;			/* Descriptor cache	*/

/*
 *	Bits in the descriptor cache flags word
 */
#define ADR_MASK	0x00FFFFFFul
#define DSC_VALID	0x0001u
#define DSC_READABLE	0x0002u
#define DSC_WRITEABLE	0x0004u
#define DSC_EXPANDDOWN	0x0008u

/*
 *	General-purpose 16-bit registers
 */
extern	WORD	ax, bx, cx, dx, si, di, bp, sp ;
/*
 *	286 extension registers
 */
extern	WORD	msw ;
/*
 *	286 special control registers
 */
extern	DESC	gdt, idt, ldt, tr ;
/*
 *	286 segment registers (currently inactive)
 */
/*extern  DESC	  cs, ds, ss, es ;*/

/*
 *	Definitions of	 8-bit half-registers
 */
#define al	(*LO_BYTE(&ax))
#define ah	(*HI_BYTE(&ax))
#define bl	(*LO_BYTE(&bx))
#define bh	(*HI_BYTE(&bx))
#define cl	(*LO_BYTE(&cx))
#define ch	(*HI_BYTE(&cx))
#define dl	(*LO_BYTE(&dx))
#define dh	(*HI_BYTE(&dx))

/*
 *	Special & segment registers
 */
extern	WORD	ip, flags ;
extern	WORD	cs, ds, es, ss ;

/*
 *	Memory adressing.
 *	All CPU memory requests goes throu following functions.
 *	They should ALWAYS return to caller, instead of any
 *	breakpoints set on memory access.
 */
BYTE	FASTCALL fetch_byte( WORD seg, WORD off ) ;
WORD	FASTCALL fetch_word( WORD seg, WORD off ) ;
void	FASTCALL store_word( WORD seg, WORD off, WORD x ) ;
void	FASTCALL store_byte( WORD seg, WORD off, BYTE x ) ;
WORD	FASTCALL word_IN( WORD ) ;
BYTE	FASTCALL byte_IN( WORD ) ;
void	FASTCALL word_OUT( WORD, WORD ) ;
void	FASTCALL byte_OUT( WORD, BYTE ) ;

/*
 *	Interrupt handling.
 *	interrupt_check should return interrupt number or 0xFFFF if
 *	no interrupt request pending. It should NOT indicate maskable
 *	interrupts if bit FL_IF in flags reset to zero.
 */
WORD	interrupt_check( void ) ;

/*
 *	Error handling.
 *	CPU_error will be called by decode_instruction any time then
 *	illegal instruction encoding is encountered or emulator
 *	internal error detected. Instruction exceptions (such as division
 *	by zero error) does NOT goes here.
 *	instruction_cs:instruction_ip will contain address of offending
 *	instruction. (cs:ip can point anythere)
 *	CPU_error should NOT return to it's caller.
 */
void	FASTCALL CPU_error( char *message ) ;
extern	WORD	instruction_cs ;
extern	WORD	instruction_ip ;

/*
 *	Emulator entry point.
 */
void	decode_instruction( void ) ;

/*
 *	Initialization entry point.
 */
int	init_CPU_parameters( void ) ;

extern	char	halt_condition ;
extern	char	resume_flag ;

enum	CPU_types	{  CPU_8086, CPU_186,  CPU_286, CPU_COUNT  } ;
extern	char		CPU_type ;
extern	WORD		FL_SET ;
extern	WORD		FL_RESET ;

/*
 *	Prefetch queue support
 */
#ifndef FAST_INSTRUCTION_FETCH
#define MAX_PREFETCH	40
#define MIN_PREFETCH	4
extern	WORD		prefetch_bytes ;
extern	void		(*restart_prefetch_fun)( void ) ;
extern	void		(*backspace_prefetch_fun)( void ) ;

void	restart_prefetch( void ) ;
#define restart_prefetch()		(*restart_prefetch_fun)()
#define backspace_prefetch()		(*backspace_prefetch_fun)()
extern	WORD		prefetch_bytes ;
extern	WORD		prefetch_used ;
extern	BYTE		*prefetch_head ;
extern	BYTE		prefetch_queue[ MAX_PREFETCH + 1 ] ;
#define prefetch_top	(prefetch_queue+MAX_PREFETCH)
#else
#define restart_prefetch()
#define backspace_prefetch()
#endif
