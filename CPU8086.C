#if 0 

  File name:  cpu8086.c, 80x86 CPU simulator
 
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
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "cpu8086.h"
#include "config.h"
#include "os.h"
/*
 *	Modifications history.
 *
 *	30/04/91 - Added 'resume_flag = 1' to popf code.
 *	03 Dec 91
 *		 - Added CPU_type variable (no init yet).
 *		 - Fixed bug in 'pop sp' (removed increment by 2)
 *		 - Added AAD/AAM support for bases other then 10
 *		 - Added cf2al (0xD6) instruction
 *	09 Dec 91
 *		 - Added 80186 extended instruction set.
 *		 - Added new section 'CPU Options' to configuration file.
 *			Possible values are: Processor and Prefetch.
 *		 - Added shift_mask to mask out shift counts >= 32 on
 *			186 and above CPUs
 *		 - Fixed push sp for 286 mode
 *		 - Added decoder for 186/286 instructions (no code yet
 *			for PM control instructions)
 *		 - Added new interface function for all instructions with
 *			memory access: fetch_word_EA_data(), fetch_byte_EA_data()
 *				       store_word_EA_data(), store_byte_EA_data
 *		 - decode_EA function call moved to parce_send_byte for sake of
 *			interface consistency
 *	10 Dec 91
 *		 - Most memory acesses now pass thru fetch/store_word/byte_EA_data()
 *	12 Dec 91
 *		 - Changed some emulated functions names to avoid clash wish
 *			assembler reserved words.
 *		 - Completed 80186 instruction set.
 *	12 Dec 91
 *		 - Fixed bug in 8086 code for idiv instruction, so that quotient value
 *			of 0x8000 now causes exception 0 while in 8086 mode.
 *		 - Fixed bug with return address of divide but zero exception in 286 mode.
 *		 - ESC/WAIT are now sensitive to bits in msw while in 286 mode.
 *		 - call_interrupt now takes notice of idt.base in 286 mode.
 *	14 Dec 91
 *		 - illegal() now calls int 6 in 80286 mode
 *	15 Dec 91
 *		 - Added prefetch queue support
 *	18 Dec 91 released version 0.15
 */
#define NIL	((WORD)0)
/*
 *	Emulator definitions & variables
 */
char	CPU_type = CPU_8086 ;
/*
 *	General-purpose 16-bit registers
 */
WORD	ax, bx, cx, dx, si, di, bp, sp ;
/*
 *	286 extension registers
 */
WORD	msw ;
/*
 *	286 special control registers
 */
DESC	gdt, idt, ldt, tr ;
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
WORD	ip, flags ;
WORD	cs, ds, es, ss ;

/*
 *	Instruction encoding
 */
static	WORD	*_regsw_[]  = {  &ax, &cx, &dx, &bx, &sp, &bp, &si, &di  } ;
static	BYTE	*_regsb_[]  = {  &al, &cl, &dl, &bl, &ah, &ch, &dh, &bh  } ;
static	WORD	*_segreg_[] = {  &es, &cs, &ss, &ds  } ;
WORD	*decode_reg16( unsigned x ) ;
BYTE	*decode_reg8( unsigned x ) ;
WORD	*decode_segreg( unsigned x ) ;
#define decode_reg16(x) 	(_regsw_[(x)])
#define decode_reg8(x)		(_regsb_[(x)])
#define decode_segreg(x)	(_segreg_[(x)])
WORD	instruction_cs ;		/* cs:ip of current			*/
WORD	instruction_ip ;		/* instruction				*/
static	WORD	*seg_override ; 	/* segment override			*/
static	WORD	EA ;			/* effective address			*/
static	BYTE	fetched_byte ;		/* 3 low bits of 1st instruction byte	*/
static	BYTE	second_fetched_byte ;	/* 2nd instruction byte 		*/
static unsigned mod, reg, r_m ; 	/* scratch pad				*/
static	BYTE	shift_count ;		/* shift count				*/
static	BYTE	shift_mask ;		/* mask used to mask out unused bits	*/
static	WORD	call_cs ;		/* used by call_far & call_near 	*/
static	WORD	call_ip ;		/*					*/
	WORD	FL_SET, FL_RESET ;	/* This used to be define, but		*/
					/* some CPUs behave differently...	*/

/*
 *	Register select & memory data latch
 */
static	WORD	*src_reg16, *dst_reg16 ;
static	BYTE	*src_reg8,  *dst_reg8 ;
static	WORD	latch_word, latch_word_1 ;
static	BYTE	latch_byte, latch_byte_1 ;

/*
 *	Temporary arifmetic engine variables
 */
static	DWORD	temp32 ;
static	WORD	temp16 ;
static	BYTE	temp8 ;
static	BYTE	temp5 ;
static	SDWORD	stemp32 ;
static	SWORD	stemp16 ;

/*
 *	Secondary memory access
 */
WORD	pop_word( void ) ;
void	push_word( WORD x ) ;
BYTE	fetch_byte_data( void ) ;		/* From CS:IP */
WORD	fetch_word_data( void ) ;		/* From CS:IP */
WORD	fetch_signed_byte_data( void ) ;	/* From CS:IP */
void	fetch_word_EA_data( void ) ;
void	fetch_byte_EA_data( void ) ;
void	store_word_EA_data( void ) ;
void	store_byte_EA_data( void ) ;
#define pop_word()			fetch_word(ss,(sp+=2)-2)
#define push_word(x)			store_word(ss,sp-=2,x)
#define fetch_signed_byte_data()	CBW(fetch_byte_data())

#ifdef	FAST_INSTRUCTION_FETCH
	#define fetch_byte_data()	fetch_byte(cs,(ip+=1)-1)
	#define fetch_word_data()	fetch_word(cs,(ip+=2)-2)
#else
	BYTE	immediate_fetch_byte_data(  void ) ;
	WORD	immediate_fetch_word_data(  void ) ;
	void	immediate_restart_prefetch( void ) ;
static	BYTE	(*fetch_byte_data_fun)( void )	  = immediate_fetch_byte_data ;
static	WORD	(*fetch_word_data_fun)( void )	  = immediate_fetch_word_data ;
	void	(*restart_prefetch_fun)( void )   = immediate_restart_prefetch ;
	void	(*backspace_prefetch_fun)( void ) = immediate_restart_prefetch ;
static	char	prefetch_stopped = 0 ;
static	WORD	prefetch_lowwater ;
static	WORD	prefetch_highwater ;
static	WORD	prefetch_ip ;
static	BYTE	*prefetch_tail ;
	WORD	prefetch_bytes = 0 ;
	WORD	prefetch_used ;
	BYTE	*prefetch_head ;
	BYTE	prefetch_queue[ MAX_PREFETCH + 1 ] ;
	#define fetch_byte_data()	(*fetch_byte_data_fun)()
	#define fetch_word_data()	(*fetch_word_data_fun)()
#endif

/*
 *	Execution control
 */
	char	resume_flag ;
	char	halt_condition = 0 ;

/*
 *	First instruction byte decode table (5 hi bits, 32 entries)
 */
static	void	illegal( void ) ;
static	void	pm_instructions_decode( void ) ;
static
void	add_esstk(void),	or_csstk(void), 	adc_ssstk(void),
	sbb_dsstk(void),	and_daa(void),		sub_das(void),
	xor_aaa(void),		cmp_aas(void),		inc_reg16(void),
	dec_reg16(void),	push_reg16(void),	pop_reg16(void),
	jmp_group1(void),	jmp_group2(void),	group1_test_xchg(void),
	mov_seg_pop(void),	xchg_ax_reg(void),	scnv_flagsops(void),
	mov_strops(void),	test_strops(void),	mov_reg8_immed(void),
	mov_reg16_immed(void),	ret_lxs_mov(void),	retf_int(void),
	shifts_xlat(void),	extension_escape(void), loops_io(void),
	call_jmp_io(void),	reps_group2(void),	flags_group3(void),
	/* 286 extensions */
	pusha_etc(void),	push_mul_ios(void) ;

static	void	call_interrupt(void) ;
static	void	call_far(void) ;
static	void	call_near(void) ;
static	void	decode_EA( void ) ;

static	void	(*(decode_MSB_entries[]))( void ) = {
	/* 00-07 */	add_esstk,		/* 08-0F */	or_csstk,
	/* 10-17 */	adc_ssstk,		/* 18-1F */	sbb_dsstk,
	/* 20-27 */	and_daa,		/* 28-2F */	sub_das,
	/* 30-37 */	xor_aaa,		/* 38-3F */	cmp_aas,
	/* 40-47 */	inc_reg16,		/* 48-4F */	dec_reg16,
	/* 50-57 */	push_reg16,		/* 58-5F */	pop_reg16,
	/* 60-67 */	pusha_etc,		/* 68-6F */	push_mul_ios,
	/* 70-77 */	jmp_group1,		/* 78-7F */	jmp_group2,
	/* 80-87 */	group1_test_xchg,	/* 88-8F */	mov_seg_pop,
	/* 90-97 */	xchg_ax_reg,		/* 98-9F */	scnv_flagsops,
	/* A0-A7 */	mov_strops,		/* A8-AF */	test_strops,
	/* B0-B7 */	mov_reg8_immed, 	/* B8-BF */	mov_reg16_immed,
	/* C0-C7 */	ret_lxs_mov,		/* C8-CF */	retf_int,
	/* D0-D7 */	shifts_xlat,		/* D8-DF */	extension_escape,
	/* E0-E7 */	loops_io,		/* E8-EF */	call_jmp_io,
	/* F0-F7 */	reps_group2,		/* F8-FF */	flags_group3
	} ;

void
decode_instruction( void )
{
static	BYTE	scratch_pad ;

instruction_cs = cs ;
instruction_ip = ip ;
seg_override   = NULL ;
for( ; ; )
	switch( scratch_pad = fetch_byte_data() ){
		case 0x2e:	seg_override = &cs ;	break ;
		case 0x3e:	seg_override = &ds ;	break ;
		case 0x26:	seg_override = &es ;	break ;
		case 0x36:	seg_override = &ss ;	break ;
		case 0xF0:	/* LOCK */		break ;
		default:
			fetched_byte = scratch_pad & 7 ;
			(*decode_MSB_entries[ scratch_pad >> 3 ] )() ;
			goto instruction_executed ;
		}
instruction_executed:;
if( halt_condition ){
	while( ( latch_word = interrupt_check() ) == 0xFFFF ) ;
	halt_condition = 0 ;
	latch_word *= 4 ;
	call_interrupt() ;
	}
else
if( resume_flag ) resume_flag = 0 ;
else
if( flags & FL_TF ){
	latch_word = 1 * 4 ;
	call_interrupt() ;
	}
else
if( ( latch_word = interrupt_check() ) != 0xFFFF ){
	latch_word *= 4 ;
	call_interrupt() ;
	}
}

static void
illegal( void )
{
if( CPU_type >= CPU_286 ){
	ip = instruction_ip ;
	cs = instruction_cs ;
	latch_word = 6 * 4 ;
	call_interrupt() ;
	}
else	CPU_error( "Illegal instruction encountered !" ) ;
}

#ifndef FAST_INSTRUCTION_FETCH
/*
 *	Instruction prefetch queue control
 */
static BYTE
immediate_fetch_byte_data( void )
{
return fetch_byte(cs,(ip+=1)-1) ;
}

static WORD
immediate_fetch_word_data( void )
{
return fetch_word(cs,(ip+=2)-2) ;
}

static void
immediate_restart_prefetch( void )
{
}

static void
fill_prefetch( void )
{
static	WORD	temp ;
static	BYTE	temp1 ;

if( prefetch_stopped ) return ;
while( prefetch_used < prefetch_highwater ){
	if( prefetch_ip <= 0xFFFE ){
		temp = fetch_word( cs, ( prefetch_ip += 2 ) - 2 ) ;
		/*
		 *	note what for the word fetched at the end of prefetch
		 *	buffer high byte will be stored TWICE - at the last and
		 *	first position of circular buffer.
		 */
		if( prefetch_tail == prefetch_queue )
			*prefetch_top = *LO_BYTE( &temp ) ;
		*prefetch_tail++ = *LO_BYTE( &temp ) ;
		*prefetch_tail++ = *HI_BYTE( &temp ) ;
		if( prefetch_tail >= prefetch_top ){
			if( prefetch_tail == prefetch_top )
				prefetch_tail	= prefetch_queue ;
			else {
				*prefetch_queue = *HI_BYTE( &temp ) ;
				prefetch_tail	= prefetch_queue + 1 ;
				}
			}
		prefetch_used += 2 ;
		}
	else {
		temp1 = fetch_word( cs, ( prefetch_ip += 1 ) - 1 ) ;
		if( prefetch_tail == prefetch_queue )
			*prefetch_top = temp1 ;
		*prefetch_tail++ = temp1 ;
		if( prefetch_tail >= prefetch_top )
			prefetch_tail = prefetch_queue ;
		prefetch_used++ ;
		if( CPU_type >= CPU_286 ){
			prefetch_stopped = 1 ;
			return ;
			}
		}
	}
}

static void
restart_prefetch_queue( void )
{
prefetch_head	 = prefetch_tail = prefetch_queue ;
prefetch_used	 = 0 ;
prefetch_ip	 = ip ;
prefetch_stopped = 0 ;
fill_prefetch() ;
}

static void
backspace_prefetch_queue( void )
{
static	WORD	chg ;

if( MAX_PREFETCH - prefetch_used < ( chg = prefetch_ip - prefetch_used - ip ) ){
	restart_prefetch_queue() ;
	return ;
	}
prefetch_used += chg ;
if( ( prefetch_head -= chg ) < prefetch_queue )
	prefetch_head += MAX_PREFETCH ;
/*
if( prefetch_used > prefetch_bytes ){
	chg	       = prefetch_used - prefetch_bytes ;
	prefetch_used  = prefetch_bytes ;
	prefetch_ip   -= chg ;
	if( ( prefetch_tail -= chg ) < prefetch_queue )
		prefetch_tail += MAX_PREFETCH ;
	}
*/
}

static WORD
prefetch_word_data( void )
{
static	WORD	result ;

ip += 2 ;
if( prefetch_used < 2 ){
	CPU_error( "Attempt to execute past end of segment !" ) ;
	return 0 ;
	}
result = MK_WORD( prefetch_head[0], prefetch_head[1] ) ;
if( ( prefetch_head += 2 ) >= prefetch_top )
	prefetch_head -= MAX_PREFETCH ;
if( ( prefetch_used -= 2 ) <= prefetch_lowwater )
	fill_prefetch() ;
return result ;
}

static BYTE
prefetch_byte_data( void )
{
static	BYTE	result ;

ip++ ;
if( prefetch_used < 1 ){
	CPU_error( "Attempt to execute past end of segment !" ) ;
	return 0 ;
	}
result = *prefetch_head ;
if( ++prefetch_head >= prefetch_top )
	prefetch_head = prefetch_queue ;
if( --prefetch_used <= prefetch_lowwater )
	fill_prefetch() ;
return result ;
}
#endif
/*
 *	Memory adressing decoding functions
 */
static void
parce_second_byte( void )
{
second_fetched_byte = fetch_byte_data() ;
r_m =	second_fetched_byte	   & 7 ;
reg = ( second_fetched_byte >> 3 ) & 7 ;
if( ( mod = ( second_fetched_byte >> 6 ) & 3 ) != 3 )
	decode_EA() ;
}

static void
parce_immediate_addr( void )
{
mod = 0 ;
r_m = 6 ;
decode_EA() ;
}

#define set_segment(seg)	(seg_override=(seg_override==NULL?(seg):seg_override))

static WORD
decode_rm( void )
{
switch( r_m ){
	case 0: set_segment( &ds ) ;
		return bx + si ;
	case 1: set_segment( &ds ) ;
		return bx + di ;
	case 2: set_segment( &ss ) ;
		return bp + si ;
	case 3: set_segment( &ss ) ;
		return bp + di ;
	case 4: set_segment( &ds ) ;
		return si ;
	case 5: set_segment( &ds ) ;
		return di ;
	case 6: set_segment( &ss ) ;
		return bp ;
	case 7: set_segment( &ds ) ;
		return bx ;
	}
CPU_error( "Internal error - illegal r/m value" ) ;
return 0 ;
}

static void
decode_EA( void )
{
switch( mod ){
	case 0: /* zero length displacement	*/
		if( r_m == 6 ){ /* direct memory operand	*/
			EA = fetch_word_data() ;
			set_segment( &ds ) ;
			}
		else	EA = decode_rm() ;
		break ;
	case 1: /* 1-byte displacement		*/
		EA = decode_rm() + fetch_signed_byte_data() ;
		break ;
	case 2: /* 2-byte displacement		*/
		EA = decode_rm() + fetch_word_data() ;
		break ;
	default:
		CPU_error( "Illegal adressing mode for instruction !" ) ;
	}
}

/*
 *	Common EA memory access
 */

static void
fetch_word_EA_data( void )
{
if( mod == 3 )
	latch_word = *decode_reg16( r_m ) ;
else	latch_word = fetch_word( *seg_override, EA ) ;
}

static void
fetch_byte_EA_data( void )
{
if( mod == 3 )
	latch_byte = *decode_reg8( r_m ) ;
else	latch_byte = fetch_byte( *seg_override, EA ) ;
}

static void
store_word_EA_data( void )
{
if( mod == 3 )
	*decode_reg16( r_m ) = latch_word ;
else	store_word( *seg_override, EA, latch_word ) ;
}

static void
store_byte_EA_data( void )
{
if( mod == 3 )
	*decode_reg8( r_m ) = latch_byte ;
else	store_byte( *seg_override, EA, latch_byte ) ;
}

/*
 *	Low-nibble decoders
 */
/*
 *	Instruction engines (sets CCs according to result)
 *	If operation result depends upon argument order, first arg is
 *	command 'dst'
 */
WORD FASTCALL word_ADD( WORD, WORD ) ;	BYTE FASTCALL byte_ADD( BYTE, BYTE ) ;
WORD FASTCALL word_OR(	WORD, WORD ) ;	BYTE FASTCALL byte_OR(	BYTE, BYTE ) ;
WORD FASTCALL word_ADC( WORD, WORD ) ;	BYTE FASTCALL byte_ADC( BYTE, BYTE ) ;
WORD FASTCALL word_SBB( WORD, WORD ) ;	BYTE FASTCALL byte_SBB( BYTE, BYTE ) ;
WORD FASTCALL word_INC( WORD ) ;	BYTE FASTCALL byte_INC( BYTE ) ;
WORD FASTCALL word_DEC( WORD ) ;	BYTE FASTCALL byte_DEC( BYTE ) ;
WORD FASTCALL word_AND( WORD, WORD ) ;	BYTE FASTCALL byte_AND( BYTE, BYTE ) ;
WORD FASTCALL word_SUB( WORD, WORD ) ;	BYTE FASTCALL byte_SUB( BYTE, BYTE ) ;
WORD FASTCALL word_XOR( WORD, WORD ) ;	BYTE FASTCALL byte_XOR( BYTE, BYTE ) ;
WORD FASTCALL word_ROL( WORD ) ;	BYTE FASTCALL byte_ROL( BYTE ) ;
WORD FASTCALL word_ROR( WORD ) ;	BYTE FASTCALL byte_ROR( BYTE ) ;
WORD FASTCALL word_RCL( WORD ) ;	BYTE FASTCALL byte_RCL( BYTE ) ;
WORD FASTCALL word_RCR( WORD ) ;	BYTE FASTCALL byte_RCR( BYTE ) ;
WORD FASTCALL word_SHL( WORD ) ;	BYTE FASTCALL byte_SHL( BYTE ) ;
WORD FASTCALL word_SHR( WORD ) ;	BYTE FASTCALL byte_SHR( BYTE ) ;
WORD FASTCALL word_SAR( WORD ) ;	BYTE FASTCALL byte_SAR( BYTE ) ;
WORD FASTCALL word_NOT( WORD ) ;	BYTE FASTCALL byte_NOT( BYTE ) ;
WORD FASTCALL word_NEG( WORD ) ;	BYTE FASTCALL byte_NEG( BYTE ) ;
void FASTCALL word_MUL( WORD ) ;	void FASTCALL byte_MUL( BYTE ) ;
void FASTCALL word_DIV( WORD ) ;	void FASTCALL byte_DIV( BYTE ) ;
void FASTCALL word_IMUL(SWORD,SWORD) ;	void FASTCALL byte_IMUL( SBYTE ) ;
void FASTCALL word_IDIV( SWORD ) ;	void FASTCALL byte_IDIV( SBYTE ) ;
void FASTCALL _aam( SBYTE ) ;		void FASTCALL _aad( SBYTE ) ;
void FASTCALL _daa( void ) ;		void FASTCALL _das( void ) ;
void FASTCALL _aaa( void ) ;		void FASTCALL _aas( void ) ;
void FASTCALL cf2al( void ) ;		void FASTCALL _wait( void ) ;
/*
 *	186 instruction additions
 */
void insb( void ) ;			void insw( void ) ;
void outsb( void ) ;			void outsw( void ) ;
/*
 *	286 special control instructions
 */
void FASTCALL _clts( void ) ;		void FASTCALL shutdown( void ) ;
void FASTCALL loadall( void ) ; 	WORD FASTCALL _lar( WORD ) ;
WORD FASTCALL _lsl( WORD ) ;		void FASTCALL _lldt( WORD ) ;
void FASTCALL _ltr( WORD ) ;		void FASTCALL _verr( WORD ) ;
void FASTCALL _verw( WORD ) ;		WORD FASTCALL _sldt( void ) ;
WORD FASTCALL _str( void ) ;		void FASTCALL _lmsw( WORD ) ;
WORD FASTCALL _smsw( void ) ;		WORD FASTCALL _arpl( WORD, WORD ) ;
void FASTCALL store_idt_gdt( DESC * ) ; void FASTCALL load_idt_gdt( DESC * ) ;

/*
 *	Segment registers stack operations
 */
void	check_segop( WORD *seg ) ;
void	pushpop_seg( WORD *seg ) ;
#define check_segop(seg)	{if(fetched_byte>=6){pushpop_seg(seg);return;}}
#define pushpop_seg(seg)	(fetched_byte&1?(void)(*seg=pop_word()):push_word(*(seg)))

static	int	_2index ;

static	WORD FASTCALL	(*(_2address_16[]))( WORD x, WORD y ) = {
	word_ADD,	word_OR,	word_ADC,	word_SBB,
	word_AND,	word_SUB,	word_XOR
	} ;

static	BYTE FASTCALL	(*(_2address_8[]))( BYTE x, BYTE y ) = {
	byte_ADD,	byte_OR,	byte_ADC,	byte_SBB,
	byte_AND,	byte_SUB,	byte_XOR
	} ;

static void
common_2address_decoder( void )
{
	#define word_OP(x,y)	((*(_2address_16[_2index]))((x),(y)))
	#define byte_OP(x,y)	((*(_2address_8[_2index]))((x),(y)))

switch( fetched_byte ){
	case 0: 				/* xxx r/m, reg8	*/
		parce_second_byte() ;
		fetch_byte_EA_data() ;
		latch_byte = byte_OP( latch_byte, *decode_reg8( reg ) ) ;
		store_byte_EA_data() ;
		break ;
	case 1: 				/* xxx r/m, reg16	*/
		parce_second_byte() ;
		fetch_word_EA_data() ;
		latch_word = word_OP( latch_word, *decode_reg16( reg ) ) ;
		store_word_EA_data() ;
		break ;
	case 2: 				/* xxx reg8, r/m	*/
		parce_second_byte() ;
		dst_reg8 = decode_reg8( reg ) ;
		fetch_byte_EA_data() ;
		*dst_reg8 = byte_OP( *dst_reg8, latch_byte ) ;
		break ;
	case 3: 				/* xxx reg16, r/m	*/
		parce_second_byte() ;
		dst_reg16 = decode_reg16( reg ) ;
		fetch_word_EA_data() ;
		*dst_reg16 = word_OP( *dst_reg16, latch_word ) ;
		break ;
	case 4: 				/* xxx accum, immed	*/
		al = byte_OP( al, fetch_byte_data() ) ;
		break ;
	case 5:
		ax = word_OP( ax, fetch_word_data() ) ;
		break ;
	default:
		illegal() ;
	}
	#undef	word_OP
	#undef	byte_OP
}

static void
add_esstk( void )
{
check_segop( &es ) ;				/* push es ; pop es	*/
_2index = 0 ;
common_2address_decoder() ;
}

static void
or_csstk( void )
{
switch( fetched_byte ){
	default:
		_2index = 1 ;
		common_2address_decoder() ;
		break ;
	case 6: 				/* push cs		*/
		push_word( cs ) ;
		break ;
	case 7: 				/* pop cs on 86,186	*/
		if( CPU_type < CPU_286 )	/* PM ops on 286	*/
			cs = pop_word() ;
		else	pm_instructions_decode() ;
	}
}

static void
adc_ssstk( void )
{
if( fetched_byte >= 6 ){	/* push ss ; pop ss				*/
				/* Can't use check_segop() - need to set RF     */
	if( fetched_byte & 1 ){
		ss = pop_word() ;
		resume_flag = 1 ;
		}
	else	push_word( ss ) ;
	return ;
	}
_2index = 2 ;
common_2address_decoder() ;
}

static void
sbb_dsstk( void )
{
check_segop( &ds ) ;				/* push ds ; pop ds	*/
_2index = 3 ;
common_2address_decoder() ;
}

static void
and_daa( void )
{
/*
 *	es: eliminated by previous scan
 */
if( fetched_byte == 7 ){ _daa() ; return ; }
_2index = 4 ;
common_2address_decoder() ;
}

static void
sub_das( void )
{
/*
 *	cs: eliminated by previous scan
 */
if( fetched_byte == 7 ){ _das() ; return ; }
_2index = 5 ;
common_2address_decoder() ;
}

static void
xor_aaa( void )
{
/*
 *	ss: eliminated by previous scan
 */
if( fetched_byte == 7 ){ _aaa() ; return ; }
_2index = 6 ;
common_2address_decoder() ;
}

static void
cmp_aas( void )
{
/*
 *	ds: eliminated by previous scan
 */
if( fetched_byte == 7 ){ _aas() ; return ; }
/*
 *	Can't use common_2address_decoder() because CMP
 *	do not store data to dst
 */
switch( fetched_byte ){
	case 0: 				/* cmp r/m, reg8	*/
		parce_second_byte() ;
		fetch_byte_EA_data() ;
		byte_SUB( latch_byte, *decode_reg8( reg ) ) ;
		break ;
	case 1: 				/* cmp r/m, reg16	*/
		parce_second_byte() ;
		fetch_word_EA_data() ;
		word_SUB( latch_word, *decode_reg16( reg ) ) ;
		break ;
	case 2: 				/* cmp reg8, r/m	*/
		parce_second_byte() ;
		fetch_byte_EA_data() ;
		byte_SUB( *decode_reg8( reg ), latch_byte ) ;
		break ;
	case 3: 				/* cmp reg16, r/m	*/
		parce_second_byte() ;
		fetch_word_EA_data() ;
		word_SUB( *decode_reg16( reg ), latch_word ) ;
		break ;
	case 4: 				/* xxx accum, immed	*/
		byte_SUB( al, fetch_byte_data() ) ;
		break ;
	case 5:
		word_SUB( ax, fetch_word_data() ) ;
		break ;
	default:
		CPU_error( "Internal error in common_2address_decoder !" ) ;
	}
}

static void
inc_reg16( void )
{
dst_reg16  = decode_reg16( fetched_byte ) ;
*dst_reg16 = word_INC( *dst_reg16 ) ;
}

static void
dec_reg16( void )
{
dst_reg16  = decode_reg16( fetched_byte ) ;
*dst_reg16 = word_DEC( *dst_reg16 ) ;
}

static void
push_reg16( void )
{
latch_word = *decode_reg16( fetched_byte ) ;
if( fetched_byte == 4 && CPU_type < CPU_286 )
	/* Simulate bug in 8086 & 80186 push sp */
	latch_word -= 2 ;
push_word( latch_word ) ;
}

static void
pop_reg16( void )
{
*decode_reg16( fetched_byte ) = pop_word() ;
}

static	WORD	*stack_all_regs[] = { &ax, &cx, &dx, &bx, &latch_word, &bp, &si, &di } ;

static void
pusha( void )
{
	WORD	**p ;

latch_word = sp ;
for( p = stack_all_regs ; p < stack_all_regs + 8 ; p++ )
	push_word( **p ) ;
}

static void
popa( void )
{
	WORD	**p ;

for( p = stack_all_regs + 7 ; p >= stack_all_regs ; p-- )
	**p = pop_word() ;
}

static void
pusha_etc( void )
{
if( CPU_type < CPU_186 ){ illegal() ; return ; }
switch( fetched_byte ){
	case 0: 				/*  pusha		*/
		pusha() ;
		break ;
	case 1: 				/*  popa		*/
		popa() ;
		break ;
	case 2: 				/*  bound   reg, mem32	*/
		parce_second_byte() ;
		if( mod == 3 ){ illegal() ; return ; }
		src_reg16 = decode_reg16( reg ) ;
		fetch_word_EA_data() ;
		latch_word_1 = latch_word ;
		EA += 2 ;
		fetch_word_EA_data() ;
		if( (SWORD)*src_reg16 < (SWORD)latch_word_1 ||
		    (SWORD)*src_reg16 > (SWORD)latch_word ){
			latch_word = 4 * 5 ;
			call_interrupt() ;
			}
		break ;
	case 3: 				/*  arpl    r/m16, reg	*/
		if( CPU_type < CPU_286 ){ illegal() ; return ; }
		if( !( msw & MSW_PE )  ){ illegal() ; return ; }
		parce_second_byte() ;
		fetch_word_EA_data() ;
		latch_word = _arpl( latch_word, *decode_reg16( reg ) ) ;
		store_word_EA_data() ;
		break ;
	default:				/*  386 prefixes	*/
		illegal() ;
		break ;
	}
}

static void
push_mul_ios( void )
{
	void	insb(void), insw(void), outsb(void), outsw(void) ;

if( CPU_type < CPU_186 ){ illegal() ; return ; }
switch( fetched_byte ){
	case 0: 				/*	push	imm16		*/
		push_word( fetch_word_data() ) ;
		break ;
	case 1: 				/*	imul  r16, r/m16, imm16 */
		parce_second_byte() ;
		fetch_word_EA_data() ;
		word_IMUL( latch_word, fetch_word_data() ) ;
		*decode_reg16( reg ) = (WORD) stemp32 ;
		break ;
	case 2: 				/*	push	simm8		*/
		push_word( fetch_signed_byte_data() ) ;
		break ;
	case 3: 				/*	imul  r16, r/m16, simm8 */
		parce_second_byte() ;
		fetch_word_EA_data() ;
		word_IMUL( latch_word, fetch_signed_byte_data() ) ;
		*decode_reg16( reg ) = (WORD) stemp32 ;
		break ;
	case 4: 	insb() ;	break ;
	case 5: 	insw() ;	break ;
	case 6: 	outsb() ;	break ;
	case 7: 	outsw() ;	break ;
	}
}

static void
pm_group1( void )
{
parce_second_byte() ;
switch( reg ){
	default:
		illegal() ;
		break ;
	case 0: 			/*	sldt	r/m16	*/
		latch_word = _sldt() ;
		store_word_EA_data() ;
		break ;
	case 1: 			/*	str	r/m16	*/
		latch_word = _str() ;
		store_word_EA_data() ;
		break ;
	case 2: 			/*	lldt	r/m16	*/
		fetch_word_EA_data() ;
		_lldt( latch_word ) ;
		break ;
	case 3: 			/*	ltr	r/m16	*/
		fetch_word_EA_data() ;
		_ltr( latch_word ) ;
		break ;
	case 4: 			/*	verr	r/m16	*/
		fetch_word_EA_data() ;
		_verr( latch_word ) ;
		break ;
	case 5: 			/*	verw	r/m16	*/
		fetch_word_EA_data() ;
		_verw( latch_word ) ;
		break ;
	}
}

static void
pm_group2( void )
{
parce_second_byte() ;
switch( reg ){
	default:
		illegal() ;
		break ;
	case 0: 			/*	sgdt	mem64	*/
		if( mod == 3 ) illegal() ;
		store_idt_gdt( &gdt ) ;
		break ;
	case 1: 			/*	sidt	mem64	*/
		if( mod == 3 ) illegal() ;
		store_idt_gdt( &idt ) ;
		break ;
	case 2: 			/*	lgdt	mem64	*/
		if( mod == 3 ) illegal() ;
		load_idt_gdt( &gdt ) ;
		break ;
	case 3: 			/*	lidt	mem64	*/
		if( mod == 3 ) illegal() ;
		load_idt_gdt( &idt ) ;
		break ;
	case 4: 			/*	smsw	r/m16	*/
		latch_word = _smsw() ;
		store_word_EA_data() ;
		break ;
	case 6: 			/*	lmsw	r/m16	*/
		fetch_word_EA_data() ;
		_lmsw( latch_word ) ;
		break ;
	}
}

static void
pm_instructions_decode( void )
{
switch( fetch_byte_data() ){
	default:	illegal() ;	break ;
	case 0x00:	pm_group1() ;	break ;
	case 0x01:	pm_group2() ;	break ;
	case 0x02: /*  lar reg16, r/m16  */
		parce_second_byte() ;
		fetch_word_EA_data() ;
		*decode_reg16( reg ) = _lar( latch_word ) ;
		break ;
	case 0x03: /*  lsl reg16, r/m16  */
		parce_second_byte() ;
		fetch_word_EA_data() ;
		*decode_reg16( reg ) = _lsl( latch_word ) ;
		break ;
	case 0x04:	shutdown() ;	break ;
	case 0x05:	loadall() ;	break ;
	case 0x06:	_clts() ;	 break ;
	}
}


static void
jmp_group1( void )
{
latch_word = fetch_signed_byte_data() ;
switch( fetched_byte ){
	case 0: 				/* jo		*/
		if( !( flags & FL_OF ) ) return ;
		break ;
	case 1: 				/* jno		*/
		if( flags & FL_OF ) return ;
		break ;
	case 2: 				/* jc		*/
		if( !( flags & FL_CF ) ) return ;
		break ;
	case 3: 				/* jnc		*/
		if( flags & FL_CF ) return ;
		break ;
	case 4: 				/* jz		*/
		if( !( flags & FL_ZF ) ) return ;
		break ;
	case 5: 				/* jnz		*/
		if( flags & FL_ZF ) return ;
		break ;
	case 6: 				/* jbe		*/
		if( !( flags & (FL_ZF|FL_CF) ) ) return ;
		break ;
	case 7: 				/* ja		*/
		if( flags & (FL_ZF|FL_CF) ) return ;
		break ;
	}
ip += latch_word ;
restart_prefetch() ;
}

static void
jmp_group2( void )
{
latch_word = fetch_signed_byte_data() ;
switch( fetched_byte ){
	case 0: 				/* js		*/
		if( !( flags & FL_SF ) ) return ;
		break ;
	case 1: 				/* jns		*/
		if( flags & FL_SF ) return ;
		break ;
	case 2: 				/* jp		*/
		if( !( flags & FL_PF ) ) return ;
		break ;
	case 3: 				/* jnp		*/
		if( flags & FL_PF ) return ;
		break ;
	case 6: 				/* jle		*/
						/* ZY || SF!=OF */
		if( flags & FL_ZF ) break ;
	case 4: 				/* jl (SF!=OF)	*/
		if( ( flags & (FL_SF|FL_OF) ) == 0 ||
		    ( flags & (FL_SF|FL_OF) ) == (FL_SF|FL_OF) )
			return ;
		break ;
	case 7: 				/* jg		*/
						/* ZN && SF==OF */
		if( flags & FL_ZF ) return ;
	case 5: 				/* jge (SF==OF) */
		if( ( flags & (FL_SF|FL_OF) ) == FL_SF ||
		    ( flags & (FL_SF|FL_OF) ) == FL_OF )
			return ;
		break ;
	}
ip += latch_word ;
restart_prefetch() ;
}

static	WORD FASTCALL (*(word_group1_table[]))( WORD dst, WORD src ) = {
	word_ADD,	word_OR,	word_ADC,	word_SBB,
	word_AND,	word_SUB,	word_XOR,	word_SUB
	} ;

static	BYTE FASTCALL (*(byte_group1_table[]))( BYTE dst, BYTE src ) = {
	byte_ADD,	byte_OR,	byte_ADC,	byte_SBB,
	byte_AND,	byte_SUB,	byte_XOR,	byte_SUB
	} ;

static void
word_group1_ops( void )
{
fetch_word_EA_data() ;
latch_word = (*(word_group1_table[ reg ]))( latch_word, fetch_word_data() ) ;
if( reg != 7 ) store_word_EA_data() ;		/* not cmp  r/m, immed	*/
}

static void
byte_group1_ops( void )
{
fetch_byte_EA_data() ;
latch_byte = (*(byte_group1_table[ reg ]))( latch_byte, fetch_byte_data() ) ;
if( reg != 7 ) store_byte_EA_data() ;		/* not cmp  r/m, immed	*/
}

static void
sbyte_group1_ops( void )
{
fetch_word_EA_data() ;
latch_word = (*(word_group1_table[ reg ]))( latch_word, fetch_signed_byte_data() ) ;
if( reg != 7 ) store_word_EA_data() ;		/* not cmp  r/m, immed	*/
}

/*	page	4.991	*/

static void
xchg_reg_rm( void )
{
if( fetched_byte & 1 ){ 			/* 16-bit		*/
	fetch_word_EA_data() ;
	latch_word_1 = latch_word ;
	latch_word   = *( src_reg16 = decode_reg16( reg ) ) ;
	store_word_EA_data() ;
	*src_reg16   = latch_word_1 ;
	}
else {						/*  8-bit		*/
	fetch_byte_EA_data() ;
	latch_byte_1 = latch_byte ;
	latch_byte   = *( src_reg8 = decode_reg8( reg ) ) ;
	store_byte_EA_data() ;
	*src_reg8    = latch_byte_1 ;
	}
}

static void
test_reg_rm( void )
{
if( fetched_byte & 1 ){ 			/* 16-bit		*/
	fetch_word_EA_data() ;
	word_AND( *decode_reg16( reg ), latch_word ) ;
	}
else {						/*  8-bit		*/
	fetch_byte_EA_data() ;
	byte_AND( *decode_reg8( reg ), latch_byte ) ;
	}
}

/*	page	5	*/

static void
group1_test_xchg( void )
{
parce_second_byte() ;
switch( fetched_byte ){
	case 0: 				/*	???	byte r/m, imm8	*/
	case 2: 				/*	w = 0, s = 0/1		*/
		byte_group1_ops() ;
		break ;
	case 1: 				/*	???	word r/m, imm16 */
		word_group1_ops() ;
		break ;
	case 3: 				/*	???	word r/m, imm8	*/
		sbyte_group1_ops() ;
		break ;
	case 4: 				/*	test	reg, r/m	*/
	case 5:
		test_reg_rm() ;
		break ;
	case 6: 				/*	xchg	reg, r/m	*/
	case 7:
		xchg_reg_rm() ;
		break ;
	}
}

/*	page	6	*/

static void
mov_seg_pop( void )
{
parce_second_byte() ;
switch( fetched_byte ){
	case 0: 				/*	mov	r/m, reg8	*/
		latch_byte = *decode_reg8( reg ) ;
		store_byte_EA_data() ;
		break ;
	case 1: 				/*	mov	r/m, reg16	*/
		latch_word = *decode_reg16( reg ) ;
		store_word_EA_data() ;
		break ;
	case 2: 				/*	mov	reg8, r/m	*/
		fetch_byte_EA_data() ;
		*decode_reg8( reg ) = latch_byte ;
		break ;
	case 3: 				/*	mov	reg16, r/m	*/
		fetch_word_EA_data() ;
		*decode_reg16( reg ) = latch_word ;
		break ;
	case 4: 				/*	mov	r/m, seg	*/
		if( reg > 3 )
			CPU_error( "Invalid segment register code !" ) ;
		latch_word = *decode_segreg( reg ) ;
		store_word_EA_data() ;
		break ;
	case 5: 				/*	lea	reg, mem	*/
		if( mod == 3 )
			illegal() ;
		*decode_reg16( reg ) = EA ;
		break ;
	case 6: 				/*	mov	reg, r/m	*/
		if( reg > 3 )
			CPU_error( "Invalid segment register code !" ) ;
		if( reg == 1 && CPU_type >= CPU_286 )
			illegal() ;		/*	mov	cs, r/m 	*/
		fetch_word_EA_data() ;
		*decode_segreg( reg ) = latch_word ;
		break ;
	case 7: 				/*	pop	r/m		*/
		latch_word = pop_word() ;
		store_word_EA_data() ;
		break ;
	}
}

/*	page	7	*/

static void
xchg_ax_reg( void )
{
latch_word = *( src_reg16  = decode_reg16( fetched_byte ) ) ;
*src_reg16 = ax ;
ax	   = latch_word ;
}

static void
call_far( void )
{
push_word( cs ) ;
push_word( ip ) ;
cs = call_cs ;
ip = call_ip ;
restart_prefetch() ;
}

static void
call_near( void )
{
push_word( ip ) ;
ip = call_ip ;
restart_prefetch() ;
}

static void
call_interrupt( void )
{
if( CPU_type >= CPU_286 && latch_word + 4 > idt.limit )
	CPU_error( "Interrupt number outside IDT space" ) ;
push_word( flags ) ;
push_word( cs ) ;
push_word( ip ) ;
flags &= ~(FL_IF|FL_TF) ;
temp32 = (CPU_type >= CPU_286 ? idt.base : 0) + latch_word ;
ip = fetch_word( (WORD)(temp32 >> 4), (WORD)(temp32 & 0xF) + 0 ) ;
cs = fetch_word( (WORD)(temp32 >> 4), (WORD)(temp32 & 0xF) + 2 ) ;
restart_prefetch() ;
}

static void
scnv_flagsops( void )
{
switch( fetched_byte ){
	case 0: 				/* cbw				*/
		if( al & 0x80 ) ah = 0xff ; else ah = 0 ;
		break ;
	case 1: 				/* cwd				*/
		if( ax & 0x8000 ) dx = 0xffff ; else dx = 0 ;
		break ;
	case 2: 				/* call far immediate		*/
		call_ip = fetch_word_data() ;
		call_cs = fetch_word_data() ;
		call_far() ;
		break ;
	case 3: 				/* wait - ignore ?		*/
		_wait() ;
		break ;
	case 4: 				/* pushf			*/
		push_word( flags ) ;
		break ;
	case 5: 				/* popf 			*/
		flags = ( pop_word() | FL_RESET ) & FL_SET ;
		resume_flag = 1 ;
		break ;
	case 6: 				/* sahf 			*/
		*LO_BYTE( &flags ) = ah ;
		flags = ( flags | FL_RESET ) & FL_SET ;
		break ;
	case 7: 				/* lahf 			*/
		ah = *LO_BYTE( &flags ) ;
		break ;
	}
}

/*	page	7.99	*/

static void
movsb( void )
{
set_segment( &ds ) ;
store_byte( es, di, fetch_byte( *seg_override, si ) ) ;
if( flags & FL_DF ){ si-- ; di-- ; } else { si++, di++ ; }
}

static void
movsw( void )
{
set_segment( &ds ) ;
store_word( es, di, fetch_word( *seg_override, si ) ) ;
if( flags & FL_DF ){ si -= 2 ; di -= 2 ; } else { si += 2 ; di += 2 ; }
}

static void
cmpsb( void )
{
set_segment( &ds ) ;
byte_SUB( fetch_byte( es, di ), fetch_byte( *seg_override, si ) ) ;
if( flags & FL_DF ){ si-- ; di-- ; } else { si++, di++ ; }
}

static void
cmpsw( void )
{
set_segment( &ds ) ;
word_SUB( fetch_word( es, di ), fetch_word( *seg_override, si ) ) ;
if( flags & FL_DF ){ si -= 2 ; di -= 2 ; } else { si += 2 ; di += 2 ; }
}

static void
stosb( void )
{
store_byte( es, di, al ) ;
if( flags & FL_DF ) di-- ; else di++ ;
}

static void
stosw( void )
{
store_word( es, di, ax ) ;
if( flags & FL_DF ) di -= 2 ; else di += 2 ;
}

static void
lodsb( void )
{
set_segment( &ds ) ;
al = fetch_byte( *seg_override, si ) ;
if( flags & FL_DF ) si-- ; else si++ ;
}

static void
lodsw( void )
{
set_segment( &ds ) ;
ax = fetch_word( *seg_override, si ) ;
if( flags & FL_DF ) si -= 2 ; else si += 2 ;
}

static void
scasb( void )
{
byte_SUB( al, fetch_byte( es, di ) ) ;
if( flags & FL_DF ) di-- ; else di++ ;
}

static void
scasw( void )
{
word_SUB( ax, fetch_word( es, di ) ) ;
if( flags & FL_DF ) di -= 2 ; else di += 2 ;
}

static void
insb( void )
{
latch_byte = byte_IN( dx ) ;
store_byte( es, di, latch_byte ) ;
if( flags & FL_DF ) di-- ; else di++ ;
}

static void
insw( void )
{
latch_word = word_IN( dx ) ;
store_word( es, di, latch_word ) ;
if( flags & FL_DF ) di -= 2 ; else di += 2 ;
}

static void
outsb( void )
{
set_segment( &ds ) ;
byte_OUT( dx, fetch_byte( *seg_override, si ) ) ;
if( flags & FL_DF ) si-- ; else si++ ;
}

static void
outsw( void )
{
set_segment( &ds ) ;
word_OUT( dx, fetch_word( *seg_override, si ) ) ;
if( flags & FL_DF ) si -= 2 ; else si += 2 ;
}

/*	page	8	*/

static void
mov_strops( void )
{
if( fetched_byte < 4 )
	parce_immediate_addr() ;
switch( fetched_byte ){
/*
 *	In this instruction d-bit value is reversed.
 */
	case 2: 				/*	mov	mem, al 	*/
		latch_byte = al ;
		store_byte_EA_data() ;
		break ;
	case 3: 				/*	mov	mem, ax 	*/
		latch_word = ax ;
		store_word_EA_data() ;
		break ;
	case 0: 				/*	mov	al, mem 	*/
		fetch_byte_EA_data() ;
		al = latch_byte ;
		break ;
	case 1: 				/*	mov	ax, mem 	*/
		fetch_word_EA_data() ;
		ax = latch_word ;
		break ;
	case 4: movsb() ; break ;
	case 5: movsw() ; break ;
	case 6: cmpsb() ; break ;
	case 7: cmpsw() ; break ;
	}
}

/*	page	9	*/

static void
test_strops( void )
{
switch( fetched_byte ){
	case 0: 				/*	test	al, immed	*/
		byte_AND( al, fetch_byte_data() ) ;
		break ;
	case 1: 				/*	test	ax, immed	*/
		word_AND( ax, fetch_word_data() ) ;
		break ;
	case 2: stosb() ; break ;
	case 3: stosw() ; break ;
	case 4: lodsb() ; break ;
	case 5: lodsw() ; break ;
	case 6: scasb() ; break ;
	case 7: scasw() ; break ;
	}
}

static void
mov_reg8_immed( void )
{
*decode_reg8( fetched_byte ) = fetch_byte_data() ;
}

static void
mov_reg16_immed( void )
{
*decode_reg16( fetched_byte ) = fetch_word_data() ;
}

/*	page	10	*/

static void
ret_lxs_mov( void )
{
	void	byte_shifts(void), word_shifts(void) ;

switch( fetched_byte ){
	case 0: 				/*	byte shifts by constant */
		if( CPU_type < CPU_186 ){ illegal() ; return ; }
		parce_second_byte() ;
		shift_count = fetch_byte_data() & shift_mask ;
		byte_shifts() ;
		break ;
	case 1: 				/*	word shifts by constant */
		if( CPU_type < CPU_186 ){ illegal() ; return ; }
		parce_second_byte() ;
		shift_count = fetch_byte_data() & shift_mask ;
		word_shifts() ;
		break ;
	case 2: 				/*	retn	nn		*/
		latch_word = fetch_word_data() ;
		ip = pop_word() ;
		sp += latch_word ;
		restart_prefetch() ;
		break ;
	case 3: 				/*	retn			*/
		ip = pop_word() ;
		restart_prefetch() ;
		break ;
	case 4: 				/*	les	reg, mem	*/
		parce_second_byte() ;
		if( mod == 3 ) illegal() ;
		*decode_reg16( reg ) = fetch_word( *seg_override, EA ) ;
		es = fetch_word( *seg_override, EA + 2 ) ;
		break ;
	case 5: 				/*	lds	reg, mem	*/
		parce_second_byte() ;
		if( mod == 3 ) illegal() ;
		*decode_reg16( reg ) = fetch_word( *seg_override, EA ) ;
		ds = fetch_word( *seg_override, EA + 2 ) ;
		break ;
	case 6: 				/*	mov	r/m8, immed	*/
		parce_second_byte() ;
		if( reg != 0 )
			illegal() ;
		latch_byte = fetch_byte_data() ;
		store_byte_EA_data() ;
		break ;
	case 7: 				/*	mov	r/m16, immed	*/
		parce_second_byte() ;
		if( reg != 0 )
			illegal() ;
		latch_word = fetch_word_data() ;
		store_word_EA_data() ;
		break ;
	}
}

/*	page	11	*/

static void
make_stack_frame( WORD local, WORD level )
{
level &= 31 ;
push_word( bp ) ;
latch_word_1 = sp ;
if( level > 0 ){
	for( level-- ; level > 0 ; level-- ){
		push_word( fetch_word( ss, bp -= 2 ) ) ;
		}
	push_word( latch_word_1 ) ;
	}
bp  = latch_word_1 ;
sp -= local ;
}

static void
retf_int( void )
{
switch( fetched_byte ){
	case 0: 				/*	enter	i16, i8 	*/
		if( CPU_type < CPU_186 ){ illegal() ; return ; }
		latch_word = fetch_word_data() ;
		latch_byte = fetch_byte_data() ;
		make_stack_frame( latch_word, latch_byte ) ;
		break ;
	case 1: 				/*	leave			*/
		if( CPU_type < CPU_186 ){ illegal() ; return ; }
		sp = bp ;
		bp = pop_word() ;
		break ;
	case 2: 				/*	retf	nn		*/
		latch_word = fetch_word_data() ;
		ip	   = pop_word() ;
		cs	   = pop_word() ;
		sp	  += latch_word ;
		restart_prefetch() ;
		break ;
	case 3: 				/*	retf			*/
		ip	   = pop_word() ;
		cs	   = pop_word() ;
		restart_prefetch() ;
		break ;
	case 4: 				/*	int	3		*/
		latch_word = 3 * 4 ;
		call_interrupt() ;
		break ;
	case 5: 				/*	int	nn		*/
		latch_word = (WORD)fetch_byte_data() * 4 ;
		call_interrupt() ;
		break ;
	case 6: 				/*	into			*/
		if( flags & FL_OF ){
			latch_word = 4 * 4 ;
			call_interrupt() ;
			}
		break ;
	case 7: 				/*	iret			*/
		resume_flag = 1 ;
		ip    = pop_word() ;
		cs    = pop_word() ;
		flags = ( pop_word() | FL_RESET ) & FL_SET ;
		restart_prefetch() ;
		break ;
	}
}

/*	page	11.99	*/

static void
xlat( void )
{
set_segment( &ds ) ;
al = fetch_byte( *seg_override, bx + al ) ;
}

static WORD FASTCALL
word_bad_shift( WORD x )
{
illegal() ;
return x ;
}

static BYTE FASTCALL
byte_bad_shift( BYTE x )
{
illegal() ;
return x ;
}

static	WORD FASTCALL	(*(word_shifts_table[]))( WORD src ) = {
	word_ROL,	word_ROR,	word_RCL,	word_RCR,
	word_SHL,	word_SHR,	word_bad_shift, word_SAR
	} ;

static	BYTE FASTCALL	(*(byte_shifts_table[]))( BYTE src ) = {
	byte_ROL,	byte_ROR,	byte_RCL,	byte_RCR,
	byte_SHL,	byte_SHR,	byte_bad_shift, byte_SAR
	} ;

static void
word_shifts( void )
{
fetch_word_EA_data() ;
latch_word = (*(word_shifts_table[reg]))( latch_word ) ;
store_word_EA_data() ;
}

static void
byte_shifts( void )
{
fetch_byte_EA_data() ;
latch_byte = (*(byte_shifts_table[reg]))( latch_byte ) ;
store_byte_EA_data() ;
}

/*	page	12	*/

static void
shifts_xlat( void )
{
if( fetched_byte < 4 ){
	if( fetched_byte & 2 )
		shift_count = cl & shift_mask ;
	else	shift_count = 1 ;
	parce_second_byte() ;
	if( fetched_byte & 1 )
		word_shifts() ;
	else	byte_shifts() ;
	}
else
switch( fetched_byte ){
	case 4: _aam( fetch_byte_data() ) ;	 break ;
	case 5: _aad( fetch_byte_data() ) ;	 break ;
	case 6: cf2al() ;			break ;
	case 7: xlat() ;			break ;
	}
}

static void
check_coprocessor_permitted( void )
{
if( CPU_type >= CPU_286 ){
	if( ( msw & MSW_EM ) || ( msw & (MSW_MP|MSW_TS) ) == (MSW_MP|MSW_TS) ){
		cs = instruction_cs ;
		ip = instruction_ip ;
		latch_word = 7 * 4 ;
		call_interrupt() ;
		}
	}
}

static void
extension_escape( void )			/* this is almost nop...*/
{
check_coprocessor_permitted() ;
parce_second_byte() ;
}

/*	page	13	*/

static void
loops_io( void )
{
if( fetched_byte < 4 ){ 			/* loop??, jcxz 	*/
	latch_word = fetch_signed_byte_data() ;
	switch( fetched_byte ){
		case 0: 			/* loopnz		*/
			if( --cx != 0 && ! (flags & FL_ZF) ){
				ip += latch_word ;
				restart_prefetch() ;
				}
			break ;
		case 1: 			/* loopz		*/
			if( --cx != 0 &&    flags & FL_ZF  ){
				ip += latch_word ;
				restart_prefetch() ;
				}
			break ;
		case 2: 			/* loop 		*/
			if( --cx != 0 ){
				ip += latch_word ;
				restart_prefetch() ;
				}
			break ;
		case 3: 			/* jcxz 		*/
			if( cx == 0 ){
				ip += latch_word ;
				restart_prefetch() ;
				}
			break ;
		}
	}
else {
	latch_word = (WORD) fetch_byte_data() ;
	switch( fetched_byte ){
		case 4: 			/* in	al, [imm]	*/
			al = byte_IN( latch_word ) ;
			break ;
		case 5: 			/* in	ax, [imm]	*/
			ax = word_IN( latch_word ) ;
			break ;
		case 6: 			/* out	[imm], al	*/
			byte_OUT( latch_word, al ) ;
			break ;
		case 7: 			/* out	[imm], ax	*/
			word_OUT( latch_word, ax ) ;
			break ;
		}
	}
}

/*	page	14	*/

static void
call_jmp_io( void )
{
switch( fetched_byte ){
	case 0: 				/* call near imm	*/
		call_ip    = fetch_word_data() ;
		call_ip   += ip ;
		call_near() ;
		break ;
	case 1: 				/* jmp	near imm	*/
		ip	  += fetch_word_data() ;
		restart_prefetch() ;
		break ;
	case 2: 				/* jmp	far  imm	*/
		latch_word = fetch_word_data() ;
		cs	   = fetch_word_data() ;
		ip	   = latch_word ;
		restart_prefetch() ;
		break ;
	case 3: 				/* jmp	short		*/
		ip	  += fetch_signed_byte_data() ;
		restart_prefetch() ;
		break ;
	case 4: 				/* in	al, dx		*/
		al	   = byte_IN( dx ) ;
		break ;
	case 5: 				/* in	ax, dx		*/
		ax	   = word_IN( dx ) ;
		break ;
	case 6: 				/* out	dx, al		*/
		byte_OUT( dx, al ) ;
		break ;
	case 7: 				/* out	dx, ax		*/
		word_OUT( dx, ax ) ;
		break ;
	}
}

static void
skip_rep_instruction( void )
{
while( 1 ){
switch( latch_byte = fetch_byte_data() ){
	case 0xA4:
	case 0xA5:
	case 0xAA:
	case 0xAB:
	case 0xAC:
	case 0xAD:
	case 0xA6:
	case 0xA7:
	case 0xAE:
	case 0xAF:  break ;
	case 0x2e:
	case 0x3e:
	case 0x26:
	case 0x36:
	case 0xF0:  continue ;
	default:
		if( CPU_type >= CPU_186 )
			switch( latch_byte ){
	case 0x6C:
	case 0x6D:
	case 0x6E:
	case 0x6F:  break ;
	default:    CPU_error( "REP/REPNZ prefix invalid for instruction !" ) ;
				}
		else CPU_error( "REP/REPNZ prefix invalid for instruction !" ) ;
		break ;
	}
    break ;
    }
}

static void
repnz( void )
{
#define check_rep()	\
		{if(--cx!=0){ip=instruction_ip;backspace_prefetch();}}
#define check_repnz()	\
		{if(--cx!=0&&!(flags&FL_ZF)){ip=instruction_ip;backspace_prefetch();}}
#define check_repz()	\
		{if(--cx!=0&&(flags&FL_ZF)){ip=instruction_ip;backspace_prefetch();}}
/*
 *	Meaning if repnz depends upon following instruction :
 *	rep with movs, stos, lods; repnz with cmps and scas
 *	repnz can be used with other instructions, but this
 *	will cause emulator error.
 */
if( cx == 0 ){
	skip_rep_instruction() ; return ;
	}
while( 1 ){
switch( latch_byte = fetch_byte_data() ){
	case 0xA4:  movsb() ; check_rep() ;   break ;
	case 0xA5:  movsw() ; check_rep() ;   break ;
	case 0xAA:  stosb() ; check_rep() ;   break ;
	case 0xAB:  stosw() ; check_rep() ;   break ;
	case 0xAC:  lodsb() ; cx-- ;	      break ;
	case 0xAD:  lodsw() ; cx-- ;	      break ;
	case 0xA6:  cmpsb() ; check_repnz() ; break ;
	case 0xA7:  cmpsw() ; check_repnz() ; break ;
	case 0xAE:  scasb() ; check_repnz() ; break ;
	case 0xAF:  scasw() ; check_repnz() ; break ;
	case 0x2e:  seg_override = &cs ;      continue ;
	case 0x3e:  seg_override = &ds ;      continue ;
	case 0x26:  seg_override = &es ;      continue ;
	case 0x36:  seg_override = &ss ;      continue ;
	case 0xF0:  /* LOCK */		      continue ;
	default:
		if( CPU_type >= CPU_186 )
			switch( latch_byte ){
	case 0x6C:  insb() ;  check_rep() ;   break ;
	case 0x6D:  insw() ;  check_rep() ;   break ;
	case 0x6E:  outsb() ; check_rep() ;   break ;
	case 0x6F:  outsw() ; check_rep() ;   break ;
	default:    CPU_error( "REP/REPNZ prefix invalid for instruction !" ) ;
				}
		else CPU_error( "REP/REPNZ prefix invalid for instruction !" ) ;
		break ;
	}
    break ;
    }
}

static void
repz( void )
{
if( cx == 0 ){
	skip_rep_instruction() ; return ;
	}
while( 1 ){
switch( latch_byte = fetch_byte_data() ){
	case 0xA4:  movsb() ; check_rep() ;   break ;
	case 0xA5:  movsw() ; check_rep() ;   break ;
	case 0xAA:  stosb() ; check_rep() ;   break ;
	case 0xAB:  stosw() ; check_rep() ;   break ;
	case 0xAC:  lodsb() ; cx-- ;	      break ;
	case 0xAD:  lodsw() ; cx-- ;	      break ;
	case 0xA6:  cmpsb() ; check_repz() ;  break ;
	case 0xA7:  cmpsw() ; check_repz() ;  break ;
	case 0xAE:  scasb() ; check_repz() ;  break ;
	case 0xAF:  scasw() ; check_repz() ;  break ;
	case 0x2e:  seg_override = &cs ;      continue ;
	case 0x3e:  seg_override = &ds ;      continue ;
	case 0x26:  seg_override = &es ;      continue ;
	case 0x36:  seg_override = &ss ;      continue ;
	case 0xF0:  /* LOCK */		      continue ;
	default:
		if( CPU_type >= CPU_186 )
			switch( latch_byte ){
	case 0x6C:  insb() ;  check_rep() ;   break ;
	case 0x6D:  insw() ;  check_rep() ;   break ;
	case 0x6E:  outsb() ; check_rep() ;   break ;
	case 0x6F:  outsw() ; check_rep() ;   break ;
	default:    CPU_error( "REP/REPNZ prefix invalid for instruction !" ) ;
				}
		else CPU_error( "REP/REPNZ prefix invalid for instruction !" ) ;
		break ;
	}
    break ;
    }
	#undef	check_rep
	#undef	check_repz
	#undef	check_repnz
}

static void
byte_group2_ops( void )
{
parce_second_byte() ;
fetch_byte_EA_data() ;
switch( reg ){
	case 0: 				/*   test   r/m8, imm	*/
		byte_AND( latch_byte, fetch_byte_data() ) ;
		break ;
	case 1:
		illegal() ;
		break ;
	case 2: 				/*   not    r/m8	*/
		latch_byte = byte_NOT( latch_byte ) ;
		store_byte_EA_data() ;
		break ;
	case 3: 				/*   neg    r/m8	*/
		latch_byte = byte_NEG( latch_byte ) ;
		store_byte_EA_data() ;
		break ;
	case 4: 				/*   mul    r/m8	*/
		byte_MUL( latch_byte ) ;
		break ;
	case 5: 				/*   imul   r/m8	*/
		byte_IMUL( latch_byte ) ;
		break ;
	case 6:
		byte_DIV( latch_byte ) ;	/*   div    r/m8	*/
		break ;
	case 7:
		byte_IDIV( latch_byte ) ;	/*   idiv   r/m8	*/
		break ;
	}
}

static void
word_group2_ops( void )
{
parce_second_byte() ;
fetch_word_EA_data() ;
switch( reg ){
	case 0: 				/*   test   r/m16, imm	*/
		word_AND( latch_word, fetch_word_data() ) ;
		break ;
	case 1:
		illegal() ;
		break ;
	case 2: 				/*   not    r/m16	*/
		latch_word = word_NOT( latch_word ) ;
		store_word_EA_data() ;
		break ;
	case 3: 				/*   neg    r/m16	*/
		latch_word = word_NEG( latch_word ) ;
		store_word_EA_data() ;
		break ;
	case 4: 				/*   mul    r/m16	*/
		word_MUL( latch_word ) ;
		break ;
	case 5: 				/*   imul   r/m16	*/
		word_IMUL( ax, latch_word ) ;
		ax = (WORD)  stemp32 ;
		dx = (WORD)( stemp32 >> 16 ) ;
		break ;
	case 6:
		word_DIV( latch_word ) ;	/*   div    r/m16	*/
		break ;
	case 7:
		word_IDIV( latch_word ) ;	/*   idiv   r/m16	*/
		break ;
	}
}

static void
reps_group2( void )
{
switch( fetched_byte ){
	case 0:
	case 1:
		illegal() ;
		break ;
	case 2: 	repnz() ;	break ;
	case 3: 	repz() ;	break ;
	case 4: 				/*	hlt		*/
		halt_condition = 1 ;
		break ;
	case 5: 				/*	cmc		*/
		flags ^= FL_CF ;
		break ;
	case 6: 				/*	xxx  byte r/m	*/
		byte_group2_ops() ;
		break ;
	case 7: 				/*	xxx  word r/m	*/
		word_group2_ops() ;
		break ;
	}
}

static	void
byte_group3_ops( void )
{
parce_second_byte() ;
fetch_byte_EA_data() ;
switch( reg ){
	case 0:
		latch_byte = byte_INC( latch_byte ) ;
		break ;
	case 1:
		latch_byte = byte_DEC( latch_byte ) ;
		break ;
	default:
		illegal() ;
		break ;
	}
store_byte_EA_data() ;
}

static	void
word_group3_ops( void )
{
parce_second_byte() ;
fetch_word_EA_data() ;
switch( reg ){
	case 0:
		latch_word = word_INC( latch_word ) ;
		store_word_EA_data() ;
		break ;
	case 1:
		latch_word = word_DEC( latch_word ) ;
		store_word_EA_data() ;
		break ;
	case 2: 			/*	call near []	*/
		call_ip = latch_word ;
		call_near() ;
		break ;
	case 3: 			/*	call far []	*/
		if( mod == 3 ) illegal() ;
		else {
			call_ip = latch_word ;
			call_cs = fetch_word( *seg_override, EA + 2 ) ;
			call_far() ;
			}
		break ;
	case 4: 			/*	jmp  near []	*/
		ip	= latch_word ;
		restart_prefetch() ;
		break ;
	case 5: 			/*	jmp  far []	*/
		if( mod == 3 ) illegal() ;
		else {
			cs	= fetch_word( *seg_override, EA + 2 ) ;
			ip	= latch_word ;
			restart_prefetch() ;
			}
		break ;
	case 6: 			/*	push word mem	*/
		push_word( latch_word ) ;
		break ;
	default:
		illegal() ;
		break ;
	}
}

static	void
flags_group3( void )
{
switch( fetched_byte ){
	case 0: 				/*	clc		*/
		flags &= ~FL_CF ;	break ;
	case 1: 				/*	stc		*/
		flags |= FL_CF ;	break ;
	case 2: 				/*	cli		*/
		flags &= ~FL_IF ;	break ;
	case 3: 				/*	sti		*/
		flags |= FL_IF ;	break ;
	case 4: 				/*	cld		*/
		flags &= ~FL_DF ;	break ;
	case 5: 				/*	std		*/
		flags |= FL_DF ;	break ;
	case 6:
		byte_group3_ops() ;	break ;
	case 7:
		word_group3_ops() ;	break ;
	}
}

/*
 *	Emulator arifmetic engine
 */
/*
 *	parity_table[x] = 1 if x has even number of 1s. (PF=PE)
 */
static	char	parity_table[256] = {
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0,
	1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1,
	0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1,
	0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0,
	0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1,
	0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1
	} ;

#define RAISE_OF()	(flags|= FL_OF)
#define CLEAR_OF()	(flags&=~FL_OF)
#define RAISE_DF()	(flags|= FL_DF)
#define CLEAR_DF()	(flags&=~FL_DF)
#define RAISE_SF()	(flags|= FL_SF)
#define CLEAR_SF()	(flags&=~FL_SF)
#define RAISE_ZF()	(flags|= FL_ZF)
#define CLEAR_ZF()	(flags&=~FL_ZF)
#define RAISE_AF()	(flags|= FL_AF)
#define CLEAR_AF()	(flags&=~FL_AF)
#define RAISE_PF()	(flags|= FL_PF)
#define CLEAR_PF()	(flags&=~FL_PF)
#define RAISE_CF()	(flags|= FL_CF)
#define CLEAR_CF()	(flags&=~FL_CF)

/*
 *	SET_PF(X)  - Set PF according to parity of byte X
 *	SET_ZF(X)  - Set ZF=ZY if X == 0
 *	SET_WSF(X) - Set SF=NE if bit 15 of word X = 1
 *	SET_BSF(X) - Set SF=NE if bit  7 of byte X = 1
 */
void	SET_PF(BYTE) ;
void	SET_WSF(WORD) ;
void	SET_BSF(BYTE) ;
#define SET_ZF(x)	if(x) CLEAR_ZF() ; else RAISE_ZF() ;
#define SET_PF(x)	if( parity_table[ (BYTE)(x) ] ) RAISE_PF() ; else CLEAR_PF() ;
#define SET_WSF(x)	if( (WORD)(x) & 0x8000u ) RAISE_SF() ; else CLEAR_SF() ;
#define SET_BSF(x)	if( (BYTE)(x) & 0x80u ) RAISE_SF() ; else CLEAR_SF() ;

static void FASTCALL
_aaa( void )
{
if( al > 9 ){
	ax += 0x0106 ;
	al &= 0x0F ;
	RAISE_CF() ; RAISE_AF() ;
	}
else {
	CLEAR_CF() ; CLEAR_AF() ;
	}
}

static void FASTCALL
_aad( SBYTE base )
{
ax = ah * base + al ;
SET_BSF( al ) ;
SET_ZF( ax ) ;
SET_PF( al ) ;
}

static void FASTCALL
_aam( SBYTE base )
{
ah  = al / base ;
al %= base ;
CLEAR_SF() ;
SET_ZF( ax ) ;
SET_PF( al ) ;
}

static void FASTCALL
_aas( void )
{
if( al > 9 ){
	ah-- ; al = ( al - 6 ) & 0xf ;
	RAISE_CF() ; RAISE_AF() ;
	}
else {
	CLEAR_CF() ; CLEAR_AF() ;
	}
}

static WORD FASTCALL
word_ADC( WORD dst, WORD src )
{
temp32 = (DWORD)dst + src ;
temp16 = ( dst & 0x7FFFu ) + ( src & 0x7FFFu ) ;
temp5  = ( dst & 0xFu ) + ( src & 0xFu ) ;
if( flags & FL_CF ){
	temp32++ ; temp16++ ; temp5++ ;
	}
if( temp32 & 0x10000ul ){
	RAISE_CF() ;
	if( temp16 & 0x8000u )
		CLEAR_OF() ;
	else	RAISE_OF() ;
	}
else {
	CLEAR_CF() ;
	if( temp16 & 0x8000u )
		RAISE_OF() ;
	else	CLEAR_OF() ;
	}
if( temp5 & 0x10u )
	RAISE_AF() ;
else	CLEAR_AF() ;
SET_WSF( (WORD)temp32 ) ;
SET_ZF(  (WORD)temp32 ) ;
SET_PF(  (BYTE)temp32 ) ;
return (WORD) temp32 ;
}

static BYTE FASTCALL
byte_ADC( BYTE dst, BYTE src )
{
temp16 = (WORD)dst + src ;
temp8  = ( dst & 0x7Fu ) + ( src & 0x7Fu ) ;
temp5  = ( dst & 0xFu ) + ( src & 0xFu ) ;
if( flags & FL_CF ){
	temp16++ ; temp8++ ; temp5++ ;
	}
if( temp16 & 0x100u ){
	RAISE_CF() ;
	if( temp8 & 0x80u )
		CLEAR_OF() ;
	else	RAISE_OF() ;
	}
else {
	CLEAR_CF() ;
	if( temp8 & 0x80u )
		RAISE_OF() ;
	else	CLEAR_OF() ;
	}
if( temp5 & 0x10u )
	RAISE_AF() ;
else	CLEAR_AF() ;
SET_BSF( (BYTE)temp16 ) ;
SET_ZF(  (BYTE)temp16 ) ;
SET_PF(  (BYTE)temp16 ) ;
return (BYTE) temp16 ;
}

static WORD FASTCALL
word_ADD( WORD dst, WORD src )
{
temp32 = (DWORD)dst + src ;
temp16 = ( dst & 0x7FFFu ) + ( src & 0x7FFFu ) ;
temp5  = ( dst & 0xFu ) + ( src & 0xFu ) ;
if( temp32 & 0x10000ul ){
	RAISE_CF() ;
	if( temp16 & 0x8000u )
		CLEAR_OF() ;
	else	RAISE_OF() ;
	}
else {
	CLEAR_CF() ;
	if( temp16 & 0x8000u )
		RAISE_OF() ;
	else	CLEAR_OF() ;
	}
if( temp5 & 0x10u )
	RAISE_AF() ;
else	CLEAR_AF() ;
SET_WSF( (WORD)temp32 ) ;
SET_ZF(  (WORD)temp32 ) ;
SET_PF(  (BYTE)temp32 ) ;
return (WORD) temp32 ;
}

static BYTE FASTCALL
byte_ADD( BYTE dst, BYTE src )
{
temp16 = (WORD)dst + src ;
temp8  = ( dst & 0x7Fu ) + ( src & 0x7Fu ) ;
temp5  = ( dst & 0xFu ) + ( src & 0xFu ) ;
if( temp16 & 0x100u ){
	RAISE_CF() ;
	if( temp8 & 0x80u )
		CLEAR_OF() ;
	else	RAISE_OF() ;
	}
else {
	CLEAR_CF() ;
	if( temp8 & 0x80u )
		RAISE_OF() ;
	else	CLEAR_OF() ;
	}
if( temp5 & 0x10u )
	RAISE_AF() ;
else	CLEAR_AF() ;
SET_BSF( (BYTE)temp16 ) ;
SET_ZF(  (BYTE)temp16 ) ;
SET_PF(  (BYTE)temp16 ) ;
return (BYTE) temp16 ;
}

static WORD FASTCALL
word_AND( WORD dst, WORD src )
{
CLEAR_CF() ; CLEAR_OF() ; CLEAR_AF() ;
dst &= src ;
SET_WSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static BYTE FASTCALL
byte_AND( BYTE dst, BYTE src )
{
CLEAR_CF() ; CLEAR_OF() ; CLEAR_AF() ;
dst &= src ;
SET_BSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static void FASTCALL
_daa( void )
{
temp8 = al ;
if( ( temp8 & 0xF ) > 9 || flags & FL_AF ){
	temp8 += 6 ; RAISE_AF() ;
	}
else	CLEAR_AF() ;
if( ( temp8 & 0xF0 ) > (9<<4) || flags & FL_CF ){
	temp8 += 6 << 4 ; RAISE_CF() ;
	}
else	CLEAR_CF() ;
if( (al^temp8) & 0x80 )
	RAISE_OF() ;
else	CLEAR_OF() ;
al = temp8 ;
SET_BSF( al ) ;
SET_ZF(  al ) ;
SET_PF(  al ) ;
}

static void FASTCALL
_das( void )
{
temp8 = al ;
if( ( temp8 & 0xF ) > 9 || flags & FL_AF ){
	temp8 -= 6 ; RAISE_AF() ;
	}
else	CLEAR_AF() ;
if( ( temp8 & 0xF0 ) > (9<<4) || flags & FL_CF ){
	temp8 -= 6 << 4 ; RAISE_CF() ;
	}
else	CLEAR_CF() ;
if( (al^temp8) & 0x80 )
	RAISE_OF() ;
else	CLEAR_OF() ;
al = temp8 ;
SET_BSF( al ) ;
SET_ZF(  al ) ;
SET_PF(  al ) ;
}

static WORD FASTCALL
word_DEC( WORD dst )
{
if( --dst == 0x7FFFu )
	RAISE_OF() ;
else	CLEAR_OF() ;
if( ( dst & 0xF ) == 0xF )
	RAISE_AF() ;
else	CLEAR_AF() ;
SET_WSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static BYTE FASTCALL
byte_DEC( BYTE dst )
{
if( --dst == 0x7F )
	RAISE_OF() ;
else	CLEAR_OF() ;
if( ( dst & 0xF ) == 0xF )
	RAISE_AF() ;
else	CLEAR_AF() ;
SET_BSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static void FASTCALL
word_DIV( WORD divisor )
{
/*
 *	Largest possible quotient is 0xFFFF = 65535, largest remainder is
 *	divisor - 1, so largest dividend for this instruction is
 *	divisor * 65535u + divisor - 1 = 65536u * divisor - 1
 */
temp32 = ( (DWORD)dx << 16 ) | ax ;
if( (DWORD) divisor * 65536ul - 1 < temp32 ){
	latch_word = 0 ;
	if( CPU_type >= CPU_286 ){
		ip = instruction_ip ;
		cs = instruction_cs ;
		}
	call_interrupt() ;
	}
dx = temp32 % divisor ;
ax = temp32 / divisor ;
}

static void FASTCALL
byte_DIV( BYTE divisor )
{
/*
 *	Largest possible quotient is 0xFF = 255, largest remainder is
 *	divisor - 1, so largest dividend for this instruction is
 *	divisor * 255u + divisor - 1 = 256u * divisor - 1
 */
temp16 = ax ;
if( (WORD) divisor * 256u - 1 < temp16 ){
	latch_word = 0 ;
	if( CPU_type >= CPU_286 ){
		ip = instruction_ip ;
		cs = instruction_cs ;
		}
	call_interrupt() ;
	}
ah = temp16 % divisor ;
al = temp16 / divisor ;
}

static void FASTCALL
word_IDIV( SWORD divisor )
{
/*
 *	Largest quotient is (SWORD)0x7FFF = 32767, smallest (SWORD)0x8000 =
 *	-32768. Remainder could be from 0 to ( abs( divisor ) - 1 ) *
 *	sign( dividend ).
 *
 *	dividend divisor	Largest (or smallest) dividend
 *	  sign	  sign
 *
 *	   +	   +		32767*divisor+(divisor-1)    =	32768*divisor-1
 *	   +	   -	       -32768*divisor+((-divisor)-1) = -32769*divisor-1
 *	   -	   +	       -32768*divisor-(divisor-1)    = -32769*divisor+1
 *	   -	   -		32767*divisor-((-divisor)-1) =	32768*divisor+1
 */
#define MIN_QUOTIENT	(CPU_type>=CPU_286?-32769l:-32758l)
stemp32 = ( (SDWORD) (SWORD) dx << 16 ) | (DWORD) ax ;
do {
	if( stemp32 >= 0 )
		if( divisor >= 0 )
			if(  32768l * (SDWORD)divisor - 1 >= stemp32 )
				break ;
			else ;
		else	if( MIN_QUOTIENT * (SDWORD)divisor - 1 >= stemp32 )
				break ;
			else ;
	else	if( divisor >= 0 )
			if( MIN_QUOTIENT * (SDWORD)divisor + 1 <= stemp32 )
				break ;
			else ;
		else	if(  32768l * (SDWORD)divisor + 1 <= stemp32 )
				break ;
			else ;
	latch_word = 0 ;
	if( CPU_type >= CPU_286 ){
		ip = instruction_ip ;
		cs = instruction_cs ;
		}
	call_interrupt() ;
	} while( 0 ) ;
dx = stemp32 % divisor ;
ax = stemp32 / divisor ;
#undef	MIN_QUOTIENT
}

static void FASTCALL
byte_IDIV( SBYTE divisor )
{
/*
 *	Largest quotient is (SBYTE)0x7F = 127, smallest (SBYTE)0x80 =
 *	-128. Remainder could be from 0 to ( abs( divisor ) - 1 ) *
 *	sign( dividend ).
 *
 *	dividend divisor	Largest (or smallest) dividend
 *	  sign	  sign
 *
 *	   +	   +		127*divisor+(divisor-1)      =	128*divisor-1
 *	   +	   -	       -128*divisor+((-divisor)-1)   = -129*divisor-1
 *	   -	   +	       -128*divisor-(divisor-1)      = -129*divisor+1
 *	   -	   -		127*divisor-((-divisor)-1)   =	128*divisor+1
 */
#define MIN_QUOTIENT	(CPU_type>=CPU_286?-129:-128)
stemp16 = (SWORD) ax ;
do {
	if( stemp16 >= 0 )
		if( divisor >= 0 )
			if(  128 * (SWORD)divisor - 1 >= stemp16 )
				break ;
			else ;
		else	if( MIN_QUOTIENT * (SWORD)divisor - 1 >= stemp16 )
				break ;
			else ;
	else	if( divisor >= 0 )
			if( MIN_QUOTIENT * (SWORD)divisor + 1 <= stemp16 )
				break ;
			else ;
		else	if(  128 * (SWORD)divisor + 1 <= stemp16 )
				break ;
			else ;
	latch_word = 0 ;
	if( CPU_type >= CPU_286 ){
		ip = instruction_ip ;
		cs = instruction_cs ;
		}
	call_interrupt() ;
	} while( 0 ) ;
ah = stemp16 % divisor ;
al = stemp16 / divisor ;
#undef	MIN_QUOTIENT
}

static void FASTCALL
word_IMUL( SWORD multiplicant, SWORD multiplier )
{
stemp32 = (SDWORD)multiplicant * multiplier ;
if( ( (WORD) ( stemp32 >> 16 ) ) == 0 )
	if( ( (WORD) stemp32 ) & 0x8000u )
		{ RAISE_CF(); RAISE_OF(); }
	else	{ CLEAR_CF(); CLEAR_OF(); }
else
if( ( (WORD) ( stemp32 >> 16 ) ) == 0xFFFFu )
	if( !( ( (WORD) stemp32 ) & 0x8000u ) )
		{ RAISE_CF(); RAISE_OF(); }
	else	{ CLEAR_CF(); CLEAR_OF(); }
else		{ RAISE_CF(); RAISE_OF(); }
}

static void FASTCALL
byte_IMUL( SBYTE multiplier )
{
ax	= (SWORD)al * multiplier ;
if( ah == 0 )
	if( al & 0x80u )
		{ RAISE_CF(); RAISE_OF(); }
	else	{ CLEAR_CF(); CLEAR_OF(); }
else
if( ah == 0xFFu )
	if( !( al & 0x80u ) )
		{ RAISE_CF(); RAISE_OF(); }
	else	{ CLEAR_CF(); CLEAR_OF(); }
else		{ RAISE_CF(); RAISE_OF(); }
}

static WORD FASTCALL
word_INC( WORD dst )
{
if( ++dst == 0x8000 )
	RAISE_OF() ;
else	CLEAR_OF() ;
if( ( dst & 0xF ) == 0x0 )
	RAISE_AF() ;
else	CLEAR_AF() ;
SET_WSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static BYTE FASTCALL
byte_INC( BYTE dst )
{
if( ++dst == 0x80 )
	RAISE_OF() ;
else	CLEAR_OF() ;
if( ( dst & 0xF ) == 0x0 )
	RAISE_AF() ;
else	CLEAR_AF() ;
SET_BSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static void FASTCALL
word_MUL( WORD multiplier )
{
temp32	= (DWORD)ax * multiplier ;
ax	= (WORD) temp32 ;
dx	= (WORD) ( temp32 >> 16 ) ;
if( dx != 0 )
	{ RAISE_CF(); RAISE_OF(); }
else	{ CLEAR_CF(); CLEAR_OF(); }
}

static void FASTCALL
byte_MUL( BYTE multiplier )
{
ax	= (WORD) al * multiplier ;
if( ah != 0 )
	{ RAISE_CF(); RAISE_OF(); }
else	{ CLEAR_CF(); CLEAR_OF(); }
}

static WORD FASTCALL
word_NEG( WORD dst )
{
return word_SUB( 0, dst ) ;
}

static BYTE FASTCALL
byte_NEG( BYTE dst )
{
return byte_SUB( 0, dst ) ;
}

static WORD FASTCALL
word_NOT( WORD dst )
{
return ~dst ;
}

static BYTE FASTCALL
byte_NOT( BYTE dst )
{
return ~dst ;
}

static WORD FASTCALL
word_OR( WORD dst, WORD src )
{
dst |= src ;
CLEAR_OF() ;
CLEAR_CF() ;
CLEAR_AF() ;
SET_WSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static BYTE FASTCALL
byte_OR( BYTE dst, BYTE src )
{
dst |= src ;
CLEAR_OF() ;
CLEAR_CF() ;
CLEAR_AF() ;
SET_BSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static WORD FASTCALL
word_RCL( WORD dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- )
	if( flags & FL_CF ){
		if( dst & 0x8000u ){
			RAISE_CF() ;
			if( dst & 0x4000u )
				CLEAR_OF() ;
			else	RAISE_OF() ;
			}
		else {
			CLEAR_CF() ;
			if( dst & 0x4000u )
				RAISE_OF() ;
			else	CLEAR_OF() ;
			}
		dst = ( dst << 1 ) | 1 ;
		}
	else {
		if( dst & 0x8000u ){
			RAISE_CF() ;
			if( dst & 0x4000u )
				CLEAR_OF() ;
			else	RAISE_OF() ;
			}
		else {
			CLEAR_CF() ;
			if( dst & 0x4000u )
				RAISE_OF() ;
			else	CLEAR_OF() ;
			}
		dst = dst << 1 ;
		}
return dst ;
}

static BYTE FASTCALL
byte_RCL( BYTE dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- )
	if( flags & FL_CF ){
		if( dst & 0x80u ){
			RAISE_CF() ;
			if( dst & 0x40u )
				CLEAR_OF() ;
			else	RAISE_OF() ;
			}
		else {
			CLEAR_CF() ;
			if( dst & 0x40u )
				RAISE_OF() ;
			else	CLEAR_OF() ;
			}
		dst = ( dst << 1 ) | 1 ;
		}
	else {
		if( dst & 0x80u ){
			RAISE_CF() ;
			if( dst & 0x40u )
				CLEAR_OF() ;
			else	RAISE_OF() ;
			}
		else {
			CLEAR_CF() ;
			if( dst & 0x40u )
				RAISE_OF() ;
			else	CLEAR_OF() ;
			}
		dst = dst << 1 ;
		}
return dst ;
}

static WORD FASTCALL
word_RCR( WORD dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- )
	if( flags & FL_CF ){
		if( dst & 0x8000 )
			CLEAR_OF() ;
		else	RAISE_OF() ;
		if( dst & 1 ){
			RAISE_CF() ;
			}
		else	CLEAR_CF() ;
		dst = ( dst >> 1 ) | 0x8000 ;
		}
	else {
		if( dst & 0x8000 )
			RAISE_OF() ;
		else	CLEAR_OF() ;
		if( dst & 1 )
			RAISE_CF() ;
		else	CLEAR_CF() ;
		dst = dst >> 1 ;
		}
return dst ;
}

static BYTE FASTCALL
byte_RCR( BYTE dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- )
	if( flags & FL_CF ){
		if( dst & 0x80 )
			CLEAR_OF() ;
		else	RAISE_OF() ;
		if( dst & 1 )
			RAISE_CF() ;
		else	CLEAR_CF() ;
		dst = ( dst >> 1 ) | 0x80 ;
		}
	else {
		if( dst & 0x8000 )
			RAISE_OF() ;
		else	CLEAR_OF() ;
		if( dst & 1 )
			RAISE_CF() ;
		else	CLEAR_CF() ;
		dst = dst >> 1 ;
		}
return dst ;
}

static WORD FASTCALL
word_ROL( WORD dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- )
	if( dst & 0x8000u ){
		RAISE_CF() ;
		if( dst & 0x4000u )
			CLEAR_OF() ;
		else	RAISE_OF() ;
		dst = ( dst << 1 ) | 1 ;
		}
	else {
		CLEAR_CF() ;
		if( dst & 0x4000u )
			RAISE_OF() ;
		else	CLEAR_OF() ;
		dst = dst << 1 ;
		}
return dst ;
}

static BYTE FASTCALL
byte_ROL( BYTE dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- )
	if( dst & 0x80u ){
		RAISE_CF() ;
		if( dst & 0x40u )
			CLEAR_OF() ;
		else	RAISE_OF() ;
		dst = ( dst << 1 ) | 1 ;
		}
	else {
		CLEAR_CF() ;
		if( dst & 0x40u )
			RAISE_OF() ;
		else	CLEAR_OF() ;
		dst = dst << 1 ;
		}
return dst ;
}

static WORD FASTCALL
word_ROR( WORD dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- )
	if( dst & 1 ){
		RAISE_CF() ;
		if( dst & 0x8000u )
			CLEAR_OF() ;
		else	RAISE_OF() ;
		dst = ( dst >> 1 ) | 0x8000 ;
		}
	else {
		CLEAR_CF() ;
		if( dst & 0x8000u )
			RAISE_OF() ;
		else	CLEAR_OF() ;
		dst = dst >> 1 ;
		}
return dst ;
}

static BYTE FASTCALL
byte_ROR( BYTE dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- )
	if( dst & 1 ){
		RAISE_CF() ;
		if( dst & 0x80u )
			CLEAR_OF() ;
		else	RAISE_OF() ;
		dst = ( dst >> 1 ) | 0x80 ;
		}
	else {
		CLEAR_CF() ;
		if( dst & 0x80u )
			RAISE_OF() ;
		else	CLEAR_OF() ;
		dst = dst >> 1 ;
		}
return dst ;
}

static WORD FASTCALL
word_SHL( WORD dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- ){
	if( dst & 0x8000u ){
		RAISE_CF() ;
		if( dst & 0x4000 )
			CLEAR_OF() ;
		else	RAISE_OF() ;
		}
	else {
		CLEAR_CF() ;
		if( dst & 0x4000 )
			RAISE_OF() ;
		else	CLEAR_OF() ;
		}
	if( dst & 0x8 )
		RAISE_AF() ;
	else	CLEAR_AF() ;
	dst <<= 1 ;
	}
SET_WSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static BYTE FASTCALL
byte_SHL( BYTE dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- ){
	if( dst & 0x80u ){
		RAISE_CF() ;
		if( dst & 0x40 )
			CLEAR_OF() ;
		else	RAISE_OF() ;
		}
	else {
		CLEAR_CF() ;
		if( dst & 0x40 )
			RAISE_OF() ;
		else	CLEAR_OF() ;
		}
	if( dst & 0x8 )
		RAISE_AF() ;
	else	CLEAR_AF() ;
	dst <<= 1 ;
	}
SET_BSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static WORD FASTCALL
word_SAR( WORD dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- ){
	if( dst & 1 )
		RAISE_CF() ;
	else	CLEAR_CF() ;
	if( dst & 0x10 )
		RAISE_AF() ;
	else	CLEAR_AF() ;
	dst >>= 1 ;
	if( dst & 0x4000 )
		dst |= 0x8000 ;
	}
CLEAR_OF() ;
SET_WSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static BYTE FASTCALL
byte_SAR( BYTE dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- ){
	if( dst & 1 )
		RAISE_CF() ;
	else	CLEAR_CF() ;
	if( dst & 0x10 )
		RAISE_AF() ;
	else	CLEAR_AF() ;
	dst >>= 1 ;
	if( dst & 0x40 )
		dst |= 0x80 ;
	}
CLEAR_OF() ;
SET_BSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static WORD FASTCALL
word_SHR( WORD dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- ){
	if( dst & 1 )
		RAISE_CF() ;
	else	CLEAR_CF() ;
	if( dst & 0x10 )
		RAISE_AF() ;
	else	CLEAR_AF() ;
	dst >>= 1 ;
	}
CLEAR_OF() ;
SET_WSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static BYTE FASTCALL
byte_SHR( BYTE dst )
{
	int	i ;

for( i = shift_count ; i > 0 ; i-- ){
	if( dst & 1 )
		RAISE_CF() ;
	else	CLEAR_CF() ;
	if( dst & 0x10 )
		RAISE_AF() ;
	else	CLEAR_AF() ;
	dst >>= 1 ;
	}
CLEAR_OF() ;
SET_BSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static WORD FASTCALL
word_SBB( WORD dst, WORD src )
{
temp32 = ( dst | 0x10000ul ) - src ;
temp16 = ( dst | 0x8000u )  - ( src & 0x7FFFu ) ;
temp5  = ( dst | 0x10u )    - ( src & 0xFu ) ;
if( flags & FL_CF ){
	temp32-- ; temp16-- ; temp5-- ;
	}
if( temp32 & 0x10000ul ){
	CLEAR_CF() ;
	if( temp16 & 0x8000u )
		CLEAR_OF() ;
	else	RAISE_OF() ;
	}
else {
	RAISE_CF() ;
	if( temp16 & 0x8000u )
		RAISE_OF() ;
	else	CLEAR_OF() ;
	}
if( temp5 & 0x10u )
	CLEAR_AF() ;
else	RAISE_AF() ;
SET_WSF( (WORD)temp32 ) ;
SET_ZF(  (WORD)temp32 ) ;
SET_PF(  (BYTE)temp32 ) ;
return (WORD) temp32 ;
}

static BYTE FASTCALL
byte_SBB( BYTE dst, BYTE src )
{
temp16 = ( dst | 0x100u ) - src ;
temp8  = ( dst | 0x80u )  - ( src & 0x7Fu ) ;
temp5  = ( dst | 0x10u )  - ( src & 0xFu ) ;
if( flags & FL_CF ){
	temp16-- ; temp8-- ; temp5-- ;
	}
if( temp16 & 0x100u ){
	CLEAR_CF() ;
	if( temp8 & 0x80u )
		CLEAR_OF() ;
	else	RAISE_OF() ;
	}
else {
	RAISE_CF() ;
	if( temp8 & 0x80u )
		RAISE_OF() ;
	else	CLEAR_OF() ;
	}
if( temp5 & 0x10u )
	CLEAR_AF() ;
else	RAISE_AF() ;
SET_BSF( (BYTE)temp16 ) ;
SET_ZF(  (BYTE)temp16 ) ;
SET_PF(  (BYTE)temp16 ) ;
return (BYTE) temp16 ;
}

static WORD FASTCALL
word_SUB( WORD dst, WORD src )
{
temp32 = ( dst | 0x10000ul ) - src ;
temp16 = ( dst | 0x8000u )  - ( src & 0x7FFFu ) ;
temp5  = ( dst | 0x10u )    - ( src & 0xFu ) ;
if( temp32 & 0x10000ul ){
	CLEAR_CF() ;
	if( temp16 & 0x8000u )
		CLEAR_OF() ;
	else	RAISE_OF() ;
	}
else {
	RAISE_CF() ;
	if( temp16 & 0x8000u )
		RAISE_OF() ;
	else	CLEAR_OF() ;
	}
if( temp5 & 0x10u )
	CLEAR_AF() ;
else	RAISE_AF() ;
SET_WSF( (WORD)temp32 ) ;
SET_ZF(  (WORD)temp32 ) ;
SET_PF(  (BYTE)temp32 ) ;
return (WORD) temp32 ;
}

static BYTE FASTCALL
byte_SUB( BYTE dst, BYTE src )
{
temp16 = ( dst | 0x100u ) - src ;
temp8  = ( dst | 0x80u )  - ( src & 0x7Fu ) ;
temp5  = ( dst | 0x10u )  - ( src & 0xFu ) ;
if( temp16 & 0x100u ){
	CLEAR_CF() ;
	if( temp8 & 0x80u )
		CLEAR_OF() ;
	else	RAISE_OF() ;
	}
else {
	RAISE_CF() ;
	if( temp8 & 0x80u )
		RAISE_OF() ;
	else	CLEAR_OF() ;
	}
if( temp5 & 0x10u )
	CLEAR_AF() ;
else	RAISE_AF() ;
SET_BSF( (BYTE)temp16 ) ;
SET_ZF(  (BYTE)temp16 ) ;
SET_PF(  (BYTE)temp16 ) ;
return (BYTE) temp16 ;
}

static WORD FASTCALL
word_XOR( WORD dst, WORD src )
{
dst ^= src ;
CLEAR_CF() ;
CLEAR_OF() ;
CLEAR_AF() ;
SET_WSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static BYTE FASTCALL
byte_XOR( BYTE dst, BYTE src )
{
dst ^= src ;
CLEAR_CF() ;
CLEAR_OF() ;
CLEAR_AF() ;
SET_BSF( dst ) ;
SET_ZF(  dst ) ;
SET_PF(  dst ) ;
return dst ;
}

static void FASTCALL
cf2al( void )
{
if( flags & FL_CF )
	al = 0xFF ;
else	al = 0x00 ;
}

static void FASTCALL
_wait( void )
{
check_coprocessor_permitted() ;
}

/*
 *	286 control instructions
 */
void	protect_only( void ) ;
#define protect_only()		if(!(msw&MSW_PE))illegal();else
void	no_protect( void ) ;
#define no_protect()		if(msw&MSW_PE) CPU_error( "Instruction is not implemented in protected mode !" ) ; else

static void FASTCALL
_clts( void )
{
no_protect() ;
msw &= ~MSW_TS ;
}

static void FASTCALL
_lmsw( WORD new_msw )
{
no_protect() ;
if( new_msw & MSW_PE )
	CPU_error( "286 protected mode is not currently supported by EDB !" ) ;
msw = ( new_msw | MSW_RESET ) & MSW_SET ;
}

static WORD FASTCALL
_arpl( WORD dst, WORD src )
{
if( ( dst & 3 ) < ( src & 3 ) ){
	RAISE_ZF() ;
	dst += ( src - dst ) & 3 ;
	}
else	CLEAR_ZF() ;
return dst ;
}

static void FASTCALL
load_idt_gdt( DESC *reg )
{
no_protect() ;
fetch_word_EA_data() ; EA += 2 ;
reg->limit = latch_word ;
fetch_word_EA_data() ; EA += 2 ;
reg->base  = (DWORD) latch_word ;
fetch_word_EA_data() ;
reg->base |= ( (DWORD) ( latch_word & 0xFF ) ) << 16 ;
}

static void FASTCALL
store_idt_gdt( DESC *reg )
{
no_protect() ;
latch_word = reg->limit ;
store_word_EA_data() ; EA += 2 ;
latch_word = (WORD) reg->base ;
store_word_EA_data() ; EA += 2 ;
latch_word = ( (WORD) ( reg->base >> 16 ) ) | 0xFF00 ;
store_word_EA_data() ;
}

static WORD FASTCALL
_smsw( void )
{
no_protect() ;
return msw ;
}

static void FASTCALL
shutdown( void )
{
CPU_error( "Unimplemented CPU instruction encountered !" ) ;
}

static void FASTCALL
loadall( void )
{
CPU_error( "Unimplemented CPU instruction encountered !" ) ;
}

#ifdef	__TURBOC__
#pragma argsused
#endif

static WORD FASTCALL
_lar( WORD selector )
{
protect_only() ;
no_protect() ;
return NIL ;
}

#ifdef	__TURBOC__
#pragma argsused
#endif

static WORD FASTCALL
_lsl( WORD selector )
{
protect_only() ;
no_protect() ;
return NIL ;
}

#ifdef	__TURBOC__
#pragma argsused
#endif

static void FASTCALL
_lldt( WORD selector )
{
protect_only() ;
no_protect() ;
}

#ifdef	__TURBOC__
#pragma argsused
#endif

static void FASTCALL
_ltr( WORD selector )
{
protect_only() ;
no_protect() ;
}

#ifdef	__TURBOC__
#pragma argsused
#endif

static void FASTCALL
_verr( WORD selector )
{
protect_only() ;
no_protect() ;
}

#ifdef	__TURBOC__
#pragma argsused
#endif

static void FASTCALL
_verw( WORD selector )
{
protect_only() ;
no_protect() ;
}

static WORD FASTCALL
_sldt( void )
{
protect_only() ;
no_protect() ;
return NIL ;
}

static WORD FASTCALL
_str( void )
{
protect_only() ;
no_protect() ;
return NIL ;
}

/*
 *	CPU parameters initialization section
 */
static void
init_CPU_dependent_variables( void )
{
shift_mask = 0x1F ;
switch( CPU_type ){
	case CPU_8086:
		shift_mask = 0xFF ;
	case CPU_186:
		FL_SET	   = FL_SET_86 ;
		FL_RESET   = FL_RESET_86 ;
		break ;
	case CPU_286:
		FL_SET	   = FL_SET_286 ;
		FL_RESET   = FL_RESET_286 ;
		idt.base   = 0 ;
		idt.limit  = 0x3FF ;
		break ;
	}
}

typedef struct	{
	int	code ;
	char	*name ;
	} CPU_TYPES ;

static	CPU_TYPES	cpu_types_table[] = {
	{	CPU_8086,	"8086"          },
	{	CPU_8086,	"8088"          },
	{	CPU_186,	"80186"         },
	{	CPU_186,	"80188"         },
	{	CPU_186,	"186"           },
	{	CPU_186,	"188"           },
	{	CPU_286,	"80286"         },
	{	CPU_286,	"286"           },
	} ;
#define CPU_TYPES_COUNT (sizeof(cpu_types_table)/sizeof(CPU_TYPES))

static	char	*CPU_names[ CPU_COUNT ] = {
	"8086",         "80186",        "80286 (incomplete)",
	} ;

static int
set_CPU_type( char *p )
{
	CPU_TYPES	*t ;
	int		i ;

for( i = CPU_TYPES_COUNT, t = cpu_types_table ; i > 0 ; i--, t++ )
	if( strcmp( t->name, p ) == 0 ){
		CPU_type = t->code ;
		cprint( "Simulating %s CPU\r\n", CPU_names[ CPU_type ] ) ;
		init_CPU_dependent_variables() ;
		return 0 ;
		}
cprint( "Invalid CPU type specified: '%s'\r\n", p ) ;
return -1 ;
}

static int
set_prefetch_size( char *p )
{
#ifndef FAST_INSTRUCTION_FETCH
	unsigned	size ;

if( sscanf( p, "%u", &size ) != 1 ){
	cprint( "Invalid prefetch queue size : '%s'\r\n", p ) ;
	return -1 ;
	}
if( ( prefetch_bytes = size ) != 0 ){
	if( size < MIN_PREFETCH || size > MAX_PREFETCH ){
		cprint( "Prefetch queue size out of range : %d\r\n", size ) ;
		return -1 ;
		}
	fetch_byte_data_fun    = prefetch_byte_data ;
	fetch_word_data_fun    = prefetch_word_data ;
	restart_prefetch_fun   = restart_prefetch_queue ;
	backspace_prefetch_fun = backspace_prefetch_queue ;
	prefetch_bytes	       = size ;
	prefetch_lowwater      = size - 2 ;
	prefetch_highwater     = size - 1 ;
	}
return 0 ;
#else
cprint( "Instruction prefetch size set to zero at compilation time\r\n" ) ;
return -1 ;
#endif
}

int
init_CPU_parameters( void )
{
	char	cfg[ MAX_CFG_STRING ] ;
	char	*p ;

init_CPU_dependent_variables() ;
if( open_config_section( "CPU options" ) == -1 ) return -1 ;
while( get_config_string( cfg ) != -1 ){
	if( ( p = strchr( cfg, '=' ) ) == NULL ){
		cprint( "Invalid configuration string '%s'\r\n", cfg ) ;
		return -1 ;
		}
	p += strspn( p + 1, " \t" ) + 1 ;
	if( strncmp( cfg, "Processor",  9 ) == 0 ){
		if( set_CPU_type( p ) == -1 )
			return -1 ;
		}
	else
	if( strncmp( cfg, "Prefetch",   8 ) == 0 ){
		if( set_prefetch_size( p ) == -1 )
			return -1 ;
		}
	else {
		cprint( "Unrecognized option '%s' in '%s'\r\n", cfg, "CPU Options" ) ;
		return -1 ;
		}
	}
close_config() ;
return 0 ;
}
