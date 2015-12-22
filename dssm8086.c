#if 0 

  File name:  dssm8086.c, (very) simple 80x86 disassembler
 
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
#include "dssm8086.h"

/*
 *	Modification history.
 *
 *	03 Dec 91
 *	      - added extended aad/aam codes
 *	      - added cf2al (0D6h) instruction
 *	      - added 80286 instructions (w/ protected mode)
 *	      - added shutdown (0F 04) instruction
 *	      - added loadadd  (0F 05) instruction
 *	09 Dec 91
 *	      - chaged disassemble logic, so that instructions unavailable
 *		with current CPU type will be shown as 'invalid'
 *	      - changed variable name _EA to EA_val to prevent clash with
 *		tasm
 *	18 Dec 91 released version 0.15
 */

/*
 *	Instruction encoding
 */
static	char	*_regsw_[]  = {  "ax", "cx", "dx", "bx", "sp", "bp", "si", "di"  } ;
static	char	*_regsb_[]  = {  "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"  } ;
static	char	*_segreg_[] = {  "es", "cs", "ss", "ds", "??seg", "??seg", "??seg", "??seg"  } ;

char	*decode_reg16( unsigned x ) ;
char	*decode_reg8( unsigned x ) ;
char	*decode_segreg( unsigned x ) ;
#define decode_reg16(x) 	(_regsw_[(x)])
#define decode_reg8(x)		(_regsb_[(x)])
#define decode_segreg(x)	(_segreg_[(x)])
static	WORD	temp_cs, temp_ip, save_ip ;
static	char	*out_p ;
static	BYTE	scratch_pad ;
static	char	word_EA ;
static	char	EA[20] ;
static	BYTE	fetched_byte ;		/* 3 low bits of 1st instruction byte	*/
static	BYTE	second_fetched_byte ;	/* 2nd instruction byte 		*/
static unsigned mod, reg, r_m ; 	/* scratch pad				*/
static	WORD	call_cs ;		/* used by call_far & call_near 	*/
static	WORD	call_ip ;		/*					*/
static	WORD	EA_val ;
static	char	valid_EA ;
static	WORD	*EA_override ;
static	char	*seg_override ;

static	WORD	latch_word ;
static	BYTE	latch_byte ;
/*
 *	Secondary memory access
 */
BYTE	fetch_byte_data( void ) ;		/* From CS:IP */
WORD	fetch_word_data( void ) ;		/* From CS:IP */
WORD	fetch_signed_byte_data( void ) ;	/* From CS:IP */
#define fetch_byte_data()		fetch_byte(temp_cs,(temp_ip+=1)-1)
#define fetch_word_data()		fetch_word(temp_cs,(temp_ip+=2)-2)
#define fetch_signed_byte_data()	CBW(fetch_byte_data())

/*
 *	First instruction byte decode table (5 hi bits, 32 entries)
 */
static	void	illegal( void ), illegal2( void ) ;
static	void	pm_instructions_decode( void ) ;
static	void
	add_esstk(void),	or_csstk(void), 	adc_ssstk(void),
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

static	char	hex_cnv_tab[] = "0123456789ABCDEF" ;
#define bit2hex(x)   (hex_cnv_tab[(x)&0xF])

static void
save_str( char *str )
{
for( ; ( *out_p = *str ) != 0 ; out_p++, str++ ) ;
}

static void
save_comma( void )
{
*out_p++ = ',' ;
*out_p	 = 0 ;
}

static void
save_hex_byte( BYTE x )
{
*out_p++ = bit2hex( x >> 4 ) ;
*out_p++ = bit2hex( x ) ;
*out_p	 = 0 ;
}

static void
save_hex_word( WORD x )
{
*out_p++ = bit2hex( x >> 12 ) ;
*out_p++ = bit2hex( x >> 8 ) ;
*out_p++ = bit2hex( x >> 4 ) ;
*out_p++ = bit2hex( x ) ;
*out_p	 = 0 ;
}

static void
append_hex_word( char *out, WORD data )
{
while( *out ) out++ ;
out[ 0 ] = bit2hex( data >> 12 ) ;
out[ 1 ] = bit2hex( data >>  8 ) ;
out[ 2 ] = bit2hex( data >>  4 ) ;
out[ 3 ] = bit2hex( data       ) ;
out[ 4 ] = 0 ;
}

static void
append_char( char *out, char c )
{
while( *out ) out++ ;
*out++ = c ;
*out = 0 ;
}

static void
print_EA_content( void )
{
	if( EA_override == &cs )
		save_str( "CS:[" ) ;
else	if( EA_override == &ds )
		save_str( "DS:[" ) ;
else	if( EA_override == &es )
		save_str( "ES:[" ) ;
else	if( EA_override == &ss )
		save_str( "SS:[" ) ;
	else {
		save_str( "" ) ;
		return ;
		}
save_hex_word( EA_val ) ;
save_str( "]=" ) ;
if( word_EA )
	save_hex_word( fetch_word( *EA_override, EA_val ) ) ;
else	save_hex_byte( fetch_byte( *EA_override, EA_val ) ) ;
}

WORD
disassemble( WORD start_cs, WORD start_ip, char *area, char *EA_area )
{
temp_cs      = start_cs ;
save_ip      = temp_ip = start_ip ;
out_p	     = area ;
valid_EA     = 0 ;
seg_override = NULL ;
EA_override  = NULL ;
area[0]      = 0 ;
for( ; ; ){
	scratch_pad = fetch_byte( temp_cs, temp_ip++ ) ;
	switch( scratch_pad ){
		case 0x2e: EA_override = &cs ; seg_override = "cs:" ; break ;
		case 0x3e: EA_override = &ds ; seg_override = "ds:" ; break ;
		case 0x26: EA_override = &es ; seg_override = "es:" ; break ;
		case 0x36: EA_override = &ss ; seg_override = "ss:" ; break ;
		case 0xF0: save_str( "lock    " ) ;                   break ;
		default:
			fetched_byte = scratch_pad & 7 ;
			(*decode_MSB_entries[ scratch_pad >> 3 ] )() ;
			goto instruction_executed ;
		}
	}
instruction_executed:;
if( EA_area != NULL ) ;
if( valid_EA ){
	out_p = EA_area ;
	print_EA_content() ;
	}
else	EA_area[0] = 0 ;
return temp_ip ;
}

static void
illegal( void )
{
save_str( "db      " ) ; save_hex_byte( scratch_pad ) ;
temp_ip = save_ip + 1 ;
}

static void
illegal2( void )
{
save_str( "db      " ) ; save_hex_byte( scratch_pad ) ;
save_comma() ;	      save_hex_byte( second_fetched_byte ) ;
temp_ip = save_ip + 2 ;
}

/*
 *	Memory adressing decoding functions
 */
#define set_segment(x)	(EA_override=(EA_override==NULL?(x):EA_override))

static char *
decode_rm( void )
{
switch( r_m ){
	case 0: EA_val = bx + si ; set_segment( &ds ) ; return "bx+si" ;
	case 1: EA_val = bx + di ; set_segment( &ds ) ; return "bx+di" ;
	case 2: EA_val = bp + si ; set_segment( &ss ) ; return "bp+si" ;
	case 3: EA_val = bp + di ; set_segment( &ss ) ; return "bp+di" ;
	case 4: EA_val = si ;	   set_segment( &ds ) ; return "si" ;
	case 5: EA_val = di ;	   set_segment( &ds ) ; return "di" ;
	case 6: EA_val = bp ;	   set_segment( &ss ) ; return "bp" ;
	case 7: EA_val = bx ;	   set_segment( &ds ) ; return "bx" ;
	}
return "????" ;
}

static void
decode_EA( void )
{
static	WORD	temp ;
EA_val = 0 ;
EA[0] = 0 ;
if( seg_override != NULL ){
	strcpy( EA, seg_override ) ;
	}
switch( mod ){
	case 0: 			/* zero length displacement	*/
		valid_EA = 1 ;
		append_char( EA, '[' ) ;
		if( r_m == 6 ){ 	/* direct memory operand	*/
			set_segment( &ds ) ;
			append_hex_word( EA + 1, EA_val = fetch_word_data() ) ;
			}
		else	strcat( EA, decode_rm() ) ;
		append_char( EA, ']' ) ;
		break ;
	case 1: 			/* 1-byte displacement		*/
		valid_EA = 1 ;
		append_char( EA, '[' ) ;
		strcat( EA, decode_rm() ) ;
		append_char( EA, '+' ) ;
		append_hex_word( EA, temp = fetch_signed_byte_data() ) ;
		append_char( EA, ']' ) ;
		EA_val += temp ;
		break ;
	case 2: 			/* 2-byte displacement		*/
		valid_EA = 1 ;
		append_char( EA, '[' ) ;
		strcat( EA, decode_rm() ) ;
		append_char( EA, '+' ) ;
		append_hex_word( EA, temp = fetch_word_data() ) ;
		append_char( EA, ']' ) ;
		EA_val += temp ;
		break ;
	case 3:
		if( word_EA )
			strcat( EA, decode_reg16( r_m ) ) ;
		else	strcat( EA, decode_reg8( r_m ) ) ;
	}
}

static void
parce_second_byte( void )
{
second_fetched_byte = fetch_byte( temp_cs, temp_ip++ ) ;
r_m =	second_fetched_byte	   & 7 ;
reg = ( second_fetched_byte >> 3 ) & 7 ;
mod = ( second_fetched_byte >> 6 ) & 3 ;
decode_EA() ;
}

static void
parce_immediate_addr( void )
{
mod = 0 ;
r_m = 6 ;
decode_EA() ;
}

/*
 *	Segment registers stack operations
 */
void	check_segop( WORD *seg ) ;
#define check_segop(seg)	{if(fetched_byte>=6){pushpop_seg(seg);return;}}

static void
pushpop_seg( char *seg )
{
if( fetched_byte & 1 )
	save_str( "pop     " ) ;
else	save_str( "push    " ) ;
save_str( seg ) ;
}

static void
common_2address_decoder( void )
{
switch( fetched_byte ){
	case 0: 				/* xxx r/m, reg8	*/
		word_EA = 0 ;
		parce_second_byte() ;
		save_str( EA ) ; save_comma() ;
		save_str( decode_reg8( reg ) ) ;
		break ;
	case 1: 				/* xxx r/m, reg16	*/
		word_EA = 1 ;
		parce_second_byte() ;
		save_str( EA ) ; save_comma() ;
		save_str( decode_reg16( reg ) ) ;
		break ;
	case 2: 				/* xxx reg8, r/m	*/
		word_EA = 0 ;
		parce_second_byte() ;
		save_str( decode_reg8( reg ) ) ;
		save_comma() ; save_str( EA ) ;
		break ;
	case 3: 				/* xxx reg16, r/m	*/
		word_EA = 1 ;
		parce_second_byte() ;
		save_str( decode_reg16( reg ) ) ;
		save_comma() ; save_str( EA ) ;
		break ;
	case 4: 				/* xxx accum, immed	*/
		save_str( "al," ) ;
		save_hex_byte( fetch_byte_data() ) ;
		break ;
	case 5:
		save_str( "ax," ) ;
		save_hex_word( fetch_word_data() ) ;
		break ;
	}
}

static void
add_esstk( void )
{
check_segop( "es" ) ;                           /* push es ; pop es     */
save_str( "add     " ) ;
common_2address_decoder() ;
}

static void
or_csstk( void )
{
switch( fetched_byte ){
	default:
		save_str( "or      " ) ;
		common_2address_decoder() ;
		break ;
	case 6:
		save_str( "push    cs" ) ;
		break ;
	case 7:
		if( CPU_type < CPU_286 )
			save_str( "pop     cs" ) ;
		else	pm_instructions_decode() ;
		break ;
	}
}

static void
adc_ssstk( void )
{
check_segop( "ss" ) ;                           /* push ss ; pop ss     */
save_str( "adc     " ) ;
common_2address_decoder() ;
}

static void
sbb_dsstk( void )
{
check_segop( "ds" ) ;                           /* push ds ; pop ds     */
save_str( "sbb     " ) ;
common_2address_decoder() ;
}

static void
and_daa( void )
{
/*
 *	es: eliminated by previous scan
 */
if( fetched_byte == 7 ){ save_str( "daa" ) ; return ; }
save_str( "and     " ) ;
common_2address_decoder() ;
}

static void
sub_das( void )
{
/*
 *	cs: eliminated by previous scan
 */
if( fetched_byte == 7 ){ save_str( "das" ) ; return ; }
save_str( "sub     " ) ;
common_2address_decoder() ;
}

static void
xor_aaa( void )
{
/*
 *	ss: eliminated by previous scan
 */
if( fetched_byte == 7 ){ save_str( "aaa" ) ; return ; }
save_str( "xor     " ) ;
common_2address_decoder() ;
}

static void
cmp_aas( void )
{
/*
 *	ds: eliminated by previous scan
 */
if( fetched_byte == 7 ){ save_str( "aas" ) ; return ; }
save_str( "cmp     " ) ;
common_2address_decoder() ;
}

static void
inc_reg16( void )
{
save_str( "inc     " ) ;
save_str( decode_reg16( fetched_byte ) ) ;
}

static void
dec_reg16( void )
{
save_str( "dec     " ) ;
save_str( decode_reg16( fetched_byte ) ) ;
}

static void
push_reg16( void )
{
save_str( "push    " ) ;
save_str( decode_reg16( fetched_byte ) ) ;
}

static void
pop_reg16( void )
{
save_str( "pop     " ) ;
save_str( decode_reg16( fetched_byte ) ) ;
}

static void
pusha_etc( void )
{
if( CPU_type < CPU_186 ){ illegal() ; return ; }
switch( fetched_byte ){
	case 0: 	save_str( "pusha" ) ;           return ;
	case 1: 	save_str( "popa" ) ;            return ;
	case 2: 	save_str( "bound   " ) ;        break ;
	case 3: 	if( CPU_type < CPU_286 ){ illegal() ; return ; }
			save_str( "arpl    " ) ;        break ;
	default:	/* 386 prefixes */
			illegal() ;			return ;
	}
word_EA = 1 ;
parce_second_byte() ;
save_str( decode_reg16( reg ) ) ;
save_comma() ;
save_str( EA ) ;
}

static void
push_mul_ios( void )
{
	void	insb(void), insw(void), outsb(void), outsw(void) ;

if( CPU_type < CPU_186 ){ illegal() ; return ; }
word_EA = 1 ;
switch( fetched_byte ){
	case 0: /*	push	imm16			*/
		save_str( "push    " ) ;
		save_hex_word( fetch_word_data() ) ;
		return ;
	case 1: /*	imul	r16, r/m16, imm16	*/
		parce_second_byte() ;
		latch_word = fetch_word_data() ;
		break ;
	case 2: /*	push	imm8 (sxt)		*/
		save_str( "push    " ) ;
		save_hex_word( fetch_signed_byte_data() ) ;
		return ;
	case 3: /*	imul	r16, r/m16, imm8 (sxt)	*/
		parce_second_byte() ;
		latch_word = fetch_signed_byte_data() ;
		break ;
	case 4: 	insb() ;	return ;
	case 5: 	insw() ;	return ;
	case 6: 	outsb() ;	return ;
	case 7: 	outsw() ;	return ;
	}
save_str( "imul    " ) ;
save_str( decode_reg16( reg ) ) ;
save_comma() ;
save_str( EA ) ;
save_comma() ;
save_hex_word( latch_word ) ;
}

static	char	*pm_group1_names[] = {
	"sldt    ",     "str     ",     "lldt    ",     "ltr     ",
	"verr    ",     "verw    ",     "invalid ",     "invalid "
	} ;

static	char	*pm_group2_names[] = {
	"sgdt    ",     "sidt    ",     "lgdt    ",     "lidt    ",
	"smsw    ",     "invalid ",     "lmsw    ",     "invalid "
	} ;

static void
pm_group1( void )
{
word_EA = 1 ;
parce_second_byte() ;
save_str( pm_group1_names[ reg ] ) ;
save_str( EA ) ;
}

static void
pm_group2( void )
{
word_EA = 1 ;
parce_second_byte() ;
save_str( pm_group2_names[ reg ] ) ;
save_str( EA ) ;
}

static void
pm_instructions_decode( void )
{
switch( fetch_byte_data() ){
	default:			    illegal2() ;	     return ;
	case 0x00: /*  PM control grp 1  */ pm_group1() ;	     return ;
	case 0x01: /*  PM control grp 2  */ pm_group2() ;	     return ;
	case 0x02: /*  lar reg16, r/m16  */ save_str( "lar     " ) ; break ;
	case 0x03: /*  lsl reg16, r/m16  */ save_str( "lsl     " ) ; break ;
	case 0x04: /*  shutdown 	 */ save_str( "shutdown" ) ; return ;
	case 0x05: /*  loadall		 */ save_str( "loadall" ) ;  return ;
	case 0x06: /*  clts		 */ save_str( "clts" ) ;     return ;
	}
word_EA = 1 ;
parce_second_byte() ;
save_str( decode_reg16( reg ) ) ;
save_comma() ;
save_str( EA ) ;
}

static void
jmp_group1( void )
{
static	char	*jmp_name[] = {
	"jo      ",     "jno     ",     "jc      ",     "jnc     ",
	"jz      ",     "jnz     ",     "jbe     ",     "ja      "
	} ;
save_str( jmp_name[ fetched_byte ] ) ;
latch_word = fetch_signed_byte_data() ;
save_hex_word( temp_ip + latch_word ) ;
}

static void
jmp_group2( void )
{
static	char	*jmp_name[] = {
	"js      ",     "jns     ",     "jp      ",     "jnp     ",
	"jl      ",     "jge     ",     "jle     ",     "jg      "
	} ;
save_str( jmp_name[ fetched_byte ] ) ;
latch_word = fetch_signed_byte_data() ;
save_hex_word( temp_ip + latch_word ) ;
}

static	char	*group1_table[] = {
	"add     ",     "or      ",     "adc     ",     "sbb     ",
	"and     ",     "sub     ",     "xor     ",     "cmp     "
	} ;

static void
test_xchg_common( void )
{
if( fetched_byte & 1 ){ 			/* 16-bit		*/
	save_str( decode_reg16( reg ) ) ;
	save_comma() ;
	save_str( EA ) ;
	}
else {						/*  8-bit		*/
	save_str( decode_reg8( reg ) ) ;
	save_comma() ;
	save_str( EA ) ;
	}
}

static void
report_ptr_size( void )
{
if( mod == 3 ) return ;
if( word_EA )
	save_str( "word ptr " ) ;
else	save_str( "byte ptr " ) ;
}

static void
group1_test_xchg( void )
{
word_EA = fetched_byte & 1 ;
parce_second_byte() ;
switch( fetched_byte ){
	case 0: 				/*	???	byte r/m, imm8	*/
	case 2: 				/*	w = 0, s = 0/1		*/
		save_str( group1_table[ reg ] ) ;
		report_ptr_size() ;
		save_str( EA ) ; save_comma() ;
		save_hex_byte( fetch_byte_data() ) ;
		break ;
	case 1: 				/*	???	word r/m, imm16 */
		save_str( group1_table[ reg ] ) ;
		report_ptr_size() ;
		save_str( EA ) ; save_comma() ;
		save_hex_word( fetch_word_data() ) ;
		break ;
	case 3: 				/*	???	word r/m, imm8	*/
		save_str( group1_table[ reg ] ) ;
		report_ptr_size() ;
		save_str( EA ) ; save_comma() ;
		save_hex_word( fetch_signed_byte_data() ) ;
		break ;
	case 4: 				/*	test	reg, r/m	*/
	case 5:
		save_str( "test    " ) ;
		test_xchg_common() ;
		break ;
	case 6: 				/*	xchg	reg, r/m	*/
	case 7:
		save_str( "xchg    " ) ;
		test_xchg_common() ;
		break ;
	}
}

static void
mov_seg_pop( void )
{
if( fetched_byte < 4 ){
	word_EA = fetched_byte & 1 ;
	parce_second_byte() ;
	save_str( "mov     " ) ;
	}
else {
	word_EA = 1 ;
	parce_second_byte() ;
	}
switch( fetched_byte ){
	case 0: 				/* mov	   r/m, reg8	*/
		save_str( EA ) ; save_comma() ;
		save_str( decode_reg8( reg ) ) ;
		break ;
	case 1: 				/* mov	   r/m, reg16	*/
		save_str( EA ) ; save_comma() ;
		save_str( decode_reg16( reg ) ) ;
		break ;
	case 2: 				/* mov	   reg8, r/m	*/
		save_str( decode_reg8( reg ) ) ;
		save_comma() ; save_str( EA ) ;
		break ;
	case 3: 				/* mov	   reg16, r/m	*/
		save_str( decode_reg16( reg ) ) ;
		save_comma() ; save_str( EA ) ;
		break ;
	case 4: 				/* mov	   r/m, seg	*/
		save_str( "mov     " ) ;
		save_str( EA ) ; save_comma() ;
		save_str( decode_segreg( reg ) ) ;
		break ;
	case 5: 				/*	lea	reg, mem	*/
		save_str( "lea     " ) ;
		save_str( decode_reg16( reg ) ) ;
		save_comma() ; save_str( EA ) ;
		break ;
	case 6: 				/*	mov	seg, r/m	*/
		save_str( "mov     " ) ;
		save_str( decode_segreg( reg ) ) ;
		save_comma() ; save_str( EA ) ;
		break ;
	case 7: 				/*	pop	r/m		*/
		save_str( "pop     " ) ;
		save_str( EA ) ;
		break ;
	}
}

static void
xchg_ax_reg( void )
{
if( fetched_byte == 0 )
	save_str( "nop" ) ;
else {
	save_str( "xchg    ax," ) ;
	save_str( decode_reg16( fetched_byte ) ) ;
	}
}

static void
scnv_flagsops( void )
{
switch( fetched_byte ){
	case 0: save_str( "cbw" ) ;   break ;
	case 1: save_str( "cwd" ) ;   break ;
	case 2: 				/* call far immediate		*/
		call_ip = fetch_word_data() ;
		call_cs = fetch_word_data() ;
		save_str( "call    " ) ;
		save_hex_word( call_cs ) ;
		save_str( ":" ) ;
		save_hex_word( call_ip ) ;
		break ;
	case 3: save_str( "wait" ) ;  break ;
	case 4: save_str( "pushf" ) ; break ;
	case 5: save_str( "popf" ) ;  break ;
	case 6: save_str( "sahf" ) ;  break ;
	case 7: save_str( "lahf" ) ;  break ;
	}
}

static void
movsb( void )
{
save_str( "movsb" ) ;
if( seg_override != NULL ){
	save_str( "   es:[di]," ) ;
	save_str( seg_override ) ;
	save_str( "[si]" ) ;
	}
}

static void
movsw( void )
{
save_str( "movsw" ) ;
if( seg_override != NULL ){
	save_str( "   es:[di]," ) ;
	save_str( seg_override ) ;
	save_str( "[si]" ) ;
	}
}

static void
cmpsb( void )
{
save_str( "cmpsb" ) ;
if( seg_override != NULL ){
	save_str( "   es:[di]," ) ;
	save_str( seg_override ) ;
	save_str( "[si]" ) ;
	}
}

static void
cmpsw( void )
{
save_str( "cmpsw" ) ;
if( seg_override != NULL ){
	save_str( "   es:[di]," ) ;
	save_str( seg_override ) ;
	save_str( "[si]" ) ;
	}
}

static void
lodsb( void )
{
save_str( "lodsb" ) ;
if( seg_override != NULL ){
	save_str( "   " ) ;
	save_str( seg_override ) ;
	save_str( "[si]" ) ;
	}
}

static void
lodsw( void )
{
save_str( "lodsw" ) ;
if( seg_override != NULL ){
	save_str( "   " ) ;
	save_str( seg_override ) ;
	save_str( "[si]" ) ;
	}
}

static void
outsb( void )
{
save_str( "outsb" ) ;
if( seg_override != NULL ){
	save_str( "   " ) ;
	save_str( seg_override ) ;
	save_str( "[si]" ) ;
	}
}

static void
outsw( void )
{
save_str( "outsw" ) ;
if( seg_override != NULL ){
	save_str( "   " ) ;
	save_str( seg_override ) ;
	save_str( "[si]" ) ;
	}
}

static void stosb( void ){ save_str( "stosb" ) ; }
static void stosw( void ){ save_str( "stosw" ) ; }
static void scasb( void ){ save_str( "scasb" ) ; }
static void scasw( void ){ save_str( "scasw" ) ; }
static void insb ( void ){ save_str( "insb"  ) ; }
static void insw ( void ){ save_str( "insw"  ) ; }

static void
mov_strops( void )
{
if( fetched_byte < 4 ){
	word_EA = fetched_byte & 1 ;
	parce_immediate_addr() ;
	save_str( "mov     " ) ;
	}
switch( fetched_byte ){
/*
 *	In this instruction d-bit value is reversed.
 */
	case 2: 				/*   mov     r/m, al	*/
		save_str( EA ) ; save_str( ",al" ) ; break ;
	case 3: 				/*   mov     r/m, ax	*/
		save_str( EA ) ; save_str( ",ax" ) ; break ;
	case 0: 				/*   mov     al, r/m	*/
		save_str( "al," ) ; save_str( EA ) ; break ;
	case 1: 				/*   mov     ax, r/m	*/
		save_str( "ax," ) ; save_str( EA ) ; break ;
	case 4: movsb() ; break ;
	case 5: movsw() ; break ;
	case 6: cmpsb() ; break ;
	case 7: cmpsw() ; break ;
	}
}

static void
test_strops( void )
{
switch( fetched_byte ){
	case 0: 				/*	test	al, immed	*/
		save_str( "test    al," ) ;
		save_hex_byte( fetch_byte_data() ) ;
		break ;
	case 1: 				/*	test	ax, immed	*/
		save_str( "test    ax," ) ;
		save_hex_word( fetch_word_data() ) ;
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
save_str( "mov     " ) ;
save_str( decode_reg8( fetched_byte ) ) ;
save_comma() ;
save_hex_byte( fetch_byte_data() ) ;
}

static void
mov_reg16_immed( void )
{
save_str( "mov     " ) ;
save_str( decode_reg16( fetched_byte ) ) ;
save_comma() ;
save_hex_word( fetch_word_data() ) ;
}

static void
ret_lxs_mov( void )
{
	void	shift_operations(void) ;

if( fetched_byte >= 4 ){
	if( fetched_byte == 6 )
		word_EA = 0 ;
	else	word_EA = 1 ;
	parce_second_byte() ;
	}
switch( fetched_byte ){
	case 0: 				/*	sxx	r/m8,  imm8	*/
	case 1: 				/*	rxx	r/m16, imm8	*/
		if( CPU_type < CPU_186 ){ illegal() ; return ; }
		shift_operations() ;
		save_comma() ;
		save_hex_byte( fetch_byte_data() ) ;
		break ;
	case 2: 				/*	retn	nn		*/
		save_str( "retn    " ) ;
		save_hex_word( fetch_word_data() ) ;
		break ;
	case 3: 				/*	retn			*/
		save_str( "retn    " ) ;
		break ;
	case 4: 				/*	les	reg, mem	*/
		save_str( "les     " ) ;
		save_str( decode_reg16( reg ) ) ;
		save_comma() ;
		save_str( EA ) ;
		break ;
	case 5: 				/*	lds	reg, mem	*/
		save_str( "lds     " ) ;
		save_str( decode_reg16( reg ) ) ;
		save_comma() ;
		save_str( EA ) ;
		break ;
	case 6: 				/*	mov	r/m8, immed	*/
		if( reg != 0 )
			illegal2() ;
		save_str( "mov     " ) ;
		report_ptr_size() ;
		save_str( EA ) ; save_comma() ;
		save_hex_byte( fetch_byte_data() ) ;
		break ;
	case 7: 				/*	mov	r/m16, immed	*/
		if( reg != 0 )
			illegal2() ;
		save_str( "mov     " ) ;
		report_ptr_size() ;
		save_str( EA ) ; save_comma() ;
		save_hex_word( fetch_word_data() ) ;
		break ;
	}
}

static void
retf_int( void )
{
switch( fetched_byte ){
	case 0: 				/*	enter i16, i8	*/
		if( CPU_type < CPU_186 ){ illegal() ; return ; }
		save_str( "enter   " ) ;
		save_hex_word( fetch_word_data() ) ;
		save_comma() ;
		save_hex_byte( fetch_byte_data() ) ;
		break ;
	case 1: 				/*	leave		*/
		if( CPU_type < CPU_186 ){ illegal() ; return ; }
		save_str( "leave" ) ;
		break ;
	case 2: 				/*	retf	nn	*/
		latch_word = fetch_word_data() ;
		save_str( "retf    " ) ;
		save_hex_word( latch_word ) ;
		break ;
	case 3: 				/*	retf		*/
		save_str( "retf    " ) ;
		break ;
	case 4: 				/*	int	3	*/
		save_str( "int3" ) ;
		break ;
	case 5: 				/*	int	nn	*/
		save_str( "int     " ) ;
		save_hex_byte( fetch_byte_data() ) ;
		break ;
	case 6: 				/*	into		*/
		save_str( "into" ) ;
		break ;
	case 7: 				/*	iret		*/
		save_str( "iret" ) ;
		break ;
	}
}

static void
xlat( void )
{
save_str( "xlat" ) ;
if( seg_override != NULL ){
	save_str( "    " ) ;
	save_str( seg_override ) ;
	save_str( "bx" ) ;
	}
}

static	char	*shifts_table[] = {
	"rol     ",     "ror     ",     "rcl     ",     "rcr     ",
	"shl     ",     "shr     ",     "??shift ",     "sar     "
	} ;

static void
shift_operations( void )
{
word_EA = fetched_byte & 1 ;
parce_second_byte() ;
save_str( shifts_table[ reg ] ) ;
report_ptr_size() ;
save_str( EA ) ;
}

static void
shifts_xlat( void )
{
if( fetched_byte < 4 ){
	shift_operations() ;
	save_str( fetched_byte & 2 ? ",cl" : ",1" ) ;
	}
else
switch( fetched_byte ){
	case 4:
		if( ( latch_byte = fetch_byte_data() ) != 0x0A ){
			save_str( "aam_" ) ;
			save_hex_byte( latch_byte ) ;
			}
		else	save_str( "aam" ) ;
		break ;
	case 5:
		if( ( latch_byte = fetch_byte_data() ) != 0x0A ){
			save_str( "aad_" ) ;
			save_hex_byte( latch_byte ) ;
			}
		else	save_str( "aad" ) ;
		break ;
	case 6:
		save_str( "cf2al" ) ;
		break ;
	case 7: 	xlat() ;	break ;
	}
}

static void
extension_escape( void )			/* this is almost nop...*/
{
word_EA = 1 ;
parce_second_byte() ;
save_str( "esc     " ) ;
save_hex_byte( ( fetched_byte << 3 ) | reg ) ;
save_comma() ;
save_str( EA ) ;
}

static void
loops_io( void )
{
if( fetched_byte < 4 ){ 			/* loop??, jcxz 	*/
	latch_word = fetch_signed_byte_data() ;
	switch( fetched_byte ){
		case 0: save_str( "loopnz  " ) ; break ;
		case 1: save_str( "loopz   " ) ; break ;
		case 2: save_str( "loop    " ) ; break ;
		case 3: save_str( "jcxz    " ) ; break ;
		}
	save_hex_word( temp_ip + latch_word ) ;
	}
else {
	latch_byte = fetch_byte_data() ;
	switch( fetched_byte ){
		case 4: 			/* in	al, [imm]	*/
			save_str( "in      al," ) ;
			save_hex_byte( latch_byte ) ;
			break ;
		case 5: 			/* in	ax, [imm]	*/
			save_str( "in      ax," ) ;
			save_hex_byte( latch_byte ) ;
			break ;
		case 6: 			/* out	[imm], al	*/
			save_str( "out     " ) ;
			save_hex_byte( latch_byte ) ;
			save_str( ",al" ) ;
			break ;
		case 7: 			/* out	[imm], ax	*/
			save_str( "out     " ) ;
			save_hex_byte( latch_byte ) ;
			save_str( ",ax" ) ;
			break ;
		}
	}
}

static void
call_jmp_io( void )
{
switch( fetched_byte ){
	case 0: 				/* call near imm	*/
		call_ip    = fetch_word_data() ;
		call_ip   += temp_ip ;
		save_str( "call    " ) ;
		save_hex_word( call_ip ) ;
		break ;
	case 1: 				/* jmp	near imm	*/
		call_ip    = fetch_word_data() ;
		call_ip   += temp_ip ;
		save_str( "jmp     " ) ;
		save_hex_word( call_ip ) ;
		break ;
	case 2: 				/* jmp	far  imm	*/
		call_ip    = fetch_word_data() ;
		call_cs    = fetch_word_data() ;
		save_str( "jmp     far " ) ; save_hex_word( call_cs ) ;
		save_str( ":" ) ;            save_hex_word( call_ip ) ;
		break ;
	case 3: 				/* jmp	short		*/
		call_ip    = fetch_signed_byte_data() ;
		if( call_ip == 0 ){
			save_str( "jmp     $+2" ) ;
			}
		else {
			call_ip   += temp_ip ;
			save_str( "jmp     " ) ;
			save_hex_word( call_ip ) ;
			}
		break ;
	case 4: save_str( "in      al,dx" ) ; break ;
	case 5: save_str( "in      ax,dx" ) ; break ;
	case 6: save_str( "out     dx,al" ) ; break ;
	case 7: save_str( "out     dx,ax" ) ; break ;
	}
}

static void
repnz( void )
{
/*
 *	Meaning if repnz depends upon following instruction :
 *	rep with movs, stos, lods; repnz with cmps and scas
 *	repnz can be used with other instructions, but this
 *	will cause emulator error.
 */
do {
switch( latch_byte = fetch_byte_data() ){
	case 0xA4:	save_str( "rep     " ) ;  movsb() ; return ;
	case 0xA5:	save_str( "rep     " ) ;  movsw() ; return ;
	case 0xAA:	save_str( "rep     " ) ;  stosb() ; return ;
	case 0xAB:	save_str( "rep     " ) ;  stosw() ; return ;
	case 0xAC:	lodsb() ;			    return ;
	case 0xAD:	lodsw() ;			    return ;
	case 0xA6:	save_str( "repnz   " ) ;  cmpsb() ; return ;
	case 0xA7:	save_str( "repnz   " ) ;  cmpsw() ; return ;
	case 0xAE:	save_str( "repnz   " ) ;  scasb() ; return ;
	case 0xAF:	save_str( "repnz   " ) ;  scasw() ; return ;
	case 0x2e:	seg_override = "cs:" ;    continue ;
	case 0x3e:	seg_override = "ds:" ;    continue ;
	case 0x26:	seg_override = "es:" ;    continue ;
	case 0x36:	seg_override = "ss:" ;    continue ;
	case 0xF0:	/* LOCK */		  continue ;
	default:
		if( CPU_type >= CPU_186 )
			switch( latch_byte ){
	case 0x6C:	save_str( "rep     " ) ;  insb() ;  return ;
	case 0x6D:	save_str( "rep     " ) ;  insw() ;  return ;
	case 0x6E:	save_str( "rep     " ) ;  outsb() ; return ;
	case 0x6F:	save_str( "rep     " ) ;  outsw() ; return ;
			}
		save_str( "??rep" ) ;
		--temp_ip ;
		break ;
	}
    break ;
    } while( 1 ) ;
}

static void
repz( void )
{
do {
switch( latch_byte = fetch_byte_data() ){
	case 0xA4:	save_str( "rep     " ) ;  movsb() ; return ;
	case 0xA5:	save_str( "rep     " ) ;  movsw() ; return ;
	case 0xAA:	save_str( "rep     " ) ;  stosb() ; return ;
	case 0xAB:	save_str( "rep     " ) ;  stosw() ; return ;
	case 0xAC:	lodsb() ;			    return ;
	case 0xAD:	lodsw() ;			    return ;
	case 0xA6:	save_str( "repz    " ) ;  cmpsb() ; return ;
	case 0xA7:	save_str( "repz    " ) ;  cmpsw() ; return ;
	case 0xAE:	save_str( "repz    " ) ;  scasb() ; return ;
	case 0xAF:	save_str( "repz    " ) ;  scasw() ; return ;
	case 0x2e:	seg_override = "cs:" ;    continue ;
	case 0x3e:	seg_override = "ds:" ;    continue ;
	case 0x26:	seg_override = "es:" ;    continue ;
	case 0x36:	seg_override = "ss:" ;    continue ;
	case 0xF0:	/* LOCK */		  continue ;
	default:
		if( CPU_type >= CPU_186 )
			switch( latch_byte ){
	case 0x6C:	save_str( "rep     " ) ;  insb() ;  return ;
	case 0x6D:	save_str( "rep     " ) ;  insw() ;  return ;
	case 0x6E:	save_str( "rep     " ) ;  outsb() ; return ;
	case 0x6F:	save_str( "rep     " ) ;  outsw() ; return ;
			}
		--temp_ip ;
		save_str( "??repz" ) ;
		break ;
	}
    break ;
    } while( 1 ) ;
}

static void
group2_ops( void )
{
parce_second_byte() ;
switch( reg ){
	case 0: save_str( "test    " ) ;        break ;
	case 1: illegal2() ;			return ;
	case 2: save_str( "not     " ) ;        break ;
	case 3: save_str( "neg     " ) ;        break ;
	case 4: save_str( "mul     " ) ;        break ;
	case 5: save_str( "imul    " ) ;        break ;
	case 6: save_str( "div     " ) ;        break ;
	case 7: save_str( "idiv    " ) ;        break ;
	}
report_ptr_size() ;
save_str( EA ) ;
if( reg == 0 ){
	save_comma() ;
	if( word_EA )
		save_hex_word( fetch_word_data() ) ;
	else	save_hex_byte( fetch_byte_data() ) ;
	}
}

static void
reps_group2( void )
{
switch( fetched_byte ){
	case 0:
	case 1: illegal() ;		break ;
	case 2: repnz() ;		break ;
	case 3: repz() ;		break ;
	case 4: save_str( "hlt" ) ;     break ;
	case 5: save_str( "cmc" ) ;     break ;
	case 6: 				/*	xxx  byte r/m	*/
		word_EA = 0 ;
		group2_ops() ;
		break ;
	case 7: 				/*	xxx  word r/m	*/
		word_EA = 1 ;
		group2_ops() ;
		break ;
	}
}

static	void
group3_ops( void )
{
parce_second_byte() ;
switch( reg ){
	case 0:  save_str( "inc     " ) ;     break ;
	case 1:  save_str( "dec     " ) ;     break ;
	case 2:  save_str( "call    " ) ;     break ;
	case 3:  save_str( "call    far " ) ; break ;
	case 4:  save_str( "jmp     " ) ;     break ;
	case 5:  save_str( "jmp     far " ) ; break ;
	case 6:  save_str( "push    " ) ;     break ;
	default: save_str( "???     " ) ;     return ;
	}
if( ! word_EA || reg < 2 )
	report_ptr_size() ;
save_str( EA ) ;
}

static	void
flags_group3( void )
{
switch( fetched_byte ){
	case 0: save_str( "clc" ) ;     break ;
	case 1: save_str( "stc" ) ;     break ;
	case 2: save_str( "cli" ) ;     break ;
	case 3: save_str( "sti" ) ;     break ;
	case 4: save_str( "cld" ) ;     break ;
	case 5: save_str( "std" ) ;     break ;
	case 6:
		word_EA = 0 ;
		group3_ops() ;
		break ;
	case 7:
		word_EA = 1 ;
		group3_ops() ;
		break ;
	}
}
