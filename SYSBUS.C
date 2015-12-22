#if 0 

  File name:  sysbus.c, PC I/O bus and memory access support
 
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
#ifdef	__TURBOC__
#pragma inline
#endif
#include <stdio.h>
#include <stdlib.h>
#include <dos.h>
#include <mem.h>
#include <string.h>
#include "cpu8086.h"
#include "sysbus.h"
#include "config.h"
#include "accessory.h"
#include "os.h"
#include "edb.h"

/*
 *	Modifications history
 *
 *	15 Dec 91
 *		Added support for MirrorPort option
 *
 *	18 Dec 91 released version 0.15
 */

#define ADDR_MASK	(0xFFFFFul)

typedef struct	{
	WORD		code ;
	WORD		delay ;
	} INT_Q ;

	BYTE		port_disable_table[ BUS_ADDR_MASK + 1 ] ;
	BYTE		port_mirrors	  [ BUS_ADDR_MASK + 1 ] ;
	DWORD	 FAR	*interrupts_table ;
	BYTE		interrupts_types[ INTERRUPTS_COUNT ] ;
	int		bp_type = BP_NONE ;
	int		bp_count = 0 ;
	WORD		bp_port, bp_seg, bp_off, bp_dat ;
	int		read_bps = 0, write_bps = 0 ;
	MEM_BREAK	read_bp_table[ MAX_MEM_BPS ] ;
	MEM_BREAK	write_bp_table[ MAX_MEM_BPS ] ;
	char		resume_trace ;
	int		pass_memory_write = 0 ;
	MEM_ROUTE	virtual_mem_table[ MAX_VIRTUAL_SEGMENTS ] ;
	int		virtual_segments = 0 ;

static	char	A20wrap = 1 ;
static	DWORD	linear_addr ;
static	int	monitor_count = 0 ;
static	MONITOR dump_area[ MAX_MONITORS ] ;
static	INT_Q	int_queue[ MAX_MONITORS ] ;
static	WORD	int_q_head = 0, int_q_tail = 0 ;
static	WORD	int_delay = 0 ;

static	WORD	max_delay = 0 ;

void LIBSYM
interrupt_monitor( MONITOR *mon )
{
	INT_Q	*p ;

if( interrupts_table[ mon->code ] == ( mon->code | 0x10000ul ) ){
	(*(mon->old_vec))() ;
	return ;
	}
if( mon->code == 8 || mon->code == 9 )
	outportb( 0x20, 0x20 ) ;
p = int_queue + int_q_tail ;
p->code      = mon->code ;
if( int_delay > max_delay ){
	p->delay  = int_delay - max_delay ;
	max_delay = int_delay ;
	}
else	p->delay  = 0 ;
if( ++int_q_tail >= MAX_MONITORS )
	int_q_tail -= MAX_MONITORS ;
}

#ifndef __TURBOC__
#define produce_linear(seg,off) \
	{								\
	linear_addr = ( (DWORD)seg << 4 ) + off ;			\
	if( A20wrap && linear_addr > ADDR_MASK ){			\
		seg = 0 ; off = (WORD)( linear_addr &= ADDR_MASK ) ;	\
		}							\
	}
#else
#undef	cl
#ifdef	TARGET_8086
#define produce_linear(seg,off) \
	{								\
	asm	mov	ax, seg ;					\
	asm	mov	dx, ax ;					\
	    asm     mov     cl, 4  ;					\
	    asm     shl     ax, cl ;					\
	    asm     rol     dx, cl ;					\
	asm	and	dx, 0Fh ;					\
	asm	add	ax, off ;					\
	asm	adc	dx, 0 ; 					\
	asm	cmp	byte ptr A20wrap, 0 ;				\
	asm	je	no_wrap ;					\
	asm	cmp	dx, 10h ;					\
	asm	jb	no_wrap ;					\
		asm	mov	dx,  0 ;				\
		asm	mov	seg, dx ;				\
		asm	mov	off, ax ;				\
	no_wrap:;							\
	asm	mov	word ptr linear_addr + 0, ax ;			\
	asm	mov	word ptr linear_addr + 2, dx ;			\
	}
#else
#define produce_linear(seg,off) \
	{								\
	asm	mov	ax, seg ;					\
	asm	mov	dx, ax ;					\
	    asm     shl     ax, 4 ;					\
	    asm     rol     dx, 4 ;					\
	asm	and	dx, 0Fh ;					\
	asm	add	ax, off ;					\
	asm	adc	dx, 0 ; 					\
	asm	cmp	byte ptr A20wrap, 0 ;				\
	asm	je	no_wrap ;					\
	asm	cmp	dx, 10h ;					\
	asm	jb	no_wrap ;					\
		asm	mov	dx,  0 ;				\
		asm	mov	seg, dx ;				\
		asm	mov	off, ax ;				\
	no_wrap:;							\
	asm	mov	word ptr linear_addr + 0, ax ;			\
	asm	mov	word ptr linear_addr + 2, dx ;			\
	}
#endif
#endif

#define check_read_breakpoint() \
	{								\
		int		i ;					\
		MEM_BREAK	*p ;					\
									\
	for( i = read_bps, p = read_bp_table ; i > 0 ; i--, p++ )	\
		if( linear_addr < p->end && linear_addr >= p->start ){	\
			fake_ID = p->cmd_fake_ID ;			\
			bp_type = BP_MEMR ;				\
			bp_seg = segm ; 				\
			bp_off = off ;					\
			break ; 					\
			}						\
	}

static void
flush_breakpoint( void )
{
	unsigned long	save_linear = linear_addr ;
	int		pass_save   = pass_memory_write ;
	int		save_count  = bp_count ;

pass_memory_write = 1 ;
bp_count	  = 0 ;
switch( bp_type ){
	case BP_MEMWB:	store_byte( bp_seg, bp_off, bp_dat ) ; break ;
	case BP_MEMWW:	store_word( bp_seg, bp_off, bp_dat ) ; break ;
	}
pass_memory_write = pass_save ;
linear_addr	  = save_linear ;
bp_count	  = save_count ;
}

static int
check_write_breakpoint( void )
{
	int		i ;
	MEM_BREAK	*p ;

for( i = write_bps, p = write_bp_table ; i > 0 ; i--, p++ )
	if( linear_addr < p->end && linear_addr >= p->start ){
		if( ++bp_count > 1 )
			flush_breakpoint() ;
		fake_ID = p->cmd_fake_ID ;
		bp_type = BP_MEMWB ;
		return 1 ;
		}
return 0 ;
}

#define translate_virtual(seg,off) \
{									\
	int		i  ;						\
	MEM_ROUTE	*p ;						\
									\
for( i = virtual_segments, p = virtual_mem_table ; i > 0 ; i--, p++ ){	\
	if( linear_addr <  p->virt_start ) continue ;			\
	if( linear_addr >= p->virt_end	 ) continue ;			\
	seg -= p->segment_diff ;					\
	break ; 							\
	}								\
}

BYTE FASTCALL
fetch_byte( WORD segm, WORD off )
{
produce_linear( segm, off ) ;
check_read_breakpoint() ;
translate_virtual( segm, off ) ;
return peekb( segm, off ) ;
}

WORD FASTCALL
fetch_word( WORD segm, WORD off )
{
if( off == 0xFFFF ){
	bp_seg = segm ; bp_off = off ;
	segm++ ; off -= 0x10 ; bp_type = BP_SEGEND ;
	}
produce_linear( segm, off ) ;
check_read_breakpoint() ;
translate_virtual( segm, off ) ;
return peek( segm, off ) ;
}

void FASTCALL
store_byte( WORD segm, WORD off, BYTE x )
{
produce_linear( segm, off ) ;
if( check_write_breakpoint() ){
	bp_seg = segm ; bp_off = off ; bp_dat = x ;
	if( ! pass_memory_write ) return ;
	}
translate_virtual( segm, off ) ;
pokeb( segm, off, x ) ;
}

void FASTCALL
store_word( WORD segm, WORD off, WORD x )
{
if( off == 0xFFFF ){
	bp_seg = segm ; bp_off = off ;
	segm++ ; off -= 0x10 ; bp_type = BP_SEGEND ;
	}
produce_linear( segm, off ) ;
if( check_write_breakpoint() ){
	bp_seg = segm ; bp_off = off ; bp_dat = x ;
	bp_type++ ;
	if( ! pass_memory_write ) return ;
	}
translate_virtual( segm, off ) ;
poke( segm, off, x ) ;
}

BYTE FASTCALL
byte_IN( WORD port )
{
if( ! resume_trace && ( get_port_access( port ) & IOP_READ ) ){
	/*
	 *	resume_flag = 1 prevents stepping to trace routine when TF set,
	 *	because cs:ip will be returned lately (in edb.c) to current
	 *	state to restart execution of I/O instruction.
	 */
	if( flags & FL_TF )
		resume_flag = 1 ;
	fake_ID     = get_fake_ID( port ) ;
	bp_type     = BP_IOR ;
	bp_port     = port ;
	analyse_breakpoint() ;
	CPU_error( "I/O breakpoint should never return !" ) ;
	return 0xFF ;
	}
else {
	if( port_disable_table[ port ] & IOP_MIRROR )
		return port_mirrors[ port ] ;
	else	return inportb( port ) ;
	}
}

WORD FASTCALL
word_IN( WORD port )
{
if( ! resume_trace && ( get_port_access( port ) & IOP_READ ) ){
	/*
	 *	resume_flag = 1 prevents stepping to trace routine when TF set,
	 *	because cs:ip will be returned lately (in edb.c) to current
	 *	state to restart execution of I/O instruction.
	 */
	if( flags & FL_TF )
		resume_flag = 1 ;
	fake_ID     = get_fake_ID( port ) ;
	bp_type     = BP_IOR ;
	bp_port     = port ;
	analyse_breakpoint() ;
	CPU_error( "I/O breakpoint should never return !" ) ;
	return 0xFFFF ;
	}
else {
	if( port_disable_table[ port ] & IOP_MIRROR )
		return MK_WORD( port_mirrors[ port ], port_mirrors[ port + 1 ] ) ;
	else	return inport( port ) ;
	}
}

void FASTCALL
byte_OUT( WORD port, BYTE x )
{
if( ! resume_trace && ( get_port_access( port ) & IOP_WRITE ) ){
	/*
	 *	resume_flag = 1 prevents stepping to trace routine when TF set,
	 *	because cs:ip will be returned lately (in edb.c) to current
	 *	state to restart execution of I/O instruction.
	 */
	if( flags & FL_TF )
		resume_flag = 1 ;
	fake_ID     = get_fake_ID( port ) ;
	bp_type     = BP_IOW ;
	bp_port     = port ;
	analyse_breakpoint() ;
	CPU_error( "I/O breakpoint should never return !" ) ;
	}
else {
	if( port_disable_table[ port ] & IOP_MIRROR )
		port_mirrors[ port ] = x ;
	else	outportb( port, x ) ;
	}
}

void FASTCALL
word_OUT( WORD port, WORD x )
{
if( ! resume_trace && ( get_port_access( port ) & IOP_WRITE ) ){
	/*
	 *	resume_flag = 1 prevents stepping to trace routine when TF set,
	 *	because cs:ip will be returned lately (in edb.c) to current
	 *	state to restart execution of I/O instruction.
	 */
	if( flags & FL_TF )
		resume_flag = 1 ;
	fake_ID     = get_fake_ID( port ) ;
	bp_type     = BP_IOW ;
	bp_port     = port ;
	analyse_breakpoint() ;
	CPU_error( "I/O breakpoint should never return !" ) ;
	}
else {
	if( port_disable_table[ port ] & IOP_MIRROR ){
		port_mirrors[ port + 0 ] = *LO_BYTE( &x ) ;
		port_mirrors[ port + 1 ] = *HI_BYTE( &x ) ;
		}
	else	outport( port, x ) ;
	}
}

WORD
interrupt_check( void )
{
	INT_Q	*p ;

if( halt_condition ){
	if( monitor_count == 0 )
		CPU_error( "HLT with no hardware interrupts enabled !" ) ;
	if( break_requested() )
		CPU_error( "CPU halted." ) ;
	}
if( int_q_head == int_q_tail )
	return 0xFFFF ;
else
if( ! flags & FL_IF )
	return 0xFFFF ;
else {

	p = int_queue + int_q_head ;
	if( p->delay > 0 ){
		p->delay-- ;
		if( max_delay > 0 ) max_delay-- ;
		return 0xFFFF ;
		}
	int_q_head++ ;
	if( int_q_head >= MAX_MONITORS )
		int_q_head -= MAX_MONITORS ;
	return p->code ;
	}
}

static int
create_RAM_slot( DWORD start, DWORD end )
{
	WORD		virt_seg ;
	WORD		seg_size ;
	WORD		real_seg ;
	MEM_ROUTE	*p ;

if( virtual_segments == MAX_VIRTUAL_SEGMENTS ){
	cprint( "No room left for virtual RAM slot %lX-%lX\r\n", start, end - 1 ) ;
	return -1 ;
	}
virt_seg      = (WORD)( start >> 4 ) ;
seg_size      = (WORD)( ( end + 0xF ) >> 4 ) - virt_seg ;
if( ( real_seg = allocate_segment( seg_size ) ) == 0 ){
	cprint( "No RAM for virtual memory %lX-%lX\r\n", start, end - 1 ) ;
	return -1 ;
	}
p = virtual_mem_table + virtual_segments++ ;
p->virt_start	= start ;
p->virt_end	= end ;
p->real_start	= real_seg ;
p->segment_diff = virt_seg - real_seg ;
p->virt 	= MK_FP( virt_seg, (WORD)( start & 0xF ) ) ;
p->real 	= MK_FP( real_seg, (WORD)( start & 0xF ) ) ;
p->size 	= end - start ;
copy_RAM( real_seg, virt_seg, seg_size ) ;
return 0 ;
}

static int
default_system_init( void )
{
	int	i ;

setmem( port_disable_table, sizeof port_disable_table, 0 ) ;
if( create_RAM_slot( 0, 0x400 ) == -1 ) return -1 ;
interrupts_table = MK_FP( virtual_mem_table[ 0 ].real_start, 0 ) ;
for( i = 0 ; i < INTERRUPTS_COUNT ; i++ ){
	interrupts_table[ i ] = ((DWORD)INVALID_CS_SELECTOR << 16) | i ;
	interrupts_types[ i ] = 0 ;
	}
return 0 ;
}

static int
process_data_vectors( char *p )
{
	WORD	low ;
	WORD	temp = 0xFFFF ;
	int	range = 0 ;

for( ; p != NULL ; p = next_field( p ) ){
	if( *p == '-' && temp != 0xFFFF ){
		low = temp ;
		range = 1 ;
		p++ ;
		}
	if( sscanf( p, "%x", &temp ) != 1 ){
		cprint( "Syntax : '%s'\r\n", p ) ;
		return -1 ;
		}
	if( temp >= INTERRUPTS_COUNT ){
		cprint( "Vector number too large : '%x'\r\n", temp ) ;
		return -1 ;
		}
	if( ! range )
		low = temp ;
	for( ; low <= temp ; low++ ){
		interrupts_table[ low ]  = (DWORD) getvect( low ) ;
		interrupts_types[ low ] |= INTERRUPT_DATA ;
		}
	if( range ){
		range = 0 ;
		temp  = 0xFFFF ;
		}
	}
return 0 ;
}

void LIBSYM
clear_monitors( void )
{
	int	i ;
	INT_Q	*p ;

__emit__( 0xFA ) ;	/*	CLI	*/
for( i = 0 ; i < monitor_count ; i++ )
	setvect( dump_area[ i ].code, dump_area[ i ].old_vec ) ;
__emit__( 0xFB ) ;	/*	STI	*/
while( int_q_head != int_q_tail ){
	p = int_queue + int_q_head ;
	int_q_head++ ;
	if( int_q_head >= MAX_MONITORS )
		int_q_head -= MAX_MONITORS ;
	geninterrupt( p->code ) ;
	}
max_delay = 0 ;
}

static int
process_monitor_vectors( char *p )
{
	WORD	temp ;

for( ; p != NULL ; p = next_field( p ) ){
	if( sscanf( p, "%x", &temp ) != 1 ){
		cprint( "Syntax : '%s'\r\n", p ) ;
		return -1 ;
		}
	if( temp >= INTERRUPTS_COUNT ){
		cprint( "Vector number too large : '%x'\r\n", temp ) ;
		return -1 ;
		}
	if( monitor_count >= MAX_MONITORS ){
		cprint( "No slots left : '%s'\r\n", p ) ;
		return -1 ;
		}
	if( monitor_count == 0 )
		atexit( clear_monitors ) ;
	setup_vector_monitor( temp, dump_area + monitor_count ) ;
	interrupts_types[ temp ] |= INTERRUPT_WATCH ;
	monitor_count++ ;
	}
return 0 ;
}

static int
process_port_disable( char *p )
{
	WORD	temp ;

for( ; p != NULL ; p = next_field( p ) ){
	if( sscanf( p, "%x", &temp ) != 1 ){
		cprint( "Syntax : '%s'\r\n", p ) ;
		return -1 ;
		}
	if( temp > BUS_ADDR_MASK ){
		cprint( "I/O port number too large : '%x'\r\n", temp ) ;
		return -1 ;
		}
	set_port_access( temp, IOP_READ | IOP_WRITE ) ;
	}
return 0 ;
}

static int
set_mirror_port( char *p )
{
	WORD	temp ;

for( ; p != NULL ; p = next_field( p ) ){
	if( sscanf( p, "%x", &temp ) != 1 ){
		cprint( "Syntax : '%s'\r\n", p ) ;
		return -1 ;
		}
	if( temp > BUS_ADDR_MASK ){
		cprint( "I/O port number too large : '%x'\r\n", temp ) ;
		return -1 ;
		}
	port_disable_table[ temp ] |= IOP_MIRROR ;
	port_mirrors	  [ temp ]  = inportb( temp ) ;
	}
return 0 ;
}

static int
set_interrupt_delay( char *p )
{
if( sscanf( p, "%u", &int_delay ) != 1 ){
	cprint( "Invalid decimal value : '%s'\r\n", p ) ;
	return -1 ;
	}
return 0 ;
}

static int
create_separate_RAM( char *p )
{
	unsigned long	start ;
	unsigned long	end ;

if( sscanf( p, "%lx-%lx", &start, &end ) != 2 ){
	cprint( "Invalid memory range : '%s'\r\n", p ) ;
	return -1 ;
	}
return create_RAM_slot( start, end + 1 ) ;
}

int
init_system_bus( void )
{
	char	cfg[ MAX_CFG_STRING ] ;
	char	*p ;

if( default_system_init() == -1 ) return -1 ;
if( open_config_section( "System bus" ) == -1 ) return -1 ;
while( get_config_string( cfg ) != -1 ){
	if( ( p = strchr( cfg, '=' ) ) == NULL ){
		cprint( "Invalid configuration string '%s'\r\n", cfg ) ;
		return -1 ;
		}
	p += strspn( p + 1, " \t" ) + 1 ;
	if( strncmp( cfg, "DataVector",     10 ) == 0 ){
		if( process_data_vectors( p ) == -1 ) return -1 ;
		}
	else
	if( strncmp( cfg, "WrapA20",         7 ) == 0 )
		A20wrap = process_YesNo( p ) ;
	else
	if( strncmp( cfg, "DisablePort",    11 ) == 0 ){
		if( process_port_disable( p ) == -1 ) return -1 ;
		}
	else
	if( strncmp( cfg, "MonitorVector",  13 ) == 0 ){
		if( process_monitor_vectors( p ) == -1 ) return -1 ;
		}
	else
	if( strncmp( cfg, "InterruptDelay", 14 ) == 0 ){
		if( set_interrupt_delay( p ) == -1 ) return -1 ;
		}
	else
	if( strncmp( cfg, "VirtualRAM",     10 ) == 0 ){
		if( create_separate_RAM( p ) == -1 ) return -1 ;
		}
	else
	if( strncmp( cfg, "MirrorPort",     10 ) == 0 ){
		if( set_mirror_port( p ) == -1 ) return -1 ;
		}
	else {
		cprint( "Unrecognized configuration option '%s'\r\n", cfg ) ;
		return -1 ;
		}
	}
close_config() ;
return 0 ;
}
