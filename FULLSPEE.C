#if 0 

  File name:  fullspee.c, execute code on the native CPU
 
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
#include <dos.h>
#include <mem.h>
#include <setjmp.h>
#include "cpu8086.h"
#include "edb.h"
#include "sysbus.h"
#include "os.h"
#include "perversion.h"
#include "fullspeed.h"

static	char	local_IDT_active = 0 ;
static	char	old_IDT[8] ;
	char	IDT_descriptor[ 8 ] = { 0xFF, 0x03 } ;
static	jmp_buf done_run ;
static	char	use_common_IDT = 0 ;
	char	disable_local_IDT = 0 ;

static void LIBSYM
restore_global_IDT( void )
{
if( local_IDT_active ){
	/*cputs( "Restoring global sysytem IDT\r\n" ) ;*/
	_BX = FP_OFF( old_IDT ) ;
	__emit__( 0x0F, 0x01, 0x1F ) ;	/* lidt [bx]	*/
	local_IDT_active = 0 ;
	}
}

int
check_hardware( void )
{
if( disable_local_IDT ) return -1 ;
__emit__( 0x9C, 0x58 ) ;		/* pushf, pop ax	*/
if( ( _AX & FL_RESET_86 ) == FL_RESET_86 ){
	/*cputs( "This function requeres 286 CPU\r\n" ) ;*/
	return -1 ;
	}
__emit__( 0x0F, 0x01, 0xE0 ) ;		/* smsw ax		*/
if( _AX & 1 ){
	/*cputs( "This functions will not work in protected mode\r\n" ) ;*/
	return -1 ;
	}
_BX = FP_OFF( old_IDT ) ;
__emit__( 0x0F, 0x01, 0x0F ) ;	/* sidt [bx]	*/
if( old_IDT[ 2 ] | old_IDT[ 3 ] | old_IDT[ 4 ] ){
	/*cputs( "IDT already relocated\r\n" ) ;*/
	return -1 ;
	}
return 0 ;
}

static void
set_IDT( DWORD *IDT )
{
	DWORD	temp ;

temp = ( (DWORD)FP_SEG( IDT ) << 4 ) + FP_OFF( IDT ) ;
IDT_descriptor[ 2 ] = (BYTE) temp ;
IDT_descriptor[ 3 ] = (BYTE) ( temp >> 8 ) ;
IDT_descriptor[ 4 ] = (BYTE) ( temp >> 16 ) ;
/*cputs( "Installing local IDT\r\n" ) ;*/
_BX = FP_OFF( IDT_descriptor ) ;
__emit__( 0x0F, 0x01, 0x1F ) ;	/* lidt [bx]	*/
local_IDT_active = 1 ;
}

void
full_speed_return_point( void )
{
restore_global_IDT() ;
longjmp( done_run, 1 ) ;
}

static void
set_program_IDT( void )
{
	DWORD	FAR	*user ;
	DWORD	FAR	*system ;
	WORD		i ;

clear_monitors() ;
user   = interrupts_table ;
system = MK_FP( 0, 0 ) ;
for( i = 0 ; i < INTERRUPTS_COUNT ; i++, user++, system++ ){
	if( *user == ( 0x10000ul | i ) ) continue ;
	*system = *user ;
	}
}

static void
restore_system_IDT( DWORD *old_system )
{
	DWORD	FAR	*user ;
	BYTE		*type ;
	DWORD	FAR	*system ;
	WORD		i ;
	WORD		seg ;

user   = interrupts_table ;
system = MK_FP( 0, 0 ) ;
type   = interrupts_types ;
if( use_common_IDT ) system[ 3 ] = user[ 3 ] ;
for( i = 0 ; i < INTERRUPTS_COUNT ; i++, user++, system++, old_system++, type++ ){
	if( *system == *old_system ) continue ;
	seg = *system >> 16 ;
	if( seg == INVALID_CS_SELECTOR )
		*user = *system ;
	if( *type & INTERRUPT_DATA )
		*user = *system ;
	*system = *old_system ;
	}
}

static void
swap_data_areas( void )
{
	MEM_ROUTE	*p = virtual_mem_table + 1 ;
	int		i  = virtual_segments  - 1 ;

for( ; i > 0 ; i--, p++ )
	swap_RAM( p->virt, p->real, p->size ) ;
}

static	char	need_cleanup = 0 ;
static	DWORD	*p_save_IDT ;
static	BYTE	save_byte ;
static	WORD	p_stop_cs, p_stop_ip ;

void
full_speed_cleanup( void )
{
if( ! need_cleanup ) return ;
need_cleanup = 0 ;
restore_global_IDT() ;
restore_system_IDT( p_save_IDT ) ;
swap_data_areas() ;
__emit__( 0xFB ) ;	/*	STI	*/
pokeb( p_stop_cs, p_stop_ip, save_byte ) ;
}

int
full_speed_run( WORD stop_cs, WORD stop_ip )
{
	DWORD	IDT[ INTERRUPTS_COUNT ] ;
	DWORD	save_IDT[ INTERRUPTS_COUNT ] ;
static	char	first_call = 1 ;

if( first_call ){
	first_call = 0 ;
	atexit( restore_global_IDT ) ;
	}
p_save_IDT = save_IDT ;
p_stop_cs  = stop_cs ;
p_stop_ip  = stop_ip ;
restore_global_IDT() ;
use_common_IDT = check_hardware() ;
fill_IDT( IDT, full_speed_return_point ) ;
if( setjmp( done_run ) == 0 ){
	__emit__( 0xFA ) ;	/*	CLI	*/
	movedata( 0, 0, FP_SEG( save_IDT ), FP_OFF( save_IDT ), INTERRUPTS_COUNT * 4 ) ;
	set_program_IDT() ;
	swap_data_areas() ;
	save_byte = peekb( stop_cs, stop_ip ) ;
	pokeb( stop_cs, stop_ip, 0xCC ) ;
	set_psp( program_psp ) ;
	if( use_common_IDT ){
		poke( 0, 3*4+0, FP_OFF( break_point_interrupt ) ) ;
		poke( 0, 3*4+2, FP_SEG( break_point_interrupt ) ) ;
		}
	else	set_IDT( IDT ) ;
	need_cleanup = 1 ;
	start_program() ;
	}
else {
	full_speed_cleanup() ;
	program_psp = get_psp() ;
	set_psp( debug_psp ) ;
	}
flags = (flags | FL_RESET) & FL_SET ;
return 0 ;
}
