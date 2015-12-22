#if 0 

  File name:  os.c, MS-DOS dependent code (i.e. no part of this code
	      will be meaningful anywhere else ;)
 
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
#pragma inline
#ifndef __TURBOC__
	#error	This module can be compiled only by TurboC !
#endif
#ifndef __TINY__
	#error	This module should be compiled in TINY memory model !
#endif
#include <stdio.h>
#include <stdlib.h>
#include <dos.h>
#include <dir.h>
#include <io.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <stdarg.h>
#include "cpu8086.h"
#include "os.h"
#include "psp.h"
#include "perversion.h"
#include "sysbus.h"
#include "fullspeed.h"

/*
 *	EXT_EPB gives format for EPB of UNDOCUMENTED DOS function
 *	4B01 (load program for debugging).
 *	first four field are the same as 4B00 EXEC, cs:ip and ss:sp
 *	are filled by DOS.
 */
typedef struct	{
	WORD		env_seg ;
	char far	*cmd_line ;
	struct	fcb far *fcb1 ;
	struct	fcb far *fcb2 ;
	WORD		sp, ss ;
	WORD		ip, cs ;
	} EXT_EPB ;

	WORD		program_psp ;
	WORD		debug_psp ;
	char		journal_name[ MAX_FILE_NAME ] ;
	char		journal_status = J_CLOSED ;
	int		watch_exec = 0 ;

int
switchar( char x )
{
_AX = 0x3700 ;
geninterrupt( 0x21 ) ;
return x == _DL ;
}

WORD
get_psp( void )
{
_AX = 0x5100 ;
geninterrupt( 0x21 ) ;
return _BX ;
}

void
set_psp( WORD psp )
{
_BX = psp ;
_AX = 0x5000 ;
geninterrupt( 0x21 ) ;
}

void interrupt
(* LIBSYM getvect( int n ))()
{
asm	xor	ax, ax
asm	mov	es, ax
asm	mov	bx, n
asm	shl	bx, 1
asm	shl	bx, 1
asm	mov	ax, es:[bx]
asm	mov	dx, es:[bx+2]
}

void LIBSYM
setvect( int no, void interrupt (*isr)() )
{
asm	pushf
asm	cli
asm	xor	ax, ax
asm	mov	es, ax
asm	mov	bx, no
asm	shl	bx, 1
asm	shl	bx, 1
asm	mov	ax, isr
asm	mov	es:[bx], ax
asm	mov	ax, isr+2
asm	mov	es:[bx+2], ax
asm	popf
}

static void
get_dir_of_drive( int drive, char dir[ MAXPATH ] )
{
if( getcurdir( drive, dir + 1 ) == -1 )
	dir[ 1 ] = 0 ;
dir[ 0 ] = '\\' ;
if( dir[ strlen( dir ) - 1 ] != '\\' )
	strcat( dir, "\\" ) ;
}

static int
extract_next_field( char *path, char *name )
{
	int	width = 0 ;

if( *path == '\\' ){
	path++ ;
	width++ ;
	}
for( ; *path != 0 ; width++, path++ ){
	if( *path == '\\' ){
		width++ ;
		break ;
		}
	*name++ = *path ;
	}
*name++ = 0 ;
return width ;
}

static void
delete_last_entry( char dir[] )
{
	int	len = strlen( dir ) ;
	char	*p = dir + len - 1 ;

if( len <= 1 ) return ;
if( *p == '\\' ){
	len-- ; p-- ;
	}
for( ; len > 0 ; len--, p-- )
	if( *p == '\\' ) break ;
*p++ = '\\' ;
*p = 0 ;
}

static void
compress_path( char dir[] )
{
	char	next_name[ MAXPATH ] ;
	char	out_dir[ 2 * MAXPATH ] = "\\" ;
	char	*from ;
	int	step_size = 1 ;

for( from = dir ; ; from += step_size ){
	if( ( step_size = extract_next_field( from, next_name ) ) == 0 )
		break ;
	if( strlen( next_name ) == 0 ) continue ;
	if( strcmp( next_name, "." ) == 0 ) ;
	else
	if( strcmp( next_name, ".." ) == 0 ){
		delete_last_entry( out_dir ) ;
		}
	else {
		strcat( out_dir, next_name ) ;
		strcat( out_dir, "\\" ) ;
		}
	}
strcpy( dir, out_dir ) ;
}

void
absolute_name( char *name )
{
	char	drive[ MAXDRIVE ] ;
	char	dir[ MAXDIR ] ;
	char	file[ MAXFILE ] ;
	char	ext[ MAXEXT ] ;
	char	base_dir[ MAXPATH * 2 ] ;
	int	state ;
	int	device_ID ;

strupr( name ) ;
state = fnsplit( name, drive, dir, file, ext ) ;
if( ! (state & DRIVE) ){
	device_ID = getdisk() + 1 ;
	drive[ 0 ] = 'A' + device_ID - 1 ;
	drive[ 1 ] = ':' ;
	drive[ 2 ] = 0 ;
	}
else	device_ID = drive[ 0 ] - 'A' + 1 ;
if( ! (state & DIRECTORY) )
	get_dir_of_drive( device_ID, dir ) ;
else {
	if( dir[ 0 ] != '\\' ){
		get_dir_of_drive( device_ID, base_dir ) ;
		strcat( base_dir, dir ) ;
		}
	else	strcpy( base_dir, dir ) ;
	compress_path( base_dir ) ;
	strcpy( dir, base_dir ) ;
	}
if( ! (state & FILENAME) )
	strcpy( file, "" ) ;
if( ! (state & EXTENSION) )
	strcpy( ext, "" ) ;
fnmerge( name, drive, dir, file, ext ) ;
}

static void
create_name_ext( char *name )
{
	char	drive[ MAXDRIVE ] ;
	char	dir[ MAXDIR ] ;
	char	file[ MAXFILE ] ;
	char	ext[ MAXEXT ] ;

absolute_name( name ) ;
if( fnsplit( name, drive, dir, file, ext ) & EXTENSION )
	return ;
fnmerge( name, drive, dir, file, ".COM" ) ;
if( access( name, 0 ) == 0 )
	return ;
fnmerge( name, drive, dir, file, ".EXE" ) ;
return ;
}

static	struct	fcb	fcb1, fcb2 ;
static	EXT_EPB 	epb ;
static	unsigned char	cmd[ 0x80 ] ;
static	void		(*termprog)( void ) ;
static	void interrupt	(*old_22)( void ) ;

static void
fill_EPB( char *s )
{
epb.env_seg  = NULL ;
epb.cmd_line = cmd ;
epb.fcb1     = &fcb1 ;
epb.fcb2     = &fcb2 ;
cmd[ 0 ] = min( 0x7d, strlen( s ) ) ;
if( cmd[ 0 ] == 0 ){
	cmd[ 0 ] = 1 ; cmd[ 1 ] = ' ' ;
	}
else	memcpy( cmd + 1, s, cmd[ 0 ] ) ;
cmd[ cmd[ 0 ] + 1 ] = '\r' ;
cmd[ cmd[ 0 ] + 2 ] =	0  ;
s = parsfnm( s, &fcb1, 1 ) ;
    parsfnm( s, &fcb2, 1 ) ;
}

static void interrupt
program_terminate( void )
{
/*
 *	Debugger itself should be called instead of exit() !
 */
__emit__( 0x0E, 0x1F ) ;	/* push cs, pop ds */
set_psp( debug_psp ) ;
setvect( 0x22, old_22 ) ;
full_speed_cleanup() ;
if( termprog != NULL )
	(*termprog)() ;
exit( 0 ) ;
}

static void
fill_program_registers( EXT_EPB *epb )
{
cs    = epb->cs ;
ip    = epb->ip ;
ss    = epb->ss ;
sp    = epb->sp + 2 ;
ds    = program_psp ;
es    = program_psp ;
/*
 *	The following values are undocumented !!!
 */
ax    = entry_ax ;
bx    = entry_bx ;
cx    = entry_cx ;
bp    = entry_bp ;
dx    = program_psp ;
si    = ip ;
di    = sp ;

flags = FL_IF | FL_RESET ;
restart_prefetch() ;
}

int
load_program( char *name, char *param, void (*term_prog)(void) )
{
debug_psp = get_psp() ;
create_name_ext( name ) ;
fill_EPB( param ) ;
old_22 = getvect( 0x22 ) ;
_ES = _DS ;
_DX = (WORD) name ;
_BX = (WORD) &epb ;
_AX = 0x4B01 ;
geninterrupt( 0x21 ) ;
__emit__( 0x9C, 0x59 ) ;	/*	pushf,	pop cx		*/
if( _CX & FL_CF ){
	errno = _AX ;
	return -1 ;
	}
else {
	program_psp = get_psp() ;
	set_psp( debug_psp ) ;
	termprog = term_prog ;
	fill_program_registers( &epb ) ;
	((PSP FAR *)MK_FP( program_psp, 0 ))->terminate = MK_FP( _CS, FP_OFF( program_terminate ) ) ;
	}
return 0 ;
}

void
terminate_program( void )
{
set_psp( program_psp ) ;
_AX = 0x4C00 ;
geninterrupt( 0x21 ) ;
}

static void
simulate_iret( void )
{
ip	     = fetch_word( ss, sp + 0 ) ;
cs	     = fetch_word( ss, sp + 2 ) ;
flags	     = fetch_word( ss, sp + 4 ) ;
sp	    += 6 ;
}

static void
quick_run( void )
{
full_speed_run( fetch_word( ss, sp + 2 ), fetch_word( ss, sp + 0 ) ) ;
}

static void
simulate_terminate( void )
{
	void	interrupt	(*terminate_addr)() ;

terminate_addr = ((PSP far *)MK_FP( program_psp, 0 ))->terminate ;
if( terminate_addr == MK_FP( _CS, FP_OFF( program_terminate ) ) )
	quick_run() ;
else	full_speed_run( FP_SEG( terminate_addr ), FP_OFF( terminate_addr ) ) ;
}

static void
simulate_exec( void )
{
	EXT_EPB 	local_epb ;
	WORD		fl ;

local_epb = *(EXT_EPB far *)MK_FP( es, bx ) ;
set_psp( program_psp ) ;
__emit__( 0x1E ) ;		/*	push	ds		*/
_ES = _DS ;
_DX = (WORD) dx ;
_BX = (WORD) &local_epb ;
_AX = 0x4B01 ;
_DS = ds ;
geninterrupt( 0x21 ) ;
__emit__( 0x1F ) ;		/*	pop	ds		*/
__emit__( 0x9C, 0x59 ) ;	/*	pushf,	pop cx		*/
if( ( fl = _CX ) & FL_CF ){
	ax    = _AX ;
	simulate_iret() ;
	flags = ( fl | FL_RESET ) & FL_SET ;
	}
else {
	program_psp = get_psp() ;
	set_psp( debug_psp ) ;
	simulate_iret() ;
	((PSP FAR *)MK_FP( program_psp, 0 ))->terminate = MK_FP( cs, ip ) ;
	fill_program_registers( &local_epb ) ;
	}
}

void
execute_system_ISR( WORD new_cs, WORD new_ip )
{
	void interrupt	(*isr)( void ) ;

if( new_cs == INVALID_CS_SELECTOR ){
	dos_services:;
	if( new_ip >= INTERRUPTS_COUNT ){
		bp_type = BP_INTERNAL ;
		return ;
		}
	isr  = getvect( new_ip ) ;
	cs   = FP_SEG( isr ) ;
	ip   = FP_OFF( isr ) ;
	switch( new_ip ){
		case 0x20:		/* DOS terminate	*/
			if( ! watch_exec ) break ;
			simulate_terminate() ;
			return ;
		case 0x21:		/* DOS services 	*/
			switch( ah ){
				case 0x00:	/* DOS terminate	*/
					if( ! watch_exec ) break ;
					simulate_terminate() ;
					return ;
				case 0x25:	/* set interrupt vector */
					store_word( 0, (WORD) al * 4 + 0, dx ) ;
					store_word( 0, (WORD) al * 4 + 2, ds ) ;
					simulate_iret() ;
					return ;
				case 0x31:	/* DOS Keep		*/
					if( ! watch_exec ) break ;
					simulate_terminate() ;
					return ;
				case 0x35:	/* get interrupt vector */
					bx = fetch_word( 0, (WORD) al * 4 + 0 ) ;
					es = fetch_word( 0, (WORD) al * 4 + 2 ) ;
					simulate_iret() ;
					return ;
				case 0x4B:	/* DOS Exec		*/
					if( ! watch_exec ) break ;
					if( al != 0 ) break ;
					simulate_exec() ;
					return ;
				case 0x4C:	/* DOS Exit		*/
					if( ! watch_exec ) break ;
					simulate_terminate() ;
					return ;
				}
			break ;
		case 0x27:		/* DOS TSR		*/
			if( ! watch_exec ) break ;
			simulate_terminate() ;
			return ;
		}
	}
else {
	if( watch_exec ){
		if( MK_FP( new_cs, new_ip ) == getvect( 0x21 ) ){
			new_cs = INVALID_CS_SELECTOR ;
			new_ip = 0x21 ;
			goto dos_services ;
			}
		else
		if( MK_FP( new_cs, new_ip ) == getvect( 0x20 ) ){
			new_cs = INVALID_CS_SELECTOR ;
			new_ip = 0x20 ;
			goto dos_services ;
			}
		}
	cs   = new_cs ;
	ip   = new_ip ;
	}
quick_run() ;
return ;
}

int
break_requested( void )
{
if( ( peekb( 0, 0x417 ) & 0x0E ) == 0x0E ){
	if( journal_status == J_READING ) close_journal() ;
	return 1 ;
	}
return 0 ;
}

void
revive_screen( void )
{
_AH = 0x0f ;
geninterrupt( 0x10 ) ;
_AH = 0 ;
geninterrupt( 0x10 ) ;
}

WORD
program_memtop( void )
{
if( program_psp != NULL )
	return ((PSP FAR *)MK_FP( program_psp, 0 ))->mem_top ;
else	return 0x9FFFu ;
}

static	int	jrnl	 = -1 ;
static	int	cnt_rest = MAX_COMMANDS_IN_BUF ;

char *
hgets( char *buf, int size, int handle )
{
	char	*p ;

for( p = buf ; size > 0 && read( handle, p, 1 ) == 1 ; p++, size-- )
	if( *p == '\n' ) break ;
*p = 0 ;
return p == buf ? NULL : buf ;
}

void
cgets( char *buf )
{
	char	temp[ 92 ] ;

do {
	if( journal_status == J_READING ){
		if( eof( jrnl ) )
			close_journal() ;
		else {
			if( hgets( temp + 2, 90, jrnl ) != NULL ){
				strcat( temp + 2, "\r" ) ;
				cputs( temp + 2 ) ;
				*strchr( temp + 2, '\r' ) = 0 ;
				break ;
				}
			}
		}
	temp[ 0 ] = sizeof temp - 2 ;
	_DX = FP_OFF( temp ) ;
	_AH = 0x0A ;
	geninterrupt( 0x21 ) ;
	temp[ 2 + temp[ 1 ] ] = 0 ;
	if( journal_status == J_WRITING ){
		write( jrnl, temp + 2, strlen( temp + 2 ) ) ;
		write( jrnl, "\n", 1 ) ;
		if( --cnt_rest <= 0 ){
			cnt_rest = MAX_COMMANDS_IN_BUF ;
			close( jrnl ) ;
			if( ( jrnl = open( journal_name, O_WRONLY | O_APPEND | O_TEXT ) ) == -1 ){
				perror( journal_name ) ;
				journal_status = J_CLOSED ;
				}
			}
		}
	} while( 0 ) ;
strcpy( buf, temp + 2 ) ;
cputs( "\n" ) ;
return ;
}

void
cputc( char c )
{
static	char	s[2] = { 0, 0 } ;

s[0] = c ;
cputs( s ) ;
}

void
cputs( char *s )
{
write( STDERR_HANDLE, s, strlen( s ) ) ;
}

void
cprint( char *fmt, ... )
{
	char		buf[ 120 ] ;
	va_list 	p ;

va_start( p, fmt ) ;
vsprintf( buf, fmt, p ) ;
va_end( p ) ;
cputs( buf ) ;
}

void
close_journal( void )
{
if( jrnl != -1 )
	close( jrnl ) ;
journal_status = J_CLOSED ;
}

void
start_journal_write( char *name )
{
close_journal() ;
strcpy( journal_name, name ) ;
absolute_name( journal_name ) ;
if( ( jrnl = creat( journal_name, S_IFREG | S_IREAD | S_IWRITE ) ) == -1 ){
	perror( journal_name ) ;
	return ;
	}
journal_status = J_WRITING ;
}

void
start_journal_read( char *name )
{
close_journal() ;
strcpy( journal_name, name ) ;
absolute_name( journal_name ) ;
if( ( jrnl = open( journal_name, O_RDONLY | O_TEXT ) ) == NULL ){
	perror( journal_name ) ;
	return ;
	}
journal_status = J_READING ;
}

void
save_memory_area( int handle, WORD seg_start, WORD seg_end )
{
	WORD	len ;

for( ; seg_start < seg_end ; seg_start += len ){
	len = min( 0x800, seg_end - seg_start ) ;
	if( huge_write( handle, MK_FP( seg_start, 0 ), len << 4 ) != ( len << 4 ) ){
		cprint( "Write failed - disk full ?\r\n" ) ;
		return ;
		}
	}
}

WORD
allocate_segment( WORD size )
{
_BX = size ;
_AH = 0x48 ;
geninterrupt( 0x21 ) ;
__emit__( 0x9C, 0x59 ) ;	/*	pushf,	pop cx		*/
if( _CX & FL_CF ){
	errno = _AX ;
	return 0 ;
	}
else	return _AX ;
}

void
free_segment( WORD seg )
{
_ES = seg ;
_AH = 0x49 ;
geninterrupt( 0x21 ) ;
}

void
grow_by_one_segment( void )
{
	WORD	len ;

len = *(WORD FAR *)( ((BYTE FAR *)MK_FP( _psp - 1, 0 )) + 3 ) + 1 ;
_ES = _psp ;
_BX = len ;
_AH = 0x4A ;
geninterrupt( 0x21 ) ;
}

void LIBSYM
perror( const char *s )
{
cprint( "%s - %s\r\n", s, strerror( errno ) ) ;
}

char * LIBSYM
strerror( int errnum )
{
static	char	buf[ 100 ] ;

sprintf( buf, "Error code %d, DOS error code %d\r\n", errnum, _doserrno ) ;
return buf ;
}

long
get_file_size( char *file )
{
	unsigned	seg, off ;

_DX = FP_OFF( file ) ;
_CX = 0x23 ;
_AH = 0x4E ;
geninterrupt( 0x21 ) ;		/*	Find first file 	*/
__emit__( 0x9C, 0x59 ) ;	/*	pushf,	pop cx		*/
_doserrno = _AX ;
if( _CX & FL_CF )
	return -1L ;
_AH = 0x2F ;
geninterrupt( 0x21 ) ;		/*	Get DTA 		*/
seg = _ES ;
off = _BX + 0x1A ;
return *(long far *)MK_FP( seg, off ) ;
}
