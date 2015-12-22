#if 0 
 
  File name:  os.h

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
 *	OS and targer machine dependent procedures.
 */
#define MAX_FILE_NAME	80

/*
 *	Default debug console name.
 */
#define TTY_NAME	"con"
#define STDERR_HANDLE	2
void	cin( char *buf ) ;
void	cprint( char *fmt, ... ) ;
void	cputs( char *str ) ;
void	cputc( char c ) ;
void	cgets( char *buf ) ;
char	*hgets( char *buf, int size, int handle ) ;
/*
 *	switchar( x ) returns non-zero if c is valid command
 *	switch character.
 */
int	switchar( char c ) ;
/*
 *	load_program( name, param ) loads program image from name
 *	with arguments param.
 *	Sets all emulator registers (including cs:ip and ss:sp )
 *	to correct values.
 *	Returns -1 (and sets errno) if load failed.
 */
int	load_program( char *name, char *param, void (*term_prog)(void) ) ;
extern	WORD	program_psp ;
extern	WORD	debug_psp ;
/*
 *	set_psp(psp) and get_psp(psp) - nothing more
 */
void	set_psp( WORD psp ) ;
WORD	get_psp( void ) ;
/*
 *	absolute_name(s) builds absolute file name (which access file 's'
 *	regardless of current directory, etc.) from relative file name s
 */
void	absolute_name( char *s ) ;
/*
 *	terminate_program() quits execution of currently debugged program
 */
void	terminate_program( void ) ;
/*
 *	execute_system_ISR( n ) simulates standart system int nn action
 */
void	execute_system_ISR( WORD cs, WORD ip ) ;
/*
 *	break_requested() returns non-zero if LShift-Ctrl-Alt pressed.
 */
int	break_requested( void ) ;
/*
 *	revive_screen() sets video mode equal to current one.
 */
void	revive_screen( void ) ;
/*
 *	program_memtop() returns (segment addr. of) top of program memory.
 */
WORD	program_memtop( void ) ;
/*
 *	get_file_size() returns length of file or -1L if error (and sets errno).
 */
long	get_file_size( char *name ) ;
/*
 *
 */
void	close_journal( void ) ;
void	start_journal_write( char *name ) ;
void	start_journal_read( char *name ) ;

#define MAX_COMMANDS_IN_BUF	5

extern	char	journal_name[ MAX_FILE_NAME ] ;
extern	char	journal_status ;
enum {	J_CLOSED,  J_READING,  J_WRITING } ;


void	save_memory_area( int handle, WORD seg_start, WORD seg_end ) ;
void	grow_by_one_segment( void ) ;
WORD	allocate_segment( WORD size ) ;
void	free_segment( WORD seg ) ;
void LIBSYM copy_RAM( WORD dest, WORD src, WORD size ) ;
/*
 *	setup_vector_monitor( vec, space ) will call function
 *	interrupt_monitor each time then interrupt 'vec' occure
 *	scratch should be at least MONITOR_SPACE bytes.
 *	interrupt_monitor should not make any assumptions on ss:sp
 *	location.
 */

#ifndef __MONITOR_DEFINED__
#define __MONITOR_DEFINED__
typedef struct {
	char		call[ 6 ] ;
	WORD		code ;
	void interrupt	(*old_vec)( void ) ;
	} MONITOR ;
#endif

void LIBSYM setup_vector_monitor( WORD vector, MONITOR *scratch ) ;
void LIBSYM interrupt_monitor( MONITOR *mon ) ;
/*
 *	Magic values in CPU registers after EXEC
 */
extern	WORD	entry_ax ;
extern	WORD	entry_bx ;
extern	WORD	entry_cx ;
extern	WORD	entry_bp ;
/*
 *	Setting watch_exec to 1 will cause EDB to emulate DOS exec and terminate
 *	functions, thus leaving parent processes inside virtual CPU.
 */
extern	int	watch_exec ;
