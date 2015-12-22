#if 0 

  File name:  edb.c, command processing loop
 
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
#include <string.h>
#include <setjmp.h>
#include <ctype.h>
#include <mem.h>
#include <dos.h>
#include "cpu8086.h"
#include "edb.h"
#include "dssm8086.h"
#include "os.h"
#include "accessory.h"
#include "sysbus.h"
#include "config.h"
#include "fullspeed.h"
#include "checksize.h"

/*
 *	Modification history.
 *
 *	15 Dec 91
 *		GF will now refuse to proceed while single-stepping.
 *		G  will now accept stopping address as argument.
 *		EDB will now break out on attempt to implicitly run full
 *			speed while single-stepping.
 *	18 Dec 91 released version 0.15
 */

#define UPA_SIZE		0x80
#define CHECK_BREAK_LOOP	5000

unsigned	_stklen  = 4 * 1024 ;
unsigned	_heaplen = 0 ;

static	char	program_name[ MAX_FILE_NAME ] ;
static	char	program_params[ UPA_SIZE ] ;
static	int	program_loaded = 0 ;
static	jmp_buf debug_restart_point ;
static	char	*restart_cause ;
static	char	warn_system_trace = 1 ;
enum	{	DS_TRACE,	DS_QUIT,	DS_SUSPEND,
		DS_FULLSPEED
	} ;
static	int	debug_state = DS_TRACE ;
	WORD	low_jump_margin ;
	WORD	high_jump_margin ;
	char	fake_command = 0 ;
	BYTE	fake_ID = NO_CMD_FAKE ;

enum	{
	C_NONE, 	C_QUIT, 	C_HELP, 	C_REGISTERS,
	C_UNASSEMBLE,	C_TRACE,	C_DUMP, 	C_EXAMINE,
	C_GO,		C_BREAK,	C_RESUME,	C_PASS,
	C_INT,		C_REVIVE,	C_VIEWTRACE,	C_JOURNAL,
	C_WRITE,	C_IN,		C_OUT,		C_EXCLUDE,
	C_INVALID
	} ;

static int
parce_command( char *p )
{
	char	*o ;

while( isspace( *p ) ) p++ ;
if( *p == 0 ) return C_NONE ;
setmem( cmd_name, sizeof cmd_name, 0 ) ;
for( o = cmd_name ; ! isspace( *p ) && *p != 0 ; o++, p++ ) *o = *p ;
while( isspace( *p ) ) p++ ;
if( *p == 0 )
	arg_start = NULL ;
else	arg_start = p ;
switch( cmd_name[0] ){
	case ';': return C_NONE ;
	case 'Q': return C_QUIT ;
	case 'H':
	case '?': return C_HELP ;
	case 'R': return C_REGISTERS ;
	case 'U': return C_UNASSEMBLE ;
	case 'T': return C_TRACE ;
	case 'D': return C_DUMP ;
	case 'E': return C_EXAMINE ;
	case 'G': return C_GO ;
	case 'B': return C_BREAK ;
	case 'S': return C_RESUME ;
	case 'P': return C_PASS ;
	case 'N': return C_INT ;
	case '!': return C_REVIVE ;
	case 'V': return C_VIEWTRACE ;
	case 'J': return C_JOURNAL ;
	case 'W': return C_WRITE ;
	case 'I': return C_IN ;
	case 'O': return C_OUT ;
	case 'X': return C_EXCLUDE ;
	default:  return C_INVALID ;
	}
}

static int
get_command( void )
{
	char	*prompt ;

switch( debug_state ){
	case DS_SUSPEND:	prompt = ">" ; break ;
	default:		prompt = "]" ; break ;
	}
cputs( prompt ) ;
if( fake_command ){
	fake_command = 0 ;
	cputs( cmd_buf ) ;
	cputs( "\r\n" ) ;
	}
else {
	cgets( cmd_buf ) ;
	}
strupr( cmd_buf ) ;
return parce_command( cmd_buf ) ;
}

static void
terminate_signal( void )
{
program_loaded = 0 ;
if( debug_state == DS_FULLSPEED ){
	debug_state = DS_SUSPEND ;
	full_speed_return_point() ;
	}
if( debug_state == DS_QUIT ) return ;
restart_cause  = "Program completed execution." ;
longjmp( debug_restart_point, 1 ) ;
}

void FASTCALL
CPU_error( char *message )
{
restart_cause = message ;
cs = instruction_cs ;
ip = instruction_ip ;
restart_prefetch() ;
longjmp( debug_restart_point, 1 ) ;
}

static void
quit_execution( void )
{
debug_state = DS_QUIT ;
if( program_loaded )
	terminate_program() ;
else	exit( 0 ) ;
}

static void
check_code_breaks( void )
{
	int		i ;
	CODE_BREAK	*bp ;

for( i = code_bps, bp = code_bp_table ; i > 0 ; i--, bp++ )
	if( cs >= bp->min_cs && cs <= bp->max_cs &&
	    ip >= bp->min_ip && ip <= bp->max_ip ){
		if( bp->cmd_fake_ID != NO_CMD_FAKE ){
			strcpy( cmd_buf, cmd_fake_table[ bp->cmd_fake_ID ].text ) ;
			fake_command = 1 ;
			}
		restart_cause = "Code range breakpoint" ;
		longjmp( debug_restart_point, 1 ) ;
		}
}

void
analyse_breakpoint( void )
{
static	char	buf[ 80 ] ;

switch( bp_type ){
	case BP_NONE: return ;
	case BP_IOR:
	case BP_IOW:
		sprintf( buf, "I/O %s at %04X", "breakpoint", bp_port ) ;
		restart_cause = buf ;
		cs = instruction_cs ; ip = instruction_ip ;
		backspace_prefetch() ;
		break ;
	case BP_MEMR:
		sprintf( buf, "Memory read %s at %04X:%04X", "breakpoint", bp_seg, bp_off ) ;
		restart_cause = buf ;
		break ;
	case BP_MEMWB:
		sprintf( buf, "%semory write %s at %04X:%04X, data = %02X",
			bp_count > 1 ? "Multiply m" : "M", "breakpoint", bp_seg, bp_off, bp_dat ) ;
		restart_cause = buf ;
		bp_count      = 0 ;
		debug_state   = DS_SUSPEND ;
		break ;
	case BP_MEMWW:
		sprintf( buf, "%semory write %s at %04X:%04X, data = %04X",
			bp_count > 1 ? "Multiply m" : "M", "breakpoint", bp_seg, bp_off, bp_dat ) ;
		restart_cause = buf ;
		bp_count      = 0 ;
		debug_state   = DS_SUSPEND ;
		break ;
	case BP_SEGEND:
		sprintf( buf, "Illegal word memory access %04X:%04X", bp_seg, bp_off ) ;
		restart_cause = buf ;
		break ;
	case BP_INTERNAL:
		restart_cause = "Internal EDB error detected" ;
		break ;
	case BP_BREAKOUT:
		restart_cause = "Program attempts to single-step in full speed area" ;
		break ;
	case BP_RELEASE:
		restart_cause = "CPU control is about to be released to program" ;
		break ;
	default:
		restart_cause = "Unknown breakpoint type." ;
		break ;
	}
if( fake_ID != NO_CMD_FAKE ){
	fake_command = 1 ;
	strcpy( cmd_buf, cmd_fake_table[ fake_ID ].text ) ;
	fake_ID = NO_CMD_FAKE ;
	}
longjmp( debug_restart_point, 1 ) ;
}

static void
execute_instruction( void )
{
if( resume_trace == 0 ){
	check_code_breaks() ;
	if( watch_prefetch_queue )
		if( prefetch_queue_not_match() ){
			restart_cause = "Prefetch queue not match memory data" ;
			longjmp( debug_restart_point, 1 ) ;
			}
	}
next_trace->cs = cs ;
next_trace->ip = ip ;
if( --next_trace < trace_buf )
	next_trace = trace_buf + MAX_TRACE_RECORDS - 1 ;
if( trace_count != 0 ) trace_count-- ;
bp_type  = BP_NONE ;
bp_count = 0 ;
do {
	if( cs == INVALID_CS_SELECTOR || cs < low_jump_margin || cs > high_jump_margin ){
	/*
	 *	Program transferred control to one of system ISR's.
	 *	it should be executed at full speed.
	 */
		if( watch_full_speed_run && resume_trace == 0 ){
			bp_type = BP_RELEASE ;
			break ; /* to breakpoint analyser */
			}
		if( flags & FL_TF && warn_system_trace ){
			/*
			 *	Emulation scope selected incorrectly, as the program
			 *	attempts to single-step into black-box area.
			 */
			bp_type = BP_BREAKOUT ;
			break ; /* to breakpoint analyser */
			}
		pass_memory_write = 1 ;
		execute_system_ISR( cs, ip ) ;
		pass_memory_write = 0 ;
		restart_prefetch() ;
		break ; /* to breakpoint analyser */
		}
	decode_instruction() ;
	resume_trace = 0 ;
	} while( 0 ) ;
analyse_breakpoint() ;
}

static void
trace( void )
{
	int	trace_count ;

if( arg_start != NULL ){
	if( sscanf( arg_start, "%d", &trace_count ) != 1 ){
		cprint( "Invalid decimal number '%s'\r\n", arg_start ) ;
		return ;
		}
	}
else	trace_count = 1 ;
resume_trace = 1 ;
while( trace_count-- > 0 )
	execute_instruction() ;
print_status() ;
}

static void
pass_instruction( void )
{
	int	i ;
	char	dump[ 80 ] ;
	WORD	end_cs, end_ip ;

if( cs == INVALID_CS_SELECTOR ){
	trace() ;
	return ;
	}
end_cs = cs ;
end_ip = disassemble( cs, ip, dump, dump ) ;
resume_trace = 1 ;
while( 1 ){
	for( i = 0 ; i < CHECK_BREAK_LOOP ; i++ ){
		execute_instruction() ;
		if( cs == end_cs && ip == end_ip ){
			print_status() ;
			return ;
			}
		}
	if( break_requested() ) break ;
	}
print_status() ;
}

static void
skip_command( void )
{
	char	dump[ 80 ] ;

ip = disassemble( cs, ip, dump, dump ) ;
restart_prefetch() ;
print_status() ;
}

static void
emu_run_program( WORD stop_cs, WORD stop_ip )
{
	int	i ;

resume_trace = 1 ;
while( 1 ){
	for( i = 0 ; i < CHECK_BREAK_LOOP ; i++ ){
		execute_instruction() ;
		if( cs == stop_cs && ip == stop_ip ) return ;
		}
	if( break_requested() ) return ;
	}
}

static void
run_program( void )
{
	WORD	stop_cs = cs, stop_ip ;

switch( cmd_name[ 1 ] ){
	default:
		stop_ip = 0xFFFF ;
		if( decode_address( &stop_cs, &stop_ip ) == -1 )
			return ;
		if( stop_ip == 0xFFFF ) stop_cs = 0xFFFF ;
		emu_run_program( stop_cs, stop_ip ) ;
		break ;
	case 'F':
		if( flags & FL_TF ){
			cprint( "Program is now single-stepping. Can't run at full speed\r\n" ) ;
			return ;
			}
		if( arg_start == NULL ){
			cprint( "Argument missing !\r\n" ) ;
			return ;
			}
		else
		if( decode_address( &stop_cs, &stop_ip ) == -1 )
			return ;
		debug_state = DS_FULLSPEED ;
		if( full_speed_run( stop_cs, stop_ip ) == -1 )
			return ;
		restart_prefetch() ;
		if( debug_state == DS_SUSPEND )
			terminate_signal() ;
		else	debug_state = DS_TRACE ;
		break ;
	}
print_status() ;
}

static void
resume_execution( void )
{
debug_state = DS_TRACE ;
if( cmd_name[ 1 ] == 'C' ) return ;
pass_memory_write = 1 ;
switch( bp_type ){
	case BP_MEMWB: store_byte( bp_seg, bp_off, bp_dat ) ; break ;
	case BP_MEMWW: store_word( bp_seg, bp_off, bp_dat ) ; break ;
	default:       cprint( "Nothing to resume." ) ;       break ;
	}
pass_memory_write = 0 ;
}

static void
debug_help( void )
{
#ifndef NO_VERBOSE_COMMANDS
static	char	*help_lines[] = {
	"BL\t\t\t  - list breakpoints",
	"BP [seg[-seg]:]off[-off]  - set code breakPoint",
	"BM[R,W] XXXXX-XXXXX\t  - set Memory access breakpoint",
	"BI[R,W] nnn-nnn \t  - set I/O breakpoint",
	"\t\t\tActions could be used with these breakpoints",
	"B?? -number\t\t  - clear breakpoint",
	"BF {On,Off}\t\t  - set/clear breakpoint on passing control to CPU",
	"BQ {On,Off}\t\t  - set/clear prefetch queue breakpoint",
	"D[B,W,D] [addr]\t\t  - Dump memory",
	"E[B,W,D] [addr]\t\t  - modify memory",
	"G [addr]\t\t  - Go",
	"GF addr \t\t  - run program at Full speed on real CPU",
	"I[B,W] port\t\t  - I/O",
	"J{S,C}\t\t\t  - Journal {status,close}",
	"J{R,W} name\t\t  - open journal file for {read,write}",
	"N{I,T,N,F}\t\t  - simulate Int/ireT/retN/retF",
	"O[B,W] port\t\t  - I/O",
	"P\t\t\t  - steP",
	"Q\t\t\t  - Quit EDB",
	"R [name] [value]\t  - display or modify CPU Registers",
	"S[C]\t\t\t  - reSume after memory write breakpoint",
	"T [count]\t\t  - Trace",
	"U [addr]\t\t  - Unassemble",
	"V [count]\t\t  - View trace buffer",
	"W file [top]\t\t  - Write core dump",
	"X\t\t\t  - skip neXt command",
	"!\t\t\t  - cls",
	NULL
	} ;
	int	i ;
	char	**p ;
	char	dummy[ 80 ] ;

for( p = help_lines, i = 0 ; *p != NULL ; p++ ){
	cputs( *p ) ; cputs( "\r\n" ) ;
	if( ++i >= 20 ){
		i = 0 ;
		cputs( "\tPress Enter to continue." ) ;
		cgets( dummy ) ;
		}
	}
#endif NO_VERBOSE_COMMANDS
}

static void
start_debugger( void )
{
if( setjmp( debug_restart_point ) )
	cprint( "%s\r\n", restart_cause ) ;
if( program_loaded )
	print_status() ;
else	debug_state = DS_SUSPEND ;
while( 1 ){
	switch( get_command() ){
		case C_NONE:	   continue ;
		case C_QUIT:	   quit_execution() ;	     break ;
		case C_HELP:	   debug_help() ;	     break ;
		case C_REGISTERS:  registers_display() ;     break ;
		case C_UNASSEMBLE: bulk_unassemble() ;	     break ;
		case C_DUMP:	   dump_memory() ;	     break ;
		case C_EXAMINE:    examine_memory() ;	     break ;
		case C_BREAK:	   set_breakpoint() ;	     break ;
		case C_REVIVE:	   revive_screen() ;	     break ;
		case C_VIEWTRACE:  view_trace_buffer() ;     break ;
		case C_JOURNAL:    journal_functions() ;     break ;
		case C_WRITE:	   write_core_dump() ;	     break ;
		case C_IN:	   input_io() ; 	     break ;
		case C_OUT:	   output_io() ;	     break ;
		case C_TRACE:
			if( debug_state != DS_TRACE ){
				cprint( "'%s' invalid for current mode\r\n", cmd_name ) ;
				continue ;
				}
			trace() ;
			break ;
		case C_EXCLUDE:
			if( debug_state != DS_TRACE ){
				cprint( "'%s' invalid for current mode\r\n", cmd_name ) ;
				continue ;
				}
			skip_command() ;
			break ;
		case C_PASS:
			if( debug_state != DS_TRACE ){
				cprint( "'%s' invalid for current mode\r\n", cmd_name ) ;
				continue ;
				}
			pass_instruction() ;
			break ;
		case C_GO:
			if( debug_state != DS_TRACE ){
				cprint( "'%s' invalid for current mode\r\n", cmd_name ) ;
				continue ;
				}
			run_program() ;
			break ;
		case C_INT:
			if( debug_state != DS_TRACE ){
				cprint( "'%s' invalid for current mode\r\n", cmd_name ) ;
				continue ;
				}
			simulate_int() ;
			break ;
		case C_RESUME:
			if( debug_state != DS_SUSPEND ){
				cprint( "'%s' invalid for current mode\r\n", cmd_name ) ;
				continue ;
				}
			resume_execution() ;
			break ;
		case C_INVALID:
			cprint( "Can't understand '%s'\r\n", cmd_name ) ;
			continue ;
		}
	}
}

static int
init_software_interface( void )
{
	char	cfg[ MAX_CFG_STRING ] ;
	char	*p ;

low_jump_margin  = program_psp ;
high_jump_margin = program_memtop() ;
if( open_config_section( "DOS software" ) == -1 ) return 0 ;
while( get_config_string( cfg ) != -1 ){
	if( ( p = strchr( cfg, '=' ) ) == NULL ){
		cprint( "Invalid configuration string '%s'\r\n", cfg ) ;
		return -1 ;
		}
	p += strspn( p + 1, " \t" ) + 1 ;
	if( strncmp( cfg, "ProgramBottom",  13 ) == 0 ){
		if( sscanf( p, "%x", &low_jump_margin ) != 1 ){
			cprint( "Invalid configuration string '%s'\r\n", cfg ) ;
			return -1 ;
			}
		if( low_jump_margin == 0 )
			low_jump_margin = program_psp ;
		}
	else
	if( strncmp( cfg, "ProgramTop",  10 ) == 0 ){
		if( sscanf( p, "%x", &high_jump_margin ) != 1 ){
			cprint( "Invalid configuration string '%s'\r\n", cfg ) ;
			return -1 ;
			}
		}
	else
	if( strncmp( cfg, "WatchExec",    9 ) == 0 )
		watch_exec = process_YesNo( p ) ;
	else {
		cprint( "Unrecognized configuration option '%s'\r\n", cfg ) ;
		return -1 ;
		}
	}
close_config() ;
return 0 ;
}

static void
edb_title( void )
{
#ifndef NO_VERBOSE_COMMANDS
cprint( "Emulating debugger v. " VERSION
#ifdef	TARGET_8086
"/XT"
#endif
" (C) 1991 Serge Pachkovsky\r\n" ) ;
#endif
}

static void
help( void )
{
#ifndef NO_VERBOSE_COMMANDS
cprint( "Usage is : EDB [/G] [/I] [/J:name] program\r\n" ) ;
#endif
}

static char *
copy_till_blank( char *to, char *from )
{
for( ; ! isspace( *from ) && *from != 0 ; from++, to++ ) *to = *from ;
*to = 0 ;
return from ;
}

static int
parce_command_line( char *p )
{
	char	temp[ 80 ] ;

while( *p != 0 ){
	while( isspace( *p ) && *p != 0 ) p++ ;
	if( switchar( *p ) ){
		switch( toupper( *++p ) ){
			case 'G':
				grow_by_one_segment() ;
				break ;
			case 'I':
				disable_local_IDT = 1 ;
				break ;
			case 'J':
				if( p[1] != 0 && p[2] != 0 ){
					p = copy_till_blank( temp, p + 2 ) ;
					strcat( temp, ".JOU" ) ;
					start_journal_read( temp ) ;
					break ;
					}
			default:
				cprint( "Illegal switch '%c'\r\n", *p ) ;
				return -1 ;
			}
		while( ! isspace( *p ) && *p != 0 ) p++ ;
		}
	else	break ;
	}
p = copy_till_blank( program_name, p ) ;
strcpy( program_params, p ) ;
return 0 ;
}

static int
parce_parameters( int argc, char **argv )
{
#ifndef __MSDOS__
	char	cmd_line[ 200 ] ;

for( argc--, argv++ ; argc > 0 ; argc--, argv++ ){
	strcat( cmd_line, argv[0] ;
	if( argc > 1 ) strcat( cmd_line, " " ) ;
	}
return parce_command_line( cmd_line ) ;
#else
#ifndef __TINY__
	#error Code in the following two lines depends upon being compiled in tiny model!
#endif
*( (char *)0x81 + *(unsigned char _ss *)0x80 ) = 0 ;
return parce_command_line( (char *)0x81 ) ;
#endif
}

static int LIBSYM
ctrlbrk_noop( void )
{
return 1 ;
}

static void
eat_low_memory( void )
{
	WORD	seg ;

while( ( seg = allocate_segment( 0 ) ) < _psp ) ;
free_segment( seg ) ;
}

static int
set_trace_buffer_size( char *p )
{
	unsigned	cnt ;

if( sscanf( p, "%d", &cnt ) != 1 ){
	cprint( "Invalid trace records count : '%s'\r\n", p ) ;
	return -1 ;
	}
return init_trace_buffer( cnt ) ;
}

static int
init_debug_options( void )
{
	char	cfg[ MAX_CFG_STRING ] ;
	char	*p ;

if( init_trace_buffer( DEFAULT_TRACE_BUFFER ) == -1 ) return -1 ;
if( open_config_section( "Debug options" ) == -1 ) return -1 ;
while( get_config_string( cfg ) != -1 ){
	if( ( p = strchr( cfg, '=' ) ) == NULL ){
		cprint( "Invalid configuration string '%s'\r\n", cfg ) ;
		return -1 ;
		}
	p += strspn( p + 1, " \t" ) + 1 ;
	if( strncmp( cfg, "WarnSysTrace",   12 ) == 0 )
		warn_system_trace = process_YesNo( p ) ;
	else
	if( strncmp( cfg, "TraceRecords",   12 ) == 0 ){
		if( set_trace_buffer_size( p ) == -1 ) return -1 ;
		}
	else {
		cprint( "Unrecognized configuration option '%s'\r\n", cfg ) ;
		return -1 ;
		}
	}
close_config() ;
return 0 ;
}

int LIBSYM
main( int argc, char *argv[] )
{
edb_title() ;
if( parce_parameters( argc, argv ) == -1 ){
	help() ;
	return -1 ;
	}
#ifndef NO_VERBOSE_COMMANDS
check_size() ;
#endif
ctrlbrk( ctrlbrk_noop ) ;
eat_low_memory() ;
if( init_debug_options() == -1 )
	return -1 ;
if( init_system_bus() == -1 )
	return -1 ;
if( init_CPU_parameters() == -1 )
	return -1 ;
if( strlen( program_name ) != 0 ){
	if( load_program( program_name, program_params, terminate_signal ) == -1 ){
		cprint( "%s: %s\r\n", program_name, strerror( errno ) ) ;
		return -1 ;
		}
	else {
		program_loaded = 1 ;
		cprint( "PSP = %04X\r\n", program_psp ) ;
		}
	}
if( init_software_interface() == -1 ){
	quit_execution() ;
	return -1 ;
	}
if( check_hardware() == -1 )
	cprint( "Warning - IDT relocation disabled\r\n" ) ;
start_debugger() ;
return 0 ;
}
