#if 0 

  File name:  accessor.c, printing and breakpoint manipulation code
 
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
#include <ctype.h>
#include <io.h>
#include <fcntl.h>
#include <dos.h>
#include "cpu8086.h"
#include "dssm8086.h"
#include "accessory.h"
#include "os.h"
#include "sysbus.h"
#include "edb.h"

/*
 *	Revision history.
 *
 *	09 Dec 91
 *		Fixed bug in 'bp' command with action expression incorrectly recognised
 *			when offset range was used.
 *		Altered behaviour of 'bi' command with overlapping break points. Bi will
 *			now preserve read breakpoint when write one is removed and vv.
 *			Action expression will be preserved when no new action was
 *			specified.
 *	14 Dec 91
 *		Trace buf moved to far heap to conserve DGROUP space under MS DOS
 *		view_trace_buffer changed to show only screenful of trace records
 *	15 Dec 91
 *		Added breakpoint on prefetch queue different from memory (BQ)
 *
 *	18 Dec 91 released version 0.15
 */

#define DISASSEMBLE_LINES	10
#define DUMP_LINES		5

	char	cmd_buf[ 80 ], cmd_name[ 10 ] ;
	char	*arg_start ;
	int		code_bps = 0 ;
	CODE_BREAK	code_bp_table[ MAX_CODE_BPS ] ;
	TRACE_RECORD	FAR *trace_buf = NULL ;
	TRACE_RECORD	FAR *next_trace ;
	int		trace_count ;
	unsigned	MAX_TRACE_RECORDS ;
	CMD_FAKE	cmd_fake_table[ MAX_CMD_FAKES ] ;
	char		watch_prefetch_queue = 0 ;
	char		watch_full_speed_run = 0 ;


static	WORD	last_unassemble_cs, last_unassemble_ip ;
static	WORD	last_dump_seg, last_dump_off ;

typedef struct	{
	char	*name ;
	WORD	*reg ;
	} REGADDR ;

static	REGADDR regs_table[] = {
	{"AX", &ax}, {"BX", &bx}, {"CX", &cx}, {"DX", &dx}, {"SP", &sp},
	{"BP", &bp}, {"SI", &si}, {"DI", &di}, {"DS", &ds}, {"ES", &es},
	{"SS", &ss}, {"CS", &cs}, {"IP", &ip}, {"FLAGS", &flags},
	{NULL,NULL}
	} ;

static	WORD		fl_values[] = {
	FL_OF, FL_DF, FL_IF, FL_SF, FL_ZF, FL_AF, FL_PF, FL_CF, FL_TF, 0
	} ;
static	char		*names[] = {
	"OV", "NV", "DN", "UP", "EI", "DI", "NG", "PL", "ZR",
	"NZ", "AC", "NA", "PE", "PO", "CY", "NC", "TR", "NT"
	} ;


void
skip_one_arg( void )
{
arg_start = next_field( arg_start ) ;
}

char	*
next_field( char *p )
{
while( ! isspace( *p ) && *p != '-' && *p != ':' && *p != 0 ) p++ ;
while( isspace( *p ) ) p++ ;
if( *p == 0 )
	p = NULL ;
return p ;
}

int
arg_len( char *p )
{
	int	i ;

for( i = 0 ; ! isspace( *p ) && *p != '-' && *p != ':' && *p != 0 ; p++, i++ ) ;
return i ;
}

#ifdef	FAST_INSTRUCTION_FETCH
int
prefetch_queue_not_match( void )
{
return 0 ;
}

static void
show_prefetch_queue( void )
{
}
#else
int
prefetch_queue_not_match( void )
{
	BYTE	*p ;
	WORD	tmp_ip ;
	WORD	i ;

for( p = prefetch_head, i = prefetch_used, tmp_ip = ip ; i > 0 ; i--, tmp_ip++ ){
	if( *p != fetch_byte( cs, tmp_ip ) ) return 1 ;
	if( ++p >= prefetch_top ) p = prefetch_queue ;
	}
return 0 ;
}

static void
show_prefetch_queue( void )
{
	BYTE	*p ;
	WORD	i ;

cprint( "Prefetch:" ) ;
for( p = prefetch_head, i = prefetch_used ; i > 0 ; i-- ){
	cprint( " %02X", *p ) ;
	if( ++p >= prefetch_top ) p = prefetch_queue ;
	}
cprint( "\r\n" ) ;
}
#endif

static	WORD	*
search_register( char *name )
{
	REGADDR 	*p ;
	int		len ;

len = arg_len( name ) ;
for( p = regs_table ; p->name != NULL ; p++ )
	if( ! strncmp( p->name, name, len ) )
		return p->reg ;
return NULL ;
}

static WORD
print_disassembled( WORD seg, WORD off )
{
	char	buf[ 80 ] ;
	char	EA_buf[ 80 ] ;
	WORD	instr_len ;
	int	i ;

instr_len = disassemble( seg, off, buf, EA_buf ) - off ;
cprint( "%04X:%04X ", seg, off ) ;
for( i = 0 ; i < instr_len ; i++, off++ )
	cprint( "%02X", fetch_byte( seg, off ) ) ;
for(	   ; i < 10 ; i++ )
	cputs( "  " ) ;
cputs( buf ) ;
if( EA_buf[ 0 ] != 0 ){
	cputc( ' ' ) ;
	for( i = 34 - strlen( buf ) ; i > 0 ; i-- )
		cputc( ' ' ) ;
	cputs( EA_buf ) ;
	}
cputs( "\r\n" ) ;
return off ;
}

void
print_status( void )
{
static	WORD		old_flags ;
	WORD		*f ;
	char		**n ;

cprint( "AX=%04X  BX=%04X  CX=%04X  DX=%04X  SP=%04X  BP=%04X  SI=%04X  DI=%04X\r\n",
	ax, bx, cx, dx, sp, bp, si, di ) ;
cprint( "DS=%04X  ES=%04X  SS=%04X  CS=%04X  IP=%04X  ", ds, es, ss, cs, ip ) ;
for( f = fl_values, n = names ; *f != 0 ; f++, n += 2 )
	if( flags & *f )
		cprint( "%s%c", n[ 0 ], old_flags & *f ? ' ' : '*' ) ;
	else	cprint( "%s%c", n[ 1 ], old_flags & *f ? '*' : ' ' ) ;
cputs( "\r\n" ) ;
old_flags = flags ;
if( prefetch_queue_not_match() )
	show_prefetch_queue() ;
if( cs == INVALID_CS_SELECTOR ){
	cprint( "     About to execute original INT %02X\r\n", ip ) ;
	}
else {
	if( cs < low_jump_margin || cs > high_jump_margin )
		cprint( "     About to run on real CPU\r\n" ) ;
	last_unassemble_cs = cs ;
	last_unassemble_ip = print_disassembled( cs, ip ) ;
	}
}

void
registers_display( void )
{
	WORD	*reg ;
	char	*regname ;

if( arg_start != NULL ){
	if( ( reg = search_register( arg_start ) ) == NULL ){
		cprint( "Unrecognized register name '%s'\r\n", arg_start ) ;
		return ;
		}
	regname = arg_start ;
	skip_one_arg() ;
	if( arg_start == NULL ){
		cprint( "%s=%04X\r\n:", regname, *reg ) ;
		cgets( cmd_buf ) ;
		arg_start = cmd_buf ;
		}
	if( sscanf( arg_start, "%x", reg ) != 1 ){
		cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
		return ;
		}
	if( reg == &ip || reg == &cs )
		restart_prefetch() ;
	}
else	print_status() ;
}

int
decode_address( WORD *seg, WORD *off )
{
if( arg_start != NULL ){
	if( arg_start[ 1 ] == 'S' && arg_start[ 2 ] == ':' ){   /* ?S:  */
		switch( arg_start[ 0 ] ){
			case 'D': *seg = ds ; break ;
			case 'E': *seg = es ; break ;
			case 'C': *seg = cs ; break ;
			case 'S': *seg = ss ; break ;
			default:
				cprint( "Invalid segment register specified '%s'\r\n", arg_start ) ;
				return -1 ;
			}
		arg_start += 3 ;
		*off = 0 ;
		}
	else
	if( strchr( arg_start, ':' ) != NULL ){
		if( sscanf( arg_start, "%x", seg ) != 1 ){
			cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
			return -1 ;
			}
		arg_start = strchr( arg_start, ':' ) + 1 ;
		*off = 0 ;
		}
	if( sscanf( arg_start, "%x", off ) != 1 ){
		cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
		return -1 ;
		}
	skip_one_arg() ;
	}
return 0 ;
}

void
bulk_unassemble( void )
{
	int	i ;

if( decode_address( &last_unassemble_cs, &last_unassemble_ip ) == -1 )
	return ;
for( i = 0 ; i < DISASSEMBLE_LINES ; i++ )
	last_unassemble_ip = print_disassembled( last_unassemble_cs, last_unassemble_ip ) ;
}

static void
dump_bytes( void )
{
	int	i, j ;
	BYTE	c ;

for( i = 0 ; i < DUMP_LINES ; i++ ){
	cprint( "%04X:%04X  ", last_dump_seg, last_dump_off ) ;
	for( j = 0 ; j < 16 ; j++ )
		cprint( "%02X ", fetch_byte( last_dump_seg, last_dump_off + j * 1 ) ) ;
	cputc( ' ' ) ;
	for( j = 0 ; j < 16 ; j++ ){
		c = fetch_byte( last_dump_seg, last_dump_off + j * 1 ) ;
		if( c < ' ' )
			c = '.' ;
		cputc( c ) ;
		}
	cputs( "\r\n" ) ;
	last_dump_off += 16 ;
	}
}

static void
dump_words( void )
{
	int	i, j ;

for( i = 0 ; i < DUMP_LINES ; i++ ){
	cprint( "%04X:%04X  ", last_dump_seg, last_dump_off ) ;
	for( j = 0 ; j < 8 ; j++, last_dump_off += 2 )
		cprint( "%04X ", fetch_word( last_dump_seg, last_dump_off ) ) ;
	cputs( "\r\n" ) ;
	}
}

static void
dump_doublewords( void )
{
	int	i, j ;

for( i = 0 ; i < DUMP_LINES ; i++ ){
	cprint( "%04X:%04X  ", last_dump_seg, last_dump_off ) ;
	for( j = 0 ; j < 4 ; j++, last_dump_off += 4 ){
		cprint( "%04X", fetch_word( last_dump_seg, last_dump_off + 2 ) ) ;
		cprint( "%04X ", fetch_word( last_dump_seg, last_dump_off ) ) ;
		}
	cputs( "\r\n" ) ;
	}
}

void
dump_memory( void )
{
if( decode_address( &last_dump_seg, &last_dump_off ) == -1 )
	return ;
switch( cmd_name[ 1 ] ){
	case  0 :
	case 'B':  dump_bytes() ;        break ;
	case 'W':  dump_words() ;        break ;
	case 'D':  dump_doublewords() ;  break ;
	default:
		cprint( "Can't understand '%s'\r\n", cmd_name ) ;
	}
}

static void
examine_byte( void )
{
	WORD	x ;

if( arg_start == NULL ){
	cprint( "%04X:%04X  %02X.", last_dump_seg, last_dump_off,
			fetch_byte( last_dump_seg, last_dump_off ) ) ;
	cgets( cmd_buf ) ;
	if( strlen( cmd_buf ) == 0 ) return ;
	arg_start = cmd_buf ;
	}
if( sscanf( arg_start, "%x", &x ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return ;
	}
store_byte( last_dump_seg, last_dump_off, (BYTE)x ) ;
}

static void
examine_word( void )
{
	WORD	x ;

if( arg_start == NULL ){
	cprint( "%04X:%04X  %04X.", last_dump_seg, last_dump_off,
			fetch_word( last_dump_seg, last_dump_off ) ) ;
	cgets( cmd_buf ) ;
	if( strlen( cmd_buf ) == 0 ) return ;
	arg_start = cmd_buf ;
	}
if( sscanf( arg_start, "%x", &x ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return ;
	}
store_word( last_dump_seg, last_dump_off, x ) ;
}

static void
examine_doubleword( void )
{
	DWORD	x ;

if( arg_start == NULL ){
	cprint( "%04X:%04X  %04X%04X.", last_dump_seg, last_dump_off,
			fetch_word( last_dump_seg, last_dump_off + 2 ),
			fetch_word( last_dump_seg, last_dump_off ) ) ;
	cgets( cmd_buf ) ;
	if( strlen( cmd_buf ) == 0 ) return ;
	arg_start = cmd_buf ;
	}
if( sscanf( arg_start, "%lx", &x ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return ;
	}
store_word( last_dump_seg, last_dump_off,     (WORD)x ) ;
store_word( last_dump_seg, last_dump_off + 2, (WORD)( x >> 16 ) ) ;
}

void
examine_memory( void )
{
if( decode_address( &last_dump_seg, &last_dump_off ) == -1 )
	return ;
switch( cmd_name[ 1 ] ){
	case  0 :
	case 'B':  examine_byte() ;        break ;
	case 'W':  examine_word() ;        break ;
	case 'D':  examine_doubleword() ;  break ;
	default:
		cprint( "Can't understand '%s'\r\n", cmd_name ) ;
	}
}

static void
dump_code_breaks( void )
{
	int		i ;
	CODE_BREAK	*p ;

cprint( "   Code breakpoints :\r\n" ) ;
for( i = 0, p = code_bp_table ; i < code_bps ; i++, p++ ){
	cprint( "%02d  ", i ) ;
	if( p->min_cs == p->max_cs )
		cprint( "%04X:", p->min_cs ) ;
	else	cprint( "%04X-%04X:", p->min_cs, p->max_cs ) ;
	if( p->min_ip == p->max_ip )
		cprint( "%04X", p->min_ip ) ;
	else	cprint( "%04X-%04X", p->min_ip, p->max_ip ) ;
	if( p->cmd_fake_ID != NO_CMD_FAKE )
		cprint( " \'%s\'\r\n", cmd_fake_table[ p->cmd_fake_ID ].text ) ;
	else	cputs( "\r\n" ) ;
	}
}

static void
dump_memory_breaks( char *type, int count, MEM_BREAK *p )
{
	int	i ;

cprint( "   Memory %s breakpoints :\r\n", type ) ;
for( i = 0 ; i < count ; i++, p++ ){
	cprint( "%02d   %05lX-%05lX", i, p->start, p->end - 1 ) ;
	if( p->cmd_fake_ID != NO_CMD_FAKE )
		cprint( " \'%s\'\r\n", cmd_fake_table[ p->cmd_fake_ID ].text ) ;
	else	cputs( "\r\n" ) ;
	}
}

static void
dump_io_breaks( void )
{
	WORD		start, end ;
	int		rights ;
	BYTE		cmd_fake ;

cprint( "   I/O breakpoints :\r\n" ) ;
for( start = 0 ; ; start = end ){
	while( start <= BUS_ADDR_MASK && get_port_access( start ) == 0 ) start++ ;
	if( start > BUS_ADDR_MASK ) break ;
	rights	 = get_port_access( start ) ;
	cmd_fake = get_fake_ID( start ) ;
	for( end = start + 1 ; end <= BUS_ADDR_MASK ; end++ )
		if( get_port_access( end ) != rights ||
		    cmd_fake != get_fake_ID( end ) ) break ;
	if( end == start + 1 )
		cprint( "Port  %3X    ", start ) ;
	else	cprint( "Ports %3X-%3X", start, end - 1 ) ;
	cprint( " %c%c", rights & IOP_READ  ? 'r' : ' ',
			 rights & IOP_WRITE ? 'w' : ' ' ) ;
	if( cmd_fake != NO_CMD_FAKE )
		cprint( " \'%s\'\r\n", cmd_fake_table[ cmd_fake ].text ) ;
	else	cputs( "\r\n" ) ;
	}
}

static void
dump_prefetch_break( void )
{
cprint( "Prefetch queue mismatch breakpoint is %s\r\n", watch_prefetch_queue ? "On" : "Off" ) ;
}

static void
dump_fullspeed_break( void )
{
cprint( "Implicit full speed run breakpoint is %s\r\n", watch_full_speed_run ? "On" : "Off" ) ;
}

static void
list_breakpoints( void )
{
dump_code_breaks() ;
dump_memory_breaks( "read", read_bps, read_bp_table ) ;
dump_memory_breaks( "write", write_bps, write_bp_table ) ;
dump_io_breaks() ;
dump_prefetch_break() ;
dump_fullspeed_break() ;
}

#define CLEAR	0x1000
#define READ	IOP_READ
#define WRITE	IOP_WRITE
#define BOTH	(READ|WRITE)

static int
detect_type( void )
{
	int	x ;

switch( cmd_name[ 2 ] ){
	case  0:  x = BOTH ;	break ;
	case 'R': x = READ ;    break ;
	case 'W': x = WRITE ;   break ;
	default:
		  cprint( "Can't understand '%s'\r\n", cmd_name ) ;
		  return -1 ;
	}
if( arg_start != NULL && *arg_start == '-' ){
	arg_start++ ; x |= CLEAR ;
	}
return x ;
}

static int
get_range( DWORD *min, DWORD *max )
{
if( arg_start == NULL ){
	cputs( "Required argument missing.\r\n" ) ;
	return -1 ;
	}
if( sscanf( arg_start, "%lx", min ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return -1 ;
	}
skip_one_arg() ;
if( arg_start == NULL ){
	*max = *min ; return 0 ;
	}
if( *arg_start == '-' ) arg_start++ ;
if( sscanf( arg_start, "%lx", max ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return -1 ;
	}
skip_one_arg() ;
if( *max < *min ){
	cprint( "Lower margin large then upper ?\r\n" ) ;
	return -1 ;
	}
return 0 ;
}

static BYTE
alloc_fake_ID( void )
{
	int	i ;

if( arg_start == NULL ) return NO_CMD_FAKE ;
for( i = 0 ; i < MAX_CMD_FAKES ; i++ )
	if( cmd_fake_table[ i ].use_count == 0 ) break ;
if( i >= MAX_CMD_FAKES ){
	cprint( "No room left for command fake codes\r\n" ) ;
	return NO_CMD_FAKE ;
	}
strcpy( cmd_fake_table[ i ].text, arg_start ) ;
arg_start = NULL ;
return (BYTE) i ;
}

static int
count_bits( unsigned x )
{
	int	count ;

for( count = 0 ; x != 0 ; x >>= 1 ) count += x & 1 ;
return count ;
}

static void
set_io_bp( void )
{
	int	or_mask, and_mask ;
	int	old_rights, new_rights ;
	BYTE	temp ;
	DWORD	min, max ;
	WORD	i ;
	BYTE	fake_ID ;

if( ( or_mask = detect_type() ) == -1 ) return ;
if( get_range( &min, &max ) == -1 ) return ;
if( max > BUS_ADDR_MASK ){
	cputs( "Port adress out of valid range\r\n" ) ;
	return ;
	}
if( or_mask & CLEAR ){
	and_mask = ~( or_mask & BOTH ) ;
	or_mask  = 0 ;
	}
else {
	and_mask = BOTH ;
	or_mask  = or_mask & BOTH ;
	fake_ID  = alloc_fake_ID() ;
	}
for( i = (WORD)min ; i <= (WORD)max ; i++ ){
	new_rights = ( ( old_rights = get_port_access( i ) ) | or_mask ) & and_mask ;
	if( ( temp = get_fake_ID( i ) ) != NO_CMD_FAKE )
		cmd_fake_table[ temp ].use_count -= count_bits( old_rights ) ;
	set_port_access( i, new_rights ) ;
	if( fake_ID != NO_CMD_FAKE )
		set_fake_ID( i, fake_ID ) ;
	if( ( temp = get_fake_ID( i ) ) != NO_CMD_FAKE )
		cmd_fake_table[ temp ].use_count += count_bits( new_rights ) ;
	}
}

static void
set_mem_bp( void )
{
	int	number ;
	int	type ;
	DWORD	min, max ;
	BYTE	fake_ID ;

if( ( type = detect_type() ) == -1 ) return ;
if( type & CLEAR ){
	if( sscanf( arg_start, "%d", &number ) != 1 ){
		cprint( "Invalid breakpoint number '%s'\r\n", arg_start ) ;
		return ;
		}
	if( type & READ ){
		if( number >= read_bps ){
			cprint( "Invalid breakpoint number '%s'\r\n", arg_start ) ;
			return ;
			}
		if( ( fake_ID = read_bp_table[ read_bps ].cmd_fake_ID ) != NO_CMD_FAKE )
			cmd_fake_table[ fake_ID ].use_count-- ;
		if( number < read_bps - 1 )
			memcpy( read_bp_table + number, read_bp_table + number + 1,
				( read_bps - 1 - number ) * sizeof( MEM_BREAK ) ) ;
		read_bps-- ;
		}
	if( type & WRITE ){
		if( number >= write_bps ){
			cprint( "Invalid breakpoint number '%s'\r\n", arg_start ) ;
			return ;
			}
		if( ( fake_ID = write_bp_table[ read_bps ].cmd_fake_ID ) != NO_CMD_FAKE )
			cmd_fake_table[ fake_ID ].use_count-- ;
		if( number < write_bps - 1 )
			memcpy( write_bp_table + number, write_bp_table + number + 1,
				( write_bps - 1 - number ) * sizeof( MEM_BREAK ) ) ;
		write_bps-- ;
		}
	return ;
	}
if( get_range( &min, &max ) == -1 ) return ;
fake_ID = alloc_fake_ID() ;
if( type & READ ){
	if( read_bps == MAX_MEM_BPS ){
		cprint( "Memory breakpoints table full\r\n" ) ;
		return ;
		}
	read_bp_table[ read_bps ].start       = min ;
	read_bp_table[ read_bps ].end	      = max + 1 ;
	read_bp_table[ read_bps ].cmd_fake_ID = fake_ID ;
	if( fake_ID != NO_CMD_FAKE )
		cmd_fake_table[ fake_ID ].use_count++ ;
	read_bps++ ;
	}
if( type & WRITE ){
	if( write_bps == MAX_MEM_BPS ){
		cprint( "Memory breakpoints table full\r\n" ) ;
		return ;
		}
	write_bp_table[ write_bps ].start	= min ;
	write_bp_table[ write_bps ].end 	= max + 1 ;
	write_bp_table[ write_bps ].cmd_fake_ID = fake_ID ;
	if( fake_ID != NO_CMD_FAKE )
		cmd_fake_table[ fake_ID ].use_count++ ;
	write_bps++ ;
	}
}

static BYTE
get_code_fake( void )
{
	BYTE	fake_ID = alloc_fake_ID() ;

if( fake_ID != NO_CMD_FAKE )
	cmd_fake_table[ fake_ID ].use_count++ ;
return fake_ID ;
}

static int
fill_code_bp( CODE_BREAK *bp )
{
if( arg_start == NULL ){
	cputs( "Required argument missing.\r\n" ) ;
	return -1 ;
	}
if( *arg_start == '*' ){
	skip_one_arg() ;
	if( *arg_start != ':' ){
		bp->min_cs	= last_unassemble_cs ;
		bp->max_cs	= last_unassemble_cs ;
		bp->min_ip	= 0 ;
		bp->max_ip	= 0xFFFF ;
		bp->cmd_fake_ID = get_code_fake() ;
		return 0 ;
		}
	else {
		arg_start++ ;
		bp->min_cs = 0 ;
		bp->max_cs = 0xFFFF ;
		}
	}
else
if( arg_start[ 1 ] == 'S' && arg_start[ 2 ] == ':' ){
	switch( arg_start[ 0 ] ){
		case 'D': bp->min_cs = ds ; break ;
		case 'E': bp->min_cs = es ; break ;
		case 'S': bp->min_cs = ss ; break ;
		case 'C': bp->min_cs = cs ; break ;
		default:
			cprint( "Invalid segment register specified '%s'\r\n", arg_start ) ;
			return -1 ;
		}
	bp->max_cs = bp->min_cs ;
	arg_start += 3 ;
	}
else
if( strchr( arg_start, ':' ) != NULL ){
	if( sscanf( arg_start, "%x", & bp->min_cs ) != 1 ){
		cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
		return -1 ;
		}
	skip_one_arg() ;
	if( *arg_start != ':' ){
		if( *arg_start == '-' ) arg_start++ ;
		if( sscanf( arg_start, "%x", & bp->max_cs ) != 1 ){
			cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
			return -1 ;
			}
		skip_one_arg() ;
		if( *arg_start == ':' ) arg_start++ ;
		if( bp->max_cs < bp->min_cs ){
			cprint( "Lower margin large then upper ?\r\n" ) ;
			return -1 ;
			}
		}
	else {
		bp->max_cs = bp->min_cs ;
		arg_start++ ;
		}
	}
else {
	bp->min_cs = bp->max_cs = cs ;
	}
if( *arg_start == '*' ){
	skip_one_arg() ;
	bp->min_ip	= 0 ;
	bp->max_ip	= 0xFFFF ;
	bp->cmd_fake_ID = get_code_fake() ;
	return 0 ;
	}
if( sscanf( arg_start, "%x", & bp->min_ip ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return -1 ;
	}
skip_one_arg() ;
if( *arg_start != '-' ){
	bp->max_ip	= bp->min_ip ;
	bp->cmd_fake_ID = get_code_fake() ;
	return 0 ;
	}
else	arg_start++ ;
if( sscanf( arg_start, "%x", & bp->max_ip ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return -1 ;
	}
skip_one_arg() ;
if( bp->max_ip < bp->min_ip ){
	cprint( "Lower margin large then upper ?\r\n" ) ;
	return -1 ;
	}
bp->cmd_fake_ID = get_code_fake() ;
return 0 ;
}

static void
set_code_bp( void )
{
	int	number ;

if( arg_start != NULL && *arg_start == '-' ){
	arg_start++ ;
	if( sscanf( arg_start, "%d", &number ) != 1 ){
		cprint( "Invalid breakpoint number '%s'\r\n", arg_start ) ;
		return ;
		}
	if( number >= code_bps ){
		cprint( "Invalid breakpoint number '%s'\r\n", arg_start ) ;
		return ;
		}
	if( number < code_bps - 1 )
		memcpy( code_bp_table + number, code_bp_table + number + 1,
			( code_bps - 1 - number ) * sizeof( CODE_BREAK ) ) ;
	code_bps-- ;
	return ;
	}
if( code_bps == MAX_CODE_BPS ){
	cprint( "Code breakpoints table full\r\n" ) ;
	return ;
	}
if( fill_code_bp( code_bp_table + code_bps ) == -1 ) return ;
code_bps++ ;
}

static void
set_option_bp( char *flag )
{
if( arg_start != 0 )
	do {
		if( strncmp( arg_start, "ON",  2 ) == 0 )
			*flag = 1 ;
		else
		if( strncmp( arg_start, "OFF", 3 ) == 0 )
			*flag = 0 ;
		else	break ;
		return ;
		} while( 0 ) ;
cprint( "Required argument missing.\r\n" ) ;
}

void
set_breakpoint( void )
{
switch( cmd_name[ 1 ] ){
	case  0:
	case 'L':  list_breakpoints() ;                     break ;
	case 'I':  set_io_bp() ;                            break ;
	case 'M':  set_mem_bp() ;                           break ;
	case 'P':  set_code_bp() ;                          break ;
	case 'Q':  set_option_bp( &watch_prefetch_queue ) ; break ;
	case 'F':  set_option_bp( &watch_full_speed_run ) ; break ;
	default:
		cprint( "Can't understand '%s'\r\n", cmd_name ) ;
	}
}

static void
call_int( WORD number )
{
if( number >= INTERRUPTS_COUNT ){
	cprint( "'%s' number too large\r\n", arg_start ) ;
	return ;
	}
number *= 4 ;
store_word( ss, sp -= 2, flags ) ;
store_word( ss, sp -= 2, cs    ) ;
store_word( ss, sp -= 2, ip    ) ;
flags &= ~( FL_IF | FL_TF ) ;
ip     = fetch_word( 0, number + 0 ) ;
cs     = fetch_word( 0, number + 2 ) ;
}

static void
call_iret( void )
{
ip    = fetch_word( ss, sp + 0 ) ;
cs    = fetch_word( ss, sp + 2 ) ;
flags = ( fetch_word( ss, sp + 4 ) | FL_RESET ) & FL_SET ;
sp   += 6 ;
}

static void
call_retn( WORD args )
{
ip    = fetch_word( ss, sp ) ;
sp   += 2 + args ;
}

static void
call_retf( WORD args )
{
ip    = fetch_word( ss, sp + 0 ) ;
cs    = fetch_word( ss, sp + 2 ) ;
sp   += 4 + args ;
}

void
simulate_int( void )
{
	WORD	number ;

if( arg_start == NULL )
	number = 0 ;
else
if( sscanf( arg_start, "%x", &number ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return ;
	}
switch( cmd_name[ 1 ] ){
	case 0:
	case 'I': call_int( number ) ;  break ;
	case 'T': call_iret() ;         break ;
	case 'N': call_retn( number ) ; break ;
	case 'F': call_retf( number ) ; break ;
	default:
		cprint( "Can't understand '%s'\r\n", cmd_name ) ;
	}
restart_prefetch() ;
print_status() ;
}

int
init_trace_buffer( unsigned count )
{
	unsigned	size ;

if( trace_buf != 0 ){
	free_segment( FP_SEG( trace_buf ) ) ;
	trace_buf = 0 ;
	}
MAX_TRACE_RECORDS = count ;
size = ( count + 2 ) * sizeof( TRACE_RECORD ) ;
if( size / sizeof( TRACE_RECORD ) < count + 2 ){
	cprint( "Trace buffer size exceeds 64K\r\n" ) ;
	return -1 ;
	}
size = ( size + 15 ) >> 4 ;
if( ( size = allocate_segment( size ) ) == 0 ){
	cprint( "No RAM for trace buffer\r\n" ) ;
	return -1 ;
	}
trace_buf   = MK_FP( size, 2 * sizeof( TRACE_RECORD ) ) ;
next_trace  = trace_buf + MAX_TRACE_RECORDS - 1 ;
trace_count = MAX_TRACE_RECORDS ;
return 0 ;
}

void
view_trace_buffer( void )
{
	int	offset ;
	int	count ;
	int	pos ;

if( arg_start != NULL ){
	if( sscanf( arg_start, "%d", &count ) != 1 ){
		cprint( "Invalid decimal value '%s'\r\n", arg_start ) ;
		return ;
		}
	}
else	count = 10 ;
count  = min( count, MAX_TRACE_RECORDS - trace_count ) ;
if( count == 0 ) return ;
offset = ( (unsigned)( next_trace - trace_buf ) + count ) % MAX_TRACE_RECORDS ;
count  = min( count, 5 * 18 ) ;
for( pos = 0 ; count > 0 ; count--, offset-- ){
	if( offset < 0 ) offset += MAX_TRACE_RECORDS ;
	cprint( "%04X:%04X   ", trace_buf[ offset ].cs, trace_buf[ offset ].ip ) ;
	if( ++pos == 5 ){
		cputs( "\r\n" ) ;
		pos = 0 ;
		}
	}
if( pos != 0 ) cputs( "\r\n" ) ;
}

static void
print_jrnl_status( void )
{
switch( journal_status ){
	case J_CLOSED:
		cprint( "No journal\r\n" ) ;
		break ;
	case J_READING:
		cprint( "Reading journal '%s'\r\n", journal_name ) ;
		break ;
	case J_WRITING:
		cprint( "Writing journal '%s'\r\n", journal_name ) ;
		break ;
	}
}

static void
open_jrnl_write( void )
{
switch( journal_status ){
	case J_READING:
		close_journal() ;
	case J_CLOSED:
		if( arg_start == NULL ){
			cputs( "Required argument missing.\r\n" ) ;
			return ;
			}
		strcat( arg_start, ".JOU" ) ;
		start_journal_write( arg_start ) ;
		if( journal_status == J_CLOSED )
			return ;
		break ;
	case J_WRITING:
		break ;
	}
journal_status = J_WRITING ;
}

static void
open_jrnl_read( void )
{
if( journal_status != J_CLOSED )
	close_journal() ;
if( arg_start == NULL ){
	cputs( "Required argument missing.\r\n" ) ;
	return ;
	}
strcat( arg_start, ".JOU" ) ;
start_journal_read( arg_start ) ;
}

void
journal_functions( void )
{
switch( cmd_name[ 1 ] ){
	case  0:
	case 'S':  print_jrnl_status() ;   break ;
	case 'W':  open_jrnl_write() ;     break ;
	case 'R':  open_jrnl_read() ;      break ;
	case 'C':  close_journal() ;       break ;
	default:
		cprint( "Can't understand '%s'\r\n", cmd_name ) ;
	}
}

void
write_core_dump( void )
{
	int	handle ;
	WORD	top ;
	char	temp[ MAX_FILE_NAME ] ;

if( arg_start == NULL ){
	cputs( "Required argument missing.\r\n" ) ;
	return ;
	}
strcpy( temp, arg_start ) ;
temp[ arg_len( temp ) + 1 ] = 0 ;
if( ( handle = _creat( temp, 0 ) ) == -1 ){
	perror( temp ) ;
	return ;
	}
skip_one_arg() ;
if( arg_start != NULL ){
	if( sscanf( arg_start, "%x", &top ) != 1 ){
		cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
		_close( handle ) ;
		return ;
		}
	}
else	top = program_memtop() ;
save_memory_area( handle, program_psp + 0x10, top ) ;
_close( handle ) ;
}

void
input_io( void )
{
	WORD	port ;

if( arg_start == NULL ){
	cputs( "Required argument missing.\r\n" ) ;
	return ;
	}
if( sscanf( arg_start, "%x", &port ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return ;
	}
resume_trace = 1 ;
switch( cmd_name[ 1 ] ){
	case  0:
	case 'B':
		cprint( "%3X = %2X\r\n", port, byte_IN( port ) ) ;
		break ;
	case 'W':
		cprint( "%3X = %4X\r\n", port, word_IN( port ) ) ;
		break ;
	default:
		cprint( "Can't understand '%s'\r\n", cmd_name ) ;
	}
resume_trace = 0 ;
}

void
output_io( void )
{
	WORD	port, value ;

if( arg_start == NULL ){
	cputs( "Required argument missing.\r\n" ) ;
	return ;
	}
if( sscanf( arg_start, "%x", &port ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return ;
	}
skip_one_arg() ;
if( arg_start == NULL ){
	cputs( "Required argument missing.\r\n" ) ;
	return ;
	}
if( sscanf( arg_start, "%x", &value ) != 1 ){
	cprint( "Invalid hexadecimal value '%s'\r\n", arg_start ) ;
	return ;
	}
resume_trace = 1 ;
switch( cmd_name[ 1 ] ){
	case  0:
	case 'B':
		byte_OUT( port, (BYTE) value ) ;
		break ;
	case 'W':
		word_OUT( port, value ) ;
		break ;
	default:
		cprint( "Can't understand '%s'\r\n", cmd_name ) ;
	}
resume_trace = 0 ;
}
