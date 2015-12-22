#if 0 

  File name:  accessor.h
 
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
 *	Modification history.
 *
 *	19 Feb 92
 *		MAX_CMD_FAKES changed from 12 to 10,
 *		test size of command fake changed from 40 to 25.
 *		Changes made to conserve DGROUP space...
 */

void	skip_one_arg( void ) ;
int	arg_len( char *p ) ;
char	*next_field( char *p ) ;
void	registers_display( void ) ;
void	bulk_unassemble( void ) ;
void	dump_memory( void ) ;
void	examine_memory( void ) ;
void	print_status( void ) ;
void	set_breakpoint( void ) ;
void	simulate_int( void ) ;
int	decode_address( WORD *seg, WORD *off ) ;
int	prefetch_queue_not_match( void ) ;


typedef struct	{
	WORD	min_cs, max_cs ;
	WORD	min_ip, max_ip ;
	BYTE	cmd_fake_ID ;
	} CODE_BREAK ;

#define MAX_CODE_BPS	6

extern	char		cmd_buf[ 80 ], cmd_name[ 10 ] ;
extern	char		*arg_start ;
extern	int		code_bps ;
extern	CODE_BREAK	code_bp_table[ MAX_CODE_BPS ] ;

typedef struct	{
	WORD	cs, ip ;
	} TRACE_RECORD ;

extern	TRACE_RECORD	FAR	*trace_buf ;
extern	TRACE_RECORD	FAR	*next_trace ;
extern	int			trace_count ;
extern	unsigned		MAX_TRACE_RECORDS ;
#define DEFAULT_TRACE_BUFFER	1000

int	init_trace_buffer( unsigned size ) ;
void	view_trace_buffer( void ) ;
void	journal_functions( void ) ;

void	write_core_dump( void ) ;

void	input_io( void ) ;
void	output_io( void ) ;

/*
 *	Each breakpoint is supplied with command fake ID.
 *	If ID != NO_CMD_FAKE, corresponding entry from cmd_fake_table
 *	will be activated. Zero in first byte of cmd_fake_table
 *	indicates empty slot.
 */
#define MAX_CMD_FAKES	10
#define NO_CMD_FAKE	((BYTE)0xFF)
typedef struct	{
	WORD	use_count ;
	char	text[ 25 ] ;
	} CMD_FAKE ;

extern	CMD_FAKE	cmd_fake_table[ MAX_CMD_FAKES ] ;
extern	char		watch_prefetch_queue ;
extern	char		watch_full_speed_run ;
