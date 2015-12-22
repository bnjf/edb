#if 0 
 
  File name:  sysbus.h

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
#define INTERRUPTS_COUNT	256
#define BUS_ADDR_MASK		0x3FF
#define INVALID_CS_SELECTOR	1
#define MAX_MONITORS		5

/*
 *	port_disable_table contains byte for each I/O port in the followig
 *	format:
 *
 *	bit(s)		meaning
 *
 *	0-4		Command fake ID (use PORT_FAKE_MASK to extract)
 *	5		1 if port is locally mirrowed
 *	6		1 if read breakpoint is set
 *	7		1 if write breakpoint is set
 */
extern	BYTE		port_disable_table[ BUS_ADDR_MASK + 1 ] ;
extern	BYTE		port_mirrors	  [ BUS_ADDR_MASK + 1 ] ;
extern	DWORD	 FAR	*interrupts_table ;
extern	BYTE		interrupts_types[ INTERRUPTS_COUNT ] ;
#define INTERRUPT_DATA	0x01
#define INTERRUPT_WATCH 0x02

/*
 *	init_system_bus() returns -1 and prints error explanation message
 *	in case of error.
 */
int	init_system_bus( void ) ;
/*
 *	set_port_access( port, rights ) sets access rights to I/O port
 *	number 'port' (which should be <= BUS_ADDR_MASK ).
 *	Rights could be : 0 - allow any access, IOP_READ - deny read
 *	IOP_WRITE - deny write. rights could be ORed
 */
void	set_port_access( WORD port, int rights ) ;
int	get_port_access( WORD port ) ;
int	get_fake_ID( WORD port ) ;
void	set_fake_ID( WORD port, BYTE id ) ;

#define IOP_READ	1
#define IOP_WRITE	2
#define IOP_MIRROR	0x20

#define PORT_FAKE_MASK	0x1F

#define get_port_access(port)		\
		((port_disable_table[port]>>6)&3)
#define set_port_access(port,rights)	\
		(port_disable_table[port]=(port_disable_table[port]&0x3F)|((rights)<<6))
#define get_fake_ID(port)		\
		(port_disable_table[port]&PORT_FAKE_MASK)
#define set_fake_ID(port,id)		\
		(port_disable_table[port]=(port_disable_table[port]&	\
		~PORT_FAKE_MASK)|((id)&PORT_FAKE_MASK))
/*
 *	bp_type will be set to non-zero value if "hardware"
 *	breakpoint occure. It will NOT be reset to zero by succesfull
 *	completion of next instruction.
 *	BP_MEMR requeres no more actions (because actual data was fed
 *	to program). All other breakpoints has "suspended" data left
 *	after them.
 *	bp_count specifies number of detected memory write breakpoints.
 *	when bp_count > 1, bp_count-1 memory write operations were flushed
 *	to memory.
 */
extern	int	bp_type ;
extern	int	bp_count ;
extern	int	pass_memory_write ;
extern	WORD	bp_port, bp_seg, bp_off, bp_dat ;

enum {
	BP_NONE,	/*	No breakpoint				*/
	BP_IOR, 	/*	I/O read  (offended port in bp_port)	*/
	BP_IOW, 	/*	I/O write (offended port in bp_port)	*/
	BP_MEMR,	/*	Memory read (bp_seg:bp_off)		*/
	BP_MEMWB,	/*	Memory write byte (bp_seg:bp_off,bp_dat)*/
	BP_MEMWW,	/*	Memory write word (bp_seg:bp_off,bp_dat)*/
	BP_SEGEND,	/*	Word memory access at offset 0xFFFF	*/
	BP_INTERNAL,	/*	Internal error				*/
	BP_BREAKOUT,	/*	Program attempted to single-step into	*/
			/*	full-speed execution area		*/
	BP_RELEASE,	/*	Releasing control of CPU		*/
	} ;
/*
 *	set_mem_breakpoint( addr, len, access ) sets memory access
 *	breakpoint on access to memory area from addr to addr+len-1.
 *	access could be MEMP_READ (bp on memory read), MEMP_WRITE
 *	(bp on memory write) or both. Returns -1 if no breakpoint
 *	slots left. Up to MAX_MEM_BPS breakpoints of each type can be set.
 */
/*int	  set_mem_breakpoint( DWORD linear, DWORD len, int access ) ;
void	clear_mem_breakpoint( DWORD linear, int access ) ;*/

#define MEMP_READ	1
#define MEMP_WRITE	2

#define MAX_MEM_BPS	5

typedef struct	{
	DWORD		start ;
	DWORD		end ;
	BYTE		cmd_fake_ID ;
	} MEM_BREAK ;

#ifndef __MONITOR_DEFINED__
#define __MONITOR_DEFINED__
typedef struct {
	char		call[ 6 ] ;
	WORD		code ;
	void interrupt	(*old_vec)( void ) ;
	} MONITOR ;
#endif

extern	int		read_bps, write_bps ;
extern	MEM_BREAK	read_bp_table[ MAX_MEM_BPS ] ;
extern	MEM_BREAK	write_bp_table[ MAX_MEM_BPS ] ;
extern	char		resume_trace ;
/*extern  int		  monitor_count = 0 ;
extern	MONITOR 	dump_area[ MAX_MONITORS ] ;*/

void LIBSYM clear_monitors( void ) ;

/*
 *	Virtual memory redirection.
 *	One segment of virtual memory is always occupied by interrupt table,
 *	others are user-definable thru VirtualRAM=XXXXX-YYYYY statement.
 *	Warning: MEM_ROUTE relies upon DOS memory structure !
 *	Virtual memory area should have the same starting segment offset as
 *	real memory one !
 */
#define MAX_VIRTUAL_SEGMENTS	(5+1)

typedef struct	{
	DWORD		virt_start ;		/* Linear virtual addr	 */
	DWORD		virt_end ;		/* Linear virtual addr	 */
	WORD		segment_diff ;		/* Difference between	 */
						/* virtual & physical	 */
						/* segment address	 */
	WORD		real_start ;		/* Physical segment addr */
	BYTE FAR	*virt ; 		/* For 'gf' swapping     */
	BYTE FAR	*real ;
	DWORD		size ;
	} MEM_ROUTE ;

extern	MEM_ROUTE	virtual_mem_table[ MAX_VIRTUAL_SEGMENTS ] ;
extern	int		virtual_segments ;
