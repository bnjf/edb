#if 0 

  File name:  psp.h
 
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
typedef struct	{
	BYTE		int_20h[ 2 ] ;		/* CD 20		*/
	WORD		mem_top ;		/* para top of task mem */
	BYTE		___1[1] ;
	BYTE		call_dos[ 5 ] ; 	/* opcode to call DOS	*/
	void interrupt	(*terminate)() ;	/* int 22h		*/
	void interrupt	(*ctrl_break)() ;	/* int 23h		*/
	void interrupt	(*crit_error)() ;	/* int 24h		*/
	WORD		origin ;		/* PID (PSP) of parent	*/
	BYTE		handles[20] ;		/* handles		*/
	WORD		env_seg ;		/* environment segment	*/
	WORD		___3 ;
	WORD		load_base ;		/* program load base	*/
	WORD		___4[ 2 ] ;
	WORD		psp_addr ;		/* self 		*/
	WORD		___5[ ( 0x5c - 0x38 ) / 2 ] ;
	struct fcb	FCB_1 ; 		/* unopened FCB1	*/
	struct fcb	FCB_2 ; 		/* unopened FCB2	*/
	BYTE		len ;			/* len of UPA		*/
	BYTE		UPA[ 0x7f ] ;		/* unformatted		*/
						/* parameter area	*/
	} PSP ;
