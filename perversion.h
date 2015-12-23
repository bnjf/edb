#if 0 

  File name:  perversi.h
 
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
 *	start_program sets emulated CPU environment on real CPU.
 *	!!!! This will never return to caller.
 *	break_point_interrupt should not be called directly !
 */
void LIBSYM start_program( void ) ;
void LIBSYM break_point_interrupt( void ) ;

WORD LIBSYM huge_write( int handle, void far *mem, WORD cnt ) ;
/*
 *	break_point will be called then int3 encounters in program.
 *	It should NOT return to caller. (All real CPU registers will
 *	be copied to emulator images).
 */
void LIBSYM fill_IDT( DWORD IDT[ 256 ], void (*break_point)( void ) ) ;

void LIBSYM copy_RAM( WORD dest, WORD src, WORD size ) ;

void LIBSYM swap_RAM( BYTE far *a, BYTE far *b, DWORD size ) ;
