#if 0 

  File name:  checksiz.c, verify size of EDB executable file
 
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
#include <sys/stat.h>
#include "cpu8086.h"
#include "os.h"
#include "checksize.h"

#ifdef	TARGET_8086
#define EDB_SIZE	56594L
#else
#define EDB_SIZE	56208L
#endif

#ifndef NO_VERBOSE_COMMANDS
static int
find_program_name( char *buf )
{
extern	WORD		_envseg ;
	char	_es	*p   = 0 ;
	int		size = 0x7FFF ;

_ES = _envseg ;
do {
	for( ; *p != 0 && size > 0 ; p++, size-- ) ;
	p++ ; size-- ;
	} while( *p != 0 && size > 0 ) ;
if( size <= 0 ) return -1 ;
if( *(WORD far *)++p < 1 ) return -1 ;
p += 2 ;
while( ( *buf++ = *p++ ) != 0 ) ;
return 0 ;
}

void
check_size( void )
{
	char		name_buf[ 128 ] ;
	long		file_size ;

if( find_program_name( name_buf ) == -1 ){
	cputs( "Start path unavailable.\r\n" ) ;
	return ;
	}
if( ( file_size = get_file_size( name_buf ) ) == -1 ){
	perror( "Can't obtain file size info" ) ;
	return ;
	}
if( file_size != EDB_SIZE )
	cprint( "%s is %ld bytes in size. \7It should be %ld bytes\r\n", name_buf, file_size, EDB_SIZE ) ;
}

#endif
