#if 0 

  File name:  config.c, configuration file interface routines
 
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
#include <io.h>
#include <fcntl.h>
#include "config.h"
#include "cpu8086.h"
#include "os.h"

static	int	config_handle = -1 ;

int
open_config_section( const char *section )
{
	int	len = strlen( section ) ;
	char	buf[ MAX_CFG_STRING ] ;

if( config_handle != -1 ) lseek( config_handle, 0, SEEK_SET ) ;
if( ( config_handle = open( CONFIG_NAME, O_RDONLY | O_TEXT ) ) == -1 ){
	cprint( "Can't open '%s' : %s\r\n", CONFIG_NAME, strerror( errno ) ) ;
	return -1 ;
	}
while( hgets( buf, sizeof buf, config_handle ) != NULL )
	if( buf[ 0 ] == '[' && buf[ len + 1 ] == ']' && ! strncmp( section, buf + 1, len ) )
		return 0 ;
close_config() ;
cprint( "There is no section '%s' in %s\r\n", section, CONFIG_NAME ) ;
return -1 ;
}

void
close_config( void )
{
if( config_handle != -1 ) close( config_handle ) ;
}

int
get_config_string( char *buf )
{
if( config_handle == -1 ){
	cprint( "%s not opened\r\n", CONFIG_NAME ) ;
	exit( -1 ) ;
	}
while( 1 ){
	if( hgets( buf, MAX_CFG_STRING, config_handle ) == NULL ) return -1 ;
	if( strchr( buf, '\n' ) != NULL ) *strchr( buf, '\n' ) = 0 ;
	if( strchr( buf, ';' ) != NULL ) *strchr( buf, ';' ) = 0 ;
	if( buf[ 0 ] == 0 ) continue ;
	if( buf[ 0 ] == '[' && buf[ strlen( buf ) - 1 ] == ']' ) return -1 ;
	break ;
	}
return 0 ;
}

int
process_YesNo( char *p )
{
strupr( p ) ;
	if( strncmp( p, "YES", 3 ) == 0 ) return 1 ;
else	if( strncmp( p, "NO",  2 ) == 0 ) return 0 ;
else	if( strncmp( p, "ON",  2 ) == 0 ) return 1 ;
else	if( strncmp( p, "OFF", 2 ) == 0 ) return 0 ;
else	return -1 ;
}
