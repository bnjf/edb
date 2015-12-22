#if 0 

  File name:  config.h
 
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
 *	CONFIG.EDB handling functions
 */
#define CONFIG_NAME	"CONFIG.EDB"
#define MAX_CFG_STRING	82

/*
 *	open_config_section( name ) search file CONFIG_NAME for
 *	string "[name]" (case-sensitive search).
 *	If file can't be accessed or specified
 *	section not found, prints error message and returns -1.
 */
int	open_config_section( const char *section ) ;
/*
 *	get_config_string( buf ) places next configuration string
 *	into buf. Stings beginning with ';' are treated as comments
 *	and can't be red with this function.
 *	It returns -1 if end-of-file or end-of-section
 *	was encountered. buf content is always modified.
 */
int	get_config_string( char *buf ) ;
/*
 *	process_YesNo( string ) returns 0 if 'string' == 'No' or 'Off'
 *	and 1 if 'string' == 'Yes' or 'On'. Otherwise returns -1
 */
int	process_YesNo( char *p ) ;
void	close_config( void ) ;
