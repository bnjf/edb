#if 0 
 
  File name:  fullspee.h

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
 *	check_hardware returns 0 if it is possible to use 286/386/486 IDT
 *	relocation on current environment and -1 otherwise.
 */
int	check_hardware( void ) ;
int	full_speed_run( WORD stop_cs, WORD stop_ip ) ;
void	full_speed_return_point( void ) ;
void	full_speed_cleanup( void ) ;

extern	char	disable_local_IDT ;
