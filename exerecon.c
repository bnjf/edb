#if 0 

  File name:  exerecon.c, generate MS DOS .EXE file from two memory dumps
 
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
#include <io.h>
#include <string.h>
#include <mem.h>

#ifndef __COMPACT__
#error Should be compiled in COMPACT model !
#endif

#define MAX_RELOC_ENTRIES       (16370)

typedef struct  {
        unsigned        signature ;     /* Must be 0x4d5a       */
        unsigned        part_pag ;
        unsigned        page_cnt ;
        unsigned        relo_cnt ;
        unsigned        hdr_size ;
        unsigned        min_mem ;       /* ! Para               */
                                        /* 4K pages in .EXP     */
        unsigned        max_mem ;       /* ! Para               */
                                        /* 4K pages in .EXP     */
        unsigned        relo_SS ;       /* ESP in .EXP          */
        unsigned        exe_SP ;        /*                      */
        unsigned        chk_sum ;
        unsigned        exe_IP ;        /* EIP in .EXP          */
        unsigned        relo_CS ;       /*                      */
        unsigned        tabl_off ;      /* Of reloc table       */
        unsigned        overlay ;
        } TASK_HEADER ;

FILE            *d1, *d2, *out ;
unsigned        load_base, cs, ip, ss, sp ;
TASK_HEADER     hdr ;
long            load_image_len = 0 ;
unsigned        reloc_entries = 0 ;
long            *reloc_offset_table ;

void
title( void )
{
printf( "ExeReconstruct (C) 1991 Serge Pachkovsky\n" ) ;
}

void
help( void )
{
printf(
"ExeReconstruct - usage is :\n"
"        ExeReconstruct dump1 dump2 psp ep stk\n"
"        dump1 - output of EDB 'W' command\n"
"        dump2 - output of EDB /G 'W' command\n"
"        psp   - program segment prefix (PSP) location\n"
"        ep    - entry point of program (CS:IP)\n"
"        stk   - initial stack pointer (SS:SP)\n"
        ) ;
}

int
parce_parameters( int argc, char **argv )
{
if( argc == 1 ){  help() ;  return -1 ;  }
if( ( d1 = fopen( argv[ 1 ], "rb" ) ) == NULL ){
        perror( argv[ 1 ] ) ;
        return -1 ;
        }
if( ( d2 = fopen( argv[ 2 ], "rb" ) ) == NULL ){
        perror( argv[ 2 ] ) ;
        return -1 ;
        }
if( sscanf( argv[ 3 ], "%x", &load_base ) != 1 ){
        printf( "Invalid load base '%s'\n", argv[ 3 ] ) ;
        return -1 ;
        }
load_base += 0x10 ;
if( sscanf( argv[ 4 ], "%x:%x", &cs, &ip ) != 2 ){
        printf( "Invalid entry point '%s'\n", argv[ 4 ] ) ;
        return -1 ;
        }
if( sscanf( argv[ 5 ], "%x:%x", &ss, &sp ) != 2 ){
        printf( "Invalid stack '%s'\n", argv[ 5 ] ) ;
        return -1 ;
        }
return 0 ;
}

void
process_reloc_entries( void )
{
        long            *p ;
        unsigned        i ;

for( i = reloc_entries, p = reloc_offset_table ; i > 0 ; i--, p++ )
        *p = ( ( *p & 0xFFFFF000ul ) << 12 ) | ( *p & 0xFFF ) ;
}

void
restore_reloc_entries( void )
{
        long            *p ;
        unsigned        i ;

for( i = reloc_entries, p = reloc_offset_table ; i > 0 ; i--, p++ )
        *p = ( ( *p & 0xFFFF0000ul ) >> 12 ) + ( *p & 0xFFFF ) ;
}

void
register_reloc_offset( long offset )
{
if( reloc_entries == MAX_RELOC_ENTRIES ){
        printf( "Relocation table overflow !\n" ) ;
        exit( -1 ) ;
        }
reloc_offset_table[ reloc_entries++ ] = offset ;
}

void
collect_relocation( void )
{
        unsigned char   c1, c2 ;
        unsigned        w1, w2 ;

load_image_len = 0 ;
do {
        c1 = getc( d1 ) ; c2 = getc( d2 ) ;
        if( c1 != c2 ){
                w1 = ( getc( d1 ) << 8 ) | c1 ;
                w2 = ( getc( d2 ) << 8 ) | c2 ;
                if( w2 == w1 + 1 ){
                        register_reloc_offset( load_image_len ) ;
                        load_image_len += 2 ;
                        }
                else    break ;
                }
        else    load_image_len++ ;
        } while( ! feof( d1 ) && ! feof( d2 ) ) ;
reloc_offset_table[ reloc_entries ] = 0xFFFFFFFFul ;
}

void
fill_task_header( void )
{
        long    file_len ;

hdr.signature = 0x5a4d ;
file_len      = ( 0x1c + reloc_entries * 4 + 511 ) & ~511 ;
hdr.hdr_size  = (unsigned)( file_len / 16 ) ;
file_len     += load_image_len ;
hdr.page_cnt  = (unsigned)( ( file_len + 511 ) / 512 ) ;
hdr.part_pag  = (unsigned)( file_len - ( hdr.page_cnt - 1 ) * 512L ) ;
if( hdr.part_pag == 512 ) hdr.part_pag = 0 ;
hdr.relo_cnt  = reloc_entries ;
hdr.min_mem   = 0 ;
hdr.max_mem   = 0xFFFF ;
hdr.relo_SS   = ss - load_base ;
hdr.exe_SP    = sp + 2 ;
hdr.chk_sum   = 0 ;
hdr.exe_IP    = ip ;
hdr.relo_CS   = cs - load_base ;
hdr.tabl_off  = 0x1c ;
hdr.overlay   = 0 ;
}

void
save_task_header( void )
{
if( fwrite( &hdr, sizeof hdr, 1, out ) != 1 ){
        printf( "Write failed - disk full ?\n" ) ;
        exit( -1 ) ;
        }
process_reloc_entries() ;
if( fwrite( reloc_offset_table, 4, reloc_entries, out ) != reloc_entries ){
        printf( "Write failed - disk full ?\n" ) ;
        exit( -1 ) ;
        }
restore_reloc_entries() ;
fflush( out ) ;
chsize( fileno( out ), hdr.hdr_size * 16L ) ;
fseek( out, hdr.hdr_size * 16L, SEEK_SET ) ;
}

void
save_task_image( void )
{
        long    i ;
        long    *p ;

for( i = 0, p = reloc_offset_table ; i < load_image_len ; i++ ){
        if( i == *p ){
                p++ ; i++ ;
                putw( getw( d1 ) - load_base, out ) ;
                }
        else
        if( putc( getc( d1 ), out ) == EOF ){
                printf( "Write failed - disk full ?\n" ) ;
                exit( -1 ) ;
                }
        }
}

int
main( int argc, char *argv[] )
{
title() ;
if( parce_parameters( argc, argv ) == -1 ) return -1 ;
if( ( reloc_offset_table = calloc( MAX_RELOC_ENTRIES, sizeof( long ) ) ) == NULL ){
        perror( "Can't allocate relocation ID table" ) ;
        return -1 ;
        }
if( ( out = fopen( "RECONSTRUCT.EXE", "wb" ) ) == NULL ){
        perror( "RECONSTRUCT.EXE" ) ;
        return -1 ;
        }
if( setvbuf( d1,  NULL, _IOFBF, 30 * 1024 ) != 0 ||
    setvbuf( d2,  NULL, _IOFBF, 30 * 1024 ) != 0 ||
    setvbuf( out, NULL, _IOFBF, 30 * 1024 ) != 0 ){
        perror( "Can't allocate file buffers" ) ;
        return -1 ;
        }
printf( "Scanning images for relocation table\n" ) ;
collect_relocation() ;
rewind( d1 ) ; fclose( d2 ) ;
printf( "Relocation table size is %d, module size is %ld\n", reloc_entries, load_image_len ) ;
printf( "Saving task header and relocation table\n" ) ;
fill_task_header() ;
save_task_header() ;
printf( "Saving task image\n" ) ;
save_task_image() ;
printf( "Completed\n" ) ;
fclose( d1 ) ;
fclose( d2 ) ;
fclose( out ) ;
return 0 ;
}
