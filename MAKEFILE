#
#  File name:  makefile, dependency file for
# 
#  x86 Emulating Debugger (EDB)
#
#  Copyright (C) 1991-1993 Serge Pachkovsky
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 1, or (at your option)
#  any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#  If you need to contact me, feel free to write to ps@oci.unizh.ch
#  or to Serge Pachkovsky, Erligatterweg 61, Zuerich CH 8038, Switzerland
#
#  This makefile should work with Borland Make utility (it relies upon
#  some Borland-specific extensions to make).
#
#  I had tested it with Turbo C 2.0 (my compiler of choice ;),
#  Borland BC++ 2.0, BC++ 3.0 and 3.1. It will _not_ compile with
#  Microsoft compilers due to use of Borland-specific asm syntax.
#  I got the best results compiling EDB with BC++ 2.0 and linking
#  object files against Turbo C 2.0 libraries, but it is a bit
#  complicated for general use... BTW, despite of it's "advanced"
#  optimizer, BC 3.1 generates _worse_ code for EDB then Turbo C
#  2.0. I was really amased by the fact...
#
#

cc = tcc -mt -v -w -1 -a -f- -K -C -G -O -Z -k- -d -B
# -1- -DTARGET_8086        # You should define it to run on 8086 CPU
# -DFAST_INSTRUCTION_FETCH # Disable emulation of prefetch queue 
# -DNO_VERBOSE_COMMANDS    # Omit few "unnecessary" commands. It could
                           # help if you are getting DGROUP overflows 
                           # "Abnormal program termination"'s
as = tasm /zi /m5 /r /ml
l  = tlink /v /s

.c.obj:
	$(cc) -c {$< }

all	       : edb.tds edb.com

edb.tds        : edb.exe
	tdstrip -s edb.exe
edb.com        : edb.exe
	exe2bin edb.exe edb.com
	touch edb.tds
edb.exe        : edb.obj cpu8086.obj dssm8086.obj os.obj sysbus.obj	\
		 accessory.obj config.obj fullspeed.obj c0.obj		\
		 perversion.obj checksize.obj
	$(l) @edb.lnk
edb.obj        : edb.c cpu8086.h os.h edb.h accessory.h sysbus.h	\
	       dssm8086.h config.h fullspeed.h checksize.h

cpu8086.obj    : cpu8086.c cpu8086.h config.h os.h

dssm8086.obj   : dssm8086.c cpu8086.h dssm8086.h

os.obj	       : os.c os.h cpu8086.h psp.h perversion.h sysbus.h fullspeed.h

sysbus.obj     : sysbus.c cpu8086.h sysbus.h config.h os.h edb.h

accessory.obj  : accessory.c accessory.h cpu8086.h dssm8086.h os.h	\
		 sysbus.h edb.h

config.obj     : config.c config.h os.h cpu8086.h

checksize.obj  : checksize.c checksize.h os.h cpu8086.h

fullspeed.obj  : fullspeed.c fullspeed.h cpu8086.h sysbus.h os.h	\
		perversion.h edb.h

c0.obj	       : c0.asm
	$(as) c0
perversion.obj : perversion.asm perversion.h
	$(as) perversion
