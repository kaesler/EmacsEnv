#
# SUMMARY:      What's new in this version of the OO-Browser?
#
# AUTHOR:       Bob Weiner / Mark Stern
#
# ORIG-DATE:    15-Oct-90
# LAST-MOD:     14-Sep-95 at 10:20:41 by Bob Weiner
#
# Copyright (C) 1990-1995  Free Software Foundation, Inc.
# See the file BR-COPY for license information.
#
# This file is part of the OO-Browser.

# Use -DHAVE_USLEEP if usleep() is a valid system call. Otherwise, comment
# out this line or set its value to be empty, in which case the definition in
# "usleep.c" will be used..
DEFINES         = -DHAVE_USLEEP
# Use the line below instead under Solaris.
# DEFINES         = -DSYSV

# Root directory of your X distribution.  Note how this is used in succeeding
# variables.
XROOT           = /usr/moto/X11R5
LD_LIBRARY_PATH = $(XROOT)/lib
INCLUDEDIRS     = -I$(XROOT)/include
LIBDIRS         = -L$(XROOT)/lib

# C compiler
CC		= gcc
# Compiler flags
CFLAGS		= -g -O $(INCLUDEDIRS) $(DEFINES)

# Linker flags.  Remove the -static for dynamic linking, e.g. under Linux.
# LDFLAGS		= -static $(LIBDIRS) -lXaw -lXmu -lXt -lX11 -lXext
# Use the line below instead under Solaris.
LDFLAGS        = $(LIBDIRS) -lXaw -lXmu -lXt -lX11 -lXext -lsocket

# Set this to be the directory in which to install the X OO-Browser
# executable.  This should be a directory in all users' search paths or the
# value of the Emacs variable, exec-directory.
# Use 'make install' to install the binary.
BINDIR          = /usr/local/infodock/bin

NAME		= xoobr
C_FILES 	= draw.c tree.c input.c dissolve.c dbl.c intf.c usleep.c
O_FILES		= draw.o tree.o input.o dissolve.o dbl.o intf.o usleep.o
INC_FILES	= defs.h tree.h input.h dissolve.h dbl.h rsrc.h intf.h help.h
SRC_FILES	= $(INC_FILES) $(C_FILES)

$(NAME):	$(O_FILES)
		$(CC) $(CFLAGS) -o $(NAME) $(O_FILES) $(LDFLAGS)

install:	$(NAME)
		chmod 755 $(NAME)
		mv $(NAME) $(BINDIR)/$(NAME)

clean:
		rm -f *.o *.orig *.rej $(NAME) core *~

size:
		wc $(SRC_FILES)

dbl.o: 		dbl.h 
dissolve.o:	dissolve.h
draw.o: 	defs.h tree.h dbl.h intf.h
input.o: 	defs.h tree.h input.h
intf.o: 	defs.h tree.h dbl.h intf.h rsrc.h input.h help.h
tree.o: 	defs.h tree.h intf.h
