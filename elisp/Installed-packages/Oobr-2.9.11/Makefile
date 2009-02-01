#
# SUMMARY:      Build OO-Browser directories and distributions.
#
# AUTHOR:       Bob Weiner
#
# ORIG-DATE:     6-Oct-94 at 03:42:38
# LAST-MOD:      1-Nov-95 at 22:22:45 by Bob Weiner
#
# Copyright (C) 1994-1995  Free Software Foundation, Inc.
# See the file BR-COPY for license information.
#
# This file is part of the OO-Browser.
#
# DESCRIPTION:  
#
# USAGE:        Before doing your first make, edit the CONFIGURABLE SECTION
#               in the "Makefile".  The only change you have to make is to
#               set the EMACS variable to point to the Emacs executable that
#               you want to use to byte-compile the OO-Browser Lisp files.
#               Make this change now and save the file.
#
#               If you are running Emacs V18 or Epoch, you must first rebuild
#               all byte-compiled Lisp files.  Use:
#                    make all-elc
#
#               Then, whatever Emacs version you use, invoke:
#   	             make
#
#               Use these commands if you want to build graphical interfaces
#               to the OO-Browser:
#                    Build the X OO-Browser:        make xoobr
#
#                    If you want to rebuild the NEXTSTEP OO-Browser below the
#                    "tree-nx" directory, you must use NeXT's ProjectBuilder.
#
#               Now the OO-Browser should be ready for use if you have
#               followed the installation instructions in "BR-README".
#
#   -----------------
#
#               To force rebuilding of all .elc files, even ones that are not
#               out of date:
#                    make all-elc
#
#               To build the Info version of the OO-Browser:
#                    make info
#
#               To build the Postscript version of the OO-Browser:
#                    make ps
#
# DESCRIP-END.

##########################################################################
#                         CONFIGURABLE SECTION                           #
##########################################################################

# Binaries for various versions of Emacs.  Set 'EMACS' below to the one you
# want to use to byte-compile Lisp files.
EMACS19 = emacs
EMACS18 = emacs18
EPOCH = epoch
INFODOCK = infodock.bin
XEMACS = xemacs

# Emacs version used to byte-compile .el files into .elc's.
EMACS = $(XEMACS)

# Where to install the Info version of the OO-Browser manual.
infodir = /usr/nodelocal/info
# Where to install the Texinfo version of the OO-Browser manual
# along with its im/ directory of images.
texidir = /usr/nodelocal/man

# Site-specific Emacs Lisp libraries to load before byte-compiling any files
# from this package.  Typically the only reason to set this is to get Emacs
# to include the directory of this package into its load-path variable, which
# determines where it will find Lisp library files to load.
#
# InfoDock and XEmacs 19.12 or higher include this package and automatically
# add its directory to load-path.  Under Emacs 19, if you add this directory
# to load-path in your site-lisp/site-start.el file, then you need not change
# this setting since site-start is automatically loaded whenever Emacs starts
# up.  If, however, you set load-path in your personal ~/.emacs file, you
# must add that to this setting.
#
# You must include the .el or .elc file suffix on each library name and each
# must be preceded by the `-l ' command-line flag.  If the directory in which
# the library is stored will not be in your Emacs load-path when Emacs
# attempts to load the library, you must include the full pathname to the
# library.  Here is an example setting.
#
# SITE_PRELOADS = -l ~/.emacs -l set-load-path.el
#
SITE_PRELOADS =

# Temp file to use to build .elc files.
ELISP_TO_COMPILE = /tmp/elc-${USER}

# Shell used to process this Makefile.  Bourne shell syntax is required.
SHELL = /bin/sh

# UNIX commands you may want to change for your particular system.
CP = \cp -p
DVIPS = \dvips
ETAGS = etags
INSTALL = install -c
MAKE = make
MAKEINFO = \makeinfo --fill-column 74 
MKDIR = mkdir
MV = \mv -f
RM = \rm -f
TAR = tar

# Directory in which to create new distributions of the OO-Browser.
DIST_DIR = /tmp

##########################################################################
#                     NO CHANGES REQUIRED BELOW HERE                     #
##########################################################################

OOBR_VERSION = 2.9.11

MAN_DIR = man

# Libraries that must be pre-loaded before trying to byte-compile anything.
PRELOADS = $(SITE_PRELOADS) -l ./br-start.el

# Compile in batch mode. Under Emacs 19 and XEmacs, load
# site-lisp/site-start.el, which may set load-path.
BATCHFLAGS = -batch

# Directories other than the current directory in which to find files.
# This doesn't seem to work in all versions of make, so we also add hypb/
# explicitly to those files which need it.
VPATH = hypb

HYPB_EL   = hypb/h-skip-bytec.lsp hypb/hact.el hypb/hargs.el \
	    hypb/hhist.el hypb/hmail.el hypb/hmouse-drv.el \
	    hypb/hmouse-key.el hypb/hmous-info.el \
	    hypb/hmouse-reg.el hypb/hmouse-sh.el \
	    hypb/hpath.el hypb/hui-mouse.el \
	    hypb/hui-window.el hypb/hvar.el \
	    hypb/hversion.el hypb/hypb.el hypb/set.el

HYPB_ELC  = hypb/hact.elc hypb/hargs.elc \
	    hypb/hhist.elc hypb/hmail.el hypb/hmouse-drv.elc \
	    hypb/hmouse-key.elc hypb/hmous-info.elc \
	    hypb/hmouse-reg.elc hypb/hmouse-sh.elc \
	    hypb/hpath.elc hypb/hui-mouse.elc \
	    hypb/hui-window.elc hypb/hvar.elc \
	    hypb/hversion.elc hypb/hypb.elc hypb/set.elc

# Files from Hyperbole necessary for OO-Browser mouse support.
HYPERBOLE = $(HYPB_EL) $(HYPB_ELC)

EL_COMPILE  = br-c-ft.el br-c++-ft.el br-c++.el br-clos-ft.el \
	      br-clos.el br-compl.el \
              br-eif-ft.el br-eif.el br-env.el br-ftr.el br-info.el \
              br-init.el br-java.el br-java-ft.el \
	      br-lib.el br-menu.el br-objc-ft.el \
              br-objc.el br-site.el br-vers.el \
	      br-smt.el br-start.el br-tree.el br.el c++-browse.el \
	      clos-brows.el eif-browse.el eif-calls.el eif-ise-er.el \
	      hasht.el hmouse-br.el info-brows.el java-brows.el \
	      objc-brows.el smt-browse.el

ELC_COMPILE = br-c-ft.elc br-c++-ft.elc br-c++.elc br-clos-ft.elc \
	      br-clos.elc br-compl.elc \
              br-eif-ft.elc br-eif.elc br-env.elc br-ftr.elc br-info.elc \
              br-init.elc br-java.elc br-java-ft.elc \
	      br-lib.elc br-menu.elc br-objc-ft.elc \
              br-objc.elc br-site.elc br-vers.elc \
	      br-smt.elc br-start.elc br-tree.elc br.elc c++-browse.elc \
	      clos-brows.elc eif-browse.elc eif-calls.elc eif-ise-er.elc \
	      hasht.elc hmouse-br.elc info-brows.elc java-brows.elc \
	      objc-brows.elc smt-browse.elc

EL_TAGS = $(EL_COMPILE)

.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .el .elc   # Define the list of file suffixes to match to rules

# Build the OO-Browser for use in current directory.
all: elc

doc: info ps

xoobr:
	cd tree-x; make


install: xoobr
	# Install the X OO-Browser in its executable directory.
	cd tree-x; make install
	cd $(MAN_DIR); $(INSTALL) oo-browser.info* $(infodir); \
	  $(INSTALL) oo-browser.texi $(texidir); \
	  $(INSTALL) im $(texidir)

# Record any .el files that need to be compiled.
.el.elc:
	@ echo $< >> $(ELISP_TO_COMPILE)

# Compile all recorded .el files.
elc: elc-init $(ELC_COMPILE) $(HYPB_ELC)
	@- \test ! -f $(ELISP_TO_COMPILE) \
            || (echo "These files will be compiled: " \
                 && echo "`cat $(ELISP_TO_COMPILE)`" \
                 && $(EMACS) $(BATCHFLAGS) $(PRELOADS) \
                       -f batch-byte-compile `cat $(ELISP_TO_COMPILE)`)
	@ $(RM) $(ELISP_TO_COMPILE)

elc-init:
	@ $(RM) $(ELISP_TO_COMPILE)

# Remove and then rebuild all byte-compiled .elc files for V19 Emacs, even
# those .elc files which do not yet exist.
all-elc:
	$(RM) *.elc
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $(EL_COMPILE)

# This target is used for background building of OO-Browser Environments.
# Don't change the name of the target since it is used in "br-env.el".
# Don't try to use this target manually unless you first set the following 2
# environment variables:
#    OOBR_DIR = full pathname of the directory where the OO-Browser lives
#    OOBR_ENV = full pathname of the Environment specification to build
oobr-env:
	cd $(OOBR_DIR); $(EMACS) $(BATCHFLAGS) $(PRELOADS) ${OOBR_ENV} -f br-env-batch-build

dvi: $(MAN_DIR)/oo-browser.dvi

info: $(MAN_DIR)/oo-browser.info

ps: $(MAN_DIR)/oo-browser.ps

$(MAN_DIR)/oo-browser.dvi: $(MAN_DIR)/oo-browser.info
	cd $(MAN_DIR); tex oo-browser.texi; texindex oo-browser.??; tex oo-browser.texi

$(MAN_DIR)/oo-browser.info: $(MAN_DIR)/oo-browser.texi
	cd $(MAN_DIR); $(MAKEINFO) oo-browser.texi

$(MAN_DIR)/oo-browser.ps: $(MAN_DIR)/oo-browser.texi
	make $(MAN_DIR)/oo-browser.dvi
	-$(RM) $(MAN_DIR)/oo-browser.ps
	cd $(MAN_DIR); $(DVIPS) -o oo-browser.ps oo-browser.dvi

tags: $(EL_TAGS)
	$(ETAGS) $(EL_TAGS) tree-x/*.[ch] tree-nx/*.[mh]

hypb-tags: $(EL_TAGS)
	cd $(DIST_DIR)/oobr; $(ETAGS) $(EL_TAGS) $(HYPB_EL) tree-x/*.[ch] tree-nx/*.[mh]

clean:  distclean
	$(RM) *.elc

version:
	fgrep $(OOBR_VERSION) BR-RELEASE br-vers.el man/oo-browser.texi
	echo ""

# Don't use this target.
bw-install:
	cd /usr/local/infodock/lisp/; \
	tar zxvf /tmp/oo-browser-$(OOBR_VERSION).tar.gz; \
	cd oobr/; mv man/*.info* ../../info/; mv man/*.texi ../../man/; \
	rm -rf man

dist: version elc info
	echo "Ensure version # in br-vers.el and man/oo-browser.texi is up to date."
	$(RM) -r $(DIST_DIR)/oobr
	cd ..; $(TAR) clf $(DIST_DIR)/br.tar oobr
	cd $(DIST_DIR); $(TAR) xf br.tar
	cd $(DIST_DIR)/oobr; $(RM) -r hypb man/im; make distclean; \
	  mkdir hypb man/im; chmod 755 hypb man/im; \
	  $(CP) /usr/local/infodock/info-local/im/oobr-* man/im
	$(CP) $(HYPERBOLE) $(DIST_DIR)/oobr/hypb
# This ver setup won't work under SunOS for some reason.
#	  ver="`head -3 oobr/br-vers.el | tail -1 | sed -e 's/.*|//'`"
	cd $(DIST_DIR); $(RM) br.tar; \
	  $(TAR) --gzip -clf oo-browser-$(OOBR_VERSION).tar.gz oobr

distclean:
	-$(RM) TAGS core .place* .*~ *~ *\# *.orig *.rej .nfs*
	-cd tree-x; $(RM) .*~ *~ *\# *.orig *.rej *.o xoobr .nfs*
	-cd tree-nx; $(RM) -r .*~ *~ *\# *.orig *.rej obj* TreeView.app .nfs*
	-cd $(MAN_DIR); $(RM) oo-browser.?? oo-browser.dvi .*~ *~ *\# *.orig *.rej .nfs*
