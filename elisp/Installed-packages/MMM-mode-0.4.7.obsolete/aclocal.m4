dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

dnl
dnl Execute arbitrary emacs lisp
dnl
AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS} -batch -eval "(let ((x ${elisp})) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil \"${OUTPUT}\"))" >& AC_FD_CC 2>&1  
	${EMACS} -batch -eval "(let ((x ${elisp})) (write-region (if (stringp x) (princ x 'ignore) (prin1-to-string x)) nil \"${OUTPUT}\"nil 5))" >& AC_FD_CC 2>&1
	retval=`cat ${OUTPUT}`
	echo "=> ${retval}" >& AC_FD_CC 2>&1
	rm -f ${OUTPUT}
	EMACS_cv_SYS_$1=$retval
])
$1=${EMACS_cv_SYS_$1}
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_XEMACS_P, [
  AC_MSG_CHECKING([if $EMACS is really XEmacs])
  AC_EMACS_LISP(xemacsp,(if (string-match \"XEmacs\" emacs-version) \"yes\" \"no\") ,"noecho")
  XEMACS=${EMACS_cv_SYS_xemacsp}
  EMACS_FLAVOR=emacs
  if test "$XEMACS" = "yes"; then
     EMACS_FLAVOR=xemacs
  fi
  AC_MSG_RESULT($XEMACS)
  AC_SUBST(XEMACS)
  AC_SUBST(EMACS_FLAVOR)
])

AC_DEFUN(AC_PATH_LISPDIR, [
  AC_XEMACS_P
  if test "$prefix" = "NONE"; then
	AC_MSG_CHECKING([prefix for your Emacs])
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),"noecho")
	prefix=${EMACS_cv_SYS_prefix}
	AC_MSG_RESULT($prefix)
  fi
  AC_ARG_WITH(lispdir,          --with-lispdir            Where to install lisp files, lispdir=${withval})
  AC_MSG_CHECKING([where .elc files should go])
  if test -z "$lispdir"; then
    dnl Set default value
    theprefix=$prefix
    if test "x$theprefix" = "xNONE"; then
	theprefix=$ac_default_prefix
    fi
    lispdir="\$(datadir)/${EMACS_FLAVOR}/site-lisp"
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
	   lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-lisp"
	   break
	fi
    done
  fi
  AC_MSG_RESULT($lispdir)
  AC_SUBST(lispdir)
])

dnl
dnl Determine the emacs version we are running.
dnl Automatically substitutes @EMACS_VERSION@ with this number.
dnl
AC_DEFUN(AC_EMACS_VERSION, [
AC_MSG_CHECKING(for emacs version)
AC_EMACS_LISP(version,(and (boundp 'emacs-major-version) (format \"%d.%d\" emacs-major-version emacs-minor-version)),"noecho")

EMACS_VERSION=${EMACS_cv_SYS_version}
AC_SUBST(EMACS_VERSION)
AC_MSG_RESULT(${EMACS_VERSION})
])

dnl
dnl Determine whether the specified version of Emacs supports packages
dnl or not.  Currently, only XEmacs 20.3 does, but this is a general
dnl check.
dnl
AC_DEFUN(AC_EMACS_PACKAGES, [
AC_ARG_WITH(package-dir,      --with-package-dir        Configure as a XEmacs package in directory, [ EMACS_PACKAGE_DIR="${withval}"])
if test -n "$EMACS_PACKAGE_DIR"; then
  if test "$prefix" != "NONE"; then
	AC_MSG_ERROR([--with-package-dir and --prefix are mutually exclusive])
  fi
  dnl Massage everything to use $(prefix) correctly.
  prefix=$EMACS_PACKAGE_DIR
  datadir='$(prefix)/etc/w3'
  infodir='$(prefix)/info'
  lispdir='$(prefix)/lisp/w3'
fi
AC_SUBST(EMACS_PACKAGE_DIR)
])

dnl
dnl Check whether a function exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_LIB, [
if test -z "$3"; then
	AC_MSG_CHECKING(for $2 in $1)
fi
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,(progn (fmakunbound '$2) (condition-case nil (progn (require '$library) (fboundp '$2)) (error (prog1 nil (message \"$library not found\"))))),"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
if test "${EMACS_cv_SYS_$1}" = "t"; then
	EMACS_cv_SYS_$1=yes
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
if test -z "$3"; then
	AC_MSG_RESULT($HAVE_$1)
fi
])

dnl
dnl Check whether a variable exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_VAR, [
AC_MSG_CHECKING(for $2 in $1)
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,(progn (makunbound '$2) (condition-case nil (progn (require '$library) (boundp '$2)) (error nil))),"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
AC_MSG_RESULT($HAVE_$1)
])

dnl
dnl Perform sanity checking and try to locate the custom and widget packages
dnl
AC_DEFUN(AC_CHECK_CUSTOM, [
AC_MSG_CHECKING(for acceptable custom library)
AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_CUSTOM,[
AC_EMACS_CHECK_LIB(widget,widget-convert-text,"noecho")
AC_EMACS_CHECK_LIB(wid_edit,widget-convert-text,"noecho")
if test "${HAVE_widget}" = "yes"; then
	EMACS_cv_ACCEPTABLE_CUSTOM=yes
else
	if test "${HAVE_wid_edit}" != "no"; then
		EMACS_cv_ACCEPTABLE_CUSTOM=yes
	else
		EMACS_cv_ACCEPTABLE_CUSTOM=no
	fi
fi
if test "${EMACS_cv_ACCEPTABLE_CUSTOM}" = "yes"; then
	AC_EMACS_LISP(widget_dir,(file-name-directory (locate-library \"widget\")),"noecho")
	EMACS_cv_ACCEPTABLE_CUSTOM=$EMACS_cv_SYS_widget_dir
fi
])
   AC_ARG_WITH(custom,           --with-custom             Specify where to find the custom package, [ EMACS_cv_ACCEPTABLE_CUSTOM=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   CUSTOM=${EMACS_cv_ACCEPTABLE_CUSTOM}
   AC_SUBST(CUSTOM)
   AC_MSG_RESULT("${CUSTOM}")
])


# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])


# serial 1

AC_DEFUN(AM_PATH_LISPDIR,
 [# If set to t, that means we are running in a shell under Emacs.
  # If you have an Emacs named "t", then use the full path.
  test "$EMACS" = t && EMACS=
  AC_PATH_PROGS(EMACS, emacs xemacs, no)
  if test $EMACS != "no"; then
    AC_MSG_CHECKING([where .elc files should go])
    dnl Set default value
    lispdir="\$(datadir)/emacs/site-lisp"
    emacs_flavor=`echo "$EMACS" | sed -e 's,^.*/,,'`
    if test "x$prefix" = "xNONE"; then
      if test -d $ac_default_prefix/share/$emacs_flavor/site-lisp; then
	lispdir="\$(prefix)/share/$emacs_flavor/site-lisp"
      else
	if test -d $ac_default_prefix/lib/$emacs_flavor/site-lisp; then
	  lispdir="\$(prefix)/lib/$emacs_flavor/site-lisp"
	fi
      fi
    else
      if test -d $prefix/share/$emacs_flavor/site-lisp; then
	lispdir="\$(prefix)/share/$emacs_flavor/site-lisp"
      else
	if test -d $prefix/lib/$emacs_flavor/site-lisp; then
	  lispdir="\$(prefix)/lib/$emacs_flavor/site-lisp"
	fi
      fi
    fi
    AC_MSG_RESULT($lispdir)
  fi
  AC_SUBST(lispdir)])

