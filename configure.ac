dnl Autoconf configure script for wrap-syscall.
dnl Copyright (C) 2014 Free Software Foundation, Inc.
dnl
dnl This file is part of wrap-syscall.
dnl 
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ(2.64)dnl

AC_INIT(gen-ws.scm)
AC_CONFIG_HEADER(config.h:config.in)

AM_MAINTAINER_MODE

AC_CANONICAL_SYSTEM

AC_PROG_CC

AC_PROG_INSTALL

AC_PATH_PROG(GUILE, guile)

# -------------------- #
# Check for libguile.  #
# -------------------- #

dnl Utility to simplify finding libguile.
dnl $1 = guile-config-program
dnl $2 = yes|no, indicating whether to flag errors or ignore them
dnl $3 = the shell variable to assign the result to
dnl      If libguile is found we store "yes" here.

dnl FIXME: Need to support pkg-config guile-2.0 --cflags|--libs

AC_DEFUN([AC_TRY_LIBGUILE],
[
  guile_config=$1
  flag_errors=$2
  define([have_libguile_var],$3)
  found_usable_guile=checking
  AC_MSG_CHECKING([for usable guile from ${guile_config}])
  new_CPPFLAGS=`${guile_config} compile`
  if test $? != 0; then
    if test "${flag_errors}" = yes; then
      AC_ERROR(failure running guile-config compile)
    fi
    found_usable_guile=no
  fi
  new_LIBS=`${guile_config} link`
  if test $? != 0; then
    if test "${flag_errors}" = yes; then
      AC_ERROR(failure running guile-config link)
    fi
    found_usable_guile=no
  fi
  if test "${found_usable_guile}" = checking; then
    save_CPPFLAGS=$CPPFLAGS
    save_LIBS=$LIBS
    CPPFLAGS="$CPPFLAGS $new_CPPFLAGS"
    LIBS="$LIBS $new_LIBS"
    AC_LINK_IFELSE(AC_LANG_PROGRAM([[#include "libguile.h"]],
                                   [[scm_init_guile ();]]),
                   [have_libguile_var=yes
                    found_usable_guile=yes
                    GUILE_CPPFLAGS=$new_CPPFLAGS
                    GUILE_LIBS=$new_LIBS],
		   [found_usable_guile=no])
    CPPFLAGS=$save_CPPFLAGS
    LIBS=$save_LIBS
  fi
  AC_MSG_RESULT([${found_usable_guile}])
])

dnl There are several different values for --with-guile:
dnl
dnl no -   Don't include guile support.
dnl yes -  Include guile support, error if it's missing.
dnl        The guile-config program must be in $PATH.
dnl auto - Same as "yes", but if guile is missing from the system,
dnl        fall back to "no".
dnl /path/to/guile-config -
dnl        Use the guile-config program located in this directory.
dnl        NOTE: This needn't be the "real" guile-config program.
dnl        In a cross-compilation scenario (build != host), this could be
dnl        a shell script that provides what guile-config provides for
dnl        "compile" and "link".

AC_ARG_WITH(guile,
  AS_HELP_STRING([--with-guile@<:@=GUILE@:>@], [include guile support (auto/yes/no/<guile-config-program>)]),
  [], [with_guile=auto])
AC_MSG_CHECKING([whether to use guile])
AC_MSG_RESULT([$with_guile])

have_libguile=no
case "${with_guile}" in
no)
  AC_MSG_WARN([guile support disabled; some features may be unavailable.])
  ;;
auto)
  AC_TRY_LIBGUILE(guile-config, no, have_libguile)
  ;;
yes)
  AC_TRY_LIBGUILE(guile-config, yes, have_libguile)
  ;;
[[\\/]]* | ?:[[\\/]]*)
  AC_TRY_LIBGUILE(${with_guile}, yes, have_libguile)
  ;;
*/*)
  # Disallow --with-guile=foo/bar.
  AC_ERROR(invalid value for --with-guile)
  ;;
*)
  AC_ERROR(invalid value for --with-guile)
  ;;
esac

AC_SUBST(GUILE_CPPFLAGS)
AC_SUBST(GUILE_LIBS)

GDB=${GDB:=gdb}
AC_SUBST(GDB)

AC_OUTPUT(Makefile mock-run.sh mock-strace.sh,
[case x$CONFIG_HEADERS in
xconfig.h:config.in)
echo > stamp-h ;;
esac
])

exit 0
