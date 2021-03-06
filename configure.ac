dnl gwl - Workflow management extension for GNU Guix
dnl Copyright © 2017 Roel Janssen <roel@gnu.org>
dnl Copyright © 2019, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program. If not, see <http://www.gnu.org/licenses/>.
dnl -*- Autoconf -*-

AC_INIT([GNU GWL],
  [0.4.0],
  [gwl-devel@gnu.org], [gwl],
  [https://www.gnu.org/software/gwl/])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([3.0 2.2])
GUILE_PROGS

if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your Guile installation.])
fi

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

GUILE_MODULE_REQUIRED(language wisp spec)
GUILE_MODULE_REQUIRED(gcrypt hash)
GUILE_MODULE_REQUIRED(pfds sets)
GUILE_MODULE_REQUIRED(drmaa v1 high)
GUILE_MODULE_REQUIRED(config)
AC_PATH_PROG([DOT], [dot])
if test "x$DOT" = "x"; then
   AC_MSG_ERROR(['dot' could not be found; please check your Graphviz installation.])
fi

AC_PATH_PROG([SED], [sed])
if test "x$SED" = "x"; then
   AC_MSG_ERROR(['sed' could not be found; it is needed during the build.])
fi

AC_ARG_VAR(GUILE_LOAD_PATH, [The Guile load path to be embedded in the main script])
AC_ARG_VAR(GUILE_LOAD_COMPILED_PATH, [The Guile load path for compiled modules to be embedded in the main script])

AC_CONFIG_FILES([Makefile gwl/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
