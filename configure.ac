dnl gwl - Workflow management extension for GNU Guix
dnl Copyright © 2017 Roel Janssen <roel@gnu.org>
dnl Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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
  [0.1.1],
  [gwl-devel@gnu.org], [gwl],
  [https://www.gnu.org/software/gwl/])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([2.2])
GUILE_PROGS

if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.2 installation.])
fi

GUILE_MODULE_REQUIRED(language wisp spec)
GUILE_MODULE_REQUIRED(gcrypt hash)
AC_PATH_PROG([DOT], [dot])
if test "x$DOT" = "x"; then
   AC_MSG_ERROR(['dot' could not be found; please check your Graphviz installation.])
fi

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([gwl/www/config.scm])
AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
