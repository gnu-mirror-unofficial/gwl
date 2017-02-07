# gwl - Workflow management extension for GNU Guix
# Copyright © 2017 Roel Janssen <roel@gnu.org>
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Affero General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

include guile.am

moddir=$(prefix)/share/guile/site/2.0
godir=$(libdir)/guile/2.0/ccache

SOURCES = 						\
  guix/processes.scm					\
  guix/process-engines.scm				\
  guix/process-engines/bash-engine.scm			\
  guix/process-engines/grid-engine.scm			\
  guix/workflows.scm					\
  guix/workflows/execution-order.scm			\
  guix/workflows/utils.scm				\
  guix/scripts/workflow.scm				\
  guix/scripts/process.scm				\
  web-interface.scm					\
  www/config.scm					\
  www/util.scm						\
  www/pages.scm						\
  www/pages/error.scm					\
  www/pages/getting-started.scm				\
  www/pages/getting-started/defining-workflows.scm	\
  www/pages/welcome.scm					\
  www/pages/workflow-viewer.scm

EXTRA_DIST += env.in
