# gwl - Workflow management extension for GNU Guix
# Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
# Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

moddir=$(prefix)/share/guile/site/@GUILE_EFFECTIVE_VERSION@
godir=$(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache

SOURCES =                                                 \
  gwl/config.scm                                          \
  gwl/oop.scm                                             \
  gwl/processes.scm                                       \
  guix/processes.scm                                      \
  gwl/process-engines.scm                                 \
  gwl/process-engines/bash-engine.scm                     \
  gwl/process-engines/grid-engine.scm                     \
  gwl/process-engines/simple-engine.scm                   \
  gwl/utils.scm                                           \
  gwl/ui.scm                                              \
  gwl/workflows/execution-order.scm                       \
  gwl/workflows/graph.scm                                 \
  gwl/workflows/utils.scm                                 \
  gwl/workflows.scm                                       \
  guix/workflows.scm                                      \
  gwl/cache.scm                                           \
  gwl/sugar.scm                                           \
  gwl/web-interface.scm                                   \
  gwl/www/config.scm                                      \
  gwl/www/pages.scm                                       \
  gwl/www/pages/beyond-started.scm                        \
  gwl/www/pages/community.scm                             \
  gwl/www/pages/designer.scm                              \
  gwl/www/pages/error.scm                                 \
  gwl/www/pages/extended-start.scm                        \
  gwl/www/pages/getting-started.scm                       \
  gwl/www/pages/welcome.scm                               \
  gwl/www/pages/workflow-viewer.scm                       \
  gwl/www/render.scm                                      \
  gwl/www/util.scm                                        \
  guix/scripts/workflow.scm

assetsdir = $(prefix)/share/@PACKAGE@
nobase_dist_assets_DATA =                                 \
  gwl/static/css/main.css                                 \
  gwl/static/fonts/FiraMono-Regular.ttf                   \
  gwl/static/fonts/Roboto-Bold.ttf                        \
  gwl/static/fonts/Roboto-LightItalic.ttf                 \
  gwl/static/fonts/Roboto-Light.ttf                       \
  gwl/static/fonts/OFL.txt                                \
  gwl/static/graphs/index.html                            \
  gwl/static/images/grid.png                              \
  gwl/static/images/logo.png

examplesdir = $(prefix)/share/@PACKAGE@/doc/examples
nobase_dist_examples_DATA =                               \
  doc/examples/simple.scm                                 \
  doc/examples/simple-wisp.w                              \
  doc/examples/example-workflow1.scm                      \
  doc/examples/example-workflow1.w                        \
  doc/examples/example-workflow.scm                       \
  doc/examples/example-workflow.w                         \
  doc/examples/extended-example-workflow.scm              \
  doc/examples/extended-example-workflow.w

info_TEXINFOS = doc/gwl.texi

TEST_EXTENSIONS = .scm

SCM_TESTS = \
  tests/processes.scm \
  tests/workflows.scm \
  tests/sugar.scm

test_data = \
  tests/data/test.wisp

TESTS = $(SCM_TESTS)

EXTRA_DIST += $(TESTS) $(test_data)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0

SCM_LOG_DRIVER =                                          \
  $(top_builddir)/env                                     \
  $(GUILE) --no-auto-compile -e main                      \
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

EXTRA_DIST +=      \
  env.in           \
  build-aux/test-driver.scm \
  doc/fdl-1.3.texi \
  doc/examples/simple.scm

dist-hook: $(distdir)/ChangeLog
gen-ChangeLog $(distdir)/ChangeLog:
	$(AM_V_GEN)if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;							\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;		\
	fi

.PHONY: gen-ChangeLog
