# gwl - Workflow management extension for GNU Guix
# Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
# Copyright © 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =                                                 \
  gwl/config.scm                                          \
  gwl/main.scm                                            \
  gwl/oop.scm                                             \
  gwl/processes.scm                                       \
  gwl/process-engines.scm                                 \
  gwl/process-engines/simple-engine.scm                   \
  gwl/process-engines/bash-engine.scm                     \
  gwl/process-engines/grid-engine.scm                     \
  gwl/utils.scm                                           \
  gwl/ui.scm                                              \
  gwl/workflows/execution-order.scm                       \
  gwl/workflows/graph.scm                                 \
  gwl/workflows/utils.scm                                 \
  gwl/workflows.scm                                       \
  gwl/cache.scm                                           \
  gwl/sugar.scm                                           \
  gwl/sugar/process.scm                                   \
  gwl/sugar/workflow.scm                                  \
  gwl/sugar/reader.scm                                    \
  gwl/web-interface.scm                                   \
  gwl/www/pages.scm                                       \
  gwl/www/pages/tutorial.scm                              \
  gwl/www/pages/community.scm                             \
  gwl/www/pages/error.scm                                 \
  gwl/www/pages/welcome.scm                               \
  gwl/www/pages/workflow-viewer.scm                       \
  gwl/www/render.scm                                      \
  gwl/www/util.scm

extensionsdir = $(prefix)/share/guix/extensions
dist_extensions_DATA =                                    \
  guix/extensions/workflow.scm

assetsdir = $(prefix)/share/$(PACKAGE)
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

examplesdir = $(prefix)/share/$(PACKAGE)/doc/examples
dist_examples_DATA =                                      \
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
  tests/cache.scm \
  tests/processes.scm \
  tests/workflows.scm \
  tests/sugar.scm \
  tests/utils.scm

TESTS = $(SCM_TESTS)

EXTRA_DIST += $(TESTS)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0

SCM_LOG_DRIVER =                                          \
  $(top_builddir)/pre-inst-env                            \
  $(GUILE) --no-auto-compile -e main                      \
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

EXTRA_DIST +=      \
  guix/extensions/workflow.scm.in \
  pre-inst-env.in           \
  build-aux/test-driver.scm \
  doc/fdl-1.3.texi \
  $(dist_examples_DATA)

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE_LOAD_COMPILED_PATH[@],$(GUILE_LOAD_COMPILED_PATH),g'				\
  -e 's,[@]GUILE_LOAD_PATH[@],$(GUILE_LOAD_PATH),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]abs_top_srcdir[@],$(abs_top_srcdir),g'	\
  -e 's,[@]abs_top_builddir[@],$(abs_top_builddir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

guix/extensions/workflow.scm: guix/extensions/workflow.scm.in Makefile
	$(AM_V_at)rm -f $@ $@-t
	$(AM_V_at)$(MKDIR_P) "$(@D)"
	$(AM_V_GEN)$(do_subst) < "$(srcdir)/$@.in" > "$@-t"
	mv -f "$@-t" "$@"

dist-hook: $(distdir)/ChangeLog
gen-ChangeLog $(distdir)/ChangeLog:
	$(AM_V_GEN)if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;							\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;		\
	fi

.PHONY: gen-ChangeLog

CLEANFILES += \
  guix/extensions/workflow.scm                      \
  $(SCM_TESTS:tests/%.scm=%.log)

# Like web-manual in maint.mk but with post-processing
manual: web-manual
	$(top_builddir)/pre-inst-env                            \
		$(GUILE) --no-auto-compile -e main                      \
		$(top_srcdir)/build-aux/post-process-manual.scm \
		"$(abs_builddir)/doc/manual/" "$(abs_builddir)/doc/manual-highlighted/"
	rsync -avhr "$(abs_builddir)/doc/manual-highlighted/" "$(abs_builddir)/doc/manual/"
