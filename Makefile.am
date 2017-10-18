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

moddir=$(prefix)/share/guile/site/2.2
godir=$(libdir)/guile/2.2/ccache

SOURCES =                                                 \
  gnu/processes.scm                                       \
  gnu/workflows.scm                                       \
  guix/processes.scm                                      \
  guix/process-engines.scm                                \
  guix/process-engines/bash-engine.scm                    \
  guix/process-engines/grid-engine.scm                    \
  guix/workflows.scm                                      \
  guix/workflows/execution-order.scm                      \
  guix/workflows/utils.scm                                \
  guix/workflows/graph.scm                                \
  guix/scripts/workflow.scm                               \
  guix/scripts/process.scm                                \
  gwl/web-interface.scm                                   \
  gwl/www/config.scm                                      \
  gwl/www/util.scm                                        \
  gwl/www/pages.scm                                       \
  gwl/www/pages/error.scm                                 \
  gwl/www/pages/getting-started.scm                       \
  gwl/www/pages/getting-started/defining-workflows.scm    \
  gwl/www/pages/getting-started/real-world-usage.scm      \
  gwl/www/pages/welcome.scm                               \
  gwl/www/pages/workflow-viewer.scm

WWW_STATIC_RESOURCES =                                    \
  gwl/static/css/main.css                                 \
  gwl/static/fonts/FiraMono-Regular.ttf                   \
  gwl/static/fonts/Roboto-Bold.ttf                        \
  gwl/static/fonts/Roboto-LightItalic.ttf                 \
  gwl/static/fonts/Roboto-Light.ttf                       \
  gwl/static/fonts/OFL.txt                                \
  gwl/static/graphs/index.html                            \
  gwl/static/images/g16.png                               \
  gwl/static/images/grid.png                              \
  gwl/static/images/grid.svg                              \
  gwl/static/images/what-is-gwl.svg                       \
  gwl/static/images/workflows-processes.svg               \
  gwl/static/jquery/jquery-3.1.1.min.js                   \
  gwl/static/jqueryui/external/jquery/jquery.js           \
  gwl/static/jqueryui/images                              \
  gwl/static/jqueryui/images/ui-icons_444444_256x240.png  \
  gwl/static/jqueryui/images/ui-icons_555555_256x240.png  \
  gwl/static/jqueryui/images/ui-icons_777620_256x240.png  \
  gwl/static/jqueryui/images/ui-icons_777777_256x240.png  \
  gwl/static/jqueryui/images/ui-icons_cc0000_256x240.png  \
  gwl/static/jqueryui/images/ui-icons_ffffff_256x240.png  \
  gwl/static/jqueryui/jquery-ui.css                       \
  gwl/static/jqueryui/jquery-ui.js                        \
  gwl/static/jqueryui/jquery-ui.min.css                   \
  gwl/static/jqueryui/jquery-ui.min.js                    \
  gwl/static/jqueryui/jquery-ui.structure.css             \
  gwl/static/jqueryui/jquery-ui.structure.min.css         \
  gwl/static/jqueryui/jquery-ui.theme.css                 \
  gwl/static/jqueryui/jquery-ui.theme.min.css             \
  gwl/static/highlight/highlight.pack.js                  \
  gwl/static/highlight/LICENSE                            \
  gwl/static/highlight/styles/github.css

EXTRA_DIST += env.in $(WWW_STATIC_RESOURCES)
