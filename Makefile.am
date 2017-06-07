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

SOURCES =                                               \
  gnu/processes.scm                                     \
  gnu/workflows.scm                                     \
  guix/processes.scm                                    \
  guix/process-engines.scm                              \
  guix/process-engines/bash-engine.scm                  \
  guix/process-engines/grid-engine.scm                  \
  guix/workflows.scm                                    \
  guix/workflows/execution-order.scm                    \
  guix/workflows/utils.scm                              \
  guix/workflows/graph.scm                              \
  guix/scripts/workflow.scm                             \
  guix/scripts/process.scm                              \
  web-interface.scm                                     \
  www/config.scm                                        \
  www/util.scm                                          \
  www/pages.scm                                         \
  www/pages/error.scm                                   \
  www/pages/getting-started.scm                         \
  www/pages/getting-started/defining-workflows.scm      \
  www/pages/welcome.scm                                 \
  www/pages/workflow-viewer.scm

WWW_STATIC_RESOURCES =                                  \
  static/css/main.css                                   \
  static/fonts/FiraMono-Regular.ttf                     \
  static/fonts/Roboto-Bold.ttf                          \
  static/fonts/Roboto-LightItalic.ttf                   \
  static/fonts/Roboto-Light.ttf                         \
  static/fonts/OFL.txt                                  \
  static/graphs/index.html                              \
  static/images/g16.png                                 \
  static/images/grid.png                                \
  static/images/grid.svg                                \
  static/images/what-is-gwl.svg                         \
  static/images/workflows-processes.svg                 \
  static/jquery/jquery-3.1.1.min.js                     \
  static/jqueryui/external/jquery/jquery.js             \
  static/jqueryui/images                                \
  static/jqueryui/images/ui-icons_444444_256x240.png    \
  static/jqueryui/images/ui-icons_555555_256x240.png    \
  static/jqueryui/images/ui-icons_777620_256x240.png    \
  static/jqueryui/images/ui-icons_777777_256x240.png    \
  static/jqueryui/images/ui-icons_cc0000_256x240.png    \
  static/jqueryui/images/ui-icons_ffffff_256x240.png    \
  static/jqueryui/jquery-ui.css                         \
  static/jqueryui/jquery-ui.js                          \
  static/jqueryui/jquery-ui.min.css                     \
  static/jqueryui/jquery-ui.min.js                      \
  static/jqueryui/jquery-ui.structure.css               \
  static/jqueryui/jquery-ui.structure.min.css           \
  static/jqueryui/jquery-ui.theme.css                   \
  static/jqueryui/jquery-ui.theme.min.css               \
  static/highlight/highlight.pack.js                    \
  static/highlight/LICENSE                              \
  static/highlight/styles/androidstudio.css

EXTRA_DIST += env.in $(WWW_STATIC_RESOURCES)
