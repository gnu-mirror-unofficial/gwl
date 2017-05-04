GNU Guix Workflow Language extension
====================================

This project provides two subcommands to GNU Guix and introduces two record
types that provide a workflow management extension built on top of GNU Guix's
package manager.

## Installation

1. [Install GNU Guix](https://www.gnu.org/software/guix/manual/html_node/Binary-Installation.html)

2. Clone this repository:

```bash
git clone https://git.roelj.com/guix/gwl.git
```

3. Set the `GUILE_LOAD_PATH` to include GNU Guix and GWL:

```bash
GUIX_DIR=`guix build guix`
GWL_DIR=`pwd`/gwl
export GUILE_LOAD_PATH="$GUIX_DIR:$GWL_DIR${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
```

Now you're all set.

### Bonus: Compile the GWL modules for a speed-up

To compile the GWL source code, run these commands from the project's directory:
```bash
guix environment guix --ad-hoc guix
autoreconf -vfi
./configure
make
```

Now, modify the `GUILE_LOAD_COMPILED_PATH` environment variable:
```bash
GWL_DIR=`pwd`
export GUILE_LOAD_COMPILED_PATH="$GWL_DIR${GUILE_COMPILED_LOAD_PATH:+:}$GUILE_COMPILED_LOAD_PATH"
```

## Getting started

GWL has a built-in getting-started guide.  To use it, run:
```bash
guix workflow --web-interface
```

Then point your web browser to the following location:
```bash
http://localhost:5000
```
