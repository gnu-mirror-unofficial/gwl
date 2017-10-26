## Getting started

### Installation

This guide assumes [GNU Guix](https://www.gnu.org/software/guix/manual/html_node/Binary-Installation.html) 
and the [GWL extension](https://git.roelj.com/guix/gwl.git) have been installed
already.  In case the GWL extension hasn't been installed, run:

```
guix package -i gwl
```

If you are wondering about which editor to use, anything that
can edit text will do, but [GNU Emacs](https://www.gnu.org/software/emacs)
with [Geiser](http://nongnu.org/geiser/) is an excellent choice for interactively
running the Scheme code used in this guide.

### An overview of the concepts

In the GWL there are two concepts we need to know about: *processes* and
*workflows*.  We describe a computation (running a program, or evaluating
a Scheme expression) using a `process`.  With a `workflow` we describe how
multiple processes relate to each other (process `B` must run after
process `A`, process `C` must run before process `A`).

### Defining a process

Writing *processes* involves writing Scheme code.  Because this workflow language
builds on top of [GNU Guix](https://www.gnu.org/software/guix), we use 
[GNU Guile](https://www.gnu.org/software/guile).  First, we need to load the
module called `(guix processes)`.

```
(use-modules (guix processes))
```

Now that we can use the `process` type, we can define a process:

```
(define hello-world
  (process
    (name "hello-world")
    (procedure '(begin
                  (display "Hello world!")
                  (newline)))))
```

We've created a symbol called `hello-world` that contains a `process` with
the name `hello-world` and a Scheme expression to display `Hello world` on
our screen as the computational procedure.

#### Running programs as `procedure`

The common use-case of running an existing program involves two steps:
1. Making sure the program is available to run (deployment);
2. Running the program in its proper environment (setting environment
   variables in such a way that the program can find its modules).

In the GWL, we use GNU Guix to perform both steps.  To make this syntactically
easy, we use the `(guix gexp)` module, which will deploy a program before
running it.

Additionally, to find the programs, we need to load the module wherein
the package for a program is defined.  So, for `samtools`, we must load
the `(guix packages bioinformatics)` module.

So we now get:
```
(use-modules (guix processes)
             (guix gexp)
             (gnu packages bioinformatics))

(define run-samtools
  (process
    (name "samtools-index")
    (package-inputs (list samtools))
    (data-inputs "/data/sample.bam")
    (procedure
      #~(system* "samtools" "index" #$data-inputs))))

```

With `package-inputs` we describe which packages make up the environment
for the procedure.  The `data-inputs` field can be used to specify the
input data, and the syntactic elements `#~` and `#$` make the G-Expression.

The field `package-inputs` must be a list (defined using `(list ...)` that
contains the symbol names of the packages.  For `samtools`, that is simply
`samtools`.

The `data-inputs` field can contain anything, and in this case we specify 
the path to the file we want to index as a string, which we can directly
pass to the `system*` function, that will run this system command for us.

### Dynamically defining processes

Let's say we have multiple files: `FILE1`, `FILE2`, `FILE3`, `FILE4`, 
`FILE5`, `FILE6`, `FILE7`, and `FILE8`.  And let's assume these files are
located in the folders: `/path/to/<filename>/<filename>`.

And generate a specific process for each file by writing a function
that returns a process, like so:
```
(define (run-samtools filename)
  (process
    (name (string-append "samtools-index-on-" filename))
    (package-inputs (list samtools))
    (data-inputs (string-append "/path/to/" filename "/" filename))
    (procedure
     #~(system* "samtools" "index" #$data-inputs))))

```

Then, we can generate a process description by running:
```
(run-samtools "FILE1")
```

So, we can put these filenames in a list:
```
(define my-files '("FILE1" "FILE2" "FILE3" "FILE4" 
                   "FILE5" "FILE6" "FILE7" "FILE8"))
```


And define a process for each file in the list:
```
(for-each (lambda (filename)
            (define-dynamically 
              (symbol-append 'run-samtools- (string->symbol filename))
              (run-samtools filename)))
          my-files)
```

### Listing processes

We can verify that this works by listing the processes using the command-line:
```
$ guix process --list-available
```

GWL looks for workflow definitions in the directory of the environment
variable `GUIX_WORKFLOW_PATH`.  This environment variable can be used in
a similar way to `GUIX_PACKAGE_PATH`.

### Combining processes in a workflow

Let's assume we have a set of processes: `A` to `F`, and another set of 
processes: `G` to `L`.  If we would like to run `G` after `A`, `H` after
`B`, and so on until `L` after `F`, we can define a `workflow` that runs
all `A` to `L` processes, in the right order.

```
(use-modules (srfi srfi-1)     ; For the 'zip' function.
             (guix workflows)) ; For the 'workflow' record type.

(define example-workflow
  (let ((first-set  (list A B C D E F))
        (second-set (list G H I J K L)))
    (workflow
      (name "example-workflow")
      (processes (append first-set second-set))
      (restrictions
        (zip first-set second-set)))))
```

Running the workflow can be done using the command:
```
$ guix workflow --run=example-workflow
```

A plot of the execution order can be generated using:
```
guix workflow --graph=example-workflow | dot -Tpdf > execution-graph.pdf
```

### Running a workflow on a computing cluster

GWL implements *execution engines* to scale the computing capabilities of
computing clusters.  The default execution engine is a local engine, which
creates a shell script for each process, and executes them in the right order.

In computing clusters implementing a Grid Engine job-scheduling system, the
`grid-engine` engine can be used by specifying it on the command-line:

```
$ guix workflow --run=example-workflow --engine=grid-engine
```

This command will submit the jobs to the grid scheduler, maintaining the
run-time restrictions (`G` can only run after `A` has finished).

The execution engines are implemented as separate Scheme modules, allowing
one to create new execution engines for specific compute environments.
