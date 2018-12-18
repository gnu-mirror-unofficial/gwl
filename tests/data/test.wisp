define-module
    test

use-modules
    guix workflows
    guix processes

process: python-test
    package-inputs
        list python
    data-inputs
        list "A" "B" "C"
    procedure #---{python}
import os

def hello():
  print "hello from python"
  print(GWL['data-inputs'])
  print(GWL['name'])

hello()
---

workflow: test-workflow
    processes
        list python-test
