define-module
    test

use-modules
    gwl workflows
    gwl processes
    gnu packages python

process: python-test
    package-inputs
        list python-wrapper
    data-inputs
        list "A" "B" "C"
    procedure
        . ##python
import os

def hello():
  print("hello from python")
  print("inputs:", os.environ["_GWL_PROCESS_DATA_INPUTS"])
  print("name:", os.environ["_GWL_PROCESS_NAME"])

hello()
##

workflow: test-workflow
    processes
        list python-test
