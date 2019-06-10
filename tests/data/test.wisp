import
    gnu packages python

process: python-test
    packages
        list python-wrapper
    inputs
        list "A" "B" "C"
    procedure
        . ##python
import os

def hello():
  print("hello from python")
  print("inputs:", os.environ["_GWL_PROCESS_INPUTS"])
  print("name:", os.environ["_GWL_PROCESS_NAME"])

hello()
##

workflow: test-workflow
    processes
        list python-test
