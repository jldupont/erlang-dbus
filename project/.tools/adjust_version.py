#!/usr/bin/env python
"""
    @author: Jean-Lou Dupont
    
"""
import os
import shutil
import sys
from string import Template
from types import *

usage="""
    adjust_version.py version File1 [File2 ...]
"""

def replace_params(path_src, path_dest, params):
    """
    Replace the parameters in the target path
    """
    file = open(path_src,"r")
    contents = file.read()
    file.close()
    
    tpl=Template(contents)
    updated_content = tpl.safe_substitute( **params )
    
    file = open(path_dest, "w")
    file.write(updated_content)
    file.close()


def main():
    if len(sys.argv) < 3:
        print usage
        sys.exit(1)
        
    version = sys.argv[1]
    files=sys.argv[2:]
    for file in files:
        try:
            replace_params(file, file, {'version':version})
        except:
            print "! Error processing file: %s\n" % file
    
main()
