#!/usr/bin/env python
"""
    @author: Jean-Lou Dupont
"""
import os
import sys
import gcupload as gc

def get_gcpwd():
    path = os.path.expanduser("~")
    file = open("%s/.gcpwd" % path)
    pwd = file.read().strip()
    file.close()
    return pwd

def get_gcuser():
    path = os.path.expanduser("~")
    file = open("%s/.gcuser" % path)
    user = file.read().strip()
    file.close()
    return user

def main():
    try:
        user= get_gcuser()
    except:
        print "! cannot find ~/.gcuser"
        sys.exit(1)
        
    try:
        pwd =  get_gcpwd()
    except:
        print "! cannot find ~/.gcpwd"
        sys.exit(1)
        
    if len(sys.argv) != 3:
        print "! parameters error"
        sys.exit(1)
    
    src= sys.argv[1]
    prj= sys.argv[2]
    
    print "> uploading file <%s>" % src
    #upload(file, project_name, user_name, password, summary, labels=None)
    gc.upload(src, prj, user, pwd, "sources archive", ["sources", "featured"])
    

main()
