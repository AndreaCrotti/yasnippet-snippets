#!/usr/bin/env python
import os
import re
from os.path import join
from shutil import move
def rename(root, f):
    if not f.endswith(".yasnippet"):
        move(join(root, f), join(root, f + ".yasnippet"))
    else:
        print "already ending with the right extension"

CONT = "# contributor: Andrea crotti\n# --"
END = "# --\n\n"

orig = "# --\n\n"
to  = "# --\n"

def insert(root, f, orig, to):
    fname = join(root, f)
    text = open(fname).read()
    nex_text = re.sub(orig, to, text)
    open(fname, 'w').write(nex_text)

for root, dirs, files in os.walk('.'):
    if "mode" in root:
        # os.popen("git add *yasnippet")
        for f in files:
            insert(root, f, orig, to)


            
