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
END = "# --"

def insert(root, f, from, to):
    fname = join(root, f)
    nex_text = re.sub(from, to, open(fname).read())
    open(fname, 'w').write(nex_text)

for root, dirs, files in os.walk('.'):
    if "mode" in root:
        # os.popen("git add *yasnippet")
        for f in files:
            if f != '.yas-parents':
                rename(root, f)
                insert(root, f, END+"\n", END)


            
