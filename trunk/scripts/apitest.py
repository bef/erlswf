#!/usr/bin/env python

import pexpect
import sys

if len(sys.argv) == 1:
	sys.exit(1)

filenames = sys.argv[1:]

p = pexpect.spawn("ssacli api")
# p.logfile = sys.stdout

for fn in filenames:
	p.expect("100 .*?\n")
	print "ready for " + fn
	
	p.sendline("version " + fn)
	p.expect(".*?\n") ## discard sendline echo
	
	p.expect("\n([2-9]\d\d) (.*?)\r\n")
	ret_code, ret_msg = (p.match.group(1), p.match.group(2))
	print "return:", ret_code, ret_msg
	print "output:",
	print p.before
	

p.close()
