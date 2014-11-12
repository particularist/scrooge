#!/usr/bin/env python
import sys
import os
import os.path
if os.environ["TRAVIS_SECURE_ENV_VARS"] == "false":
  print "no secure env vars available, skipping deployment"
  sys.exit()

homedir = os.path.expanduser("~")

host = os.environ["HOST"]
user = os.environ["SONATYPE_USERNAME"]
password = os.environ["SONATYPE_PASSWORD"]

f = open(homedir + '/.sbt/.credentials', 'w')
f.write("host="+host+"\nuser="+user+"\npass="+password+"\n")
f.close()