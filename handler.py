#!/usr/bin/env python
"""Handle a Uzbl callback with Emacs.

Correctly formats a specially-crafted Uzbl callback to work correctly for the
Ezbl library. Invokes emacsclient, which calls 'ezbl-callback' with the type of
the argument and the normal Uzbl arguments.

This script must be called as follows:

  $ handler.py config-file pid x-window-id fifo-filename socket-filename current-url current-title [...] handler-type server-name

Where "[...]" indicates the optional additional Uzbl arguments (such as when the
cookie handler script is called). "handler-type" is one of:
  - "load_finish"
  - "load_start"
  - "load_commit"
  - "history"
  - "download"
  - "cookie"
  - "new_window"

and "server-name" is the value of the variable "server-name" in the Emacs
instance in which Uzbl is running.

The proper way to ensure that Uzbl calls this script correctly is to set the
handler as follows:

  set load_finish_handler = spawn /path/to/handler.py load_finish ezbl

Where "ezbl" is the Emacs server name (which defaults to "server").
"""

import sys
import subprocess

args = sys.argv[1:-2]
handler_type = sys.argv[-2]
server = sys.argv[-1]

def join_array(args):
    return '" "'.join(args)

subprocess.call(["emacsclient",
                 "-s",
                 server,
                 "-e",
                 '(ezbl-callback "%s" "%s")' % (handler_type, join_array(args))])
