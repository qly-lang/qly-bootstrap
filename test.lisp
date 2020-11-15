#!/usr/bin/env -S sbcl --script
(let ((*default-pathname-defaults* (user-homedir-pathname)))
  (load ".sbclrc"))
(asdf:test-system :qly)
