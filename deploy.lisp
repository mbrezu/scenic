
(declaim (optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "lispbuilder-sdl")
(ql:quickload "cl-cairo2")

(load "sdl-patch.lisp")
(load "packages.lisp")
(load "scenic-utils.lisp")
(load "scenic-classes.lisp")
(load "scenic-macros.lisp")
(load "scenic.lisp")
(load "scenic-test.lisp")
