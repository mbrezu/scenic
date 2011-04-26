
(declaim (optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "lispbuilder-sdl")
(ql:quickload "cl-cairo2")

(load (compile-file "sdl-patch.lisp"))
(load (compile-file "packages.lisp"))
(load (compile-file "scenic-utils.lisp"))
(load (compile-file "scenic-classes.lisp"))
(load (compile-file "scenic-macros.lisp"))
(load (compile-file "scenic.lisp"))
(load (compile-file "scenic-test.lisp"))
