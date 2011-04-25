
(in-package :scenic-test)

(defun make-scene ()
  (scene 800 800
    (layer
        (stk
         (bg (list 1.0 1.0 1.0)
             (flr))
         (upad 3
           (vbox 10
             (border (list 1.0 1.0 1.0)
                     1
                     (bg (list 1.0 0.3 0.3)
                         (spc 100 100)))
             (border (list 1.0 1.0 1.0)
                     1
                     (bg (list 0.3 1.0 0.3)
                         (spc 100 100)))
             (border (list 1.0 1.0 1.0)
                     1
                     (bg (list 0.3 0.3 1.0)
                         (spc 100 100)))))))))


