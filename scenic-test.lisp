
(in-package :scenic-test)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defun make-scene ()
  (scenic:scene
    (scenic:layer
        (scenic:upad 3
          (scenic:vbox 10
            (scenic:border (list 1.0 1.0 1.0)
                           1
                           (scenic:bg (list 1.0 0.3 0.3)
                                      (scenic:spc 100 100)))
            (scenic:border (list 1.0 1.0 1.0)
                           1
                           (scenic:bg (list 0.3 1.0 0.3)
                                      (scenic:spc 100 100)))
            (scenic:border (list 1.0 1.0 1.0)
                           1
                           (scenic:bg (list 0.3 0.3 1.0)
                                      (scenic:spc 100 100))))))))


