
(in-package :scenic-test)

(defun make-scene ()
  (scene 800 800
    (layer
        (stk
          (bg (list 1.0 1.0 1.0)
              (flr))
          (upad 3
            (bg (list 0.9 0.9 0.9)
                (vbox 10
                  (border (list 0.3 0.3 0.3)
                          1
                          (bg (list 1.0 0.3 0.3)
                              (spc 100 100)))
                  (border (list 0.3 0.3 0.3)
                          1
                          (bg (list 0.3 1.0 0.3)
                              (spc 100 100)))
                  (border (list 0.3 0.3 0.3)
                          1
                          (bg (list 0.3 0.3 1.0)
                              (spc 100 100)))
                  (hbox 10
                    (border (list 0.3 0.3 0.3)
                            1
                            (bg (list 0.7 0.7 0.7)
                                (upad 3 (lbl "Ana are pere." :size 20 :slant :italic))))
                    (border (list 0.3 0.3 0.3)
                            1
                            (bg (list 0.7 0.7 0.7)
                                (upad 3 (lbl "Petre n-are mere."
                                             :color (list 0.2 0.4 0.6)
                                             :size 20))))
                    (Border (list 0.3 0.3 0.3)
                            1
                            (bg (list 0.7 0.7 0.7)
                                (upad 3 (lbl "on." :size 20 :weight :bold))))))))))))


