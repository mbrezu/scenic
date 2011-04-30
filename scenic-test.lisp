
(in-package :scenic-test)

(defun make-scene ()
  (let (text1 text2 text3 button scn hslider)
    (setf scn
          (scene 800 800
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
                                                    (upad 3
                                                          (setf text1
                                                                (lbl "Ana are pere." :size 20 :slant :italic)))))
                                        (border (list 0.3 0.3 0.3)
                                                1
                                                (bg (list 0.7 0.7 0.7)
                                                    (upad 3
                                                          (setf text2
                                                                (lbl "Petre n-are mere."
                                                                     :color (list 0.2 0.4 0.6)
                                                                     :size 20)))))
                                        (border (list 0.3 0.3 0.3)
                                                1
                                                (bg (list 0.7 0.7 0.7)
                                                    (upad 3
                                                          (setf text3
                                                                (lbl "on."
                                                                     :size 20 :weight :bold))))))
                                  (hbox 10
                                        (border (list 0 0 0) 1 (setf button (btntxt "Gigel")))
                                        (border (list 0 0 0) 1 (toggle "Titel"))
                                        (szr (setf hslider (hslider 0 50 1))
                                             :max-width 200
                                             :max-height 20))))))))
    ;; text1 events
    (scenic:add-event-handler text1 :mouse-move :bubble
                              (lambda (object event)
                                (format t "text1 move: ~a ~a~%" object event)))
    (scenic:add-event-handler text1 :mouse-enter :bubble
                              (lambda (object event)
                                (format t "text1 enter: ~a ~a~%" object event)))
    (scenic:add-event-handler text1 :mouse-leave :bubble
                              (lambda (object event)
                                (format t "text1 leave: ~a ~a~%" object event)))

    ;; text2 events
    (scenic:add-event-handler text2 :mouse-move :bubble
                              (lambda (object event)
                                (format t "text2 move: ~a ~a~%" object event)))
    (scenic:add-event-handler text2 :mouse-enter :bubble
                              (lambda (object event)
                                (format t "text2 enter: ~a ~a~%" object event)))
    (scenic:add-event-handler text2 :mouse-leave :bubble
                              (lambda (object event)
                                (format t "text2 leave: ~a ~a~%" object event)))

    ;; text3 events
    (scenic:add-event-handler text3 :mouse-move :cascade
                              (lambda (object event)
                                (format t "text3 move: ~a ~a~%" object event)))
    (scenic:add-event-handler text3 :mouse-enter :cascade
                              (lambda (object event)
                                (format t "text3 enter: ~a ~a~%" object event)))
    (scenic:add-event-handler text3 :mouse-leave :cascade
                              (lambda (object event)
                                (format t "text3 leave: ~a ~a~%" object event)))

    ;; button events
    (scenic:add-event-handler button :mouse-move :cascade
                              (lambda (object event)
                                (format t "button move: ~a ~a~%" object event)))
    (scenic:add-event-handler button :mouse-enter :cascade
                              (lambda (object event)
                                (format t "button enter: ~a ~a~%" object event)))
    (scenic:add-event-handler button :mouse-leave :cascade
                              (lambda (object event)
                                (format t "button leave: ~a ~a~%" object event)))
    (scenic:add-event-handler button :mouse-button-down :cascade
                              (lambda (object event)
                                (format t "button button-down: ~a ~a~%" object event)))
    (scenic:add-event-handler button :mouse-button-up :cascade
                              (lambda (object event)
                                (format t "button button-up: ~a ~a~%" object event)))
    (scenic:add-event-handler button :click :bubble
                              (lambda (object event)
                                (declare (ignore object event))
                                (format t "button click~%")))
    (scenic:add-event-handler hslider :position-changed :bubble
                              (lambda (object event)
                                (declare (ignore event))
                                (scenic:print-all t
                                           (scenic:current-min-position object)
                                           (scenic:page-size object)
                                           (scenic:min-value object)
                                           (scenic:max-value object))))
    scn))


