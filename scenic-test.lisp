
(in-package :scenic-test)

(defun test-scene (scene)
  (scenic:run-scene scene))

;;; A very simple scene that clears the screen.
(defun background-clear ()
  (scene 800 800
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr)))))

;;; A scene with a couple of rectangles
(defun colored-rectangles ()
  (scene 800 800
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr))
           (hbox 5 '(:auto :auto :auto)
             (bg (list 1.0 0.3 0.3)
                 (spc 100 100))
             (bg (list 0.3 1.0 0.3)
                 (spc 100 100))
             (bg (list 0.3 0.3 1.0)
                 (spc 100 100))))))

;;; Hello world!
(defun hello-world ()
  (scene 800 800
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr))
           (upad 1
             (lbl "Hello, world!" :size 20)))))

;;; Button test
(defun buttons ()
  (let (scn push-button toggle-button)
    (setf scn (scene 800 800
                     (stk
                       (bg (list 1.0 1.0 1.0)
                           (flr))
                       (upad 5
                         (hbox 10 nil
                           (border (list 0 0 0) 1 (setf push-button
                                                        (btntxt "Push Button")))
                           (border (list 0 0 0) 1 (setf toggle-button
                                                        (toggle "Toggle Button"))))))))
    (scenic:add-event-handler push-button :mouse-move :cascade
                              (lambda (object event)
                                (format t "button mouse move: ~a ~a~%" object event)))
    (scenic:add-event-handler push-button :mouse-enter :cascade
                              (lambda (object event)
                                (format t "button enter: ~a ~a~%" object event)))
    (scenic:add-event-handler push-button :mouse-leave :cascade
                              (lambda (object event)
                                (format t "button mouse leave: ~a ~a~%" object event)))
    (scenic:add-event-handler push-button :mouse-button-down :cascade
                              (lambda (object event)
                                (format t "button mouse button-down: ~a ~a~%" object event)))
    (scenic:add-event-handler push-button :mouse-button-up :cascade
                              (lambda (object event)
                                (format t "button mouse button-up: ~a ~a~%" object event)))
    (scenic:add-event-handler push-button :click :bubble
                              (lambda (object event)
                                (declare (ignore object event))
                                (format t "button click~%")))
    scn))

(defun slider ()
  (let (scn slider)
    (setf scn (scene 800 800
                     (stk
                       (bg (list 1.0 1.0 1.0)
                           (flr))
                       (vbox 10 nil
                         (upad 5
                           (szr (setf slider (hslider 0 50 30))
                                :max-width 200
                                :max-height 19))))))
    (scenic:add-event-handler slider :position-changed :bubble
                              (lambda (object event)
                                (declare (ignore event))
                                (scenic:print-all t
                                                  (scenic:current-min-position object)
                                                  (scenic:page-size object)
                                                  (scenic:min-value object)
                                                  (scenic:max-value object))))

    scn))

(defun scrollbars ()
  (let (scn horizontal-scrollbar vertical-scrollbar)
    (setf scn (scene 800 800
                     (stk
                       (bg (list 1.0 1.0 1.0)
                           (flr))
                       (upad 5
                         (hbox 2 nil
                           (vbox 2 nil
                             (bg (list 0.3 0.4 0.5)
                                 (spc 238 238))
                             (szr (setf horizontal-scrollbar (hsbar 0 50 30))
                                  :max-height 19
                                  :max-width 200))
                           (szr (setf vertical-scrollbar (vsbar 0 50 30))
                                :max-width 19
                                :max-height 200))))))
    (scenic:add-event-handler horizontal-scrollbar :position-changed :bubble
                              (lambda (object event)
                                (declare (ignore event))
                                (scenic:print-all t
                                                  (scenic:current-min-position object)
                                                  (scenic:page-size object)
                                                  (scenic:min-value object)
                                                  (scenic:max-value object))))
    (scenic:add-event-handler vertical-scrollbar :position-changed :bubble
                              (lambda (object event)
                                (declare (ignore event))
                                (scenic:print-all t
                                                  (scenic:current-min-position object)
                                                  (scenic:page-size object)
                                                  (scenic:min-value object)
                                                  (scenic:max-value object))))
    scn))

(defun icon ()
  (scene 800 800
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr))
           (upad 10
             (stk
               (bg (list 0.8 0.8 0.8)
                   (spc 16 16))
               (szr (img "icons/arrow_in.png")
                    :max-width 16
                    :min-width 16
                    :max-height 16
                    :max-width 16))))))

(defun text-baseline-alignment ()
  (scene 800 800
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr))
           (vbox 0 '(:auto)
             (upad 10
               (hbox 10 '(:auto :auto :auto)
                 (border (list 0.3 0.3 0.3)
                         1
                         (bg (list 0.7 0.7 0.7)
                             (upad 3
                               (lbl "S p" :size 20 :slant :italic))))
                 (border (list 0.3 0.3 0.3)
                         1
                         (bg (list 0.7 0.7 0.7)
                             (upad 3
                               (lbl "S a"
                                    :color (list 0.2 0.4 0.6)
                                    :size 20))))
                 (border (list 0.3 0.3 0.3)
                         1
                         (bg (list 0.7 0.7 0.7)
                             (upad 3
                               (lbl "s j"
                                    :size 20 :weight :bold))))))))))

(defun run-all-tests ()
  (test-scene (background-clear))
  (test-scene (colored-rectangles))
  (test-scene (hello-world))
  (test-scene (buttons))
  (test-scene (slider))
  (test-scene (scrollbars))
  (test-scene (icon))
  (test-scene (text-baseline-alignment)))




