
(in-package :scenic-test)

(defun test-scene (scene)
  (scenic:run-scene scene))

(defvar *scene-width*)
(setf *scene-width* 600)

(defvar *scene-height*)
(setf *scene-height* 600)

;;; A very simple scene that clears the screen.
(defun background-clear ()
  (scene *scene-width* *scene-height*
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr)))))

;;; A scene with a couple of rectangles
(defun colored-rectangles ()
  (scene *scene-width* *scene-height*
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr))
           (vbox 0
                 '(:auto)
                 (list (hbox 5 '(:auto :auto :auto)
                             (list
                              (bg (list 1.0 0.3 0.3)
                                  (spc 100 100))
                              (bg (list 0.3 1.0 0.3)
                                  (spc 100 100))
                              (bg (list 0.3 0.3 1.0)
                                  (spc 100 100)))))))))

;;; Hello world!
(defun hello-world ()
  (scene *scene-width* *scene-height*
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr))
           (upad 1
             (lbl "Hello, world!" :size 20)))))

;;; Button test
(defun buttons ()
  (let (scn push-button toggle-button)
    (setf scn (scene *scene-width* *scene-height*
                     (stk
                       (bg (list 1.0 1.0 1.0)
                           (flr))
                       (upad 5
                         (vbox 0
                               '(:auto)
                               (list
                                (hbox 10
                                      '(:auto :auto)
                                      (list
                                       (border (list 0 0 0) 1
                                               (setf push-button
                                                     (btntxt "Push Button")))
                                       (border (list 0 0 0) 1
                                               (setf toggle-button
                                                     (toggle "Toggle Button")))))))))))
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
    (setf scn (scene *scene-width* *scene-height*
                     (stk
                       (bg (list 1.0 1.0 1.0)
                           (flr))
                       (vbox 10
                             '(:auto)
                             (list (upad 5
                                     (hbox 10
                                           '(:auto)
                                           (list
                                            (szr (setf slider (hslider 0 50 30))
                                                 :max-width 200
                                                 :max-height 19)))))))))
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
    (setf scn (scene *scene-width* *scene-height*
                     (stk
                       (bg (list 1.0 1.0 1.0)
                           (flr))
                       (upad 5
                         (hbox 0
                               '(:auto :auto)
                               (list
                                (vbox 0
                                      '(:auto :auto)
                                      (list (bg (list 0.3 0.4 0.5)
                                                (spc 200 200))
                                            (szr (setf horizontal-scrollbar
                                                       (hsbar 0 50 30))
                                                 :max-height 19
                                                 :max-width 200)))
                                (szr (setf vertical-scrollbar (vsbar 0 50 30))
                                     :max-width 19
                                     :max-height 200)))))))
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
  (scene *scene-width* *scene-height*
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
  (scene *scene-width* *scene-height*
         (stk
           (bg (list 1.0 1.0 1.0)
               (flr))
           (vbox 0 '(:auto)
                 (list
                  (upad 10
                    (hbox 10 '(:auto :auto :auto)
                          (list
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
                                              :size 20 :weight :bold))))))))))))

(defun vbox-layout-options ()
  (labels ((make-strip (text color &optional (max-height nil))
             (upad 3
               (stk
                 (bg color
                     (szr (flr) :max-height max-height))
                 (upad 3 (lbl text :size 18))))))
    (scene *scene-width* *scene-height*
           (stk
             (bg (list 1.0 1.0 1.0)
                 (flr))
             (let ((strips '(((1.0 0.2 0.2) :auto 150)
                             ((0.2 1.0 0.2) (1 :ext))
                             ((0.2 0.2 1.0) (2 :ext))
                             ((0.2 1.0 1.0) (100 :px))
                             ((1.0 0.2 1.0) (200 :px)))))
               (vbox 0
                     (mapcar #'second strips)
                     (mapcar (lambda (layout-option color max-height)
                               (make-strip (with-output-to-string (str)
                                             (prin1 layout-option str))
                                           color
                                           max-height))
                             (mapcar #'second strips)
                             (mapcar #'first strips)
                             (mapcar #'third strips))))))))

(defun hbox-layout-options ()
  (labels ((make-strip (text color &optional (max-width nil))
             (upad 3
               (stk
                 (bg color
                     (szr (flr) :max-width max-width))
                 (upad 3 (lbl text :size 18))))))
    (scene *scene-width* *scene-height*
           (stk
             (bg (list 1.0 1.0 1.0)
                 (flr))
             (let ((strips '(((1.0 0.2 0.2) :auto 150)
                             ((0.2 1.0 0.2) (1 :ext))
                             ((0.2 0.2 1.0) (2 :ext))
                             ((0.2 1.0 1.0) (100 :px))
                             ((1.0 0.2 1.0) (200 :px)))))
               (hbox 0
                     (mapcar #'second strips)
                     (mapcar (lambda (layout-option color max-width)
                               (make-strip (with-output-to-string (str)
                                             (prin1 layout-option str))
                                           color
                                           max-width))
                             (mapcar #'second strips)
                             (mapcar #'first strips)
                             (mapcar #'third strips))))))))

(defun grid-basic ()
  (labels ((make-cell (text)
             (upad 3
               (bg (list 0.8 0.8 0.8)
                   (upad 10 (lbl text :size 14))))))
    (scene *scene-width* *scene-height*
           (stk
             (bg (list 1.0 1.0 1.0)
                 (flr))
             (grid nil
                   nil
                   `((:column (:cell ,(make-cell "Cell 0 0"))
                              (:cell ,(make-cell "Cell 0 1"))
                              (:cell ,(make-cell "Cell 0 2")))
                     (:column (:cell ,(make-cell "Cell 1 0"))
                              (:cell ,(make-cell "Cell 1 1"))
                              (:cell ,(make-cell "Cell 1 2")))
                     (:column (:cell ,(make-cell "Cell 2 0"))
                              (:cell ,(make-cell "Cell 2 1"))
                              (:cell ,(make-cell "Cell 2 2")))))))))

(defun grid-offset ()
  (labels ((make-cell (text color)
             (upad 3
               (bg color
                   (upad 10 (lbl text :size 14))))))
    (let ((color1 (list 0.8 0.8 0.3))
          (color2 (list 0.3 0.8 0.8))
          (color3 (list 0.8 0.3 0.8))
          (color4 (list 0.9 0.3 0.5)))
      (scene *scene-width* *scene-height*
             (stk
               (bg (list 1.0 1.0 1.0)
                   (flr))
               (grid nil
                     nil
                     `((:offset 0 0
                                (:row (:cell ,(make-cell "Cell 0 0" color1))
                                      (:cell ,(make-cell "Cell 1 0" color1)))
                                (:row (:cell ,(make-cell "Cell 0 1" color1))
                                      (:cell ,(make-cell "Cell 1 1" color1)))
                                (:row (:cell ,(make-cell "Cell 0 2" color1))
                                      (:cell ,(make-cell "Cell 1 2" color1))))
                       (:offset 2 0
                                (:row (:cell ,(make-cell "Cell 2 0" color2))
                                      (:cell ,(make-cell "Cell 3 0" color2)))
                                (:row (:cell ,(make-cell "Cell 2 1" color2))
                                      (:cell ,(make-cell "Cell 3 1" color2)))
                                (:row (:cell ,(make-cell "Cell 2 2" color2))
                                      (:cell ,(make-cell "Cell 3 2" color2))))
                       (:offset 4 0
                                (:row (:cell ,(make-cell "Cell 4 0" color3))
                                      (:cell ,(make-cell "Cell 5 0" color3)))
                                (:row (:cell ,(make-cell "Cell 4 1" color3))
                                      (:cell ,(make-cell "Cell 5 1" color3)))
                                (:row (:cell ,(make-cell "Cell 4 2" color3))
                                      (:cell ,(make-cell "Cell 5 2" color3))))
                       (:offset 0 3
                                (:row (:cell ,(make-cell "Cell 0 3" color4))
                                      (:cell ,(make-cell "Cell 1 3" color4))
                                      (:cell ,(make-cell "Cell 2 3" color4))
                                      (:cell ,(make-cell "Cell 3 3" color4))
                                      (:cell ,(make-cell "Cell 4 3" color4))
                                      (:cell ,(make-cell "Cell 5 3" color4)))))))))))

(defun grid-spans ()
  (labels ((make-cell (text color)
             (upad 3
               (bg color
                   (upad 10 (lbl text :size 14))))))
    (let ((color1 (list 0.8 0.8 0.3))
          (color2 (list 0.3 0.8 0.8))
          (color3 (list 0.8 0.3 0.8))
          (color4 (list 0.9 0.3 0.5)))
      (scene *scene-width* *scene-height*
             (stk
               (bg (list 1.0 1.0 1.0)
                   (flr))
               (grid nil
                     nil
                     `((:offset 0 0
                                (:row (:cell :colspan 2 ,(make-cell "Cell 0 0" color1)))
                                (:row (:cell ,(make-cell "Cell 0 1" color1))
                                      (:cell :rowspan 2 ,(make-cell "Cell 1 1" color1)))
                                (:row (:cell ,(make-cell "Cell 0 2" color1))))
                       (:offset 2 0
                                (:row (:cell ,(make-cell "Cell 2 0" color2))
                                      (:cell ,(make-cell "Cell 3 0" color2)))
                                (:row (:cell ,(make-cell "Cell 2 1" color2))
                                      (:cell ,(make-cell "Cell 3 1" color2)))
                                (:row (:cell ,(make-cell "Cell 2 2" color2))
                                      (:cell ,(make-cell "Cell 3 2" color2))))
                       (:offset 4 0
                                (:row (:cell ,(make-cell "Cell 4 0" color3))
                                      (:cell ,(make-cell "Cell 5 0" color3)))
                                (:row (:cell ,(make-cell "Cell 4 1" color3))
                                      (:cell ,(make-cell "Cell 5 1" color3)))
                                (:row (:cell ,(make-cell "Cell 4 2" color3))
                                      (:cell ,(make-cell "Cell 5 2" color3))))
                       (:offset 0 3
                                (:row (:cell ,(make-cell "Cell 0 3" color4))
                                      (:cell :colspan 3 ,(make-cell "Cell 1 3" color4))
                                      (:cell ,(make-cell "Cell 4 3" color4))
                                      (:cell ,(make-cell "Cell 5 3" color4)))))))))))

(defun run-all-tests ()
  (test-scene (background-clear))
  (test-scene (colored-rectangles))
  (test-scene (hello-world))
  (test-scene (buttons))
  (test-scene (slider))
  (test-scene (scrollbars))
  (test-scene (icon))
  (test-scene (text-baseline-alignment))
  (test-scene (vbox-layout-options))
  (test-scene (hbox-layout-options))
  (test-scene (grid-basic))
  (test-scene (grid-offset))
  (test-scene (grid-spans)))
