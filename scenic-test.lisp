
(in-package :scenic-test)

(declaim (optimize (debug 3)))

(defun test-scene (scene &optional record)
  (let ((scenic:*event-recording-enabled* record)
        (scenic:*test-channel-enabled* record))
    (labels ((event-handler (o e)
               (declare (ignore o))
               (when (and (scenic:unicode e)
                          (char= (scenic:unicode e) #\Esc))
                 (sdl:push-quit-event))))
      (scenic:add-event-handler (scenic:widget scene) :key-down :cascade #'event-handler)
      (scenic:run-scene scene)
      (scenic:remove-event-handler (scenic:widget scene) :key-down :cascade #'event-handler)))
  (when record
    (reverse scenic:*session-record*)))

(defvar *scene-width*)
(setf *scene-width* 700)

(defvar *scene-height*)
(setf *scene-height* 700)

(defvar *manual-test-run* t)

;;; A very simple scene that clears the screen.
(defun background-clear ()
  (scene *scene-width* *scene-height*
         (stack
          (background (list 1.0 1.0 1.0)
                      (filler)))))

;;; A scene with a couple of rectangles
(defun colored-rectangles ()
  (scene *scene-width* *scene-height*
         (stack
          (background (list 1.0 1.0 1.0)
                      (filler))
          (vertical-box 0
                        '(:auto)
                        (list (horizontal-box 5 '(:auto :auto :auto)
                                              (list
                                               (background (list 1.0 0.3 0.3)
                                                           (placeholder 100 100))
                                               (background (list 0.3 1.0 0.3)
                                                           (placeholder 100 100))
                                               (background (list 0.3 0.3 1.0)
                                                           (placeholder 100 100))))))

          )))

;;; Hello world!
(defun hello-world ()
  (scene *scene-width* *scene-height*
         (stack
          (background (list 1.0 1.0 1.0)
                      (filler))
          (label "Hello, world!" :size 20))))

;;; Button test
(defun buttons ()
  (let (scn push-button toggle-button)
    (setf scn (scene *scene-width* *scene-height*
                     (stack
                      (background (list 1.0 1.0 1.0)
                                  (filler))
                      (uniform-padding
                       5
                       (vertical-box
                        0
                        '(:auto)
                        (list
                         (horizontal-box 10
                                         '(:auto :auto)
                                         (list
                                          (border (list 0 0 0) 1
                                                  (setf push-button
                                                        (button-text "Push Button")))
                                          (border
                                           (list 0 0 0) 1
                                           (setf toggle-button
                                                 (toggle "Toggle Button")))))))))))
    (scenic:add-event-handler push-button :mouse-move :cascade
                              (lambda (object event)
                                (scenic:test-channel-write
                                 (format nil "button mouse move: ~a ~a~%" object event))))
    (scenic:add-event-handler push-button :mouse-enter :cascade
                              (lambda (object event)
                                (scenic:test-channel-write
                                 (format nil "button enter: ~a ~a~%" object event))))
    (scenic:add-event-handler push-button :mouse-leave :cascade
                              (lambda (object event)
                                (scenic:test-channel-write
                                 (format nil "button mouse leave: ~a ~a~%" object event))))
    (scenic:add-event-handler push-button :mouse-button-down :cascade
                              (lambda (object event)
                                (scenic:test-channel-write
                                 (format nil "button mouse button-down: ~a ~a~%"
                                         object event))))
    (scenic:add-event-handler push-button :mouse-button-up :cascade
                              (lambda (object event)
                                (scenic:test-channel-write
                                 (format nil "button mouse button-up: ~a ~a~%"
                                         object event))))
    (scenic:add-event-handler push-button :click nil
                              (lambda (object event)
                                (declare (ignore object event))
                                (when *manual-test-run*
                                  (format t "push button clicked~%"))
                                (scenic:test-channel-write
                                 (format nil "push button clicked"))))
    (scenic:add-event-handler toggle-button :state-changed nil
                              (lambda (object event)
                                (declare (ignore object event))
                                (when *manual-test-run*
                                  (format t "toggle button ~a~%"
                                          (if (scenic:state toggle-button)
                                              "on"
                                              "off")))
                                (scenic:test-channel-write
                                 (format nil "toggle button ~a"
                                         (if (scenic:state toggle-button)
                                             "on"
                                             "off")))))
    scn))

(defun slider ()
  (let (scn slider)
    (setf scn (scene *scene-width* *scene-height*
                     (stack
                      (background (list 1.0 1.0 1.0)
                                  (filler))
                      (vertical-box
                       10
                       '(:auto)
                       (list (uniform-padding 5
                                              (horizontal-box
                                               10
                                               '(:auto)
                                               (list
                                                (sizer (setf slider
                                                             (horizontal-slider 0 50 30))
                                                       :max-width 200
                                                       :max-height 19)))))))))
    (scenic:add-event-handler slider :position-changed nil
                              (lambda (object event)
                                (declare (ignore event))
                                (when *manual-test-run*
                                  (print-all t
                                             (scenic:current-min-position object)
                                             (scenic:page-size object)
                                             (scenic:min-value object)
                                             (scenic:max-value object)))
                                (scenic:test-channel-write
                                 (list (scenic:current-min-position object)
                                       (scenic:page-size object)
                                       (scenic:min-value object)
                                       (scenic:max-value object)))))
    scn))

(defun scrollbar ()
  (let (scn horizontal-scrollbar vertical-scrollbar)
    (setf scn (scene *scene-width* *scene-height*
                     (stack
                      (background (list 1.0 1.0 1.0)
                                  (filler))
                      (sizer (setf horizontal-scrollbar
                                   (vertical-scrollbar 0 50 30))
                             :max-height 200
                             :max-width 19))))
    (scenic:add-event-handler horizontal-scrollbar :position-changed nil
                              (lambda (object event)
                                (declare (ignore event))
                                (when *manual-test-run*
                                  (print-all t
                                             (scenic:current-min-position object)
                                             (scenic:page-size object)
                                             (scenic:min-value object)
                                             (scenic:max-value object)))
                                (scenic:test-channel-write
                                 (list (scenic:current-min-position object)
                                       (scenic:page-size object)
                                       (scenic:min-value object)
                                       (scenic:max-value object)))))
    scn))

(defun scrollbars ()
  (let (scn horizontal-scrollbar vertical-scrollbar)
    (setf scn (scene *scene-width* *scene-height*
                     (stack
                      (background (list 1.0 1.0 1.0)
                                  (filler))
                      (uniform-padding 5
                                       (horizontal-box
                                        0
                                        '(:auto (1 :auto))
                                        (list
                                         (vertical-box
                                          0
                                          '(:auto (1 :auto))
                                          (list (background
                                                 (list 0.3 0.4 0.5)
                                                 (placeholder 200 200))
                                                (sizer (setf horizontal-scrollbar
                                                             (horizontal-scrollbar 0 50 30))
                                                       :max-height 19
                                                       :max-width 200)))
                                         (sizer (setf vertical-scrollbar
                                                      (vertical-scrollbar 0 50 30))
                                                :max-width 19
                                                :max-height 200)))))))
    (scenic:add-event-handler horizontal-scrollbar :position-changed nil
                              (lambda (object event)
                                (declare (ignore event))
                                (when *manual-test-run*
                                  (print-all t
                                             (scenic:current-min-position object)
                                             (scenic:page-size object)
                                             (scenic:min-value object)
                                             (scenic:max-value object)))
                                (scenic:test-channel-write
                                 (list (scenic:current-min-position object)
                                       (scenic:page-size object)
                                       (scenic:min-value object)
                                       (scenic:max-value object)))))
    (scenic:add-event-handler vertical-scrollbar :position-changed nil
                              (lambda (object event)
                                (declare (ignore event))
                                (when *manual-test-run*
                                  (print-all t
                                             (scenic:current-min-position object)
                                             (scenic:page-size object)
                                             (scenic:min-value object)
                                             (scenic:max-value object)))
                                (scenic:test-channel-write
                                 (list (scenic:current-min-position object)
                                       (scenic:page-size object)
                                       (scenic:min-value object)
                                       (scenic:max-value object)))))
    scn))

(defun icon ()
  (scene *scene-width* *scene-height*
         (stack
          (background (list 1.0 1.0 1.0)
                      (filler))
          (uniform-padding 10
                           (sizer (stack
                                   (background (list 0.8 0.8 0.8)
                                               (filler))
                                   (image "icons/arrow_in.png"))
                                  :max-width 16
                                  :min-width 16
                                  :max-height 16
                                  :max-width 16)))))

(defun text-baseline-alignment ()
  (scene *scene-width* *scene-height*
         (stack
          (background (list 1.0 1.0 1.0)
                      (filler))
          (vertical-box
           0 '(:auto)
           (list
            (uniform-padding
             10
             (horizontal-box
              10 '(:auto :auto :auto)
              (list
               (border
                (list 0.3 0.3 0.3) 1
                (background (list 0.7 0.7 0.7)
                            (uniform-padding 3
                                             (label "S p" :size 20
                                                    :slant :italic))))
               (border
                (list 0.3 0.3 0.3) 1
                (background (list 0.7 0.7 0.7)
                            (uniform-padding 3
                                             (label "S a"
                                                    :color (list 0.2 0.4 0.6)
                                                    :size 20))))
               (border
                (list 0.3 0.3 0.3) 1
                (background (list 0.7 0.7 0.7)
                            (uniform-padding 3
                                             (label "s o"
                                                    :size 20
                                                    :weight :bold))))))))))))

(defun vertical-box-layout-options ()
  (labels ((make-strip (text color &optional (max-height nil))
             (uniform-padding 3
                              (stack
                               (background color
                                           (sizer (filler) :max-height max-height))
                               (uniform-padding 3 (label text :size 18))))))
    (scene *scene-width* *scene-height*
           (stack
            (background (list 1.0 1.0 1.0)
                        (filler))
            (let ((strips '(((1.0 0.2 0.2) :auto 150)
                            ((0.2 1.0 0.2) (1 :ext))
                            ((0.2 0.2 1.0) (2 :ext))
                            ((0.2 1.0 1.0) (100 :px))
                            ((1.0 0.2 1.0) (200 :px)))))
              (vertical-box 0
                            (mapcar #'second strips)
                            (mapcar (lambda (layout-option color max-height)
                                      (make-strip (with-output-to-string (str)
                                                    (prin1 layout-option str))
                                                  color
                                                  max-height))
                                    (mapcar #'second strips)
                                    (mapcar #'first strips)
                                    (mapcar #'third strips))))))))

(defun horizontal-box-layout-options ()
  (labels ((make-strip (text color &optional (max-width nil))
             (uniform-padding 3
                              (stack
                               (background color
                                           (sizer (filler) :max-width max-width))
                               (uniform-padding 3 (label text :size 18))))))
    (scene *scene-width* *scene-height*
           (stack
            (background (list 1.0 1.0 1.0)
                        (filler))
            (let ((strips '(((1.0 0.2 0.2) :auto 150)
                            ((0.2 1.0 0.2) (1 :ext))
                            ((0.2 0.2 1.0) (2 :ext))
                            ((0.2 1.0 1.0) (100 :px))
                            ((1.0 0.2 1.0) (200 :px)))))
              (horizontal-box 0
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
             (uniform-padding 3
                              (background (list 0.8 0.8 0.8)
                                          (uniform-padding 10 (label text :size 14))))))
    (scene *scene-width* *scene-height*
           (stack
            (background (list 1.0 1.0 1.0)
                        (filler))
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
             (uniform-padding 3
                              (background color
                                          (uniform-padding 10 (label text :size 14))))))
    (let ((color1 (list 0.8 0.8 0.3))
          (color2 (list 0.3 0.8 0.8))
          (color3 (list 0.8 0.3 0.8))
          (color4 (list 0.9 0.3 0.5)))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
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
             (uniform-padding 3
                              (background color
                                          (uniform-padding 10 (label text :size 14))))))
    (let ((color1 (list 0.8 0.8 0.3))
          (color2 (list 0.3 0.8 0.8))
          (color3 (list 0.8 0.3 0.8))
          (color4 (list 0.9 0.3 0.5)))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
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

(defun grid-layout-options ()
  (labels ((make-cell (text color)
             (uniform-padding 3
                              (background color
                                          (uniform-padding 10 (label text :size 14))))))
    (let ((color1 (list 0.8 0.8 0.3))
          (color2 (list 0.3 0.8 0.8))
          (color3 (list 0.8 0.3 0.8))
          (color4 (list 0.9 0.3 0.5)))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (grid '((100 :px) (80 :px) (100 :px) (80 :px))
                    '((100 :px) (100 :px) (1 :ext) (100 :px))
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

(defun grid-layout-options-2 ()
  (let ((max-height 30))
    (labels ((make-prompt-cell (text color)
               (uniform-padding 3
                                (background color
                                            (uniform-padding 3 (label text :size 14)))))
             (make-text-cell (color1 color2)
               (sizer (uniform-padding 3
                                       (border color1 1
                                               (background color2 (filler))))
                      :max-height max-height))
             (make-button-cell (text)
               (uniform-padding 3
                                (button-text text))))
      (let ((color1 (list 0.8 0.8 0.3))
            (black (list 0.0 0.0 0.0)))
        (scene *scene-width* *scene-height*
               (stack
                (background (list 1.0 1.0 1.0)
                            (filler))
                (grid '((150 :px) (1 :ext) :auto)
                      '(:auto (100 :px) (50 :px) (1 :ext))
                      `((:offset 0 0
                                 (:row (:cell ,(make-prompt-cell "Field 1:" color1))
                                       (:cell ,(make-text-cell black color1))
                                       (:cell ,(make-button-cell "Activate")))
                                 (:row (:cell ,(make-prompt-cell "Field 2:" color1))
                                       (:cell ,(make-text-cell black color1))
                                       (:cell ,(make-button-cell "Deactivate")))
                                 (:row (:cell ,(make-prompt-cell "Field 3:" color1))
                                       (:cell ,(make-text-cell black color1))
                                       (:cell ,(make-button-cell "Edit")))
                                 (:row (:cell ,(make-prompt-cell "Field 4:" color1))
                                       (:cell ,(make-text-cell black color1))
                                       (:cell ,(make-button-cell "Filler"))))))))))))

(defun grid-layout-options-3 ()
  (let ((color1 (list 0.8 0.8 0.3)))
    (labels ((make-prompt-cell (text color)
               (uniform-padding 3
                                (background color
                                            (uniform-padding 3 (label text :size 14)))))
             (make-layout-options ()
               (loop
                  for i from 1 to 5
                  collect :auto
                  collect (list 15 :px)
                  collect (list 1 :ext)))
             (make-cells ()
               (loop
                  for i from 1 to 15
                  collect `(:cell ,(make-prompt-cell "T" color1))))
             (make-rows ()
               (loop
                  for i from 1 to 15
                  collect `(:row ,@(make-cells)))))
      (let ()
        (scene *scene-width* *scene-height*
               (stack
                (background (list 1.0 1.0 1.0)
                            (filler))
                (grid (make-layout-options)
                      (make-layout-options)
                      `((:offset 0 0
                                 ,@(make-rows))))))))))

(defun aligner-1 ()
  (let ((color1 (list 0.7 0.9 0.4))
        (h-options '(:left :center :right :fill))
        (v-options '(:top :center :bottom :fill)))
    (labels ((make-cell (text &key (horizontal :center) (vertical :center))
               (uniform-padding 2
                                (stack
                                 (background color1 (filler))
                                 (aligner (button-text text)
                                          :horizontal horizontal
                                          :vertical vertical))))
             (make-row (v-option)
               (loop
                  for h-option in h-options
                  collect (list :cell (make-cell (format nil "~a-~a" h-option v-option)
                                                 :horizontal h-option
                                                 :vertical v-option)))))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (grid nil
                    nil
                    `((:row ,@(make-row (elt v-options 0)))
                      (:row ,@(make-row (elt v-options 1)))
                      (:row ,@(make-row (elt v-options 2)))
                      (:row ,@(make-row (elt v-options 3))))))))))

(defun clipper-1 ()
  (let ((color1 (list 0.7 0.9 0.4))
        (color2 (list 0.4 0.7 0.8)))
    (labels ((unbounded-cell ()
               (uniform-padding
                3
                (background
                 color2
                 (uniform-padding 3 (label "Not clipped cell." :size 18)))))
             (bounded-cell ()
               (uniform-padding
                3
                (background
                 color2
                 (uniform-padding 3 (clipper (label "Clipped cell." :size 18)))))))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (horizontal-box
               0
               '((70 :px))
               (list
                (background
                 color1
                 (grid nil
                       nil
                       `((:column (:cell ,(unbounded-cell))
                                  (:cell ,(bounded-cell))
                                  (:cell ,(unbounded-cell)))))))))))))

(defun glass-1 ()
  (scene *scene-width* *scene-height*
         (stack
          (background (list 1.0 1.0 1.0)
                      (filler))
          (vertical-box 0
                        '(:auto)
                        (list (horizontal-box
                               5 '(:auto :auto :auto)
                               (list
                                (glass 0.2 (background (list 1.0 0.3 0.3)
                                                       (placeholder 100 100)))
                                (glass 0.2 (background (list 0.3 1.0 0.3)
                                                       (placeholder 100 100)))
                                (glass 0.2 (background (list 0.3 0.3 1.0)
                                                       (placeholder 100 100)))))
                              (uniform-padding
                               3
                               (border
                                (list 0.0 0.0 0.0) 1
                                (background (list 0.8 0.8 0.8)
                                            (uniform-padding
                                             3
                                             (label "The quick brown etc." :size 20))))))))))

(defun henchman-1 ()
  (labels ((make-child (color)
             (background color
                         (filler))))
    (let ((color1 (list 1.0 0.3 0.3))
          (color2 (list 0.3 1.0 0.3))
          (color3 (list 0.3 0.3 1.0))
          (color4 (list 0.7 0.3 1.0)))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (henchman '((:left 10 :top 10 :width 100 :height 100)
                          (:left 10 :bottom 10 :width 100 :height 100)
                          (:right 10 :top 10 :width 100 :height 100)
                          (:right 10 :bottom 10 :width 100 :height 100))
                        (list (make-child color1)
                              (make-child color2)
                              (make-child color3)
                              (make-child color4))))))))

(defun henchman-glass ()
  (labels ((make-child (color)
             (glass 0.2 (background color
                                    (filler)))))
    (let ((color1 (list 1.0 0.3 0.3))
          (color2 (list 0.3 1.0 0.3))
          (color3 (list 0.3 0.3 1.0))
          (color4 (list 0.7 0.3 1.0)))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (henchman '((:left 10 :top 10 :width 100 :height 100)
                          (:left 50 :top 10 :width 100 :height 100)
                          (:left 10 :top 50 :width 100 :height 100)
                          (:left 50 :top 50 :width 100 :height 100))
                        (list (make-child color1)
                              (make-child color2)
                              (make-child color3)
                              (make-child color4))))))))

(defun scroll-view-1 ()
  (labels ((make-child (color)
             (glass 0.2 (background color
                                    (filler)))))
    (let* ((color1 (list 1.0 0.3 0.3))
           (color2 (list 0.3 1.0 0.3))
           (color3 (list 0.3 0.3 1.0))
           (color4 (list 0.7 0.3 1.0)))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (sizer (scroll-view-auto (sizer
                                        (background
                                         (list 1.0 1.0 1.0)
                                         (henchman
                                          '((:left 10 :top 10 :width 100 :height 100)
                                            (:right 10 :top 10 :width 100 :height 100)
                                            (:left 10 :bottom 10 :width 100 :height 100)
                                            (:right 10 :bottom 10 :width 100 :height 100))
                                          (list (make-child color1)
                                                (make-child color2)
                                                (make-child color3)
                                                (make-child color4))))
                                        :max-width 500
                                        :max-height 500))
                     :max-height 400
                     :max-width 400))))))

(defun textbox-1 ()
  (let (scene textbox)
    (setf scene
          (scene *scene-width* *scene-height*
                 (stack
                  (background (list 1.0 1.0 1.0)
                              (filler))
                  (grid '((100 :px))
                        '((30 :px))
                        `((:row (:cell ,(uniform-padding
                                         3
                                         (border
                                          (list 0.0 0.0 0.0) 1
                                          (uniform-padding
                                           3
                                           (setf textbox
                                                 (textbox "The quick brown fox..." 0))))))))))))
    (scenic:add-event-handler textbox :text-changed nil
                              (lambda (o e)
                                (declare (ignore o e))
                                (when *manual-test-run*
                                  (print-all t (scenic:text textbox)))
                                (scenic:test-channel-write (list :text (scenic:text textbox)))))
    scene))

(defun textbox-2 ()
  (let (scene text1 text2 text3)
    (setf scene
          (scene *scene-width* *scene-height*
                 (stack
                  (background (list 1.0 1.0 1.0)
                              (filler))
                  (grid '((100 :px) (100 :px) :auto)
                        '((30 :px))
                        `((:row (:cell ,(uniform-padding
                                         3
                                         (border (list 0.0 0.0 0.0) 1
                                                 (uniform-padding
                                                  3
                                                  (setf text1
                                                        (textbox "The quick brown fox..." 0))))))
                                (:cell ,(uniform-padding
                                         3
                                         (border (list 0.0 0.0 0.0) 1
                                                 (uniform-padding
                                                  3
                                                  (setf text2
                                                        (textbox "The quick brown fox..." 0))))))
                                (:cell ,(uniform-padding
                                         3
                                         (border
                                          (list 0.0 0.0 0.0) 1
                                          (uniform-padding
                                           3
                                           (setf text3
                                                 (textbox "The quick brown fox..." 0))))))))))))
    (scenic:add-event-handler text1 :text-changed nil
                              (lambda (o e)
                                (declare (ignore o e))
                                (when *manual-test-run*
                                  (print-all t (scenic:text text1)))
                                (scenic:test-channel-write (list :text1 (scenic:text text1)))))
    (scenic:add-event-handler text2 :text-changed nil
                              (lambda (o e)
                                (declare (ignore o e))
                                (when *manual-test-run*
                                  (print-all t (scenic:text text2)))
                                (scenic:test-channel-write (list :text2 (scenic:text text2)))))
    (scenic:add-event-handler text3 :text-changed nil
                              (lambda (o e)
                                (declare (ignore o e))
                                (when *manual-test-run*
                                  (print-all t (scenic:text text3)))
                                (scenic:test-channel-write (list :text3 (scenic:text text3)))))
    scene))

(defun scroll-view-hittest ()
  (labels ((make-child (text)
             (let ((btn (button-text text)))
               (scenic:add-event-handler
                btn :click nil
                (lambda (o e)
                  (declare (ignore o e))
                  (when *manual-test-run*
                    (print-all t text))
                  (scenic:test-channel-write (list text "clicked"))))
               btn)))
    (multiple-value-bind (sva sv)
        (scroll-view-auto (sizer
                           (background
                            (list 1.0 1.0 1.0)
                            (henchman
                             '((:left 10 :top 10 :width 100 :height 100)
                               (:right 10 :top 10 :width 100 :height 100)
                               (:left 10 :bottom 10 :width 100 :height 100)
                               (:right 10 :bottom 10 :width 100 :height 100))
                             (list (make-child "Button 1")
                                   (make-child "Button 2")
                                   (make-child "Button 3")
                                   (make-child "Button 4"))))
                           :max-width 500
                           :max-height 500))
      (setf (scenic:horizontal-offset sv) 50)
      (setf (scenic:vertical-offset sv) 50)
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (sizer sva
                     :max-height 400
                     :max-width 400))))))

(defun scroll-view-mouse-adjust ()
  (labels ((make-child (text)
             (let ((btn (button-text text)))
               (scenic:add-event-handler btn :mouse-move :bubble
                                         (lambda (o e)
                                           (declare (ignore o))
                                           (when *manual-test-run*
                                             (print-all t e))))
               btn)))
    (multiple-value-bind (sva sv)
        (scroll-view-auto (sizer
                           (background
                            (list 1.0 1.0 1.0)
                            (henchman
                             '((:left 10 :top 10 :width 100 :height 100)
                               (:right 10 :top 10 :width 100 :height 100)
                               (:left 10 :bottom 10 :width 100 :height 100)
                               (:right 10 :bottom 10 :width 100 :height 100))
                             (list (make-child "Button 1")
                                   (make-child "Button 2")
                                   (make-child "Button 3")
                                   (make-child "Button 4"))))
                           :max-width 500
                           :max-height 500))
      (setf (scenic:horizontal-offset sv) 50)
      (setf (scenic:vertical-offset sv) 60)
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (sizer sva
                     :max-height 400
                     :max-width 400))))))

(defun checkbox-1 ()
  (let (cb1 cb2 scn)
    (setf scn
          (scene *scene-width* *scene-height*
                 (stack
                  (background (list 1.0 1.0 1.0)
                              (filler))
                  (uniform-padding
                   5
                   (vertical-box
                    0 '(:auto)
                    (list
                     (horizontal-box 10
                                     '(:auto :auto)
                                     (list
                                      (set2val1 cb1 (checkbox "Smart"))
                                      (set2val1 cb2 (checkbox "Beautiful"))))))))))
    (scenic:add-event-handler cb1 :state-changed nil
                              (lambda (o e)
                                (declare (ignore o e))
                                (let ((message (if (scenic:state cb1) "Smart" "Dumb")))
                                  (when *manual-test-run* (print message))
                                  (scenic:test-channel-write message))))
    (scenic:add-event-handler cb2 :state-changed nil
                              (lambda (o e)
                                (declare (ignore o e))
                                (let ((message (if (scenic:state cb2) "Beautiful" "Ugly")))
                                  (when *manual-test-run*
                                    (print message))
                                  (scenic:test-channel-write message))))
    scn))

(defun radio-button-1 ()
  (let (rb1 rb2 scn)
    (setf scn
          (scene *scene-width* *scene-height*
                 (stack
                  (background (list 1.0 1.0 1.0)
                              (filler))
                  (uniform-padding
                   5
                   (vertical-box
                    0 '(:auto)
                    (list
                     (horizontal-box 10
                                     '(:auto :auto)
                                     (list
                                      (set2val1 rb1 (radio-button "Smart"))
                                      (set2val1 rb2 (radio-button "Beautiful"))))))))))
    (scenic:add-event-handler rb1 :state-changed nil
                              (lambda (o e)
                                (declare (ignore o e))
                                (let ((message (if (scenic:state rb1) "Smart" "Dumb")))
                                  (when *manual-test-run* (print message))
                                  (scenic:test-channel-write message))))
    (scenic:add-event-handler rb2 :state-changed nil
                              (lambda (o e)
                                (declare (ignore o e))
                                (let ((message (if (scenic:state rb2) "Beautiful" "Ugly")))
                                  (when *manual-test-run*
                                    (print message))
                                  (scenic:test-channel-write message))))
    (group-stateful-buttons rb1 (list rb1 rb2))
    scn))

(defun simple-boxes ()
  (scene *scene-width* *scene-height*
         (stack
          (background (list 1.0 1.0 1.0) (filler))
          (simple-vertical-box
           2
           (list (simple-horizontal-box
                  2
                  (list (background (list 0.9 0.2 0.2) (placeholder 100 100))
                        (background (list 0.2 0.9 0.2) (placeholder 100 100))
                        (background (list 0.2 0.2 0.9) (placeholder 100 100))))
                 (background (list 0.9 0.3 0.7) (placeholder 100 100))
                 (background (list 0.3 0.7 0.8) (placeholder 100 100)))))))

(defun scroll-view-2 ()
  (let* ((color1 (list 1.0 0.3 0.3))
         (color2 (list 0.3 1.0 0.3))
         (color3 (list 0.3 0.3 1.0))
         (color4 (list 0.7 0.3 1.0)))
    (labels ((make-square (color)
               (background color (filler)))
             (make-child (inner-width inner-height)
               (sizer
                (scroll-view-auto (sizer (background
                                          (list 1.0 1.0 1.0)
                                          (henchman
                                           '((:left 10 :top 10 :width 100 :height 100)
                                             (:right 10 :top 10 :width 100 :height 100)
                                             (:left 10 :bottom 10 :width 100 :height 100)
                                             (:right 10 :bottom 10 :width 100 :height 100))
                                           (list (make-square color1)
                                                 (make-square color2)
                                                 (make-square color3)
                                                 (make-square color4))))
                                         :max-width inner-width
                                         :max-height inner-height)
                                  :always-horizontal-scrollbar nil
                                  :always-vertical-scrollbar nil)
                :max-height 298
                :max-width 298))
             (black-border (child)
               (border (list 0.0 0.0 0.0) 1 child)))
      (scene *scene-width* *scene-height*
             (stack
              (background (list 1.0 1.0 1.0)
                          (filler))
              (uniform-padding 45
                               (grid '((300 :px) (10 :px) (300 :px))
                                     '((300 :px) (10 :px) (300 :px))
                                     `((:row (:cell ,(black-border (make-child 350 350)))
                                             (:cell ,(filler))
                                             (:cell ,(black-border (make-child 279 350))))
                                       (:row (:cell ,(filler)))
                                       (:row (:cell ,(black-border (make-child 350 279)))
                                             (:cell ,(filler))
                                             (:cell ,(black-border (make-child 279 279))))))))))))

(defun run-all-tests ()
  (test-scene (background-clear))
  (test-scene (colored-rectangles))
  (test-scene (hello-world))
  (test-scene (buttons))
  (test-scene (slider))
  (test-scene (scrollbars))
  (test-scene (icon))
  (test-scene (text-baseline-alignment))
  (test-scene (vertical-box-layout-options))
  (test-scene (horizontal-box-layout-options))
  (test-scene (grid-basic))
  (test-scene (grid-offset))
  (test-scene (grid-spans))
  (test-scene (grid-layout-options))
  (test-scene (grid-layout-options-2))
  (test-scene (grid-layout-options-3))
  (test-scene (aligner-1))
  (test-scene (clipper-1))
  (test-scene (glass-1))
  (test-scene (henchman-1))
  (test-scene (henchman-glass))
  (test-scene (scroll-view-1))
  (test-scene (textbox-1))
  (test-scene (textbox-2))
  (test-scene (scroll-view-hittest))
  (test-scene (scroll-view-mouse-adjust))
  (test-scene (checkbox-1))
  (test-scene (radio-button-1))
  (test-scene (simple-boxes))
  (test-scene (scroll-view-2)))
