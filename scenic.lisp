
(in-package :scenic)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(defparameter *width* 800)
(defparameter *height* 600)

(defmacro pixels-of (surface)
  `(cffi:foreign-slot-value (slot-value ,surface 'sdl::foreign-pointer-to-object)
                            'sdl-cffi::sdl-Surface
                            'sdl-cffi::pixels))

(defmacro pitch-of (surface)
  `(cffi:foreign-slot-value (slot-value ,surface 'sdl::foreign-pointer-to-object)
                            'sdl-cffi::sdl-Surface
                            'sdl-cffi::pitch))

;; this macro could use unwind-protect :-)
(defmacro draw-with-cairo (sdl-surface &body body)
  (let ((g-sdl-surface (gensym "sdl-surface"))
        (g-cairo-surface (gensym "cairo-surface")))
    `(let* ((,g-sdl-surface ,sdl-surface)
            (,g-cairo-surface (cl-cairo2:create-image-surface-for-data
                               (pixels-of ,g-sdl-surface)
                               :argb32
                               (lispbuilder-sdl:width ,g-sdl-surface)
                               (lispbuilder-sdl:height ,g-sdl-surface)
                               (pitch-of ,g-sdl-surface)))
            (cl-cairo2:*context* (cl-cairo2:create-context ,g-cairo-surface)))
       (cl-cairo2:destroy ,g-cairo-surface)
       (progn ,@body)
       (cl-cairo2:destroy cl-cairo2:*context*)
       ,g-sdl-surface)))

(defun font-extents ()
  (let ((extents (cl-cairo2:get-font-extents)))
    `((ascent . ,(cl-cairo2:font-ascent extents))
      (descent . ,(cl-cairo2:font-descent extents)))))

(defun text-extents (text)
  (mapcar #'(lambda (name dim)
              (cons name dim))
          (multiple-value-list (cl-cairo2:text-extents text))
          '(x_bearing y_bearing width height x_advance y_advance)))

(defclass widget ()
  ((min-width :accessor min-width :initarg :min-width :initform nil)
   (min-height :accessor min-height :initarg :min-height :initform nil)
   (max-width :accessor max-width :initarg :max-width :initform nil)
   (max-height :accessor max-height :initarg :max-height :initform nil)
   (measured-width :accessor measured-width :initarg measured-width :initform nil)
   (measured-height :accessor measured-height :initarg measured-height :initform nil)
   (layout-left :accessor layout-left :initarg :layout-left :initform 0)
   (layout-top :accessor layout-top :initarg :layout-top :initform 0)
   (layout-width :accessor layout-width :initarg :layout-width :initform nil)
   (layout-height :accessor layout-height :initarg :layout-height :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (event-mouse-down :accessor event-mouse-down :initarg :event-mouse-down :initform nil)
   (event-mouse-up :accessor event-mouse-up :initarg :event-mouse-up :initform nil)
   (event-mouse-enter :accessor event-mouse-enter :initarg :event-mouse-enter :initform nil)
   (event-mouse-move :accessor event-mouse-move :initarg :event-mouse-move :initform nil)
   (event-mouse-leave :accessor event-mouse-leave :initarg :event-mouse-leave :initform nil)
   (event-key-down :accessor event-key-down :initarg :event-key-down :initform nil)
   (event-key-up :accessor event-key-up :initarg :event-key-up :initform nil)
   (event-got-focus :accessor event-got-focus :initarg :event-got-focus :initform nil)
   (event-lost-focus :accessor event-lost-focus :initarg :event-lost-focus :initform nil)))

(defgeneric measure (widget available-width available-height))

(defgeneric layout (widget left top width height))

;; (defclass label (widget)
;;   ((text :accessor text :initarg :text :initform "")))

;; (defclass button (widget)
;;   ((text :accessor text :initarg :text :initform "")
;;    (event-click :accessor event-click :initarg :event-click :initform nil)))

(defclass container (widget)
  ((children :accessor children :initarg :children)))

(defclass vertical-box (container)
  ((space-between-cells :accessor space-between-cells :initarg :space-between-cells :initform 0)))

(defclass stack (container)
  ())

(defclass padding (widget)
  ((left-padding :accessor left-padding :initarg :left-padding :initform 0)
   (top-padding :accessor top-padding :initarg :top-padding :initform 0)
   (right-padding :accessor right-padding :initarg :right-padding :initform 0)
   (bottom-padding :accessor bottom-padding :initarg :bottom-padding :initform 0)
   (child :accessor child :initarg :child :initform nil)))

(defclass border (widget)
  ((stroke-color :accessor stroke-color :initarg :stroke-color :initform 0)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 0)
   (child :accessor child :initarg :child :initform nil)))

(defclass background (widget)
  ((fill-color :accessor fill-color :initarg :fill-color :initform 0)
   (child :accessor child :initarg :child :initform nil)))

(defclass event ()
  ((handled :accessor handled :initarg :handled :initform nil)))

(defclass mouse-event (event)
  ((mouse-x :accessor mouse-x :initarg :mouse-x :initform nil)
   (mouse-y :accessor mouse-y :initarg :mouse-y :initform nil)
   (mouse-button :accessor mouse-button :initarg :mouse-button :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)))

(defclass key-event (event)
  ((key :accessor key :initarg :key :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)))

(defclass layer ()
  ((widget :accessor widget :initarg :widget :initform nil)
   (children :accessor children :initarg :children :initform nil)))

(defclass scene ()
  ((layers :accessor layers :initarg :layers :initform nil)))

(defgeneric paint (object))

(defmethod paint ((object scene))
  (mapc #'paint (layers object)))

(defmethod paint ((object layer))
  (paint (widget object))
  (mapc #'paint (children object)))

(defmethod paint ((object background))
  (cl-cairo2:rectangle (layout-left object)
                       (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (apply #'cl-cairo2:set-source-rgb (fill-color object))
  (cl-cairo2:fill-path)
  (paint (child object)))

(defmethod paint ((object border))
  (if (> (stroke-width object) 1)
      (cl-cairo2:rectangle (+ (/ (stroke-width object) 2) (layout-left object))
                           (+ (/ (stroke-width object) 2) (layout-top object))
                           (- (layout-width object) (stroke-width object))
                           (- (layout-height object) (stroke-width object)))
      (cl-cairo2:rectangle (+ 0.5 (layout-left object))
                           (+ 0.5 (layout-top object))
                           (- (layout-width object) 1)
                           (- (layout-height object) 1)))
  (apply #'cl-cairo2:set-source-rgb (stroke-color object))
  (cl-cairo2:set-line-width (stroke-width object))
  (cl-cairo2:stroke)
  (paint (child object)))

(defmethod paint ((object padding))
  (paint (child object)))

(defmethod paint ((object container))
  (mapc #'paint (children object)))

(defun max-box (boxes)
  (list (apply #'max (mapcar #'first boxes))
        (apply #'max (mapcar #'second boxes))))

(defmethod measure ((object scene) available-width available-height)
  (max-box (mapcar #'(lambda (layer) (measure layer available-width available-height))
                   (layers object))))

(defmethod measure ((object layer) available-width available-height)
  (max-box (cons (measure (widget object) available-width available-height)
                 (mapcar #'(lambda (layer) (measure layer available-width available-height))
                         (children object)))))

(defmethod measure ((object widget) available-width available-height)
  (setf (measured-width object) available-width)
  (setf (measured-height object) available-height)
  (list available-width available-height))

(defmethod measure ((object stack) available-width available-height)
  (apply #'call-next-method
         object
         (max-box (mapcar #'(lambda (widget) (measure widget available-width available-height))
                          (children object)))))

(defmethod measure ((object vertical-box) available-width available-height)
  (let* ((child-sizes (mapcar #'(lambda (widget) (measure widget available-width available-height))
                              (children object)))
         (vertical-size (+ (* (1- (length child-sizes))
                              (space-between-cells object))
                           (reduce #'+ (mapcar #'second child-sizes))))
         (horizontal-size (apply #'max (mapcar #'first child-sizes))))
    (call-next-method object horizontal-size vertical-size)))

(defmethod measure ((object background) available-width available-height)
  (apply #'call-next-method
         object
         (measure (child object) available-width available-height)))

(defmethod measure ((object border) available-width available-height)
  (let* ((size (measure (child object) available-width available-height))
         (width (+ (* 2 (stroke-width object)) (first size)))
         (height (+ (* 2 (stroke-width object)) (second size))))
    (call-next-method object width height)))

(defmethod measure ((object padding) available-width available-height)
  (let* ((size (measure (child object) available-width available-height))
         (width (+ (left-padding object) (right-padding object) (first size)))
         (height (+ (top-padding object) (bottom-padding object) (second size))))
    (call-next-method object width height)))

(defmethod layout ((object scene) left top width height)
  (mapc #'(lambda (layer) (layout layer left top width height))
        (layers object)))

(defmethod layout ((object widget) left top width height)
  (setf (layout-left object) left)
  (setf (layout-top object) top)
  (setf (layout-width object) width)
  (setf (layout-height object) height))

(defmethod layout ((object layer) left top width height)
  (layout (widget object) left top width height)
  (mapc #'(lambda (layer) (layout layer left top width height))
        (children object)))

(defmethod layout ((object stack) left top width height)
  (mapc #'(lambda (widget)
            (layout widget left top (measured-width object) (measured-height object)))
        (children object))
  (call-next-method object left top width height))

(defmethod layout ((object vertical-box) left top width height)
  (let ((running-top top))
    (dolist (widget (children object))
      (layout widget left running-top (measured-width widget) (measured-height widget))
      (incf running-top (+ (measured-height widget) (space-between-cells object)))))
  (call-next-method object left top width height))

(defmethod layout ((object padding) left top width height)
  (layout (child object)
          (+ left (left-padding object))
          (+ top (top-padding object))
          (- width (+ (left-padding object) (right-padding object)))
          (- height (+ (top-padding object) (bottom-padding object))))
  (call-next-method object left top width height))

(defmethod layout ((object border) left top width height)
  (layout (child object)
          (+ left (stroke-width object))
          (+ top (stroke-width object))
          (measured-width (child object))
          (measured-height (child object)))
  (call-next-method object left top (measured-width object) (measured-height object)))

(defmethod layout ((object background) left top width height)
  (layout (child object)
          left
          top
          (measured-width (child object))
          (measured-height (child object)))
  (call-next-method object left top (measured-width object) (measured-height object)))

(defclass placeholder (widget)
  ((width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)))

(defmethod paint ((object placeholder)))

(defmethod measure ((object placeholder) available-width available-height)
  (call-next-method object
                    (min (width object) available-width)
                    (min (height object) available-height)))

(defmacro bg (color child)
  `(make-instance 'background
                  :fill-color ,color
                  :child ,child))

(defmacro border (color width child)
  `(make-instance 'border
                  :stroke-color ,color
                  :stroke-width ,width
                  :child ,child))

(defmacro layer (widget &body children)
  `(make-instance 'layer
                  :widget ,widget
                  :children ,children))

(defmacro scene (&body layers)
  `(make-instance 'scene
                  :layers (list ,@layers)))

(defmacro spc (width height)
  `(make-instance 'placeholder
                  :width ,width
                  :height ,height))

(defmacro pad (left top right bottom &body child)
  `(make-instance 'padding
                  :left-padding ,left
                  :top-padding ,top
                  :right-padding ,right
                  :bottom-padding ,bottom
                  :child ,(first child)))

(defmacro upad (padding &body child)
  `(make-instance 'padding
                  :left-padding ,padding
                  :top-padding ,padding
                  :right-padding ,padding
                  :bottom-padding ,padding
                  :child ,(first child)))

(defmacro vbox (space &body children)
  `(make-instance 'vertical-box
                  :space-between-cells ,space
                  :children (list ,@children)))

(defun render-scene (scene)
  (let ((from-cairo nil))
    (measure scene *width* *height*)
    (layout scene 0 0 *width* *height*)
    (setf from-cairo
          (draw-with-cairo (sdl:create-surface *width* *height*)
            (paint scene)))
    (sdl:blit-surface from-cairo)
    (sdl:free from-cairo)
    (sdl:update-display)))

(defun run-scene (scene)
  (sdl:with-init ()
    (sdl:window *width* *height*)
    (setf (sdl:frame-rate) 60)
    (render-scene scene)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (when (sdl:key= key :sdl-key-escape)
                         (sdl:push-quit-event)))
      ;; (:idle (render-scene scene))
      (:video-expose-event () (sdl:update-display)))))

