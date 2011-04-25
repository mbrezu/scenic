
(in-package :scenic)

(defgeneric measure (widget available-width available-height))

(defgeneric layout (widget left top width height))

(defgeneric paint (object))

;;; WIDGET class.

(defclass widget ()
  ((measured-width :accessor measured-width :initarg measured-width :initform nil)
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

(defmethod measure ((object widget) available-width available-height)
  (setf (measured-width object) available-width)
  (setf (measured-height object) available-height)
  (list available-width available-height))

(defmethod layout ((object widget) left top width height)
  (setf (layout-left object) left)
  (setf (layout-top object) top)
  (setf (layout-width object) width)
  (setf (layout-height object) height))

(defmethod paint ((object widget)))

;;; CONTAINER class.

(defclass container (widget)
  ((children :accessor children :initarg :children)))

(defmethod paint ((object container))
  (mapc #'paint (children object)))

;;; VERTICAL-BOX class.

(defclass vertical-box (container)
  ((space-between-cells :accessor space-between-cells :initarg :space-between-cells :initform 0)))

(defmethod measure ((object vertical-box) available-width available-height)
  (let* ((child-sizes (mapcar #'(lambda (widget) (measure widget available-width available-height))
                              (children object)))
         (vertical-size (+ (* (1- (length child-sizes))
                              (space-between-cells object))
                           (reduce #'+ (mapcar #'second child-sizes))))
         (horizontal-size (apply #'max (mapcar #'first child-sizes))))
    (call-next-method object horizontal-size vertical-size)))

(defmethod layout ((object vertical-box) left top width height)
  (let ((running-top top))
    (dolist (widget (children object))
      (layout widget left running-top (measured-width widget) (measured-height widget))
      (incf running-top (+ (measured-height widget) (space-between-cells object)))))
  (call-next-method object left top width height))

;;; HORIZONTAL-BOX class.

(defclass horizontal-box (container)
  ((space-between-cells :accessor space-between-cells :initarg :space-between-cells :initform 0)))

(defmethod measure ((object horizontal-box) available-width available-height)
  (let* ((child-sizes (mapcar #'(lambda (widget) (measure widget available-width available-height))
                              (children object)))
         (horizontal-size (+ (* (1- (length child-sizes))
                                (space-between-cells object))
                             (reduce #'+ (mapcar #'first child-sizes))))
         (vertical-size (apply #'max (mapcar #'second child-sizes))))
    (call-next-method object horizontal-size vertical-size)))

(defmethod layout ((object horizontal-box) left top width height)
  (let ((running-left left))
    (dolist (widget (children object))
      (layout widget running-left top (measured-width widget) (measured-height widget))
      (incf running-left (+ (measured-width widget) (space-between-cells object)))))
  (call-next-method object left top width height))

;;; STACK class.

(defclass stack (container)
  ())

(defmethod measure ((object stack) available-width available-height)
  (apply #'call-next-method
         object
         (max-box (mapcar #'(lambda (widget) (measure widget available-width available-height))
                          (children object)))))

(defmethod layout ((object stack) left top width height)
  (mapc #'(lambda (widget)
            (layout widget left top (measured-width object) (measured-height object)))
        (children object))
  (call-next-method object left top width height))

;;; PADDING class.

(defclass padding (widget)
  ((left-padding :accessor left-padding :initarg :left-padding :initform 0)
   (top-padding :accessor top-padding :initarg :top-padding :initform 0)
   (right-padding :accessor right-padding :initarg :right-padding :initform 0)
   (bottom-padding :accessor bottom-padding :initarg :bottom-padding :initform 0)
   (child :accessor child :initarg :child :initform nil)))

(defmethod paint ((object padding))
  (paint (child object)))

(defmethod measure ((object padding) available-width available-height)
  (let* ((size (measure (child object) available-width available-height))
         (width (+ (left-padding object) (right-padding object) (first size)))
         (height (+ (top-padding object) (bottom-padding object) (second size))))
    (call-next-method object width height)))

(defmethod layout ((object padding) left top width height)
  (layout (child object)
          (+ left (left-padding object))
          (+ top (top-padding object))
          (- width (+ (left-padding object) (right-padding object)))
          (- height (+ (top-padding object) (bottom-padding object))))
  (call-next-method object left top width height))

;;; BORDER class.

(defclass border (widget)
  ((stroke-color :accessor stroke-color :initarg :stroke-color :initform 0)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 0)
   (child :accessor child :initarg :child :initform nil)))

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

(defmethod measure ((object border) available-width available-height)
  (let* ((size (measure (child object) available-width available-height))
         (width (+ (* 2 (stroke-width object)) (first size)))
         (height (+ (* 2 (stroke-width object)) (second size))))
    (call-next-method object width height)))

(defmethod layout ((object border) left top width height)
  (layout (child object)
          (+ left (stroke-width object))
          (+ top (stroke-width object))
          (measured-width (child object))
          (measured-height (child object)))
  (call-next-method object left top (measured-width object) (measured-height object)))

;;; BACKGROUND class.

(defclass background (widget)
  ((fill-color :accessor fill-color :initarg :fill-color :initform 0)
   (child :accessor child :initarg :child :initform nil)))

(defmethod paint ((object background))
  (cl-cairo2:rectangle (layout-left object)
                       (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (apply #'cl-cairo2:set-source-rgb (fill-color object))
  (cl-cairo2:fill-path)
  (paint (child object)))

(defmethod measure ((object background) available-width available-height)
  (apply #'call-next-method
         object
         (measure (child object) available-width available-height)))

(defmethod layout ((object background) left top width height)
  (layout (child object)
          left
          top
          (measured-width (child object))
          (measured-height (child object)))
  (call-next-method object left top (measured-width object) (measured-height object)))

;;; EVENT class.

(defclass event ()
  ((handled :accessor handled :initarg :handled :initform nil)))

;;; MOUSE-event class.

(defclass mouse-event (event)
  ((mouse-x :accessor mouse-x :initarg :mouse-x :initform nil)
   (mouse-y :accessor mouse-y :initarg :mouse-y :initform nil)
   (mouse-button :accessor mouse-button :initarg :mouse-button :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)))

;;; KEY-EVENT class.

(defclass key-event (event)
  ((key :accessor key :initarg :key :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)))

;;; LAYER class.

(defclass layer ()
  ((widget :accessor widget :initarg :widget :initform nil)
   (children :accessor children :initarg :children :initform nil)))

(defmethod paint ((object layer))
  (paint (widget object))
  (mapc #'paint (children object)))

(defmethod measure ((object layer) available-width available-height)
  (max-box (cons (measure (widget object) available-width available-height)
                 (mapcar #'(lambda (layer) (measure layer available-width available-height))
                         (children object)))))

(defmethod layout ((object layer) left top width height)
  (layout (widget object) left top width height)
  (mapc #'(lambda (layer) (layout layer left top width height))
        (children object)))

;;; SCENE class.

(defclass scene ()
  ((layers :accessor layers :initarg :layers :initform nil)
   (width :accessor width :initarg :width :initform 1024)
   (height :accessor height :initarg :height :initform 768)))

(defmethod paint ((object scene))
  (mapc #'paint (layers object)))

(defmethod measure ((object scene) available-width available-height)
  (max-box (mapcar #'(lambda (layer) (measure layer available-width available-height))
                   (layers object))))

(defmethod layout ((object scene) left top width height)
  (mapc #'(lambda (layer) (layout layer left top width height))
        (layers object)))

;;; PLACEHOLDER class.

(defclass placeholder (widget)
  ((width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)))

(defmethod measure ((object placeholder) available-width available-height)
  (call-next-method object
                    (min (width object) available-width)
                    (min (height object) available-height)))

;;; FILLER class.

(defclass filler (widget)
  ())

;;; LABEL class.

(defclass label (widget)
  ((text :accessor text :initarg :text :initform "")
   (font-face :accessor font-face :initarg :font-face :initform "Arial")
   (font-size :accessor font-size :initarg :font-size :initform 12)
   (font-color :accessor font-color :initarg :font-color :initform (list 0.0 0.0 0.0))
   (font-slant :accessor font-slant :initarg :font-slant :initform :normal)
   (font-weight :accessor font-weight :initarg :font-weight :initform :normal)))

(defmethod measure ((object label) available-width available-height)
  (cl-cairo2:set-font-size (font-size object))
  (cl-cairo2:select-font-face (font-face object)
                              (font-slant object)
                              (font-weight object))
  (multiple-value-bind
        (x_bearing y_bearing width height x_advance y_advance) (cl-cairo2:text-extents
                                                                (text object))
    (declare (ignore x_bearing y_bearing x_advance y_advance height))
    (let* ((extents (cl-cairo2:get-font-extents))
           (ascent (cl-cairo2:font-ascent extents)))
      (list (min width available-width)
            (min ascent available-height)))))

(defmethod layout ((object label) left top width height)
  (call-next-method object left top (measured-width object) (measured-height object)))

(defmethod paint ((object label))
  (cl-cairo2:set-font-size (font-size object))
  (cl-cairo2:select-font-face (font-face object)
                              (font-slant object)
                              (font-weight object))
  (apply #'cl-cairo2:set-source-rgb (font-color object))

  (let* ((extents (cl-cairo2:get-font-extents))
           (ascent (cl-cairo2:font-ascent extents))
           (descent (cl-cairo2:font-descent extents)))
      (cl-cairo2:move-to (layout-left object) (- (+ (layout-top object) ascent 0.5) descent))
      (cl-cairo2:show-text (text object))))

