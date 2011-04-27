
(in-package :scenic)

(declaim (optimize (debug 3)))

(defgeneric measure (object available-width available-height))

(defgeneric layout (object left top width height))

(defgeneric paint (object))

(defgeneric on-mouse-move (object event))

(defgeneric add-mouse-move (object handler))

(defgeneric hit-test (object x y hits))

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

(defmethod print-object ((object widget) stream)
  (format stream
          "~a (~a,~a,~a,~a)"
          (type-of object)
          (layout-left object)
          (layout-top object)
          (layout-width object)
          (layout-height object)))

(defun in-widget (x y widget)
  (and (<= (layout-left widget) x)
       (< x (+ (layout-left widget) (layout-width widget)))
       (<= (layout-top widget) y)
       (< y (+ (layout-top widget) (layout-height widget)))))

(defun call-widget-event-handlers (widget widget-events event)
  (dolist (handler widget-events)
    (when (handled event)
      (return-from call-widget-event-handlers))
    (funcall handler widget event)))

(defmethod on-mouse-move ((object widget) mouse-event)
  (call-widget-event-handlers object (event-mouse-move object) mouse-event))

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

(defmethod add-mouse-move ((object widget) handler)
  (push handler (event-mouse-move object)))

(defmethod hit-test ((object widget) x y hits)
  (if (in-widget x y object)
      (cons object hits)
      hits))

;;; CONTAINER class.

(defclass container (widget)
  ((children :accessor children :initarg :children)))

(defmethod paint ((object container))
  (mapc #'paint (children object)))

(defmethod initialize-instance :after ((instance container) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (mapc (lambda (widget)
          (setf (parent widget) instance))
        (children instance)))

(defmethod hit-test ((object container) x y hits)
  (when (in-widget x y object)
    (push object hits)
    (dolist (widget (children object))
      (setf hits (hit-test widget x y hits))))
  hits)

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

;;; CONTAINER1 class.

(defclass container1 (widget)
  ((child :accessor child :initarg :child :initform nil)))

(defmethod paint ((object container1))
  (paint (child object)))

(defmethod initialize-instance :after ((instance container1) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (parent (child instance)) instance))

(defmethod hit-test ((object container1) x y hits)
  (when (in-widget x y object)
    (push object hits)
    (setf hits (hit-test (child object) x y hits)))
  hits)

;;; PADDING class.

(defclass padding (container1)
  ((left-padding :accessor left-padding :initarg :left-padding :initform 0)
   (top-padding :accessor top-padding :initarg :top-padding :initform 0)
   (right-padding :accessor right-padding :initarg :right-padding :initform 0)
   (bottom-padding :accessor bottom-padding :initarg :bottom-padding :initform 0)))

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

(defclass border (container1)
  ((stroke-color :accessor stroke-color :initarg :stroke-color :initform 0)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 0)))

(defmethod paint ((object border))
  (cl-cairo2:rectangle (+ (/ (stroke-width object) 2) (layout-left object))
                       (+ (/ (stroke-width object) 2) (layout-top object))
                       (- (layout-width object) (stroke-width object))
                       (- (layout-height object) (stroke-width object)))
  (apply #'cl-cairo2:set-source-rgb (stroke-color object))
  (cl-cairo2:set-line-width (stroke-width object))
  (cl-cairo2:stroke)
  (call-next-method))

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

(defclass background (container1)
  ((fill-color :accessor fill-color :initarg :fill-color :initform 0)))

(defmethod paint ((object background))
  (cl-cairo2:rectangle (layout-left object)
                       (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (apply #'cl-cairo2:set-source-rgb (fill-color object))
  (cl-cairo2:fill-path)
  (call-next-method))

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
  ((handled :accessor handled :initarg :handled :initform nil)
   (widget-list :accessor widget-list :initarg :widget-list :initform nil)))

;;; MOUSE-event class.

(defclass mouse-event (event)
  ((mouse-x :accessor mouse-x :initarg :mouse-x :initform nil)
   (mouse-y :accessor mouse-y :initarg :mouse-y :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)))

(defclass mouse-move-event (mouse-event)
  ((mouse-rel-x :accessor mouse-rel-x :initarg :mouse-rel-x :initform nil)
   (mouse-rel-y :accessor mouse-rel-y :initarg :mouse-rel-y :initform nil)))

(defclass mouse-button-event (mouse-event)
  ((mouse-button :accessor mouse-button :initarg :mouse-button :initform nil)))

;;; KEY-EVENT class.

(defclass key-event (event)
  ((key :accessor key :initarg :key :initform nil)
   (modifiers :accessor modifiers :initarg :modifiers :initform nil)))

;;; SCENE class.

(defclass scene ()
  ((widget :accessor widget :initarg :widget :initform nil)
   (width :accessor width :initarg :width :initform 1024)
   (height :accessor height :initarg :height :initform 768)))

(defmethod paint ((object scene))
  (paint (widget object)))

(defmethod measure ((object scene) available-width available-height)
  (measure (widget object) available-width available-height))

(defmethod layout ((object scene) left top width height)
  (layout (widget object) left top width height))

(defun get-widget-chain (widget-chain)
  (if (null widget-chain)
      nil
      (let ((parent-of-first (parent (first widget-chain))))
        (if (and parent-of-first (not (eql (type-of parent-of-first) 'scene)))
            (get-widget-chain (cons parent-of-first
                                    widget-chain))
            widget-chain))))

(defmethod on-mouse-move ((object scene) mouse-event)
  (let ((widget-chain (get-widget-chain (list (first (hit-test (widget object)
                                                               (mouse-x mouse-event)
                                                               (mouse-y mouse-event)
                                                               nil))))))
    ;; cascade events (propagate from parent to child)
    (dolist (widget widget-chain)
      (on-mouse-move widget mouse-event)
      (when (handled mouse-event)
        (return)))
    ;; bubbling events (propagate from child to parent) (not yet implemented)
    ))

(defmethod initialize-instance :after ((instance scene) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (parent (widget instance)) instance))

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
      (call-next-method object
                        (min width available-width)
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

;;; BUTTON class.

(defclass button (container1)
  ((text :accessor text :initarg :text :initform "")
   (event-click :accessor event-click :initarg :event-click :initform nil)))

(defmethod measure ((object button) available-width available-height)
  (let ((child-size (measure (child object) available-width available-height)))
    (call-next-method object (first child-size) (second child-size))))

(defmethod layout ((object button) left top width height)
  (layout (child object) left top width height)
  (call-next-method))

