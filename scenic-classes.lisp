
(in-package :scenic)

(declaim (optimize (debug 3)))

(defgeneric measure (object available-width available-height))

(defgeneric layout (object left top width height))

(defgeneric paint (object))

(defgeneric paint-order-walk (object callback))

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

(defun call-widget-event-handlers (widget widget-events event propagation)
  (dolist (handler widget-events)
    (when (eq (cdr handler) propagation)
      (funcall (car handler) widget event)
      (when (handled event)
        (return-from call-widget-event-handlers)))))

(defun widget-on-mouse-move (object mouse-event propagation)
  (call-widget-event-handlers object (event-mouse-move object) mouse-event propagation))

(defun widget-on-mouse-enter (object mouse-event propagation)
  (call-widget-event-handlers object (event-mouse-enter object) mouse-event propagation))

(defun widget-on-mouse-leave (object mouse-event propagation)
  (call-widget-event-handlers object (event-mouse-leave object) mouse-event propagation))

(defun widget-on-mouse-down (object mouse-event propagation)
  (call-widget-event-handlers object (event-mouse-down object) mouse-event propagation))

(defun widget-on-mouse-up (object mouse-event propagation)
  (call-widget-event-handlers object (event-mouse-up object) mouse-event propagation))

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

(defun add-mouse-move (object handler propagation)
  (push (cons handler propagation) (event-mouse-move object)))

(defun add-mouse-enter (object handler propagation)
  (push (cons handler propagation) (event-mouse-enter object)))

(defun add-mouse-leave (object handler propagation)
  (push (cons handler propagation) (event-mouse-leave object)))

(defun add-mouse-button-down (object handler propagation)
  (push (cons handler propagation) (event-mouse-down object)))

(defun add-mouse-button-up (object handler propagation)
  (push (cons handler propagation) (event-mouse-up object)))

(defmethod paint-order-walk ((object widget) callback)
  (funcall callback object))

;;; CONTAINER class.

(defclass container (widget)
  ((children :accessor children :initarg :children)))

(defmethod initialize-instance :after ((instance container) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (mapc (lambda (widget)
          (setf (parent widget) instance))
        (children instance)))

(defmethod paint-order-walk ((object container) callback)
  (when (funcall callback object)
    (mapc (lambda (child) (paint-order-walk child callback))
          (children object))))

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

(defmethod initialize-instance :after ((instance container1) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (parent (child instance)) instance))

(defmethod paint-order-walk ((object container1) callback)
  (when (funcall callback object)
    (paint-order-walk (child object) callback)))

;;; SCENE class.

(defclass scene ()
  ((widget :accessor widget :initarg :widget :initform nil)
   (width :accessor width :initarg :width :initform 1024)
   (height :accessor height :initarg :height :initform 768)
   (last-widget-chain :accessor last-widget-chain :initarg :last-widget-chain :initform nil)))

(defun paint-scene (scene)
  (paint-order-walk (widget scene)
                    (lambda (scene)
                      (paint scene)
                      t)))

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

(defun hit-test (widget x y)
  (let (result)
    (paint-order-walk widget
                      (lambda (object)
                        (if (in-widget x y object)
                            (setf result object))))
    result))

(defun cascade-then-bubble (widget-chain event-handler event-arg)
  (dolist (widget widget-chain)
    (funcall event-handler widget event-arg :cascade)
    (when (handled event-arg)
      (return)))
  ;; then mouse-move bubble;
  (unless (handled event-arg)
    (dolist (widget (reverse widget-chain))
      (funcall event-handler widget event-arg :bubble)
      (when (handled event-arg)
        (return)))))

(defun branch-diff (branch1 branch2)
  (cond ((null branch1) nil)
        ((null branch2) branch1)
        ((eq (first branch1) (first branch2)) (branch-diff (rest branch1)
                                                           (rest branch2)))
        (t branch1)))

(defun calculate-mouse-leave (old-chain new-chain)
  (branch-diff old-chain new-chain))

(defun calculate-mouse-enter (old-chain new-chain)
  (branch-diff new-chain old-chain))

(defun scene-on-mouse-move (scene mouse-event)
  (let* ((widget-chain (get-widget-chain (list (hit-test (widget scene)
                                                         (mouse-x mouse-event)
                                                         (mouse-y mouse-event)))))
         (mouse-leave-widgets (calculate-mouse-leave (last-widget-chain scene)
                                                     widget-chain))
         (mouse-enter-widgets (calculate-mouse-enter (last-widget-chain scene)
                                                     widget-chain)))
    (setf (last-widget-chain scene) widget-chain)
    (cascade-then-bubble mouse-leave-widgets #'widget-on-mouse-leave mouse-event)
    (setf (handled mouse-event) nil)
    (cascade-then-bubble mouse-enter-widgets #'widget-on-mouse-enter mouse-event)
    (setf (handled mouse-event) nil)
    (cascade-then-bubble widget-chain #'widget-on-mouse-move mouse-event)))

(defun scene-on-mouse-updown (scene mouse-event handler)
  (let ((widget-chain (get-widget-chain (list (hit-test (widget scene)
                                                        (mouse-x mouse-event)
                                                        (mouse-y mouse-event))))))
    (cascade-then-bubble widget-chain handler mouse-event)))

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

;;; CLICKABLE class.

(defclass clickable (container1)
  ((click-state :accessor click-state :initarg :click-state :initform :neutral)
   (event-click :accessor event-click :initarg :event-click :initform nil)))

(defun change-clickable-state (clickable new-state)
  (setf (click-state clickable) new-state))

(defmethod initialize-instance :after ((instance clickable) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (add-mouse-button-down instance
                         (lambda (clickable event)
                           (declare (ignore event))
                           (change-clickable-state clickable :half-click))
                         :bubble)
  (add-mouse-enter instance
                   (lambda (clickable event)
                     (declare (ignore event))
                     (change-clickable-state clickable :neutral))
                   :bubble)
  (add-mouse-leave instance
                   (lambda (clickable event)
                     (declare (ignore event))
                     (change-clickable-state clickable :neutral))
                   :bubble)
  (add-mouse-button-up instance
                       (lambda (clickable event)
                         (declare (ignore event))
                         (when (eql :half-click (click-state clickable))
                           (call-widget-event-handlers clickable
                                                       (event-click clickable)
                                                       (make-instance 'event)
                                                       :bubble)
                           (change-clickable-state clickable :neutral)))
                       :bubble))

(defun add-click-handler (clickable handler)
  (push (cons handler :bubble) (event-click clickable)))

;;; BUTTON class.

(defclass button (clickable)
  ())

(defmethod (setf click-state) :after (value (instance button))
  (layout instance
          (layout-left instance) (layout-top instance)
          (layout-width instance) (layout-height instance)))

(defmethod measure ((object button) available-width available-height)
  (let ((child-size (measure (child object) available-width available-height)))
    (call-next-method object (+ 5 (first child-size)) (+ 5 (second child-size)))))

(defmethod layout ((object button) left top width height)
  (case (click-state object)
    (:neutral (layout (child object)
                      (+ 2 left) (+ 2 top)
                      (- width 5) (- height 5)))
    (:half-click (layout (child object)
                         (+ 3 left) (+ 3 top)
                         (- width 5) (- height 5))))
  (call-next-method))

(defun draw-button (button pressed)
  ;; draw the outer border
  (cl-cairo2:rectangle (+ 0.5 (layout-left button))
                       (+ 0.5 (layout-top button))
                       (- (layout-width button) 1)
                       (- (layout-height button) 1))
  (cl-cairo2:set-source-rgb 0 0 0)
  (cl-cairo2:set-line-width 1)
  (cl-cairo2:stroke)
  ;; draw the inner borders
  ;; left border
  (cl-cairo2:move-to (+ 1.5 (layout-left button))
                     (+ 1.5 (layout-top button)))
  (cl-cairo2:line-to (+ 1.5 (layout-left button))
                     (- (+ (layout-top button) (layout-height button)) 1))
  ;; upper border
  (cl-cairo2:move-to (+ 1 (layout-left button))
                     (+ 1.5 (layout-top button)))
  (cl-cairo2:line-to (- (+ (layout-left button) (layout-width button)) 1)
                     (+ 1.5 (layout-top button)))
  ;; draw
  (cl-cairo2:set-line-width 1)
  (if pressed
      (cl-cairo2:set-source-rgb 0.3 0.3 0.3)
      (cl-cairo2:set-source-rgb 0.9 0.9 0.9))
  (cl-cairo2:stroke)
  ;; lower border
  (cl-cairo2:move-to (+ 2 (layout-left button))
                     (- (+ (layout-top button) (layout-height button)) 1.5))
  (cl-cairo2:line-to (- (+ (layout-left button) (layout-width button)) 1)
                     (- (+ (layout-top button) (layout-height button)) 1.5))
  ;; right border
  (cl-cairo2:move-to (- (+ (layout-left button) (layout-width button)) 1.5)
                     (+ 2 (layout-top button)))
  (cl-cairo2:line-to (- (+ (layout-left button) (layout-width button)) 1.5)
                     (- (+ (layout-top button) (layout-height button)) 2))
  ;; draw
  (cl-cairo2:set-line-width 1)
  (if pressed
      (cl-cairo2:set-source-rgb 0.9 0.9 0.9)
      (cl-cairo2:set-source-rgb 0.3 0.3 0.3))
  (cl-cairo2:stroke)
  ;; draw the background
  (cl-cairo2:rectangle (+ 2 (layout-left button)) (+ 2 (layout-top button))
                       (- (layout-width button) 4) (- (layout-height button) 4))
  (cl-cairo2:set-source-rgb 0.8 0.8 0.8)
  (cl-cairo2:fill-path))

(defmethod paint ((object button))
  (draw-button object (eql (click-state object) :half-click)))

;;; STATEFUL class.

(defclass stateful ()
  ((state :accessor state :initarg :state :initform nil)))

;;; TOGGLE-BUTTON class.

(defclass toggle-button (button stateful)
  ())

(defmethod initialize-instance :after ((instance toggle-button)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (add-click-handler instance
                     (lambda (instance event)
                       (declare (ignore event))
                       (setf (state instance)
                             (not (state instance))))))

(defmethod paint ((object toggle-button))
  (draw-button object (or (state object)
                          (eql :half-click (click-state object)))))
