
(in-package :scenic)

(declaim (optimize (debug 3)))

(defgeneric measure (object available-width available-height))

(defgeneric layout (object left top width height))

(defgeneric paint (object))

(defgeneric paint-order-walk (object callback))

(defgeneric in-widget (x y widget))

(defgeneric add-event-handler (object event propagation callback))

(defgeneric on-event (object event event-args propagation))

(defgeneric intersect (object1 object2))

;;; EVENTFUL class.

(defclass eventful ()
  ((event-handlers :accessor event-handlers :initarg event-handlers :initform nil)))

(defmethod add-event-handler ((object eventful) event propagation handler)
  (let ((handler-list (assoc event (event-handlers object))))
    (if handler-list
        (push (cons handler propagation) (cdr handler-list))
        (let ((fresh-handler-list (list (cons handler propagation))))
          (push (cons event fresh-handler-list) (event-handlers object))))))

(defmethod on-event ((object eventful) event event-arg propagation)
  (dolist (handler (cdr (assoc event (event-handlers object))))
    (when (or (null propagation) (eq (cdr handler) propagation))
      (funcall (car handler) object event-arg)
      (when (handled event-arg)
        (return-from on-event)))))

;;; WIDGET class.

(defclass widget (eventful)
  ((measured-width :accessor measured-width :initarg measured-width :initform nil)
   (measured-height :accessor measured-height :initarg measured-height :initform nil)
   (layout-left :accessor layout-left :initarg :layout-left :initform 0)
   (layout-top :accessor layout-top :initarg :layout-top :initform 0)
   (layout-width :accessor layout-width :initarg :layout-width :initform nil)
   (layout-height :accessor layout-height :initarg :layout-height :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (paint-order-number :accessor paint-order-number :initarg :paint-order-number :initform -1)))

(defmethod print-object ((object widget) stream)
  (format stream
          "~a (~a,~a,~a,~a)"
          (type-of object)
          (layout-left object)
          (layout-top object)
          (layout-width object)
          (layout-height object)))

(defun in-rectangle (x y left top width height)
  (and (<= left x)
       (< x (+ left width))
       (<= top y)
       (< y (+ top height))))

(defmethod in-widget (x y (widget widget))
  (in-rectangle x y
                (layout-left widget) (layout-top widget)
                (layout-width widget) (layout-height widget)))

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
   (last-widget-chain :accessor last-widget-chain :initarg :last-widget-chain :initform nil)
   (mouse-captors :accessor mouse-captors :initarg :mouse-captors :initform nil)
   (dirty :accessor dirty :initarg :dirty :initform t)
   (dirty-list :accessor dirty-list :initarg :dirty-list :initform nil)
   (rectangle-to-redraw :accessor rectangle-to-redraw :initarg :rectangle-to-redraw :initform nil)))

(defun get-scene (widget)
  (if (eql (type-of widget) 'scene)
      widget
      (get-scene (parent widget))))

(defun capture-mouse (widget)
  (let ((scene (get-scene widget)))
    (if (not (member widget (mouse-captors scene)))
        (push widget (mouse-captors scene)))))

(defun release-mouse (widget)
  (let ((scene (get-scene widget)))
    (setf (mouse-captors scene)
          (remove widget (mouse-captors scene)))))

(defun invalidate (widget)
  (let ((scene (get-scene widget)))
    (setf (dirty scene) t)
    (push widget (dirty-list scene))))

(defun corners-of-widget (widget)
  (list (list (layout-left widget)
              (layout-top widget))
        (list (1- (+ (layout-left widget) (layout-width widget)))
              (layout-top widget))
        (list (layout-left widget)
              (1- (+ (layout-top widget) (layout-height widget))))
        (list (1- (+ (layout-left widget) (layout-width widget)))
              (1- (+ (layout-top widget) (layout-height widget))))))

(defmethod intersect ((object1 widget) (object2 widget))
  (dolist (corner (corners-of-widget object1))
    (if (in-widget (first corner) (second corner) object2)
        (return-from intersect t))))

(defun widget-paint-member (object list)
  (cond ((null list)
         nil)
        ((or (eq object (first list))
             (and (intersect object (first list))
                  (> (paint-order-number object)
                     (paint-order-number (first list)))))
         t)
        (t (widget-paint-member object (rest list)))))

(defun bounding-box (widget)
  (list (layout-left widget)
        (layout-top widget)
        (1- (+ (layout-left widget) (layout-width widget)))
        (1- (+ (layout-top widget) (layout-height widget)))))

(defun common-bounding-box (bbox1 bbox2)
  (list (min (first bbox1) (first bbox2))
        (min (second bbox1) (second bbox2))
        (max (third bbox1) (third bbox2))
        (max (fourth bbox1) (fourth bbox2))))

(defun paint-scene (scene)
  (if (null (dirty-list scene))
      (paint-order-walk (widget scene)
                        (lambda (object)
                          (paint object)
                          t))
      (progn
        (let ((number 0))
          (paint-order-walk (widget scene)
                            (lambda (object)
                              (setf (paint-order-number object) number)
                              (incf number)
                              t)))
        (paint-order-walk (widget scene)
                          (lambda (object)
                            (when (widget-paint-member object (dirty-list scene))
                              (paint object)
                              (push object (dirty-list scene)))
                            t))
        (setf (rectangle-to-redraw scene)
              (reduce #'common-bounding-box
                      (mapcar #'bounding-box (dirty-list scene))))))
  (setf (dirty-list scene) nil))

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

(defun cascade-then-bubble (widget-chain event event-arg)
  (dolist (widget widget-chain)
    (on-event widget event event-arg :cascade)
    (when (handled event-arg)
      (return)))
  ;; then mouse-move bubble;
  (unless (handled event-arg)
    (dolist (widget (reverse widget-chain))
      (on-event widget event event-arg :bubble)
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

(defun scene-handle-mouse-captors (scene event mouse-event)
  (dolist (captor (mouse-captors scene))
    (setf (handled mouse-event) nil)
    (on-event captor event mouse-event nil)))

(defun scene-on-mouse-move (scene mouse-event)
  (let* ((widget-chain (get-widget-chain (list (hit-test (widget scene)
                                                         (mouse-x mouse-event)
                                                         (mouse-y mouse-event)))))
         (mouse-leave-widgets (calculate-mouse-leave (last-widget-chain scene)
                                                     widget-chain))
         (mouse-enter-widgets (calculate-mouse-enter (last-widget-chain scene)
                                                     widget-chain)))
    (setf (last-widget-chain scene) widget-chain)
    (cascade-then-bubble mouse-leave-widgets :mouse-leave mouse-event)
    (setf (handled mouse-event) nil)
    (cascade-then-bubble mouse-enter-widgets :mouse-enter mouse-event)
    (setf (handled mouse-event) nil)
    (cascade-then-bubble widget-chain :mouse-move mouse-event)
    (setf (handled mouse-event) nil)
    (scene-handle-mouse-captors scene :mouse-move mouse-event)))

(defun scene-on-mouse-button (scene event mouse-event)
  (let ((widget-chain (get-widget-chain (list (hit-test (widget scene)
                                                        (mouse-x mouse-event)
                                                        (mouse-y mouse-event))))))
    (cascade-then-bubble widget-chain event mouse-event)
    (scene-handle-mouse-captors scene event mouse-event)))

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
  ((click-state :accessor click-state :initarg :click-state :initform :neutral)))

(defun change-clickable-state (clickable new-state)
  (setf (click-state clickable) new-state))

(defmethod initialize-instance :after ((instance clickable) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((is-inside nil))
    (add-event-handler instance :mouse-button-down :bubble
                       (lambda (clickable event)
                         (declare (ignore))
                         (when (= 1 (mouse-button event))
                           (change-clickable-state clickable :half-click)
                           (capture-mouse instance))))
    (add-event-handler instance :mouse-enter :bubble
                       (lambda (clickable event)
                         (declare (ignore clickable event))
                         (setf is-inside t)))
    (add-event-handler instance :mouse-leave :bubble
                       (lambda (clickable event)
                         (declare (ignore clickable event))
                         (setf is-inside nil)))
    (add-event-handler instance :mouse-button-up :bubble
                       (lambda (clickable event)
                         (declare (ignore))
                         (when (= 1 (mouse-button event))
                           (release-mouse instance)
                           (when (and (eql :half-click (click-state clickable))
                                      is-inside)
                             (on-event clickable :click (make-instance 'event) nil))
                           (change-clickable-state clickable :neutral))))))

;;; BUTTON class.

(defclass button (clickable)
  ())

(defmethod (setf click-state) :after (value (instance button))
  (layout instance
          (layout-left instance) (layout-top instance)
          (layout-width instance) (layout-height instance))
  (invalidate instance))

(defmethod measure ((object button) available-width available-height)
  (let ((child-size (measure (child object) available-width available-height)))
    (call-next-method object (+ 3 (first child-size)) (+ 3 (second child-size)))))

(defmethod layout ((object button) left top width height)
  (case (click-state object)
    (:neutral (layout (child object)
                      (+ 1 left) (+ 1 top)
                      (- width 3) (- height 3)))
    (:half-click (layout (child object)
                         (+ 2 left) (+ 2 top)
                         (- width 3) (- height 3))))
  (call-next-method))

(defun draw-button-raw (left top width height pressed)
  ;; draw the inner borders (for the 3d illusion)
  ;; left border
  (cl-cairo2:move-to (+ 0.5 left)
                     (+ 0.5 top))
  (cl-cairo2:line-to (+ 0.5 left)
                     (+ top height))
  ;; upper border
  (cl-cairo2:move-to left
                     (+ 0.5 top))
  (cl-cairo2:line-to (+ left width)
                     (+ 0.5 top))
  ;; draw
  (cl-cairo2:set-line-width 1)
  (if pressed
      (cl-cairo2:set-source-rgb 0.3 0.3 0.3)
      (cl-cairo2:set-source-rgb 0.9 0.9 0.9))
  (cl-cairo2:stroke)
  ;; lower border
  (cl-cairo2:move-to (+ 1 left)
                     (- (+ top height) 0.5))
  (cl-cairo2:line-to (+ left width)
                     (- (+ top height) 0.5))
  ;; right border
  (cl-cairo2:move-to (- (+ left width) 0.5)
                     (+ 1 top))
  (cl-cairo2:line-to (- (+ left width) 0.5)
                     (- (+ top height) 1))
  ;; draw
  (cl-cairo2:set-line-width 1)
  (if pressed
      (cl-cairo2:set-source-rgb 0.9 0.9 0.9)
      (cl-cairo2:set-source-rgb 0.3 0.3 0.3))
  (cl-cairo2:stroke)
  ;; draw the background
  (cl-cairo2:rectangle (+ 1 left) (+ 1 top)
                       (- width 2) (- height 2))
  (cl-cairo2:set-source-rgb 0.8 0.8 0.8)
  (cl-cairo2:fill-path))

(defun draw-button (button pressed)
  (draw-button-raw (layout-left button)
                   (layout-top button)
                   (layout-width button)
                   (layout-height button)
                   pressed))

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
  (add-event-handler instance :click :bubble
                     (lambda (instance event)
                       (declare (ignore event))
                       (setf (state instance)
                             (not (state instance))))))

(defmethod paint ((object toggle-button))
  (draw-button object (or (state object)
                          (eql :half-click (click-state object)))))

;;; HORIZONTAL-SLIDER class.

(defclass horizontal-slider (widget)
  ((min-value :accessor min-value :initarg :min-value :initform nil)
   (max-value :accessor max-value :initarg :max-value :initform nil)
   (page-size :accessor page-size :initarg :page-size :initform nil)
   (current-min-position :accessor current-min-position
                         :initarg :current-min-position :initform nil)))

(defun get-horizontal-walker-coordinates (horizontal-slider)
  (let* ((extent (1+ (- (max-value horizontal-slider) (min-value horizontal-slider))))
         (rel-page-size (/ (page-size horizontal-slider) extent))
         (walker-width (max 10 (round (* rel-page-size (layout-width horizontal-slider)))))
         (rel-position (/ (- (current-min-position horizontal-slider)
                             (min-value horizontal-slider)) extent))
         (position (floor (* rel-position (layout-width horizontal-slider)))))
    (values (+ (layout-left horizontal-slider) position)
            (layout-top horizontal-slider)
            walker-width
            (layout-height horizontal-slider))))

(defmethod paint ((object horizontal-slider))
  (cl-cairo2:rectangle (layout-left object) (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (cl-cairo2:set-source-rgb 0.6 0.6 0.6)
  (cl-cairo2:fill-path)
  (multiple-value-bind (left top width height)
      (get-horizontal-walker-coordinates object)
    (draw-button-raw left top width height nil)))

(defun over-walker (horizontal-slider x y)
  (multiple-value-bind (left top width height)
      (get-horizontal-walker-coordinates horizontal-slider)
    (in-rectangle x y left top width height)))

(defmethod initialize-instance :after ((instance horizontal-slider)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((dragging nil))
    (add-event-handler instance :mouse-button-down :bubble
                       (lambda (instance event)
                         (when (and (= 1 (mouse-button event))
                                    (over-walker instance (mouse-x event) (mouse-y event)))
                           (setf dragging t)
                           (capture-mouse instance))))
    (add-event-handler instance :mouse-button-up :bubble
                       (lambda (instance event)
                         (when (= 1 (mouse-button event))
                           (setf dragging nil)
                           (release-mouse instance))))
    (add-event-handler instance :mouse-move :bubble
                       (lambda (instance event)
                         (when dragging
                           (let* ((extent (1+ (- (max-value instance)
                                                 (min-value instance))))
                                  (multiplier (/ extent (layout-width instance)))
                                  (new-current-min-pos (- (* multiplier (- (mouse-x event)
                                                                           (layout-left instance)))
                                                          (/ (page-size instance) 2))))
                             (setf new-current-min-pos (max (min-value instance)
                                                            new-current-min-pos))
                             (setf new-current-min-pos (min (max-value instance)
                                                            new-current-min-pos))
                             (when (not (= (current-min-position instance)
                                           new-current-min-pos))
                               (setf (current-min-position instance)
                                     new-current-min-pos)
                               (invalidate instance))))))))