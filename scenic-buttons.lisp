
(in-package :scenic)

(declaim (optimize (debug 3)))

;;; CLICKABLE class.

(defclass clickable (container1)
  ((click-state :accessor click-state :initarg :click-state :initform :neutral)))

(defun change-clickable-state (clickable new-state)
  (setf (click-state clickable) new-state))

(defmethod initialize-instance :after ((instance clickable) &rest initargs)
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
                             (on-event clickable :click event nil))
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
  (multiple-value-bind (width height)
      (measure (child object) available-width available-height)
    (set-measured object (+ 3 width) (+ 3 height))))

(defmethod layout ((object button) left top width height)
  (case (click-state object)
    (:neutral (layout (child object)
                      (+ 1 left) (+ 1 top)
                      (- width 3) (- height 3)))
    (:half-click (layout (child object)
                         (+ 2 left) (+ 2 top)
                         (- width 3) (- height 3))))
  (set-layout object left top width height))

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

;;; STATEFUL-BUTTON class.

(defclass stateful-button (button stateful)
  ())

(defmethod initialize-instance :after ((instance stateful-button) &rest initargs)
  (declare (ignore initargs))
  (add-event-handler instance :click :bubble
                     (lambda (instance event)
                       (declare (ignore event))
                       (setf (state instance)
                             (not (state instance))))))

(defmethod (setf state) :after (new-value (object stateful-button))
  (on-event object :state-changed (make-instance 'event) nil)
  (invalidate object))

;;; TOGGLE-BUTTON class.

(defclass toggle-button (stateful-button)
  ())

(defmethod paint ((object toggle-button))
  (draw-button object (or (state object)
                          (eql :half-click (click-state object)))))

;;; CHECKBOX class.

(defclass checkbox (stateful-button)
  ())

(defmethod measure ((object checkbox) available-width available-height)
  (set-measured object 16 16))

(defmethod layout ((object checkbox) left top width height)
  (set-layout object left top 16 16))

(defmethod paint ((object checkbox))
  (cl-cairo2:set-source-rgba 1 1 1 1)
  (cl-cairo2:rectangle (layout-left object)
                       (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (cl-cairo2:fill-path)
  (cl-cairo2:set-source-rgba 0 0 0 1)
  (cl-cairo2:set-line-width 1)
  (cl-cairo2:rectangle (+ 0.5 (layout-left object))
                       (+ 0.5 (layout-top object))
                       (layout-width object)
                       (layout-height object))
  (cl-cairo2:stroke)
  (when (state object)
    (cl-cairo2:move-to (+ 3 (layout-left object))
                       (+ 3 (layout-top object)))
    (cl-cairo2:line-to (+ 13 (layout-left object))
                       (+ 13 (layout-top object)))
    (cl-cairo2:stroke)
    (cl-cairo2:move-to (+ 13 (layout-left object))
                       (+ 3 (layout-top object)))
    (cl-cairo2:line-to (+ 3 (layout-left object))
                       (+ 13 (layout-top object)))
    (cl-cairo2:stroke)))

;;; RADIO-BUTTON class.

(defclass radio-button (stateful-button)
  ())

(defmethod measure ((object radio-button) available-width available-height)
  (set-measured object 16 16))

(defmethod layout ((object radio-button) left top width height)
  (set-layout object left top 16 16))

(defmethod paint ((object radio-button))
  (cl-cairo2:set-source-rgba 1 1 1 1)
  (cl-cairo2:rectangle (layout-left object)
                       (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (cl-cairo2:fill-path)
  (cl-cairo2:set-source-rgba 0 0 0 1)
  (cl-cairo2:set-line-width 1)
  (cl-cairo2:arc (+ 8 (layout-left object)) (+ 8 (layout-top object))
                 6 0 (* 2 pi))
  (cl-cairo2:stroke)
  (when (state object)
    (cl-cairo2:arc (+ 8 (layout-left object)) (+ 8 (layout-top object))
                   3
                   0 (* 2 pi))
    (cl-cairo2:fill-path)))

