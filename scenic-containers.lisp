
(in-package :scenic)

;;; CONTAINER class.

(defclass container (widget)
  ((children :accessor children :initarg :children :initform nil)))

(defmethod initialize-instance :after ((instance container) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (mapc (lambda (widget)
          (setf (parent widget) instance))
        (children instance)))

(defmethod paint-order-walk ((object container) callback)
  (when (funcall callback object)
    (mapc (lambda (child) (paint-order-walk child callback))
          (children object))))

(defmethod (setf children) :after (value (instance container))
  (mapc (lambda (child) (setf (parent child) instance))
        (children instance)))

;;; BOX class.

(defclass box (container)
  ((space-between-cells :accessor space-between-cells :initarg :space-between-cells :initform 0)
   (direction :accessor direction :initarg :direction :initform nil)))

(defmethod measure ((object box) available-width available-height)
  (ecase (direction object)
    (:vertical
     (let* ((child-sizes (mapcar #'(lambda (widget)
                                     (measure widget available-width available-height))
                                 (children object)))
            (vertical-size (+ (* (1- (length child-sizes))
                                 (space-between-cells object))
                              (reduce #'+ (mapcar #'second child-sizes))))
            (horizontal-size (apply #'max (mapcar #'first child-sizes))))
       (call-next-method object horizontal-size vertical-size)))
    (:horizontal
     (let* ((child-sizes (mapcar #'(lambda (widget)
                                     (measure widget available-width available-height))
                                 (children object)))
            (horizontal-size (+ (* (1- (length child-sizes))
                                   (space-between-cells object))
                                (reduce #'+ (mapcar #'first child-sizes))))
            (vertical-size (apply #'max (mapcar #'second child-sizes))))
       (call-next-method object horizontal-size vertical-size)))))

(defmethod layout ((object box) left top width height)
  (ecase (direction object)
    (:vertical
     (let ((running-top top))
       (dolist (widget (children object))
         (layout widget left running-top (measured-width widget) (measured-height widget))
         (incf running-top (+ (measured-height widget) (space-between-cells object))))))
    (:horizontal
     (let ((running-left left))
       (dolist (widget (children object))
         (layout widget running-left top (measured-width widget) (measured-height widget))
         (incf running-left (+ (measured-width widget) (space-between-cells object)))))))
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

(defmethod (setf child) :after (value (instance container1))
  (setf (parent value) instance))
