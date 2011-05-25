
(in-package :scenic)

;;; PADDING class.

(defclass padding (container1)
  ((left-padding :accessor left-padding :initarg :left-padding :initform 0)
   (top-padding :accessor top-padding :initarg :top-padding :initform 0)
   (right-padding :accessor right-padding :initarg :right-padding :initform 0)
   (bottom-padding :accessor bottom-padding :initarg :bottom-padding :initform 0)))

(defmethod measure ((object padding) available-width available-height)
  (let* ((size (measure (child object)
                        (- available-width
                           (left-padding object) (right-padding object))
                        (- available-height
                           (top-padding object) (bottom-padding object))))
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
  (cl-cairo2:stroke))

(defmethod measure ((object border) available-width available-height)
  (let* ((size (measure (child object)
                        (- available-width (* 2 (stroke-width object)))
                        (- available-height (* 2 (stroke-width object)))))
         (width (+ (* 2 (stroke-width object)) (first size)))
         (height (+ (* 2 (stroke-width object)) (second size))))
    (call-next-method object width height)))

(defmethod layout ((object border) left top width height)
  (layout (child object)
          (+ left (stroke-width object))
          (+ top (stroke-width object))
          (- width (* 2 (stroke-width object)))
          (- height (* 2 (stroke-width object))))
  (call-next-method object left top width height))

;;; BACKGROUND class.

(defclass background (container1)
  ((fill-color :accessor fill-color :initarg :fill-color :initform 0)))

(defmethod paint ((object background))
  (cl-cairo2:rectangle (layout-left object)
                       (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (apply #'cl-cairo2:set-source-rgb (fill-color object))
  (cl-cairo2:fill-path))

(defmethod measure ((object background) available-width available-height)
  (apply #'call-next-method
         object
         (measure (child object) available-width available-height)))

(defmethod layout ((object background) left top width height)
  (layout (child object)
          left
          top
          width
          height)
  (call-next-method object left top width height))

;;; SIZER class.

(defclass sizer (container1)
  ((min-width :accessor min-width :initarg :min-width :initform nil)
   (min-height :accessor min-height :initarg :min-height :initform nil)
   (max-width :accessor max-width :initarg :max-width :initform nil)
   (max-height :accessor max-height :initarg :max-height :initform nil)))

(declaim (optimize (debug 3)))
(defmethod measure ((object sizer) available-width available-height)
  (let* ((size (measure (child object)
                        (if (null (max-width object))
                            available-width
                            (min available-width (max-width object)))
                        (if (null (max-height object))
                            available-height
                            (min available-height (max-height object)))))
         (width (first size))
         (height (second size)))
    (when (not (null (min-width object)))
      (setf width (max width (min-width object))))
    (when (not (null (min-height object)))
      (setf height (max height (min-height object))))
    (call-next-method object width height)))

(defmethod layout ((object sizer) left top width height)
  (layout (child object) left top width height)
  (call-next-method))
