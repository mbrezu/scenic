
(in-package :scenic)

;;; PADDING class.

(defclass padding (container1)
  ((left-padding :accessor left-padding :initarg :left-padding :initform 0)
   (top-padding :accessor top-padding :initarg :top-padding :initform 0)
   (right-padding :accessor right-padding :initarg :right-padding :initform 0)
   (bottom-padding :accessor bottom-padding :initarg :bottom-padding :initform 0)))

(defmethod measure ((object padding) available-width available-height)
  (multiple-value-bind (child-width child-height)
      (measure (child object)
               (- available-width
                  (left-padding object) (right-padding object))
               (- available-height
                  (top-padding object) (bottom-padding object)))
    (let ((width (+ (left-padding object) (right-padding object) child-width))
          (height (+ (top-padding object) (bottom-padding object) child-height)))
      (set-measured object width height))))

(defmethod layout ((object padding) left top width height)
  (layout (child object)
          (+ left (left-padding object))
          (+ top (top-padding object))
          (- width (+ (left-padding object) (right-padding object)))
          (- height (+ (top-padding object) (bottom-padding object))))
  (set-layout object left top width height))

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
  (multiple-value-bind (child-width child-height)
      (measure (child object)
               (- available-width (* 2 (stroke-width object)))
               (- available-height (* 2 (stroke-width object))))
    (let ((width (+ (* 2 (stroke-width object)) child-width))
          (height (+ (* 2 (stroke-width object)) child-height)))
      (set-measured object width height))))

(defmethod layout ((object border) left top width height)
  (layout (child object)
          (+ left (stroke-width object))
          (+ top (stroke-width object))
          (- width (* 2 (stroke-width object)))
          (- height (* 2 (stroke-width object))))
  (set-layout object left top width height))

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

;;; SIZER class.

(defclass sizer (container1)
  ((min-width :accessor min-width :initarg :min-width :initform nil)
   (min-height :accessor min-height :initarg :min-height :initform nil)
   (max-width :accessor max-width :initarg :max-width :initform nil)
   (max-height :accessor max-height :initarg :max-height :initform nil)))

(declaim (optimize (debug 3)))

(defmethod measure ((object sizer) available-width available-height)
  (multiple-value-bind (width height)
      (measure (child object)
               (if (null (max-width object))
                   available-width
                   (min available-width (max-width object)))
               (if (null (max-height object))
                   available-height
                   (min available-height (max-height object))))
    (when (not (null (min-width object)))
      (setf width (max width (min-width object))))
    (when (not (null (min-height object)))
      (setf height (max height (min-height object))))
    (when (not (null (max-width object)))
      (setf width (min width (max-width object))))
    (when (not (null (max-height object)))
      (setf height (min height (max-height object))))
    (set-measured object width height)))

(defmethod layout ((object sizer) left top width height)
  (layout (child object) left top width height)
  (set-layout object left top width height))

;;; ALIGNER class.

(defclass aligner (container1)
  ((vertical :accessor vertical :initarg :vertical :initform :center)
   (horizontal :accessor horizontal :initarg :horizontal :initform :center)))

(defmethod measure ((object aligner) available-width availaible-height)
  (multiple-value-bind (width height)
      (measure (child object) available-width availaible-height)
    (call-next-method object
                      (min width available-width)
                      (min height availaible-height))))

(defmethod layout ((object aligner) left top width height)
  (let ((child-width (measured-width (child object)))
        (child-height (measured-height (child object))))
    (let ((h-space (max 0 (- width child-width)))
          (v-space (max 0 (- height child-height))))
      (layout (child object)
              (ecase (horizontal object)
                ((:left :fill) left)
                ((:right) (+ h-space left))
                ((:center) (+ (/ h-space 2) left)))
              (ecase (vertical object)
                ((:top :fill) top)
                ((:bottom) (+ v-space top))
                ((:center) (+ (/ v-space 2) top)))
              (ecase (horizontal object)
                ((:left :right :center) child-width)
                ((:fill) width))
              (ecase (vertical object)
                ((:top :bottom :center) child-height)
                ((:fill) width)))
      (set-layout object left top width height))))

;;; CLIPPER class.

(defclass clipper (container1)
  ())

(defmethod paint ((instance clipper))
  (cl-cairo2:save)
  (cl-cairo2:rectangle (layout-left instance)
                       (layout-top instance)
                       (layout-width instance)
                       (layout-height instance))
  (cl-cairo2:clip))

(defmethod after-paint ((instance clipper))
  (cl-cairo2:restore))

;;; GLASS class.

(defclass glass (container1)
  ((opacity :accessor opacity :initarg :opacity :initform :center)
   (old-context :accessor old-context
                :initarg :old-context
                :initform :center)
   (cairo-surface :accessor cairo-surface
                  :initarg :cairo-surface
                  :initform :center)))

(defmethod paint ((instance glass))
  (setf (cairo-surface instance)
        (cl-cairo2:create-image-surface :argb32
                                        (measured-width instance)
                                        (measured-height instance)))
  (setf (old-context instance) cl-cairo2:*context*)
  (setf cl-cairo2:*context*
        (cl-cairo2:create-context (cairo-surface instance)))
  (cl-cairo2:translate (- (layout-left instance)) (- (layout-top instance))))

(defmethod after-paint ((instance glass))
  (cl-cairo2:destroy cl-cairo2:*context*)
  (setf cl-cairo2:*context* (old-context instance))

  (cl-cairo2:save)
  (cl-cairo2:rectangle (layout-left instance)
                       (layout-top instance)
                       (layout-width instance)
                       (layout-height instance))
  (cl-cairo2:clip)
  (cl-cairo2:set-source-surface (cairo-surface instance)
                                (layout-left instance)
                                (layout-top instance))
  (cl-cairo2:paint-with-alpha (opacity instance))
  (cl-cairo2:restore)

  ;; Cleaning up.
  (cl-cairo2:destroy (cairo-surface instance)))
