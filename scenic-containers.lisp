
(in-package :scenic)

;;; CONTAINER class.

(defclass container (widget)
  ((children :accessor children :initarg :children :initform nil)))

(declaim (optimize (debug 3)))

(defmethod initialize-instance :after ((instance container) &rest initargs)
  (declare (ignore initargs))
  (mapc (lambda (widget)
          (setf (parent widget) instance))
        (children instance)))

(defmethod paint-order-walk ((object container) callback &key (after-callback nil))
  (when (funcall callback object)
    (mapc (lambda (child) (paint-order-walk child
                                            callback
                                            :after-callback after-callback))
          (children object)))
  (when after-callback
    (funcall after-callback object)))

(defmethod (setf children) :after (value (instance container))
  (loop
     for child in (children instance)
     for idx = 1 then (1+ idx)
     do
       (setf (parent child) instance)
       (setf (auto-name child) (make-widget-auto-name child idx))))

;;; STACK class.

(defclass stack (container)
  ())

(defmethod measure ((object stack) available-width available-height)
  (let ((max-width 0)
        (max-height 0))
    (mapc (lambda (widget)
            (multiple-value-bind (w h)
                (measure widget available-width available-height)
              (setf max-width (max max-width w))
              (setf max-height (max max-height h))))
          (children object))
    (set-measured object max-width max-height)))

(defmethod layout ((object stack) left top width height)
  (mapc (lambda (widget)
          (layout widget left top width height))
        (children object))
  (call-next-method object left top width height))

;;; CONTAINER1 class.

(defclass container1 (widget)
  ((child :accessor child :initarg :child :initform nil)))

(defmethod initialize-instance :after ((instance container1) &rest initargs)
  (declare (ignore initargs))
  (when (child instance)
    (setf (parent (child instance)) instance)))

(defmethod paint-order-walk ((object container1) callback &key (after-callback nil))
  (when (funcall callback object)
    (awhen (child object)
      (paint-order-walk it callback :after-callback after-callback)))
  (when after-callback
    (funcall after-callback object)))

(defmethod (setf child) :after (value (instance container1))
  (when value
    (setf (parent value) instance)
    (setf (auto-name value) (make-widget-auto-name value 1))))

(defmethod measure ((object container1) available-width available-height)
  (multiple-value-bind (width height)
      (measure (child object) available-width available-height)
    (set-measured object width height)))

(defmethod layout ((object container1) left top width height)
  (layout (child object)
          left
          top
          width
          height)
  (set-layout object left top width height))

;;; HENCHMAN class.

(defclass henchman (container)
  ((children-locations :accessor children-locations
                       :initarg :children-locations
                       :initform nil)))

(defmethod measure ((object henchman) available-width available-height)
  (set-measured object available-width available-height)
  (let ((children-options (locations-to-options (children-locations object))))
    (loop
       for child in (children object)
       for options in children-options
       do (multiple-value-bind (left top width height)
              (get-location options 0 0 available-width available-height)
            (declare (ignore left top))
            (measure child width height))))
  (values available-width available-height))

(defmethod layout ((object henchman) left top width height)
  (set-layout object left top width height)
  (let ((children-options (locations-to-options (children-locations object))))
    (loop
       for child in (children object)
       for options in children-options
       do (multiple-value-bind (cleft ctop cwidth cheight)
              (get-location options left top width height)
            (layout child cleft ctop cwidth cheight)))))

(defun get-location (options pleft ptop pwidth pheight)
  (let-from-options options ((left nil) (top nil)
                             (right nil) (bottom nil)
                             (width nil) (height nil))
    (let (lleft lwidth ltop lheight)
      (setf (values lleft lwidth) (get-pos-length pleft pwidth left width right))
      (setf (values ltop lheight) (get-pos-length ptop pheight top height bottom))
      (values lleft ltop lwidth lheight))))

(defun get-pos-length (parent-pos parent-length maybe-pos maybe-length maybe-antipos)
  (cond ((and maybe-pos maybe-length maybe-antipos)
         (error "Position overspecified."))
        ((and maybe-pos maybe-length)
         (values (+ parent-pos maybe-pos)
                 maybe-length))
        ((and maybe-pos maybe-antipos)
         (values (+ parent-pos maybe-pos)
                 (1+ (- (- parent-length maybe-antipos) maybe-pos))))
        ((and maybe-length maybe-antipos)
         (values (+ (1+ (- (- parent-length maybe-antipos) maybe-length))
                    parent-pos)
                 maybe-length))
        (t (error "Position underspecified."))))

(defun locations-to-options (locations)
  (let (result)
    (loop
       for location in locations
       do (push (mapcar (lambda (list)
                          (cons (first list)
                                (second list)))
                        (groups location 2))
                result))
    (nreverse result)))

;;; SCROLL-VIEW class.

(defclass scroll-view (container1)
  ((horizontal-offset :accessor horizontal-offset
                      :initarg :horizontal-offset
                      :initform 0)
   (vertical-offset :accessor vertical-offset
                    :initarg :vertical-offset
                    :initform 0)
   (inside-width :accessor inside-width
                 :initarg :inside-width
                 :initform (expt 10 6))
   (inside-height :accessor inside-height
                  :initarg :inside-height
                  :initform (expt 10 6))))

(defmethod clips-content ((object scroll-view))
  t)

(defmethod (setf horizontal-offset) :after (value (object scroll-view))
  (declare (ignore value))
  (on-event object :scroll-view-offset-changed (make-instance 'event) nil))

(defmethod (setf vertical-offset) :after (value (object scroll-view))
  (declare (ignore value))
  (on-event object :scroll-view-offset-changed (make-instance 'event) nil))

(defmethod measure ((object scroll-view) available-width available-height)
  (measure (child object) (inside-width object) (inside-height object))
  (set-measured object available-width available-height)
  (on-event object :scroll-view-measured
            (make-instance 'scroll-view-measured-event
                           :inner-width (measured-width (child object))
                           :inner-height (measured-height (child object))
                           :outer-width (measured-width object)
                           :outer-height (measured-height object))
            nil)
  (values available-width available-height))

(defmethod layout ((object scroll-view) left top width height)
  (layout (child object)
          left top
          (measured-width (child object)) (measured-height (child object)))
  (set-layout object left top width height))

(defmethod paint ((object scroll-view))
  (cl-cairo2:save)
  (cl-cairo2:rectangle (layout-left object)
                       (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (cl-cairo2:clip)
  (cl-cairo2:translate (- (horizontal-offset object))
                       (- (vertical-offset object))))

(defmethod after-paint ((object scroll-view))
  (cl-cairo2:restore))

;;; BOX class.

(defclass box (container orientable)
  ((space-between-cells :accessor space-between-cells :initarg :space-between-cells :initform 0)))

(defmethod measure ((object box) available-width available-height)
  (ecase (orientation object)
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
  (ecase (orientation object)
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

