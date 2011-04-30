
(in-package :scenic)

;;; HORIZONTAL-SLIDER class.

(defclass horizontal-slider (widget)
  ((min-value :accessor min-value :initarg :min-value :initform nil)
   (max-value :accessor max-value :initarg :max-value :initform nil)
   (page-size :accessor page-size :initarg :page-size :initform nil)
   (current-min-position :accessor current-min-position
                         :initarg :current-min-position :initform nil)))

(defun get-horizontal-walker-coordinates (horizontal-slider)
  (let* ((extent (- (max-value horizontal-slider) (min-value horizontal-slider)))
         (rel-page-size (/ (page-size horizontal-slider) extent))
         (walker-width (max 10 (floor (* rel-page-size (layout-width horizontal-slider)))))
         (rel-position (/ (- (current-min-position horizontal-slider)
                             (min-value horizontal-slider))
                          extent))
         (position (min (round (* rel-position (layout-width horizontal-slider)))
                        (- (layout-width horizontal-slider) walker-width))))
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
                             (setf new-current-min-pos (min (- (max-value instance)
                                                               (page-size instance))
                                                            new-current-min-pos))
                             (when (not (= (current-min-position instance)
                                           new-current-min-pos))
                               (setf (current-min-position instance)
                                     new-current-min-pos)
                               (invalidate instance)
                               (on-event instance
                                         :position-changed
                                         (make-instance 'event) nil))))))))

;;; ARROW class.

(defclass arrow (widget)
  ((direction :accessor direction :initarg :direction :initform nil)))

(defmethod measure ((object arrow) available-width available-height)
  (call-next-method object 16 16))

(defmethod layout ((object arrow) left top width height)
  (call-next-method object left top 16 16))

(defmethod paint ((object arrow))
  (case (direction object)
    (:left
     (cl-cairo2:move-to (+ (layout-left object) 5) (+ (layout-top object) 8))
     (cl-cairo2:line-to (+ (layout-left object) 11) (+ (layout-top object) 2))
     (cl-cairo2:line-to (+ (layout-left object) 11) (+ (layout-top object) 13))
     (cl-cairo2:set-source-rgb 0 0 0)
     (cl-cairo2:fill-path))
    (:right
     (cl-cairo2:move-to (+ (layout-left object) 11) (+ (layout-top object) 8))
     (cl-cairo2:line-to (+ (layout-left object) 5) (+ (layout-top object) 2))
     (cl-cairo2:line-to (+ (layout-left object) 5) (+ (layout-top object) 13))
     (cl-cairo2:set-source-rgb 0 0 0)
     (cl-cairo2:fill-path))))

;;; HORIZONTAL-SCROLLBAR class.

(defclass horizontal-scrollbar (horizontal-box)
  ((min-value :accessor min-value :initarg :min-value :initform nil)
   (max-value :accessor max-value :initarg :max-value :initform nil)
   (page-size :accessor page-size :initarg :page-size :initform nil)
   (current-min-position :accessor current-min-position
                         :initarg :current-min-position :initform nil)
   (slider :accessor slider :initarg :slider :initform nil)))

(defun set-slider-pos (hsbar new-pos)
  (with-slots (slider) hsbar
    (when (not (= new-pos (current-min-position hsbar)))
      (setf (current-min-position hsbar) new-pos)
      (invalidate hsbar)
      (on-event hsbar :position-changed (make-instance 'event) nil))))

(defmethod initialize-instance :after ((instance horizontal-scrollbar)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let (slider btn-left btn-right)
    (setf (space-between-cells instance) 0)
    (setf (children instance)
          (list (setf btn-left (make-instance 'button
                                              :child (make-instance 'arrow :direction :left)))
                (make-instance 'sizer
                               :max-height 19
                               :child (setf slider
                                            (make-instance
                                             'horizontal-slider
                                             :min-value (min-value instance)
                                             :max-value (max-value instance)
                                             :page-size (page-size instance)
                                             :current-min-position
                                             (current-min-position instance))))
                (setf btn-right (make-instance 'button
                                               :child (make-instance 'arrow :direction :right)))))
    (setf (slider instance) slider)
    (add-event-handler slider :position-changed :bubble
                       (lambda (object event)
                         (setf (current-min-position instance)
                               (current-min-position object))
                         (on-event instance :position-changed event nil)))
    (add-event-handler btn-left :click :bubble
                       (lambda (object event)
                         (declare (ignore object event))
                         (let ((new-pos (max (min-value slider)
                                             (- (current-min-position slider)
                                                (page-size slider)))))
                           (set-slider-pos instance new-pos))))
    (add-event-handler btn-right :click :bubble
                       (lambda (object event)
                         (declare (ignore object event))
                         (let ((new-pos (min (- (max-value slider) (page-size slider))
                                             (+ (current-min-position slider)
                                                (page-size instance)))))
                           (set-slider-pos instance new-pos))))))

(pass-to-child horizontal-scrollbar slider min-value)
(pass-to-child horizontal-scrollbar slider max-value)
(pass-to-child horizontal-scrollbar slider page-size)
(pass-to-child horizontal-scrollbar slider current-min-position)

