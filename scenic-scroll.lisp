
(in-package :scenic)

(declaim (optimize (debug 3)))

;;; SLIDER classes.

(defclass slider (widget orientable)
  ((min-value :accessor min-value :initarg :min-value :initform nil)
   (max-value :accessor max-value :initarg :max-value :initform nil)
   (page-size :accessor page-size :initarg :page-size :initform nil)
   (current-min-position :accessor current-min-position
                         :initarg :current-min-position :initform nil)))

(defun get-slider-length-position (slider)
  (let* ((screen-dimension (ifhorizontal slider
                                         (layout-width slider)
                                         (layout-height slider)))
         (extent (- (max-value slider) (min-value slider)))
         (rel-page-size (/ (page-size slider) extent))
         (walker-width-or-height (min (max 10 (floor (* rel-page-size
                                                        screen-dimension)))
                                      (ifhorizontal slider
                                                    (layout-width slider)
                                                    (layout-height slider))))
         (rel-position (/ (- (current-min-position slider)
                             (min-value slider))
                          extent))
         (position (min (round (* rel-position screen-dimension))
                        (- screen-dimension walker-width-or-height))))
    (values walker-width-or-height position)))

(defun get-walker-coordinates (slider)
  (multiple-value-bind (walker-width-or-height position)
      (get-slider-length-position slider)
    (ifhorizontal slider
                  (values (+ (layout-left slider) position)
                          (layout-top slider)
                          walker-width-or-height
                          (layout-height slider))
                  (values (layout-left slider)
                          (+ (layout-top slider) position)
                          (layout-width slider)
                          walker-width-or-height))))

(defmethod paint ((object slider))
  (cl-cairo2:rectangle (layout-left object) (layout-top object)
                       (layout-width object)
                       (layout-height object))
  (cl-cairo2:set-source-rgb 0.6 0.6 0.6)
  (cl-cairo2:fill-path)
  (multiple-value-bind (left top width height)
      (get-walker-coordinates object)
    (draw-button-raw left top width height nil)))

(defmethod initialize-instance :after ((instance slider) &rest initargs)
  (declare (ignore initargs))
  (let ((dragging nil)
        (rel-pos 0))
    (add-event-handler instance :mouse-button-down :bubble
                       (lambda (instance event)
                         (when (= 1 (mouse-button event))
                           (multiple-value-bind (left top width height)
                               (get-walker-coordinates instance)
                             (when (in-rectangle (mouse-x event) (mouse-y event)
                                                 left top width height)
                               (setf rel-pos (ifhorizontal instance
                                                           (- (mouse-x event) left)
                                                           (- (mouse-y event) top)))
                               (setf dragging t)
                               (capture-mouse instance))))))
    (add-event-handler instance :mouse-button-up :bubble
                       (lambda (instance event)
                         (when (= 1 (mouse-button event))
                           (setf dragging nil)
                           (release-mouse instance))))
    (add-event-handler instance :mouse-move :bubble
                       (lambda (instance event)
                         (when dragging
                           (let* ((screen-dimension (ifhorizontal instance
                                                                  (layout-width instance)
                                                                  (layout-height instance)))
                                  (screen-position
                                   (ifhorizontal instance
                                                 (- (mouse-x event) (layout-left instance))
                                                 (- (mouse-y event) (layout-top instance))))
                                  (extent (1+ (- (max-value instance)
                                                 (min-value instance))))
                                  (multiplier (/ extent screen-dimension))
                                  (new-current-min-pos (- (* multiplier screen-position)
                                                          (* multiplier rel-pos))))
                             (setf (current-min-position instance) new-current-min-pos)))))))

(defmethod (setf current-min-position) (new-value (slider slider))
  (when (> new-value (- (max-value slider) (page-size slider)))
    (setf new-value (- (max-value slider) (page-size slider))))
  (when (< new-value (min-value slider))
    (setf new-value (min-value slider)))
  (when (not (= new-value (current-min-position slider)))
    (setf (slot-value slider 'current-min-position) new-value)
    (invalidate slider)
    (on-event slider
              :position-changed
              (make-instance 'event)
              nil)))

;;; ARROW class.

(defclass arrow (widget)
  ((direction :accessor direction :initarg :direction :initform nil)))

(defmethod measure ((object arrow) available-width available-height)
  (set-measured object 16 16))

(defmethod layout ((object arrow) left top width height)
  (set-layout object left top 16 16))

(defmethod paint ((object arrow))
  (ecase (direction object)
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
     (cl-cairo2:fill-path))
    (:up
     (cl-cairo2:move-to (+ (layout-left object) 9) (+ (layout-top object) 5))
     (cl-cairo2:line-to (+ (layout-left object) 3) (+ (layout-top object) 11))
     (cl-cairo2:line-to (+ (layout-left object) 14) (+ (layout-top object) 11))
     (cl-cairo2:set-source-rgb 0 0 0)
     (cl-cairo2:fill-path))
    (:down
     (cl-cairo2:move-to (+ (layout-left object) 9) (+ (layout-top object) 11))
     (cl-cairo2:line-to (+ (layout-left object) 3) (+ (layout-top object) 5))
     (cl-cairo2:line-to (+ (layout-left object) 14) (+ (layout-top object) 5))
     (cl-cairo2:set-source-rgb 0 0 0)
     (cl-cairo2:fill-path))))

;;; SCROLLBAR class.

(defclass scrollbar (container1 orientable)
  ((min-value :accessor min-value :initarg :min-value :initform nil)
   (max-value :accessor max-value :initarg :max-value :initform nil)
   (page-size :accessor page-size :initarg :page-size :initform nil)
   (current-min-position :accessor current-min-position
                         :initarg :current-min-position :initform nil)
   (slider :accessor slider :initarg :slider :initform nil)))

(defmethod initialize-instance :after ((instance scrollbar) &rest initargs)
  (declare (ignore initargs))
  (let* ((slider (make-instance
                  'slider
                  :orientation (orientation instance)
                  :min-value (min-value instance)
                  :max-value (max-value instance)
                  :page-size (page-size instance)
                  :current-min-position
                  (current-min-position instance)))
         (btn-left (make-instance
                    'button
                    :child (make-instance 'arrow
                                          :direction (ifhorizontal instance :left :up))))
         (btn-right (make-instance
                     'button
                     :child (make-instance
                             'arrow
                             :direction (ifhorizontal instance :right :down))))
         (grid (ifhorizontal instance
                             (make-instance 'scenic-grid:grid
                                            :column-layout-options '(:auto (1 :ext) :auto)
                                            :row-layout-options '((19 :px))
                                            :children-descriptions `((:row (:cell ,btn-left)
                                                                           (:cell ,slider)
                                                                           (:cell ,btn-right))))
                             (make-instance
                              'scenic-grid:grid
                              :column-layout-options '((19 :px))
                              :row-layout-options '(:auto (1 :ext) :auto)
                              :children-descriptions `((:column (:cell ,btn-left)
                                                                (:cell ,slider)
                                                                (:cell ,btn-right)))))))
    (setf (child instance) grid)
    (setf (slider instance) slider)
    (add-event-handler slider :position-changed :bubble
                       (lambda (object event)
                                        ; Using slot-value to prevent
                                        ; passing down the value to
                                        ; the slider via setf (thus
                                        ; entering an infinite loop).
                         (setf (slot-value instance 'current-min-position)
                               (current-min-position object))
                         (on-event instance :position-changed event nil)))
    (add-event-handler btn-left :click :bubble
                       (lambda (object event)
                         (declare (ignore object event))
                         (setf (current-min-position instance)
                               (- (current-min-position slider)
                                  (page-size slider)))))
    (add-event-handler btn-right :click :bubble
                       (lambda (object event)
                         (declare (ignore object event))
                         (setf (current-min-position instance)
                               (+ (current-min-position slider)
                                  (page-size instance)))))))

(pass-to-child scrollbar slider min-value)
(pass-to-child scrollbar slider max-value)
(pass-to-child scrollbar slider page-size)
(pass-to-child scrollbar slider current-min-position)

