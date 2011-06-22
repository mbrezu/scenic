
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

;;; BOX class.

;; The BOX will arrange its contents horizontally or vertically,
;; according to a 'layout spec' and the sizing preferences for each
;; child.
;;
;; For horizontal boxes, all children have the same height (the
;; minimum between maximum height of the children and the maximum size
;; specified for the box) and the term 'size/space' below refers to width.
;;
;; Vertical boxes will behave similarly for width, with 'size/space'
;; meaning height.
;;
;; The 'layout spec' is a list of layout options, one layout option
;; specified for each child.
;;
;; The layout option for a child can be:
;;
;; * :auto - the child will take as much space as it requires;
;; * '(n :px) - n is the size in pixels for the child;
;; * '(n :ext) - the child will fill the space proportionally;
;;
;; If the option is '(n ext), n is used to determine the child's share
;; in the remaining space (if all exts have n 1, they will receive an
;; equal share of the remaining space in the widget; if there is an
;; ext with n 2 and the others have n 1, the one with n 2 will receive
;; a double allowance of space).
;;
;; The algorithm for calculating the sizes is as follows:
;;
;; 1. All '(n px) layout options are summed and their sum is
;; subtracted from the available space for the box.
;;
;; 2. All children with :auto layout options are measured (the space
;; they are offered is what remains in the box after step 1) and their
;; sizes are summed. This sum is subtracted to determine the space
;; available for exts.
;;
;; 3. The '(n ext) layouts are summed, remaining space is divided by
;; the sum to get the 'slice size'. Each ext widget gets a number of
;; 'slices' corresponding to its n ext multiplier.
;;
;; If the layout options list is not specified when measuring, or
;; there are fewer layout options than child controls, the layout
;; options are filled with '(1 ext) items.

(defclass box (container orientable)
  ((space-between-cells :accessor space-between-cells :initarg :space-between-cells :initform 0)
   (layout-options :accessor layout-options :initarg :layout-options :initform nil)
   (slice-size :accessor slice-size :initarg :slice-size :initform nil)))

(declaim (optimize (debug 3)))

(defun fill-in-layout-options (box)
  (setf (layout-options box)
        (fill-list (layout-options box) (length (children box)) '(1 :ext))))

(defun f. (f1 f2)
  (lambda (x)
    (funcall f1 (funcall f2 x))))

(defmethod measure ((object box) available-width available-height)
  (fill-in-layout-options object)
  (let* ((total-spacing (* (1- (length (children object)))
                           (space-between-cells object)))
         (space-left (- (ifhorizontal object available-width available-height)
                        total-spacing))
         (sum-n-ext 0)
         (allocated-space 0))
    ;; Measure pass 1 - measure the pxs and autos while we have space.
    (loop
       for lo in (layout-options object)
       for child in (children object)
       do (progn
            (when (or (eq :auto lo)
                      (and (consp lo) (eq :px (second lo))))
              (let (space-increment)
                (cond ((eq :auto lo)
                       (measure child
                                (ifhorizontal object space-left available-width)
                                (ifhorizontal object available-height space-left))
                       (setf space-increment (ifhorizontal object
                                                           (measured-width child)
                                                           (measured-height child))))
                      ((and (consp lo) (eq :px (second lo)))
                       (measure child
                                (ifhorizontal object (first lo) available-width)
                                (ifhorizontal object available-height (first lo)))
                       (setf space-increment (first lo))))
                (decf space-left space-increment)
                (when (< space-left 0)
                  (setf space-left 0))
                (incf allocated-space space-increment)))
            (when (and (consp lo) (eq :ext (second lo)))
              (incf sum-n-ext (first lo)))))

    (if (> sum-n-ext 0)
        (setf (slice-size object) (truncate (/ space-left sum-n-ext)))
        (setf (slice-size object) 0))
    ;; Measure pass 2 - calculate the slice in the remaining space and
    ;; measure the exts.
    (loop
       for lo in (layout-options object)
       for child in (children object)
       do (when (and (consp lo)
                     (eq :ext (second lo)))
            (measure child
                     (ifhorizontal object
                                   (* (first lo) (slice-size object))
                                   available-width)
                     (ifhorizontal object
                                   available-height
                                   (* (first lo) (slice-size object))))
            (incf allocated-space (* (first lo) (slice-size object)))))
    (ifhorizontal object
                  (set-measured object
                                (+ total-spacing allocated-space)
                                (apply #'max (mapcar #'measured-height
                                                     (children object))))
                  (set-measured object
                                (apply #'max (mapcar #'measured-width
                                                     (children object)))
                                (+ total-spacing allocated-space)))))

(defmethod layout ((object box) left top width height)
  (let ((running (ifhorizontal object left top)))
    (loop
       for lo in (layout-options object)
       for child in (children object)
       do (progn
            (cond ((eq :auto lo)
                   (ifhorizontal object
                                 (layout child
                                         running top
                                         (measured-width child) height)
                                 (layout child
                                         left running
                                         width (measured-height child)))
                   (incf running (ifhorizontal object
                                               (+ (measured-width child)
                                                  (space-between-cells object))
                                               (+ (measured-height child)
                                                  (space-between-cells object)))))
                  ((and (consp lo) (eq :px (second lo)))
                   (ifhorizontal object
                                 (layout child running top (first lo) height)
                                 (layout child left running width (first lo)))
                   (incf running (+ (first lo) (space-between-cells object))))
                  ((and (consp lo) (eq :ext (second lo)))
                   (ifhorizontal object
                                 (layout child
                                         running top
                                         (* (first lo) (slice-size object)) height)
                                 (layout child
                                         left running
                                         width (* (first lo) (slice-size object))))
                   (incf running (+ (* (first lo) (slice-size object))
                                    (space-between-cells object))))))))
  (call-next-method object left top width height))

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
    (paint-order-walk (child object) callback :after-callback after-callback))
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

