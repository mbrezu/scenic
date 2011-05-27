
(in-package :scenic)

;;; GRID class.

(declaim (optimize (debug 3)))

(defclass grid (container orientable)
  ((column-layout-options :accessor column-layout-options
                          :initarg :column-layout-options
                          :initform nil)
   (row-layout-options :accessor row-layout-options
                       :initarg :row-layout-options
                       :initform nil)
   (children-locations :accessor children-locations
                       :initarg :children-locations
                       :initform nil)
   (children-options :accessor children-options
                     :initarg :children-options
                     :initform nil)
   (children-descriptions :accessor children-descriptions
                          :initarg :children-descriptions
                          :initform nil)

   (column-slice-size :accessor column-slice-size
                      :initarg :column-slice-size
                      :initform nil)
   (row-slice-size :accessor row-slice-size
                   :initarg :row-slice-size
                   :initform nil)))

(defmethod initialize-instance :after ((instance grid) &rest initargs)
  (declare (ignore initargs))
  (apply-children-descriptions instance (children-descriptions instance) 0 0))

(defun apply-children-descriptions (instance descriptions column-offset row-offset)
  (let ((column-counter 0)
        (row-counter 0))
    (mapc (lambda (description)
            (cond ((eq :column (first description))
                   (add-column instance (rest description)
                               column-counter column-offset row-offset)
                   (incf column-counter))
                  ((eq :row (first description))
                   (add-row instance (rest description)
                            row-counter column-offset row-offset)
                   (incf row-counter))
                  ((eq :offset (first description))
                   (apply-children-descriptions instance
                                                (nthcdr 3 description)
                                                (+ column-offset (second description))
                                                (+ row-offset (third description))))))
          descriptions)))

(defun add-column (grid children column column-offset row-offset)
  (loop
     for child in children
     with row = 0
     do (multiple-value-bind (colspan rowspan)
            (add-cell grid
                      (+ column column-offset)
                      (+ row row-offset)
                      child)
          (declare (ignore colspan))
          (incf row rowspan))))

(defun add-row (grid children row column-offset row-offset)
  (loop
     for child in children
     with column = 0
     do
       (multiple-value-bind (colspan rowspan)
           (add-cell grid
                     (+ column column-offset)
                     (+ row row-offset)
                     child)
         (declare (ignore rowspan))
         (incf column colspan))))

(defun add-cell (grid column row child)
  (cond ((and (consp child) (eq :cell (first child)))
         (let (options)
           (setf child (rest child))
           (loop
              when (keywordp (first child))
              do
                (push (cons (first child)
                            (second child))
                      options)
                (setf child (cddr child))
              until (not (keywordp (first child))))
           (push (car child) (children grid))
           (push options (children-options grid))
           (push (list column row) (children-locations grid))
           (let ((colspan 1)
                 (rowspan 1))
             (set-from-options options colspan rowspan)
             (values colspan rowspan))))
        (t (error (format nil "Invalid cell description ~a." child)))))

(defmethod measure ((object grid) available-width available-height)
  (let ((column-count (get-column-count object))
        (row-count (get-row-count object)))
    (if (> column-count 0)
        (setf (column-slice-size object)
              (truncate (/ available-width column-count)))
        0)
    (if (> row-count 0)
        (setf (row-slice-size object)
              (truncate (/ available-width row-count)))
        0)
    (dotimes (column column-count)
      (dotimes (row row-count)
        (aif (get-child-at object column row)
             (let ((colspan 1)
                   (rowspan 1))
               (set-from-options (second it) colspan rowspan)
               (measure (first it)
                        (* colspan (column-slice-size object))
                        (* rowspan (row-slice-size object))))))))
  (call-next-method object available-width available-height))

(defun get-column-count (grid)
  (1+
   (apply #'max (mapcar #'first (children-locations grid)))))

(defun get-row-count (grid)
  (1+
   (apply #'max (mapcar #'second (children-locations grid)))))

(defun get-child-at (object column row)
  (let (result)
    (loop
       for child in (children object)
       for option in (children-options object)
       for location in (children-locations object)
       when (and (= column (first location))
                 (= row (second location)))
       do (setf result (list child option))
       until result)
    result))

(defmethod layout ((object grid) left top width height)
  (let ((column-count (get-column-count object))
        (row-count (get-row-count object)))
    (dotimes (column column-count)
      (dotimes (row row-count)
        (aif (get-child-at object column row)
             (let ((colspan 1)
                   (rowspan 1))
               (set-from-options (second it) colspan rowspan)
               (layout (first it)
                       (* column (column-slice-size object))
                       (* row (row-slice-size object))
                       (* colspan (column-slice-size object))
                       (* rowspan (row-slice-size object))))))))
  (call-next-method object left top width height))
