
(in-package :scenic-grid)

;;; GRID class.

(declaim (optimize (debug 3)))

(defclass grid (container orientable)
  ((column-layout-options :accessor column-layout-options
                          :initarg :column-layout-options
                          :initform nil)
   (column-widths :accessor column-widths
                  :initarg :column-widths
                  :initform nil)
   (row-layout-options :accessor row-layout-options
                       :initarg :row-layout-options
                       :initform nil)
   (row-heights :accessor row-heights
                :initarg :row-heights
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

(defmethod (setf children-descriptions) :after (value (instance grid))
  (apply-children-descriptions instance value 0 0))

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
     do (multiple-value-bind (colspan rowspan)
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
              while (keywordp (first child))
              do
                (push (cons (first child)
                            (second child))
                      options)
                (setf child (cddr child)))
           (push (car child) (children grid))
           (push options (children-options grid))
           (push (list column row) (children-locations grid))
           (let ((colspan 1)
                 (rowspan 1))
             (set-from-options options colspan rowspan)
             (values colspan rowspan))))
        (t (error (format nil "Invalid cell description ~a." child)))))

(defun calculate-widths-heights (object available-width available-height)
  (let (column-count
        row-count
        (current-available-width available-width)
        (current-available-height available-height)
        (column-ext-sum 0)
        (row-ext-sum 0)
        auto-cells)

    ;; Calculate the dimensions of the grid.
    (setf (values column-count row-count) (get-dimensions object))

    ;; Fill lists of layout options so we don't run into trouble later.
    (fill-in-grid-layout-options object column-count row-count)

    ;; Determine available horizontal and vertical space (subtract px
    ;; sizes from available sizes).
    (decf current-available-width
          (sum-layout-option (column-layout-options object) :px))
    (decf current-available-height
          (sum-layout-option (row-layout-options object) :px))

    ;; Determine the cells that have auto as layout option either
    ;; vertically or horizontally and have the relevant span 1.
    (setf auto-cells (determine-auto-cells object column-count row-count))
    ;; Measure them (using the grid dimensions - sum of px sizes) to
    ;; determine the column/row sizes for auto; if an auto dimension
    ;; can't be determined, it is changed to (1 :ext).
    (multiple-value-bind (auto-column-widths auto-row-heights)
        (measure-autos auto-cells
                       column-count row-count
                       current-available-width current-available-height
                       (column-layout-options object) (row-layout-options object))
      ;; Allocate the space for the autos in the column
      ;; widths. Allocate 0 width if all the space has already been
      ;; allocated.
      (setf (column-widths object) (make-array column-count :initial-element 0))
      (setf current-available-width (allocate-auto auto-column-widths
                                                   (column-widths object)
                                                   current-available-width))

      ;; Same operation as above for rows.
      (setf (row-heights object) (make-array row-count :initial-element 0))
      (setf current-available-height (allocate-auto auto-row-heights
                                                    (row-heights object)
                                                    current-available-height)))

    ;; Determine the sum of ext parameters for columns and rows.
    (setf column-ext-sum (sum-layout-option (column-layout-options object) :ext))
    (setf row-ext-sum (sum-layout-option (row-layout-options object) :ext))

    (if (> column-ext-sum 0)
        (setf (column-slice-size object)
              (truncate (/ current-available-width column-ext-sum)))
        0)
    (if (> row-ext-sum 0)
        (setf (row-slice-size object)
              (truncate (/ current-available-height row-ext-sum)))
        0)

    ;; Determine the space for px and ext columns.
    (allocate-px-ext (column-layout-options object)
                     (column-widths object)
                     (column-slice-size object))

    ;; Determine the space for px and ext rows.
    (allocate-px-ext (row-layout-options object)
                     (row-heights object)
                     (row-slice-size object))

    ;; Measure the rest of the widgets.
    (loop
       for location in (children-locations object)
       for child in (children object)
       for options in (children-options object)
       ;; If it's in auto-cells, it's already measured.
       unless (aref auto-cells (second location) (first location))
       do (let ((colspan 1)
                (rowspan 1))
            (set-from-options options colspan rowspan)
            (measure child
                     (get-size (column-widths object) (first location) colspan)
                     (get-size (row-heights object) (second location) rowspan))))))

(defun get-size (size-array start length)
  (loop
     for i from start to (1- (+ start length))
     sum (aref size-array i)))

(defun sum-layout-option (layout-options kind)
  (loop
     for lo in layout-options
     when (and (consp lo) (eq kind (second lo)))
     sum (first lo)))

(defun allocate-auto (auto-size-array size-array current-available-space)
  (dotimes (idx (length size-array))
    (let ((space (min current-available-space (aref auto-size-array idx))))
      (setf (aref size-array idx) space)
      (decf current-available-space space)))
  current-available-space)

(defun allocate-px-ext (layout-options size-array slice-size)
  (loop
     for lo in layout-options
     for idx = 0 then (1+ idx)
     when (and (consp lo) (eq :ext (second lo)))
     do (setf (aref size-array idx) (* (first lo) slice-size))
     when (and (consp lo) (eq :px (second lo)))
     do (setf (aref size-array idx)
              (first lo))))

(defun fill-in-grid-layout-options (object column-count row-count)
  (setf (column-layout-options object)
        (fill-list (column-layout-options object) column-count '(1 :ext)))
  (setf (row-layout-options object)
        (fill-list (row-layout-options object) row-count '(1 :ext))))

(defun measure-autos (auto-cells
                      column-count row-count
                      current-available-width current-available-height
                      column-layout-options row-layout-options)
  (let ((column-widths (make-array column-count :initial-element 0))
        (row-heights (make-array row-count :initial-element 0)))
    (dotimes (column column-count)
      (dotimes (row row-count)
        (aif (aref auto-cells row column)
             (multiple-value-bind (width height)
                 (measure it current-available-width current-available-height)
               (when (eq :auto (elt column-layout-options column))
                 (setf (aref column-widths column)
                       (max (aref column-widths column) width)))
               (when (eq :auto (elt row-layout-options row))
                 (setf (aref row-heights row)
                       (max (aref row-heights row) height)))))))
    (values column-widths row-heights)))

(defun determine-auto-cells (object column-count row-count)
  (let ((auto-cells (make-array (list row-count column-count)
                                :initial-element nil)))
    (loop
       for lo in (column-layout-options object)
       for column-index = 0 then (1+ column-index)
       when (eq :auto lo)
       do (loop
             for row-index from 0 to row-count
             when (aif (get-child-at object column-index row-index)
                       (colspan-1 (second it)))
             do
               (setf (aref auto-cells row-index column-index)
                     (first (get-child-at object column-index row-index)))))
    (loop
       for lo in (row-layout-options object)
       for row-index = 0 then (1+ row-index)
       when (eq :auto lo)
       do (loop
             for column-index from 0 to column-count
             when (aif (get-child-at object column-index row-index)
                       (rowspan-1 (second it)))
             do (setf (aref auto-cells row-index column-index)
                      (first (get-child-at object column-index row-index)))))
    auto-cells))

(defun colspan-1 (options)
  (let ((colspan 1))
    (set-from-options options colspan)
    (= 1 colspan)))

(defun rowspan-1 (options)
  (let ((rowspan 1))
    (set-from-options options rowspan)
    (= 1 rowspan)))

(defun get-dimensions (grid)
  (let ((column-count 0)
        (row-count 0))
    (loop
       for location in (children-locations grid)
       for options in (children-options grid)
       do (let ((colspan 1)
                (rowspan 1))
            (set-from-options options colspan rowspan)
            (setf column-count (max column-count (+ (first location) colspan)))
            (setf row-count (max row-count (+ (second location) rowspan)))))
    (values column-count row-count)))

(defun get-child-at (object column row)
  (loop
     for child in (children object)
     for option in (children-options object)
     for location in (children-locations object)
     when (and (= column (first location))
               (= row (second location)))
     return (list child option)))

(defun get-offsets (size-array)
  (let ((result (make-array (length size-array) :initial-element 0))
        (running-total 0))
    (dotimes (idx (length size-array))
      (setf (aref result idx) running-total)
      (incf running-total (aref size-array idx)))
    result))

(defmethod layout ((object grid) left top width height)
  (calculate-widths-heights object width height)
  (let ((column-left (get-offsets (column-widths object)))
        (row-top (get-offsets (row-heights object))))
    (loop
       for child in (children object)
       for location in (children-locations object)
       for options in (children-options object)
       do (let ((colspan 1)
                (rowspan 1))
            (set-from-options options colspan rowspan)
            (layout child
                    (aref column-left (first location))
                    (aref row-top (second location))
                    (get-size (column-widths object) (first location) colspan)
                    (get-size (row-heights object) (second location) rowspan)))))
  (call-next-method object left top width height))
