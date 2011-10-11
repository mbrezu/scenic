
(in-package :scenic-utils)

(defun max-box (boxes)
  (list (apply #'max (mapcar #'first boxes))
        (apply #'max (mapcar #'second boxes))))

(defun draw-button-raw (left top width height pressed)
  ;; draw the inner borders (for the 3d illusion)
  ;; left border
  (cl-cairo2:move-to (+ 0.5 left)
                     (+ 0.5 top))
  (cl-cairo2:line-to (+ 0.5 left)
                     (+ top height))
  ;; upper border
  (cl-cairo2:move-to left
                     (+ 0.5 top))
  (cl-cairo2:line-to (+ left width)
                     (+ 0.5 top))
  ;; draw
  (cl-cairo2:set-line-width 1)
  (if pressed
      (cl-cairo2:set-source-rgb 0.3 0.3 0.3)
      (cl-cairo2:set-source-rgb 0.9 0.9 0.9))
  (cl-cairo2:stroke)
  ;; lower border
  (cl-cairo2:move-to (+ 1 left)
                     (- (+ top height) 0.5))
  (cl-cairo2:line-to (+ left width)
                     (- (+ top height) 0.5))
  ;; right border
  (cl-cairo2:move-to (- (+ left width) 0.5)
                     (+ 1 top))
  (cl-cairo2:line-to (- (+ left width) 0.5)
                     (- (+ top height) 1))
  ;; draw
  (cl-cairo2:set-line-width 1)
  (if pressed
      (cl-cairo2:set-source-rgb 0.9 0.9 0.9)
      (cl-cairo2:set-source-rgb 0.3 0.3 0.3))
  (cl-cairo2:stroke)
  ;; draw the background
  (cl-cairo2:rectangle (+ 1 left) (+ 1 top)
                       (- width 2) (- height 2))
  (cl-cairo2:set-source-rgb 0.8 0.8 0.8)
  (cl-cairo2:fill-path))

(defmacro pass-to-child (class child-slot property-slot)
  `(defmethod (setf ,property-slot) :after (value (instance ,class))
     (when (not (= (,property-slot (,child-slot instance)) (,property-slot instance)))
       (setf (,property-slot (,child-slot instance)) (,property-slot instance))
       (scenic:invalidate (,child-slot instance)))))

(defmacro ifhorizontal (instance horizontal-body &optional (vertical-body nil))
  `(if (eq (scenic:orientation ,instance) :horizontal)
       ,horizontal-body
       ,vertical-body))

(defun make-keyword (str)
  (intern (string-upcase str) "KEYWORD"))

(defmacro let-from-options (options variables &body body)
  (let ((goption (gensym "option")))
    `(let ,variables
       (loop
          for ,goption in ,options
            ,@(mapcan (lambda (var)
                        `(when (eq ,(make-keyword (symbol-name (first var))) (car ,goption))
                           do (setf ,(first var) (cdr ,goption))))
                      variables))
       ,@body)))

(defun fill-list (list desired-count element)
  (let ((add-count (- desired-count (length list))))
    (if (> add-count 0)
        (append list
                (loop
                   for i from 1 to add-count
                   collect element))
        list)))

(defun groups (list n)
  (cond ((null list) nil)
        ((< (length list) n)
         (list list))
        (t (cons (subseq list 0 n)
                 (groups (subseq list n) n)))))

(defmacro gen-print-object (class slots)
  (when (not (symbolp class))
    (error "Class should be a symbol!"))
  `(defmethod print-object ((object ,class) stream)
     (write-string (string-upcase (format nil
                                          ,(format nil "~a (~{~a~^, ~})"
                                                   (symbol-name class)
                                                   (mapcar (lambda (slot)
                                                             (format nil "~a:~~a" slot))
                                                           slots))
                                          ,@(mapcar (lambda (slot) `(slot-value object ',slot))
                                                    slots)))
                   stream)))

(defmacro gen-serializer (class slots)
  (when (not (symbolp class))
    (error "Class should be a symbol!"))
  `(defmethod scenic:serialize ((object ,class))
     (list ',class
           ,@(mapcan (lambda (slot) `(,(make-keyword slot) (slot-value object ',slot)))
                     slots))))

(defun yes-no-query (prompt)
  (loop
     (format t "~%~a [Y/N] " prompt)
     (let ((reply (read-char)))
       (when (or (char-equal reply #\y)
                 (char-equal reply #\n))
         (return (char-equal reply #\y)))
       (format t "~%Please type 'Y' or 'N'."))))

(defmacro -> (obj &rest forms)
  "Similar to the -> macro from clojure, but with a tweak: if there is
  a $ symbol somewhere in the form, the object is not added as the
  first argument to the form, but instead replaces the $ symbol."
  (if forms
      (if (consp (car forms))
          (let* ((first-form (first forms))
                 (other-forms (rest forms))
                 (pos (position '$ first-form)))
            (if pos
                `(-> ,(append (subseq first-form 0 pos)
                              (list obj)
                              (subseq first-form (1+ pos)))
                     ,@other-forms)
                `(-> ,(list* (first first-form) obj (rest first-form))
                     ,@other-forms)))
          `(-> ,(list (car forms) obj)
               ,@(cdr forms)))
      obj))

(defmacro set2val1 (var form)
  "The FORM returns at least two values. This macro sets the second
  value to the given VAR, and returns the first value."
  (let ((g-val (gensym "VAL")))
    `(let (,g-val)
       (setf (values ,g-val ,var) ,form)
       ,g-val)))

(defun validate-layout-spec (layout-spec)
  (cond ((null layout-spec) (values))
        ((eq (first layout-spec) :auto) (validate-layout-spec (cdr layout-spec)))
        ((consp (first layout-spec))
         (let ((arg (first (first layout-spec)))
               (kind (second (first layout-spec))))
           (if (and (numberp arg)
                    (or (eq :auto kind) (eq :px kind) (eq :ext kind)))
               (validate-layout-spec (cdr layout-spec))
               (error (format nil "Invalid layout option ~a." (first layout-spec))))))
        (t (error (format nil "Invalid layout option ~a." (first layout-spec))))))

(defun is-auto (layout-spec-cell)
  (or (eq :auto layout-spec-cell)
      (and (consp layout-spec-cell)
           (= 2 (length layout-spec-cell))
           (eq :auto (second layout-spec-cell)))))

(defun sorted-auto-indices (layout-spec)
  (labels ((adjust-layout-spec (layout-spec next-auto)
             (when layout-spec
               (if (is-auto (car layout-spec))
                   (if (eq :auto (car layout-spec))
                       (cons (list next-auto :auto)
                             (adjust-layout-spec (cdr layout-spec) (1+ next-auto)))
                       (cons (car layout-spec) (adjust-layout-spec (cdr layout-spec) next-auto)))
                   (cons (car layout-spec) (adjust-layout-spec (cdr layout-spec) next-auto))))))
    (let* ((autos (remove-if-not #'is-auto layout-spec)))
      (when autos
        (let ((max-option (1+ (apply #'max (mapcar (lambda (auto)
                                                  (if (eq :auto auto)
                                                      0
                                                      (first auto)))
                                                   autos)))))
          (-> (adjust-layout-spec layout-spec max-option)
              (loop
                 for lo in $
                 for pos = 0 then (1+ pos)
                 when (is-auto lo)
                 collect (list pos (first lo)))
              (sort $ #'< :key #'second)
              (mapcar #'first $)))))))

(defun intersperse (list elm)
  (cond ((>= (length list) 2)
         (list* (car list)
           elm
           (intersperse (cdr list) elm)))
        (t list)))