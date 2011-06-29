
(in-package :scenic-utils)

(defun max-box (boxes)
  (list (apply #'max (mapcar #'first boxes))
        (apply #'max (mapcar #'second boxes))))

(defmacro print-all (stream &rest exprs)
  (let ((format-string (with-output-to-string (str)
                         (dolist (expr exprs)
                           (cond ((stringp expr)
                                  (format str "~~a"))
                                 (t
                                  (format str "~a: " expr)
                                  (format str "~~a")
                                  (format str "~%")))))))
    `(format ,stream ,format-string ,@exprs)))

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

(defmacro aif (test then &optional (else nil))
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(defmacro bif ((var test) then &optional (else nil))
  `(let ((,var ,test))
     (if ,var ,then ,else)))

(defmacro bwhen ((var test) &body body)
  `(let ((,var ,test))
     (when ,var ,@body)))

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
                                          ,@(mapcar (lambda (slot) `(,slot object))
                                                    slots)))
                   stream)))

(defmacro gen-serializer (class slots)
  (when (not (symbolp class))
    (error "Class should be a symbol!"))
  `(defmethod scenic:serialize ((object ,class))
     (list ',class
           ,@(mapcan (lambda (slot) `(,(make-keyword slot) (,slot object)))
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
  (if forms
      (if (consp (car forms))
          `(-> ,(list* (caar forms) obj (cdar forms))
               ,@(cdr forms))
          `(-> ,(list (car forms) obj)
               ,@(cdr forms)))
      obj))