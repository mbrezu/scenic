
(in-package :scenic)

(defun max-box (boxes)
  (list (apply #'max (mapcar #'first boxes))
        (apply #'max (mapcar #'second boxes))))

(defmacro print-all (stream &rest exprs)
  (let ((format-string (with-output-to-string (str)
                         (dolist (expr exprs)
                           (format str "~a: " expr)
                           (format str "~~a")
                           (format str "~%")))))
    `(format ,stream ,format-string ,@exprs)))