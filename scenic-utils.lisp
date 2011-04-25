
(in-package :scenic)

(defun max-box (boxes)
  (list (apply #'max (mapcar #'first boxes))
        (apply #'max (mapcar #'second boxes))))

