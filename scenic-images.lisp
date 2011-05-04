
(in-package :scenic)

(defvar *images* (make-hash-table :test 'equal))

(declaim (optimize (debug 3)))

(defun image-transformer (stream subchar arg)
  (declare (ignore subchar arg))
  (get-image (read stream)))

(defun get-image (image-path)
  (or (gethash image-path *images*)
      (setf (gethash image-path *images*)
            (handler-case
                (cl-cairo2:image-surface-create-from-png image-path)
              (simple-warning (err)
                (error err))))))

(set-dispatch-macro-character #\# #\i #'image-transformer)