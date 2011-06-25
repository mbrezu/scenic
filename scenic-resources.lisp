
(in-package :scenic)

(defun resource (name)
  (let ((basename (asdf:component-pathname (asdf:find-system '#:scenic))))
    (namestring (merge-pathnames name basename))))

(defvar *images* (make-hash-table :test 'equal))

(declaim (optimize (debug 3)))

(defun get-image (image-path)
  (or (gethash image-path *images*)
      (setf (gethash image-path *images*)
            (handler-case
                (cl-cairo2:image-surface-create-from-png (resource image-path))
              (simple-warning (err)
                (error err))))))

(defun read-stream-to-string (stream)
  (with-output-to-string (str)
    (loop
       for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       do (format str "~a~%" line))))

(defun read-resource (file-name)
  (with-open-file (file (scenic:resource file-name) :element-type '(mod 256))
    (setf file (flexi-streams:make-flexi-stream file :external-format :utf-8))
    (read-stream-to-string file)))

(defun write-gzipped-resource (resource-name content)
  (gzip-stream:with-open-gzip-file (file resource-name
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
    (setf file (flexi-streams:make-flexi-stream file :external-format :utf-8))
    (write-string content file)
    nil))

(defun read-gzipped-resource (resource-name)
  (gzip-stream:with-open-gzip-file (file resource-name :direction :input)
    (setf file (flexi-streams:make-flexi-stream file :external-format :utf-8))
    (read-stream-to-string file)))
