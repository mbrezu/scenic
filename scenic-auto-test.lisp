
(in-package :scenic-test)

(defstruct auto-test name scene-function scene-session-file description-file)

(defvar *tests*)

(setf *tests*
      (list (make-auto-test :name "Buttons"
                            :scene-function #'buttons
                            :scene-session-file "test-data/buttons.gz"
                            :description-file "test-data/buttons.txt")))

(defun find-test (name)
  (find name *tests* :test #'string-equal :key #'auto-test-name))

(defun store-test-information (test session-record)
  (scenic:write-gzipped-resource (auto-test-scene-session-file test)
                                 (format nil "~s" session-record)))

(defun record-auto-test-session (test-name)
  (let ((test (find-test test-name)))
    (unless test
      (error (format nil "Can't find test ~a!" test-name)))
    (format t "~a~%" (scenic:read-resource (auto-test-description-file test)))
    (let ((*manual-test-run* t))
      (let ((session-record (test-scene (funcall (auto-test-scene-function test))
                                        t)))
        (when (pass-fail-query)
          (store-test-information test session-record))))))

(defun run-auto-test (test)
  (let* ((*manual-test-run* nil))
    (let ((session-record
           (with-input-from-string
               (str (scenic:read-gzipped-resource (auto-test-scene-session-file test)))
             (read str))))
      (scenic:replay-scene-session
       (funcall (auto-test-scene-function test))
       session-record))))

(defun run-auto-tests ()
  (let ((total-tests 0)
        (failed-tests 0))
    (mapc (lambda (test)
            (format t "Running test '~a':" (auto-test-name test))
            (incf total-tests)
            (format t "~a~%" (if (run-auto-test test) "PASS" (progn
                                                               (incf failed-tests)
                                                               "FAIL"))))
          *tests*)
    (if (= 0 failed-tests)
      (format t "~a tests ran. ALL PASS!" total-tests)
      (format t "~a tests ran. ~a tests failed. SOME FAILED!" total-tests failed-tests))
    nil))

