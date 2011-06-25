
(in-package :scenic-test)

(declaim (optimize debug safety))

(defstruct auto-test name scene-function scene-session-file description-file)

(defvar *tests*)

(setf *tests*
      (list (make-auto-test :name "background-clear"
                            :scene-function #'background-clear
                            :scene-session-file "test-data/background-clear.gz"
                            :description-file "test-data/background-clear.txt")
            (make-auto-test :name "colored-rectangles"
                            :scene-function #'colored-rectangles
                            :scene-session-file "test-data/colored-rectangles.gz"
                            :description-file "test-data/colored-rectangles.txt")
            (make-auto-test :name "hello-world"
                            :scene-function #'hello-world
                            :scene-session-file "test-data/hello-world.gz"
                            :description-file "test-data/hello-world.txt")
            (make-auto-test :name "buttons"
                            :scene-function #'buttons
                            :scene-session-file "test-data/buttons.gz"
                            :description-file "test-data/buttons.txt")
            (make-auto-test :name "slider"
                            :scene-function #'slider
                            :scene-session-file "test-data/slider.gz"
                            :description-file "test-data/slider.txt")
            (make-auto-test :name "scrollbars"
                            :scene-function #'scrollbars
                            :scene-session-file "test-data/scrollbars.gz"
                            :description-file "test-data/scrollbars.txt")
            (make-auto-test :name "icon"
                            :scene-function #'icon
                            :scene-session-file "test-data/icon.gz"
                            :description-file "test-data/icon.txt")
            (make-auto-test :name "text-baseline-alignment"
                            :scene-function #'text-baseline-alignment
                            :scene-session-file "test-data/text-baseline-alignment.gz"
                            :description-file "test-data/text-baseline-alignment.txt")
            (make-auto-test :name "vertical-box-layout-options"
                            :scene-function #'vertical-box-layout-options
                            :scene-session-file "test-data/vertical-box-layout-options.gz"
                            :description-file "test-data/vertical-box-layout-options.txt")
            (make-auto-test :name "horizontal-box-layout-options"
                            :scene-function #'horizontal-box-layout-options
                            :scene-session-file "test-data/horizontal-box-layout-options.gz"
                            :description-file "test-data/horizontal-box-layout-options.txt")
            (make-auto-test :name "grid-basic"
                            :scene-function #'grid-basic
                            :scene-session-file "test-data/grid-basic.gz"
                            :description-file "test-data/grid-basic.txt")
            (make-auto-test :name "grid-offset"
                            :scene-function #'grid-offset
                            :scene-session-file "test-data/grid-offset.gz"
                            :description-file "test-data/grid-offset.txt")
            (make-auto-test :name "grid-spans"
                            :scene-function #'grid-spans
                            :scene-session-file "test-data/grid-spans.gz"
                            :description-file "test-data/grid-spans.txt")
            (make-auto-test :name "grid-layout-options"
                            :scene-function #'grid-layout-options
                            :scene-session-file "test-data/grid-layout-options.gz"
                            :description-file "test-data/grid-layout-options.txt")
            (make-auto-test :name "grid-layout-options-2"
                            :scene-function #'grid-layout-options-2
                            :scene-session-file "test-data/grid-layout-options-2.gz"
                            :description-file "test-data/grid-layout-options.txt")
            (make-auto-test :name "grid-layout-options-3"
                            :scene-function #'grid-layout-options-3
                            :scene-session-file "test-data/grid-layout-options-3.gz"
                            :description-file "test-data/grid-layout-options.txt")))

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
            (format t "Running test ~40a" (format nil "'~a':" (auto-test-name test)))
            (incf total-tests)
            (format t "~a~%" (if (run-auto-test test) "PASS" (progn
                                                               (incf failed-tests)
                                                               "FAIL"))))
          *tests*)
    (terpri)
    (if (= 0 failed-tests)
      (format t "~a tests ran. ALL PASS!" total-tests)
      (format t "~a tests ran. ~a tests failed. SOME FAILED!" total-tests failed-tests))
    nil))

