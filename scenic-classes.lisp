
(in-package :scenic)

(declaim (optimize (debug 3) (speed 0) safety))

(defgeneric measure (object available-width available-height))

(defgeneric layout (object left top width height))

(defgeneric paint (object))

(defgeneric after-paint (object))

(defgeneric paint-order-walk (object callback &key after-callback))

(defgeneric in-widget (x y widget))

(defgeneric add-event-handler (object event propagation callback))

(defgeneric on-event (object event event-args propagation))

(defgeneric intersect (object1 object2))

(defgeneric clips-content (object))

;;; EVENTFUL class.

(defclass eventful ()
  ((event-handlers :accessor event-handlers :initarg event-handlers :initform nil)))

(defmethod add-event-handler ((object eventful) event propagation handler)
  (let ((handler-list (assoc event (event-handlers object))))
    (if handler-list
        (push (cons handler propagation) (cdr handler-list))
        (let ((fresh-handler-list (list (cons handler propagation))))
          (push (cons event fresh-handler-list) (event-handlers object))))))

(defmethod on-event ((object eventful) event event-arg propagation)
  (dolist (handler (cdr (assoc event (event-handlers object))))
    (when (or (null propagation) (eq (cdr handler) propagation))
      (funcall (car handler) object event-arg)
      (when (handled event-arg)
        (return-from on-event)))))

;;; WIDGET class.

(defclass widget (eventful)
  ((measured-width :accessor measured-width :initarg measured-width :initform nil)
   (measured-height :accessor measured-height :initarg measured-height :initform nil)
   (layout-left :accessor layout-left :initarg :layout-left :initform 0)
   (layout-top :accessor layout-top :initarg :layout-top :initform 0)
   (layout-width :accessor layout-width :initarg :layout-width :initform nil)
   (layout-height :accessor layout-height :initarg :layout-height :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (name :accessor name :initarg :name :initform nil)
   (auto-name :accessor auto-name :initarg :auto-name :initform nil)
   (paint-order-number :accessor paint-order-number
                       :initarg :paint-order-number
                       :initform -1)
   (affected-rect :accessor affected-rect
                  :initarg :affected-rect
                  :initform nil)
   (visible :accessor visible :initarg :visible :initform t)))

(defmethod measure :around ((object widget) available-width available-height)
  (if (visible object)
      (call-next-method)
      (set-measured object 0 0)))

(defmethod (setf visible) :around (new-value (object widget))
  (let ((old-value (visible object)))
    (call-next-method)
    (unless (eq (not old-value) (not new-value))
      (let ((scene (get-scene object)))
        (setf (layedout scene) nil)
        (invalidate-scene scene)))))

(defun make-widget-auto-name (widget count)
  (format nil "~a-~a" (class-name (class-of widget)) count))

(defmethod initialize-instance :after ((object widget) &rest initargs)
  (declare (ignore initargs))
  (setf (auto-name object) (make-widget-auto-name object 1)))

;; (defmethod on-event :before ((object widget) event event-args propagation)
;;   (test-channel-write (list (full-name object) event (serialize event-args))))

(defun get-widget-chain (widget-chain)
  (if (null widget-chain)
      nil
      (let ((parent-of-first (parent (first widget-chain))))
        (if (and parent-of-first (not (eql (type-of parent-of-first) 'scene)))
            (get-widget-chain (cons parent-of-first
                                    widget-chain))
            widget-chain))))

(defun full-name (widget)
  (format nil "~{~a~^/~}"
          (mapcar (lambda (widget) (or (name widget) (auto-name widget)))
                  (get-widget-chain (list widget)))))

(defmethod print-object ((object widget) stream)
  (write-string (string-upcase (format nil
                                       "~a (~a,~a,~a,~a)"
                                       (type-of object)
                                       (layout-left object)
                                       (layout-top object)
                                       (layout-width object)
                                       (layout-height object)))
                stream))

(defun in-rectangle (x y left top width height)
  (and (<= left x)
       (< x (+ left width))
       (<= top y)
       (< y (+ top height))))

(defmethod in-widget (x y (widget widget))
  (in-rectangle x y
                (layout-left widget) (layout-top widget)
                (layout-width widget) (layout-height widget)))

(defun set-measured (widget available-width available-height)
  (setf (measured-width widget) available-width)
  (setf (measured-height widget) available-height)
  (values available-width available-height))

(defmethod measure ((object widget) available-width available-height)
  (set-measured object available-width available-height))

(defun set-layout (widget left top width height)
  (setf (layout-left widget) left)
  (setf (layout-top widget) top)
  (setf (layout-width widget) width)
  (setf (layout-height widget) height)
  (values))

(defmethod layout ((object widget) left top width height)
  (set-layout object left top width height))

(defmethod paint ((object widget)))

(defmethod paint :before ((object widget))
  (test-channel-write (list (full-name object) :paint (format nil "~a" object))))

(defmethod after-paint ((object widget)))

(defmethod paint-order-walk ((object widget) callback &key (after-callback nil))
  (funcall callback object)
  (when after-callback
    (funcall after-callback object)))

(defmethod clips-content ((object widget))
  nil)

;;; PLACEHOLDER class.

(defclass placeholder (widget)
  ((width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)))

(defmethod measure ((object placeholder) available-width available-height)
  (set-measured object
                (min (width object) available-width)
                (min (height object) available-height)))

;;; FILLER class.

(defclass filler (widget)
  ())

;;; TEXTATTR class.
(defclass textattr ()
  ((font-face :accessor font-face :initarg :font-face :initform "Arial")
   (font-size :accessor font-size :initarg :font-size :initform 12)
   (font-color :accessor font-color :initarg :font-color :initform (list 0.0 0.0 0.0))
   (font-slant :accessor font-slant :initarg :font-slant :initform :normal)
   (font-weight :accessor font-weight :initarg :font-weight :initform :normal)))

(defmacro copyslots (source dest &rest slots)
  `(progn
     ,@(loop
          for slot in slots
          collect `(setf (,slot ,dest) (,slot ,source)))))

(defun copy-textattr (source dest)
  (copyslots source dest font-face font-size font-color font-slant font-weight))

;;; LABEL class.

(defstruct test-text-info width ascent height)

(defvar *test-text-info*)

(setf *test-text-info*
      (list (cons 10 (make-test-text-info :width 6.0d0 :ascent 9.0d0 :height 12.0d0))
            (cons 12 (make-test-text-info :width 7.0d0 :ascent 10.0d0 :height 14.0d0))
            (cons 14 (make-test-text-info :width 8.0d0 :ascent 12.0d0 :height 16.0d0))
            (cons 16 (make-test-text-info :width 9.9d0 :ascent 14.0d0 :height 19.0d0))
            (cons 18 (make-test-text-info :width 11.0d0 :ascent 15.0d0 :height 20.0d0))
            (cons 20 (make-test-text-info :width 12.0d0 :ascent 17.0d0 :height 23.0d0))
            (cons 22 (make-test-text-info :width 13.0d0 :ascent 19.0d0 :height 26.0d0))
            (cons 24 (make-test-text-info :width 14.0d0 :ascent 20.0d0 :height 27.0d0))
            (cons 26 (make-test-text-info :width 15.8d0 :ascent 22.0d0 :height 30.0d0))
            (cons 28 (make-test-text-info :width 16.8d0 :ascent 23.0d0 :height 31.0d0))
            (cons 30 (make-test-text-info :width 17.8d0 :ascent 25.0d0 :height 34.0d0))))

(defun get-test-text-info (size)
  (or (cdr (assoc size *test-text-info*)) (error "Unknown size")))

(defvar *text-info-auto-test*)

(setf *text-info-auto-test* nil)

(defclass label (widget textattr)
  ((text :accessor text :initarg :text :initform "")))

(defmethod measure ((object label) available-width available-height)
  (if *text-info-auto-test*
      (let ((text-info (get-test-text-info (font-size object))))
        (set-measured object
                      (min (* (length (text object)) (test-text-info-width text-info))
                           available-width)
                      (min (test-text-info-height text-info) available-height)))
      (progn
        (cl-cairo2:set-font-size (font-size object))
        (cl-cairo2:select-font-face (font-face object)
                                    (font-slant object)
                                    (font-weight object))
        (multiple-value-bind
              (x_bearing y_bearing width height x_advance y_advance)
            (cl-cairo2:text-extents (text object))
          (declare (ignore x_bearing y_bearing x_advance y_advance height))
          (let* ((extents (cl-cairo2:get-font-extents))
                 (ascent (cl-cairo2:font-ascent extents))
                 (descent (cl-cairo2:font-descent extents)))
            (set-measured object
                          (min width available-width)
                          (min (+ ascent descent) available-height)))))))

(defmethod layout ((object label) left top width height)
  (set-layout object left top (measured-width object) (measured-height object)))

(defun prepare-text (textattr)
  (cl-cairo2:set-font-size (font-size textattr))
  (cl-cairo2:select-font-face (font-face textattr)
                              (font-slant textattr)
                              (font-weight textattr)))

(defmethod paint ((object label))
  (prepare-text object)
  (apply #'cl-cairo2:set-source-rgb (font-color object))
  (if *text-info-auto-test*
      (let* ((text-info (get-test-text-info (font-size object))))
        (cl-cairo2:move-to (layout-left object)
                           (- (+ (layout-top object) (test-text-info-ascent text-info))
                              0.5)))
      (let* ((extents (cl-cairo2:get-font-extents))
             (ascent (cl-cairo2:font-ascent extents)))
        (cl-cairo2:move-to (layout-left object) (- (+ (layout-top object) ascent) 0.5))))
  (cl-cairo2:show-text (text object)))

;;; ORIENTABLE class.

;;; This class has only one slot, orientation, which can be either
;;; :horizontal or :vertical.

(defclass orientable ()
  ((orientation :accessor orientation :initarg :orientation :initform nil)))

;;; IMAGE class.

(defclass image (widget)
  ((image-surface :accessor image-surface :initarg :image-surface :initform nil)))

(defmethod paint ((instance image))
  (cl-cairo2:set-source-surface (image-surface instance)
                                (layout-left instance) (layout-top instance))
  (cl-cairo2:rectangle (layout-left instance) (layout-top instance)
                       (layout-width instance) (layout-height instance))
  (cl-cairo2:clip)
  (cl-cairo2:paint)
  (cl-cairo2:reset-clip))

;;; FOCUSABLE class.

(defclass focusable (eventful)
  ((has-focus :accessor has-focus :initarg :has-focus :initform nil)))

(defmethod (setf has-focus) :after (value (instance focusable))
  (let ((widget-chain (get-widget-chain (list instance)))
        (event-arg (make-instance 'event)))
    (if value
        (cascade-then-bubble widget-chain :got-focus event-arg)
        (cascade-then-bubble widget-chain :lost-focus event-arg))))

(defgeneric focusable (instance))

(defmethod focusable ((instance focusable))
  t)

(defmethod focusable ((instance t))
  nil)

