
(in-package :scenic)

(declaim (optimize (debug 3) (speed 0)))

(defgeneric measure (object available-width available-height))

(defgeneric layout (object left top width height))

(defgeneric paint (object))

(defgeneric after-paint (object))

(defgeneric paint-order-walk (object callback &key after-callback))

(defgeneric in-widget (x y widget))

(defgeneric add-event-handler (object event propagation callback))

(defgeneric on-event (object event event-args propagation))

(defgeneric intersect (object1 object2))

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
   (paint-order-number :accessor paint-order-number :initarg :paint-order-number :initform -1)))

(defmethod print-object ((object widget) stream)
  (format stream
          "~a (~a,~a,~a,~a)"
          (type-of object)
          (layout-left object)
          (layout-top object)
          (layout-width object)
          (layout-height object)))

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

(defmethod after-paint ((object widget)))

(defmethod paint-order-walk ((object widget) callback &key (after-callback nil))
  (funcall callback object)
  (when after-callback
    (funcall after-callback object)))

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

(defclass label (widget textattr)
  ((text :accessor text :initarg :text :initform "")))

(defmethod measure ((object label) available-width available-height)
  (cl-cairo2:set-font-size (font-size object))
  (cl-cairo2:select-font-face (font-face object)
                              (font-slant object)
                              (font-weight object))
  (multiple-value-bind
        (x_bearing y_bearing width height x_advance y_advance) (cl-cairo2:text-extents
                                                                (text object))
    (declare (ignore x_bearing y_bearing x_advance y_advance height))
    (let* ((extents (cl-cairo2:get-font-extents))
           (ascent (cl-cairo2:font-ascent extents)))
      (set-measured object
                    (min width available-width)
                    (min ascent available-height)))))

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
  (let* ((extents (cl-cairo2:get-font-extents))
         (ascent (cl-cairo2:font-ascent extents))
         (descent (cl-cairo2:font-descent extents)))
    (cl-cairo2:move-to (layout-left object) (- (+ (layout-top object) ascent 0.5) descent))
    (cl-cairo2:show-text (text object))))

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
  (if value
      (on-event instance :got-focus (make-instance 'event) nil)
      (on-event instance :lost-focus (make-instance 'event) nil)))

(defgeneric focusable (instance))

(defmethod focusable ((instance focusable))
  t)

(defmethod focusable ((instance t))
  nil)

;;; TEXTBOX class.

(defclass textbox (clickable textattr focusable)
  ((cursor-position :accessor cursor-position :initarg :cursor-position :initform 0)
   (selection-start :accessor selection-start :initarg :selection-start :initform nil)
   (caret-color :accessor caret-color :initarg :caret-color :initform (list 0.0 0.0 0.0))
   (background-color :accessor background-color
                     :initarg :background-color
                     :initform (list 0.0 0.0 0.0))
   (selection-color :accessor selection-color
                    :initarg :selection-color
                    :initform (list 0.3 0.3 1.0))
   (text :accessor text :initarg :text :initform "")
   (scroll-view)
   (before-paint :initform nil)))

(defun space-width ()
  (let (big-str-width small-str-width)
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cl-cairo2:text-extents "| |")
      (declare (ignore x-bearing y-bearing x-advance y-advance height))
      (setf big-str-width width))
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cl-cairo2:text-extents "||")
      (declare (ignore x-bearing y-bearing x-advance y-advance height))
      (setf small-str-width width))
    (- big-str-width small-str-width)))

(defun string-width (str)
  (let ((spaces-count (- (length str) (length (string-trim '(#\Space #\Tab) str))))
        (adjustment 0))
    (when (> spaces-count 0)
      (setf adjustment (* spaces-count (space-width))))
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cl-cairo2:text-extents str)
      (declare (ignore x-bearing y-bearing x-advance y-advance height))
      (+ width adjustment))))

(defun make-textbox-key-down-handler (lbl)
  (lambda (instance event)
    (cond ((and (eq :LEFT (key event))
                (> (cursor-position instance) 0))
           (decf (cursor-position instance))
           (invalidate instance)
           (setf (handled event) t))
          ((and (eq :RIGHT (key event))
                (< (cursor-position instance)
                   (length (text instance))))
           (incf (cursor-position instance))
           (invalidate instance)
           (setf (handled event) t))
          ((eq :HOME (key event))
           (setf (cursor-position instance) 0)
           (invalidate instance)
           (setf (handled event) t))
          ((eq :END (key event))
           (setf (cursor-position instance) (length (text instance)))
           (invalidate instance)
           (setf (handled event) t))
          ((eq :DELETE (key event))
           (when (< (cursor-position instance)
                    (length (text instance)))
             (setf (text instance)
                   (concatenate 'string
                                (subseq (text instance) 0 (cursor-position instance))
                                (subseq (text instance) (1+ (cursor-position instance)))))
             (setf (text lbl) (text instance))
             (invalidate instance)
             (setf (handled event) t)))
          ((eq :BACKSPACE (key event))
           (when (> (cursor-position instance) 0)
             (setf (text instance)
                   (concatenate 'string
                                (subseq (text instance) 0 (1- (cursor-position instance)))
                                (subseq (text instance) (cursor-position instance))))
             (setf (text lbl) (text instance))
             (decf (cursor-position instance))
             (invalidate instance)
             (setf (handled event) t)))
          ((or (eq :RETURN (key event))
               (eq :TAB (key event))))
          ((unicode event)
           (setf (text instance)
                 (concatenate 'string
                              (subseq (text instance) 0 (cursor-position instance))
                              (string (unicode event))
                              (subseq (text instance) (cursor-position instance))))
           (setf (text lbl) (text instance))
           (incf (cursor-position instance))
           (invalidate instance)
           (setf (handled event) t))
          (t (print-all t
                        (key event)
                        (modifiers event))))))

(defmethod initialize-instance :after ((instance textbox) &rest initargs)
  (declare (ignore initargs))
  (let* ((bg (make-instance 'background :fill-color (background-color instance)
                            :child (make-instance 'filler)))
         (lbl (make-instance 'label :text (text instance)))
         (sv (make-instance 'scroll-view :child lbl))
         (stk (make-instance 'stack :children (list bg sv))))
    (setf (slot-value instance 'scroll-view) sv)
    (setf (child instance) stk)
    (copy-textattr instance lbl)
    (add-event-handler instance :key-down :cascade
                       (make-textbox-key-down-handler lbl))
    (add-event-handler instance :click nil
                       (lambda (o e)
                         (print-all t e)
                         ;; FIXME: this layout-left needs to be fixed
                         ;; to handle correctly the case when we're
                         ;; inside a scroll-viewer. Maybe we need a
                         ;; function 'screen-coords' that returns the
                         ;; (left, top) pair after compensating for
                         ;; scroll-viewers (right now we compensate
                         ;; for the textbox internal scroll-viewers,
                         ;; the problem is with scroll-viewers
                         ;; containing the textbox)?
                         (setf (slot-value o 'before-paint)
                               (lambda ()
                                 (setf (cursor-position o)
                                       (or (char-hit-test o (+ (horizontal-offset sv)
                                                               (- (mouse-x e)
                                                                  (layout-left o))))
                                           (cursor-position o)))))
                         (focus-widget (get-scene o) o)
                         (invalidate o)))))

(defun pos-on-char (textbox x-pos pos)
  (let ((lt (< (string-width (subseq (text textbox) 0 pos))
               x-pos)))
    (or (and (= pos (length (text textbox)))
             lt)
        (and lt
             (> (string-width (subseq (text textbox) 0 (1+ pos)))
                x-pos)))))

(defun char-hit-test (textbox x-pos)
  (let ((txtlen (length (text textbox))))
    (if (= 0 txtlen)
        0
        (let* ((avg-char-width (/ (string-width (text textbox)) txtlen))
               (pos (round (/ x-pos avg-char-width))))
          (if (pos-on-char textbox x-pos pos)
              pos
              (loop
                 for pos1 = (1+ pos) then (1+ pos1)
                 for pos2 = (1- pos) then (1- pos2)
                 when (and (< pos1 txtlen)
                           (pos-on-char textbox x-pos pos1))
                 return pos1
                 when (and (> pos2 0)
                           (pos-on-char textbox x-pos pos2))
                 return pos2
                 until (and (> pos1 txtlen)
                            (< pos2 0))))))))

(defmethod paint ((object textbox))
  (prepare-text object)
  (when (slot-value object 'before-paint)
    (funcall (slot-value object 'before-paint))
    (setf (slot-value object 'before-paint) nil))
  (with-slots (scroll-view) object
    (let ((str-width (string-width (subseq (text object) 0 (cursor-position object)))))
      (when (> (- str-width (horizontal-offset scroll-view))
               (layout-width object))
        (setf (horizontal-offset scroll-view)
              (- str-width (/ (layout-width object) 2))))
      (when (< str-width (horizontal-offset scroll-view))
        (setf (horizontal-offset scroll-view)
              (- str-width (/ (layout-width object) 2)))
        (when (< (horizontal-offset scroll-view) 0)
          (setf (horizontal-offset scroll-view) 0)))))
  ;; draw selection
  (print-all t "draw-selection")
  (call-next-method))

(defmethod after-paint ((object textbox))
  ;; draw caret
  (when (has-focus object)
    (apply #'cl-cairo2:set-source-rgb (caret-color object))
    (cl-cairo2:set-line-width 1.0)
    (prepare-text object)
    (let* ((rel-caret-x-position (string-width (subseq (text object)
                                                       0
                                                       (cursor-position object))))
           (abs-caret-x-position (- (+ (layout-left object) rel-caret-x-position)
                                    (horizontal-offset (slot-value object 'scroll-view)))))
      (if (= (horizontal-offset (slot-value object 'scroll-view))
             rel-caret-x-position)
          (incf abs-caret-x-position 0.5)
          (decf abs-caret-x-position 0.5))
      (cl-cairo2:move-to abs-caret-x-position (layout-top object))
      (cl-cairo2:line-to abs-caret-x-position
                         (1- (+ (layout-top object) (layout-height object))))
      (cl-cairo2:stroke))))

