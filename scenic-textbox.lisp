
(in-package :scenic)

(declaim (optimize (debug 3) (speed 0)))

;;; TEXTBOX class.

(defclass textbox (clickable textattr focusable)
  ((cursor-position :accessor cursor-position :initarg :cursor-position :initform 0)
   (selection-start :accessor selection-start :initarg :selection-start :initform 0)
   (caret-color :accessor caret-color :initarg :caret-color :initform (list 0.0 0.0 0.0))
   (background-color :accessor background-color
                     :initarg :background-color
                     :initform (list 0.0 0.0 0.0))
   (selection-color :accessor selection-color
                    :initarg :selection-color
                    :initform (list 0.3 0.3 1.0))
   (text :accessor text :initarg :text :initform "")
   (scroll-view)
   (selection-bg)
   (before-paint :initform nil)))

(defmethod (setf text) :after (new-value (object textbox))
  (on-event object :text-changed (make-instance 'event) nil))

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

(defun string-width (textbox str)
  (if *text-info-auto-test*
      (let ((text-info (-> textbox font-size get-test-text-info)))
        (* (length str) (test-text-info-width text-info)))
      (let ((spaces-count (- (length str) (length (string-trim '(#\Space #\Tab) str))))
            (adjustment 0))
        (when (> spaces-count 0)
          (setf adjustment (* spaces-count (space-width))))
        (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
            (cl-cairo2:text-extents str)
          (declare (ignore x-bearing y-bearing x-advance y-advance height))
          (+ width adjustment)))))

(defun unselect-if-no-shift (textbox event)
  (when (not (shifted (modifiers event)))
    (setf (selection-start textbox)
          (cursor-position textbox))))

(defun selected-text (textbox)
  (with-slots (selection-start cursor-position) textbox
    (if (= selection-start cursor-position)
        nil
        (let ((start-sel (min selection-start cursor-position))
              (end-sel (max selection-start cursor-position)))
          (subseq (text textbox) start-sel end-sel)))))

(defun delete-selection (textbox)
  (when (selected-text textbox)
    (with-slots (selection-start cursor-position text) textbox
      (let ((start-sel (min selection-start cursor-position))
            (end-sel (max selection-start cursor-position)))
        (setf text (concatenate 'string
                                (subseq text 0 start-sel)
                                (subseq text end-sel)))
        (setf selection-start start-sel)
        (setf cursor-position start-sel)))))

(defun handle-delete (instance event lbl)
  (cond
    ((selected-text instance)
     (delete-selection instance)
     (setf (text lbl) (text instance))
     (invalidate instance)
     (setf (handled event) t))
    ((< (cursor-position instance)
        (length (text instance)))
     (setf (text instance)
           (concatenate 'string
                        (subseq (text instance) 0 (cursor-position instance))
                        (subseq (text instance) (1+ (cursor-position instance)))))
     (setf (text lbl) (text instance))
     (invalidate instance)
     (setf (handled event) t))))

(defun handle-backspace (instance event lbl)
  (cond
    ((selected-text instance)
     (delete-selection instance)
     (setf (text lbl) (text instance))
     (invalidate instance)
     (setf (handled event) t))
    ((> (cursor-position instance) 0)
     (setf (text instance)
           (concatenate 'string
                        (subseq (text instance) 0 (1- (cursor-position instance)))
                        (subseq (text instance) (cursor-position instance))))
     (setf (text lbl) (text instance))
     (decf (cursor-position instance))
     (setf (selection-start instance) (cursor-position instance))
     (invalidate instance)
     (setf (handled event) t))))

(defun handle-insert-char (instance event lbl)
  (when (selected-text instance)
    (delete-selection instance))
  (setf (text instance)
        (concatenate 'string
                     (subseq (text instance) 0 (cursor-position instance))
                     (string (unicode event))
                     (subseq (text instance) (cursor-position instance))))
  (setf (text lbl) (text instance))
  (incf (cursor-position instance))
  (setf (selection-start instance) (cursor-position instance))
  (invalidate instance)
  (setf (handled event) t))

(defun make-textbox-key-down-handler (lbl)
  (lambda (instance event)
    (cond ((and (eq :LEFT (key event))
                (> (cursor-position instance) 0))
           (decf (cursor-position instance))
           (unselect-if-no-shift instance event)
           (invalidate instance)
           (setf (handled event) t))
          ((and (eq :RIGHT (key event))
                (< (cursor-position instance)
                   (length (text instance))))
           (incf (cursor-position instance))
           (unselect-if-no-shift instance event)
           (invalidate instance)
           (setf (handled event) t))
          ((eq :HOME (key event))
           (setf (cursor-position instance) 0)
           (unselect-if-no-shift instance event)
           (invalidate instance)
           (setf (handled event) t))
          ((eq :END (key event))
           (setf (cursor-position instance) (length (text instance)))
           (unselect-if-no-shift instance event)
           (invalidate instance)
           (setf (handled event) t))
          ((eq :DELETE (key event))
           (handle-delete instance event lbl))
          ((eq :BACKSPACE (key event))
           (handle-backspace instance event lbl))
          ((or (eq :RETURN (key event))
               (eq :TAB (key event))
               (eq :ESCAPE (key event))))
          ((unicode event)
           (handle-insert-char instance event lbl))
          (t ;; (print-all t
           ;;            (key event)
           ;;            (modifiers event))
           ))))

(defmethod initialize-instance :after ((instance textbox) &rest initargs)
  (declare (ignore initargs))
  (let* ((bg (make-instance 'background :fill-color (background-color instance)
                            :child (make-instance 'filler)))
         (sel-bg (make-instance 'background
                                :child (make-instance 'placeholder
                                                      :width 0
                                                      :height 0)))
         (lbl (make-instance 'label :text (text instance)))
         (stk1 (make-instance 'stack :children (list sel-bg lbl)))
         (sv (make-instance 'scroll-view :child stk1))
         (stk2 (make-instance 'stack :children (list bg sv))))
    (setf (slot-value instance 'scroll-view) sv)
    (setf (slot-value instance 'selection-bg) sel-bg)
    (setf (child instance) stk2)
    (copy-textattr instance lbl)
    (add-event-handler instance :key-down :cascade
                       (make-textbox-key-down-handler lbl))
    (add-event-handler instance :click nil
                       (lambda (o e)
                         (setf (slot-value o 'before-paint)
                               (lambda ()
                                 (setf (cursor-position o)
                                       (or (char-hit-test o (+ (horizontal-offset sv)
                                                               (- (mouse-x e)
                                                                  (layout-left o))))
                                           (cursor-position o)))
                                 (unselect-if-no-shift o e)))
                         (focus-widget (get-scene o) o)
                         (invalidate o)))))

(defun pos-on-char (textbox x-pos pos)
  (let ((lt (< (string-width textbox (subseq (text textbox) 0 pos))
               x-pos)))
    (or (and (= pos (length (text textbox)))
             lt)
        (and lt
             (> (string-width textbox (subseq (text textbox) 0 (1+ pos)))
                x-pos)))))

(defun char-hit-test (textbox x-pos)
  (let ((txtlen (length (text textbox))))
    (if (= 0 txtlen)
        0
        (let* ((avg-char-width (/ (string-width textbox (text textbox)) txtlen))
               (pos (round (/ x-pos avg-char-width))))
          (if (and (> pos 0)
                   (< pos txtlen)
                   (pos-on-char textbox x-pos pos))
              pos
              (loop
                 for pos1 = pos then (1+ pos1)
                 for pos2 = pos then (1- pos2)
                 when (and (< pos1 txtlen)
                           (pos-on-char textbox x-pos pos1))
                 return pos1
                 when (and (> pos2 0)
                           (< pos2 txtlen)
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
    (let ((str-width (string-width object
                                   (subseq (text object) 0 (cursor-position object)))))
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
  (with-slots (selection-start cursor-position text selection-bg) object
    (let ((start-sel (min selection-start cursor-position))
          (end-sel (max selection-start cursor-position)))
      (when selection-bg
        (cond ((not (= start-sel end-sel))
               (let ((start-sel-x (string-width object (subseq (text object) 0 start-sel)))
                     (end-sel-x (string-width object (subseq (text object) 0 end-sel))))
                 (set-layout selection-bg
                             (+ (layout-left object) start-sel-x)
                             (layout-top object)
                             (- end-sel-x start-sel-x)
                             (layout-height object))))
              (t (set-layout selection-bg
                             (layout-left object)
                             (layout-top object)
                             0
                             0)))
        (setf (fill-color selection-bg)
              (if (has-focus object)
                  (list 0.8 0.8 1.0)
                  (list 0.8 0.8 0.8))))))
  (call-next-method))

(defmethod after-paint ((object textbox))
  ;; draw caret
  (when (has-focus object)
    (apply #'cl-cairo2:set-source-rgb (caret-color object))
    (cl-cairo2:set-line-width 1.0)
    (prepare-text object)
    (let* ((rel-caret-x-position (string-width object (subseq (text object)
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
                         (+ (layout-top object) (layout-height object)))
      (cl-cairo2:stroke))))

