
(in-package :scenic)

(declaim (optimize (debug 3)))

(defmacro pixels-of (surface)
  `(cffi:foreign-slot-value (slot-value ,surface 'sdl::foreign-pointer-to-object)
                            'sdl-cffi::sdl-Surface
                            'sdl-cffi::pixels))

(defmacro pitch-of (surface)
  `(cffi:foreign-slot-value (slot-value ,surface 'sdl::foreign-pointer-to-object)
                            'sdl-cffi::sdl-Surface
                            'sdl-cffi::pitch))

;; this macro could use unwind-protect :-)
(defmacro draw-with-cairo (sdl-surface &body body)
  (let ((g-sdl-surface (gensym "sdl-surface"))
        (g-cairo-surface (gensym "cairo-surface")))
    `(let* ((,g-sdl-surface ,sdl-surface)
            (,g-cairo-surface (cl-cairo2:create-image-surface-for-data
                               (pixels-of ,g-sdl-surface)
                               :argb32
                               (lispbuilder-sdl:width ,g-sdl-surface)
                               (lispbuilder-sdl:height ,g-sdl-surface)
                               (pitch-of ,g-sdl-surface)))
            (cl-cairo2:*context* (cl-cairo2:create-context ,g-cairo-surface)))
       (cl-cairo2:destroy ,g-cairo-surface)
       (progn ,@body)
       (cl-cairo2:destroy cl-cairo2:*context*)
       ,g-sdl-surface)))

(defun render-scene (scene &optional force-repaint-all)
  (unless (sdl-surface scene)
    (setf (sdl-surface scene)
          (sdl:create-surface (width scene) (height scene)
                              :mask '(#x00ff0000 #x0000ff00 #x000000ff 0))))
  (when (dirty scene)
    (setf (dirty scene) nil)
    (draw-with-cairo (sdl-surface scene)
      (measure-layout scene)
      (when force-repaint-all
        (invalidate-scene scene))
      (paint-scene scene))
    (sdl:set-point-* (sdl-surface scene) :x 0 :y 0)
    (if (rectangle-to-redraw scene)
        (let* ((scene-rect (mapcar (lambda (value)
                                     (the fixnum (round value)))
                                   (rectangle-to-redraw scene)))
               (left (first scene-rect))
               (top (second scene-rect))
               (width (1+ (- (third scene-rect) (first scene-rect))))
               (height (1+ (- (fourth scene-rect) (second scene-rect))))
               (sdl-rect (sdl:rectangle :x left :y top
                                        :w width :h height
                                        :fp nil)))
          (sdl:set-clip-rect sdl-rect :surface lispbuilder-sdl:*default-display*)
          (sdl:blit-surface (sdl-surface scene) )
          (sdl:free sdl-rect)
          (sdl:clear-clip-rect lispbuilder-sdl:*default-display*)
          (setf (rectangle-to-redraw scene) nil)
          (sdl-cffi::sdl-update-rect (sdl::fp lispbuilder-sdl:*default-display*)
                                     left top width height))
        (progn
          (sdl:blit-surface (sdl-surface scene) lispbuilder-sdl:*default-display*)
          (sdl:update-display)))))

(defun translated-mods ()
  (mapcar #'sdl-translate-key (sdl:mods-down-p)))

(defvar *event-recording-enabled* nil)
(defvar *test-channel-enabled* nil)
(defvar *session-record* nil)

(defun reset-session-record ()
  (when (or *event-recording-enabled* *test-channel-enabled*)
    (setf *session-record* nil)))

(defun record-events (event-queue)
  (when *event-recording-enabled*
    (push (list* 'events (mapcar #'serialize event-queue))
          *session-record*)))

(defun test-channel-write (data)
  (check-ui-thread)
  (when *test-channel-enabled*
    (push (cons 'test-channel data) *session-record*)))

(defun record-scene (scene)
  (let ((*event-recording-enabled* t)
        (*test-channel-enabled* t))
    (run-scene scene)
    (setf *session-record* (reverse *session-record*))))

(defun break-by (list pred &optional acc)
  (cond ((null list) (values (reverse acc) list))
        ((funcall pred (first list))
         (break-by (rest list) pred (push (first list) acc)))
        (t (values (reverse acc) list))))

(defun test-render-scene (scene &optional force-repaint-all)
  (when (dirty scene)
    (setf (dirty scene) nil)
    (let* ((cairo-surface (cl-cairo2:create-image-surface :argb32
                                                          (width scene) (height scene)))
           (cl-cairo2:*context* (cl-cairo2:create-context cairo-surface)))
      (cl-cairo2:destroy cairo-surface)
      (measure-layout scene)
      (when force-repaint-all
        (invalidate-scene scene))
      (paint-scene scene)
      (cl-cairo2:destroy cl-cairo2:*context*))))

(defun get-first-diff (actual expected &optional (line 1))
  (cond ((and (null actual) (null expected))
         nil)
        ((null actual) (list "Expected output has extra item." line (car expected)))
        ((null expected) (list "Actual output has extra item." line (car actual)))
        ((equal (car actual) (car expected))
         (get-first-diff (cdr actual) (cdr expected) (1+ line)))
        (t (list "Expected and actual items differ." line (car expected) (car actual)))))

(defun replay-scene-session (scene session-record)
  (labels ((run-compare (test-replies action)
             (reset-session-record)
             (funcall action)
             (when (not (equal (reverse *session-record*) test-replies))
               (return-from replay-scene-session
                 (values nil
                         (get-first-diff (reverse *session-record*) test-replies))))
             (reset-session-record)))
    (let ((*test-channel-enabled* t)
          (*event-recording-enabled* nil)
          event-queue
          test-replies)

      (setf (values test-replies session-record)
            (break-by session-record
                      (lambda (elem) (eq (car elem) 'test-channel))))

      (run-compare test-replies (lambda ()
                                  (init-scene scene #'test-render-scene)))

      (loop
         (unless session-record
           (return-from replay-scene-session (values t nil)))

         (unless (eq 'events (caar session-record))
           (return-from replay-scene-session (values nil
                                                     "Expected event in session record.")))
         (setf event-queue (mapcar (lambda (args)
                                     (apply #'make-instance args))
                                   (cdar session-record)))
         (setf session-record (cdr session-record))
         (setf (values test-replies session-record)
               (break-by session-record
                         (lambda (elem) (eq (car elem) 'test-channel))))
         (run-compare test-replies
                      (lambda () (handle-events scene event-queue #'test-render-scene)))))))

(defvar *replay-task-timeout-ms* 5000)

(defun handle-events (scene event-queue renderer)
  (when event-queue
    (record-events event-queue)
    (dolist (event-arg event-queue)
      (etypecase event-arg
        (mouse-move-event
         (scene-on-mouse-move scene event-arg)
         (funcall renderer scene))
        (mouse-button-event
         (cond
           ((eq (button-state event-arg) :down)
            (scene-on-mouse-button scene event-arg))
           ((eq (button-state event-arg) :up)
            (scene-on-mouse-button scene event-arg)))
         (funcall renderer scene))
        (key-event
         (cond
           ((eq (key-state event-arg) :down)
            (scene-on-key scene event-arg))
           ((eq (key-state event-arg) :up)
            (scene-on-key scene event-arg)))
         (funcall renderer scene))
        (task-executed-event
         (pick-task (task-number event-arg) *replay-task-timeout-ms*))))))

(defvar *task-queue-lock*)
(setf *task-queue-lock* (bt:make-recursive-lock))

(defvar *task-list*)
(setf *task-list* nil)

(defvar *task-counters-per-thread*)
(setf *task-counters-per-thread* nil)

(defvar *ui-thread*)

(defvar *thread-counter*)
(setf *thread-counter* 0)

(defvar *scenic-threads*)
(setf *scenic-threads* (make-hash-table))

(defun check-ui-thread ()
  (unless (on-ui-thread)
    (error "Not on UI thread")))

(defun on-ui-thread ()
  (eq (bt:current-thread) *ui-thread*))

(defun allocate-thread (code &optional name)
  (check-ui-thread)
  (bt:with-recursive-lock-held (*task-queue-lock*)
    (let ((new-thread (bt:make-thread code :name name)))
      (setf (gethash new-thread *scenic-threads*) (incf *thread-counter*))
      new-thread)))

(defclass task ()
  ((number :accessor task-number :initarg :number :initform nil)
   (name :accessor task-name :initarg :name :initform nil)
   (code :accessor task-code :initarg :code :initform nil)
   (time :accessor task-time :initarg :time :initform nil)))

(gen-print-object task (number name code time))

(defun pick-task (task-number timeout-ms)
  (labels ((get-task (tasks)
             (values (find-if (lambda (task) (string= (task-number task) task-number))
                              tasks)
                     (remove-if (lambda (task) (string= (task-number task) task-number)) tasks))))
    (let ((start-time (get-internal-real-time))
          (timeout-itu (* (/ timeout-ms 1000) internal-time-units-per-second))
          task)
      (loop
         (bt:with-recursive-lock-held (*task-queue-lock*)
           (setf (values task *task-list*) (get-task *task-list*)))
         (cond (task (funcall (task-code task))
                     (return))
               (t (sleep 0.05)
                  (when (> (get-internal-real-time)
                           (+ start-time timeout-itu))
                    (error "Timed out waiting for task to be put in queue."))))))))

(defun add-task (task-code &optional task-name (after-ms 0))
  (labels ((add-sorted (task task-list)
             (if (null task-list)
                 (list task)
                 (if (< (task-time task) (task-time (car task-list)))
                     (cons task task-list)
                     (cons (car task-list) (add-sorted task (cdr task-list)))))))
    (bt:with-recursive-lock-held (*task-queue-lock*)
      (let ((thread-id (gethash (bt:current-thread) *scenic-threads*)))
        (unless thread-id
          (error
           "Cannot add a task from a thread that wasn't created using scenic:allocate-thread."))
        (when (null (gethash (bt:current-thread) *task-counters-per-thread*))
          (setf (gethash (bt:current-thread) *task-counters-per-thread*) 0))
        (let ((task (make-instance 'task
                                   :number (format nil "~a-~a"
                                                   thread-id
                                                   (incf (gethash (bt:current-thread)
                                                                  *task-counters-per-thread*)))
                                   :name task-name
                                   :code task-code
                                   :time (+ (get-internal-real-time)
                                            (* (/ after-ms 1000)
                                               internal-time-units-per-second)))))
          (setf *task-list* (add-sorted task *task-list*)))))))

(defmacro as-ui-task (&body body)
  `(add-task (lambda ()
               ,@body)))

(defmacro as-delayed-ui-task (delay-ms &body body)
  `(add-task (lambda ()
               ,@body) "" ,delay-ms))

(defun get-tasks-to-execute ()
  (labels ((split-time (tasks time &optional acc)
             (cond ((null tasks)
                    (values tasks (reverse acc)))
                   ((< (task-time (car tasks)) time)
                    (split-time (cdr tasks) time (cons (car tasks) acc)))
                   (t (values tasks (reverse acc))))))
    (bt:with-recursive-lock-held (*task-queue-lock*)
      (let ((time (get-internal-real-time))
            tasks)
        (setf (values *task-list* tasks) (split-time *task-list* time))
        tasks))))

(defun reset-tasks ()
  (setf *task-list* nil)
  (setf *task-counters-per-thread* (make-hash-table))
  (setf *thread-counter* 0)
  (setf *ui-thread* (bt:current-thread)))

(defun reset-threads ()
  (setf *scenic-threads* (make-hash-table))
  (setf (gethash (bt:current-thread) *scenic-threads*)
        0))

(defun init-scene (scene renderer)
  (calculate-focusables scene)
  (reset-session-record)
  (reset-tasks)
  (reset-threads)
  (funcall renderer scene t)
  (when (on-scene-init scene)
    (funcall (on-scene-init scene))))

(defun run-scene (scene)
  (sdl:with-init ()
    (sdl:window (width scene) (height scene))
    (sdl-cffi:sdl-enable-key-repeat 80 80)
    (setf (sdl:frame-rate) 100)
    (init-scene scene #'render-scene)
    (lispbuilder-sdl:enable-unicode)
    (let (event-queue)
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
               (handle-events scene (nreverse event-queue) #'render-scene)
               (setf event-queue nil)
               (dolist (task (get-tasks-to-execute))
                 (record-events (list (make-instance 'task-executed-event
                                                     :task-number (task-number task))))
                 (funcall (task-code task))))
        (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
                             (declare (ignore state))
                             (push (make-instance 'mouse-move-event
                                                  :mouse-x x
                                                  :mouse-y y
                                                  :mouse-rel-x x-rel
                                                  :mouse-rel-y y-rel
                                                  :modifiers (translated-mods))
                                   event-queue))
        (:mouse-button-down-event (:button button :state state :x x :y y)
                                  (declare (ignore state))
                                  (push (make-instance
                                         'mouse-button-event
                                         :mouse-x x
                                         :mouse-y y
                                         :mouse-button button
                                         :button-state :down
                                         :modifiers (translated-mods))
                                        event-queue))
        (:mouse-button-up-event (:button button :state state :x x :y y)
                                (declare (ignore state))
                                (push (make-instance 'mouse-button-event
                                                     :mouse-x x
                                                     :mouse-y y
                                                     :button-state :up
                                                     :mouse-button button
                                                     :modifiers (translated-mods))
                                      event-queue))
        (:key-down-event (:state state :scancode scancode :key key
                                 :mod mod :mod-key mod-key :unicode unicode)
                         (declare (ignore state scancode mod))
                         (push (make-instance
                                'key-event
                                :key (sdl-translate-key key)
                                :modifiers (mapcar #'sdl-translate-key mod-key)
                                :key-state :down
                                :unicode (if (= 0 unicode)
                                             nil
                                             (code-char unicode)))
                               event-queue))
        (:key-up-event (:state state :scancode scancode :key key
                               :mod mod :mod-key mod-key :unicode unicode)
                       (declare (ignore state scancode mod))
                       (push (make-instance
                              'key-event
                              :key (sdl-translate-key key)
                              :modifiers (mapcar #'sdl-translate-key mod-key)
                              :key-state :up
                              :unicode (if (= 0 unicode)
                                           nil
                                           (code-char unicode)))
                             event-queue))
        (:video-expose-event () (sdl:update-display))))))

(defun sdl-translate-key (key)
  (intern (subseq (symbol-name key) 8) "KEYWORD"))
