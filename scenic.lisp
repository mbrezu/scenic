
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

(defun record-event (event-kind event-arg)
  (when *event-recording-enabled*
    (push (list* 'event event-kind (serialize event-arg)) *session-record*)))

(defun test-channel-write (data)
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

(defun emulate-event (scene event)
  (let ((event-kind (second event))
        (event-arg (apply #'make-instance (cddr event))))
    (ecase event-kind
      ((:mouse-motion-event)
       (scene-on-mouse-move scene event-arg)
       (test-render-scene scene))
      ((:mouse-button-down-event)
       (scene-on-mouse-button scene :mouse-button-down event-arg)
       (test-render-scene scene))
      ((:mouse-button-up-event)
       (scene-on-mouse-button scene :mouse-button-up event-arg)
       (test-render-scene scene))
      ((:key-down-event)
       (scene-on-key scene :key-down event-arg)
       (test-render-scene scene))
      ((:key-up-event)
       (scene-on-key scene :key-up event-arg)
       (test-render-scene scene)))))

(defun get-first-diff (actual expected &optional (line 1))
  (cond ((and (null actual) (null expected))
         nil)
        ((null actual) (list "Expected output has extra item." line (car expected)))
        ((null expected) (list "Actual outptu has extra item." line (car actual)))
        ((equal (car actual) (car expected))
         (get-first-diff (cdr actual) (cdr expected) (1+ line)))
        (t (list "Expected and actual items differ." line (car expected) (car actual)))))

(defun replay-scene-session (scene session-record)
  (labels ((run-compare (test-replies action)
             (reset-session-record)
             (funcall action)
             (when (not (equal (reverse *session-record*) test-replies))
               (return-from replay-scene-session
                 (values
                  nil
                  (get-first-diff (reverse *session-record*) test-replies))))
             (reset-session-record)))
    (let ((*test-channel-enabled* t)
          event
          test-replies)

      (setf (values test-replies session-record)
            (break-by session-record
                      (lambda (elem) (eq (car elem) 'test-channel))))

      (run-compare test-replies (lambda ()
                                  (calculate-focusables scene)
                                  (test-render-scene scene t)))

      (loop
         (unless session-record
           (return-from replay-scene-session (values t nil)))

         (setf event (car session-record))
         (unless (eq 'event (car event))
           (return-from replay-scene-session (values nil
                                                     "Expected event in session record.")))

         (setf session-record (cdr session-record))
         (setf (values test-replies session-record)
               (break-by session-record
                         (lambda (elem) (eq (car elem) 'test-channel))))
         (run-compare test-replies (lambda () (emulate-event scene event)))))))

(defun handle-events (scene event-queue)
  (when event-queue
    (dolist (event-arg (nreverse event-queue))
      (typecase event-arg
        (mouse-move-event
         (record-event :mouse-motion-event event-arg)
         (scene-on-mouse-move scene event-arg)
         (render-scene scene))
        (mouse-button-event
         (cond
           ((eq (button-state event-arg) :down)
            (record-event :mouse-button-down-event event-arg)
            (scene-on-mouse-button scene
                                   :mouse-button-down
                                   event-arg)
            (render-scene scene))
           ((eq (button-state event-arg) :up)
            (record-event :mouse-button-up-event event-arg)
            (scene-on-mouse-button scene
                                   :mouse-button-up
                                   event-arg)
            (render-scene scene))))
        (key-event
         (cond
           ((eq (key-state event-arg) :down)
            (if (and (unicode event-arg) (char= (unicode event-arg) #\Esc))
                (sdl:push-quit-event))
            (record-event :key-down-event event-arg)
            (scene-on-key scene
                          :key-down
                          event-arg)
            (render-scene scene))
           ((eq (key-state event-arg) :up)
            (record-event :key-up-event event-arg)
            (scene-on-key scene
                          :key-up
                          event-arg)
            (render-scene scene))))))))

(defun run-scene (scene)
  (sdl:with-init ()
    (sdl:window (width scene) (height scene))
    (sdl-cffi:sdl-enable-key-repeat 50 50)
    (setf (sdl:frame-rate) 100)
    (calculate-focusables scene)
    (reset-session-record)
    (render-scene scene t)
    (lispbuilder-sdl:enable-unicode)
    (let (event-queue)
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
               (handle-events scene event-queue)
               (setf event-queue nil))
        (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
                             (declare (ignore state))
                             (let ((event-arg (make-instance 'mouse-move-event
                                                             :mouse-x x
                                                             :mouse-y y
                                                             :mouse-rel-x x-rel
                                                             :mouse-rel-y y-rel
                                                             :modifiers (translated-mods))))
                               (push event-arg event-queue)))
        (:mouse-button-down-event (:button button :state state :x x :y y)
                                  (declare (ignore state))
                                  (let ((event-arg (make-instance
                                                    'mouse-button-event
                                                    :mouse-x x
                                                    :mouse-y y
                                                    :mouse-button button
                                                    :button-state :down
                                                    :modifiers (translated-mods))))
                                    (push event-arg event-queue)))
        (:mouse-button-up-event (:button button :state state :x x :y y)
                                (declare (ignore state))
                                (let ((event-arg
                                       (make-instance 'mouse-button-event
                                                      :mouse-x x
                                                      :mouse-y y
                                                      :button-state :up
                                                      :mouse-button button
                                                      :modifiers (translated-mods))))
                                  (push event-arg event-queue)))
        (:key-down-event (:state state :scancode scancode :key key
                                 :mod mod :mod-key mod-key :unicode unicode)
                         (declare (ignore state scancode mod))
                         (let ((event-arg (make-instance
                                           'key-event
                                           :key (sdl-translate-key key)
                                           :modifiers (mapcar #'sdl-translate-key mod-key)
                                           :key-state :down
                                           :unicode (if (= 0 unicode)
                                                        nil
                                                        (code-char unicode)))))
                           (push event-arg event-queue)))
        (:key-up-event (:state state :scancode scancode :key key
                               :mod mod :mod-key mod-key :unicode unicode)
                       (declare (ignore state scancode mod))
                       (let ((event-arg (make-instance
                                         'key-event
                                         :key (sdl-translate-key key)
                                         :modifiers (mapcar #'sdl-translate-key mod-key)
                                         :key-state :up
                                         :unicode (if (= 0 unicode)
                                                      nil
                                                      (code-char unicode)))))
                         (push event-arg event-queue)))
        (:video-expose-event () (sdl:update-display))))))

(defun sdl-translate-key (key)
  (intern (subseq (symbol-name key) 8) "KEYWORD"))
