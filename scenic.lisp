
(in-package :scenic)

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

(defun font-extents ()
  (let ((extents (cl-cairo2:get-font-extents)))
    `((ascent . ,(cl-cairo2:font-ascent extents))
      (descent . ,(cl-cairo2:font-descent extents)))))

(defun text-extents (text)
  (mapcar #'(lambda (name dim)
              (cons name dim))
          (multiple-value-list (cl-cairo2:text-extents text))
          '(x_bearing y_bearing width height x_advance y_advance)))

;; (defclass button (widget)
;;   ((text :accessor text :initarg :text :initform "")
;;    (event-click :accessor event-click :initarg :event-click :initform nil)))

(defun render-scene (scene)
  (let ((from-cairo nil))
    (setf from-cairo
          (draw-with-cairo (sdl:create-surface (width scene) (height scene))
            (measure scene (width scene) (height scene))
            (layout scene 0 0 (width scene) (height scene))
            (paint scene)))
    (sdl:blit-surface from-cairo)
    (sdl:free from-cairo)
    (sdl:update-display)))

(defun run-scene (scene)
  (sdl:with-init ()
    (sdl:window (width scene) (height scene))
    (setf (sdl:frame-rate) 60)
    (render-scene scene)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (when (sdl:key= key :sdl-key-escape)
                         (sdl:push-quit-event)))
      ;; (:idle (render-scene scene))
      (:video-expose-event () (sdl:update-display)))))

