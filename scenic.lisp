
(in-package :scenic)

(defparameter *width* 800)
(defparameter *height* 600)

(defun pixels-of (surface)
  (cffi:foreign-slot-value (slot-value surface 'sdl::foreign-pointer-to-object)
                           'sdl-cffi::Sdl-Surface
                           'sdl-cffi::pixels))

(defun pitch-of (surface)
  (cffi:foreign-slot-value (slot-value surface 'sdl::foreign-pointer-to-object)
                           'sdl-cffi::Sdl-Surface
                           'sdl-cffi::pitch))

(defun font-extents ()
  (let ((extents (cl-cairo2:get-font-extents)))
    `((ascent . ,(cl-cairo2:font-ascent extents))
      (descent . ,(cl-cairo2:font-descent extents)))))

(defun text-extents (text)
  (mapcar #'(lambda (name dim)
              (cons name dim))
          (multiple-value-list (cl-cairo2:text-extents text))
          '(x_bearing y_bearing width height x_advance y_advance)))

(defun cairo-test ()
  (let* ((sdl-surf (sdl:create-surface 800 600))
         (surf (cl-cairo2:create-image-surface-for-data (pixels-of sdl-surf)
                                                        :argb32
                                                        800
                                                        600
                                                        (pitch-of sdl-surf)))
         (cl-cairo2:*context* (cl-cairo2:create-context surf)))
    (cl-cairo2:destroy surf)
    (cl-cairo2:set-source-rgb 0.2 0.2 1)
    (cl-cairo2:paint)
    (cl-cairo2:move-to 10 10.5)
    (cl-cairo2:line-to 100 10.5)
    (cl-cairo2:set-source-rgb 1 0.3 0.3)
    (cl-cairo2:set-line-width 1)
    (cl-cairo2:stroke)
    
    (cl-cairo2:set-font-size 40)
    (cl-cairo2:set-source-rgb 0 0 0)
    (cl-cairo2:select-font-face "Helvetica" :normal :normal)
    (cl-cairo2:set-line-width 1)
    (cl-cairo2:move-to 10 100)
    
    (let ((text "Ana are mere proaspete."))
      (cl-cairo2:show-text text)
      (multiple-value-bind
            (x_bearing y_bearing width height x_advance y_advance) (cl-cairo2:text-extents text)
        (cl-cairo2:rectangle 10.5  (+ 100.5 y_bearing) width height)
        (cl-cairo2:stroke))
      (format t "~a~%" (text-extents text))
      (format t "~a~%" (font-extents)))
    
    
    ;; (cl-cairo2:rectangle 0.5 0.5 200.5 100.5)
    ;; (cl-cairo2:set-source-rgb 0.3 0.7 0.7)
    ;; (cl-cairo2:fill-preserve)
    ;; (cl-cairo2:set-line-width .5)
    ;; (cl-cairo2:set-source-rgb 0.3 0.3 0.3)
    ;; (cl-cairo2:stroke)
    ;; (cl-cairo2:move-to 200 0)
    ;; (cl-cairo2:line-to 0 100)
    ;; (cl-cairo2:move-to 0 0)
    ;; (cl-cairo2:line-to 200 100)
    ;; (cl-cairo2:set-source-rgb 1 0.3 0.3)
    ;; (cl-cairo2:set-line-width 10)
    ;; (cl-cairo2:stroke)
    (cl-cairo2:destroy cl-cairo2:*context*)
    sdl-surf))

(defun render-scene (scene)
  (declare (ignorable scene))
  (let ((from-cairo (cairo-test)))
    (sdl:blit-surface from-cairo)
    (sdl:free from-cairo)
    (sdl:update-display)))

(defun run-scene (scene)
  (sdl:with-init ()
    (sdl:window *width* *height*)
    (setf (sdl:frame-rate) 60)
    (render-scene scene)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (when (sdl:key= key :sdl-key-escape)
                         (sdl:push-quit-event)))
      ;; (:idle (render-scene scene))
      (:video-expose-event () (sdl:update-display)))))