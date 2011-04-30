
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

(defvar *scenic-sdl-surface* nil)

(defun render-scene (scene)
  (when (null *scenic-sdl-surface*)
    (setf *scenic-sdl-surface*
          (sdl:create-surface (width scene) (height scene))))
  (when (dirty scene)
    (setf (dirty scene) nil)
    (draw-with-cairo *scenic-sdl-surface*
      (measure-layout scene)
      (paint-scene scene))
    (sdl:set-point-* *scenic-sdl-surface* :x 0 :y 0)
    (if (rectangle-to-redraw scene)
        (let* ((scene-rect (mapcar (lambda (value)
                                     (the fixnum (round value)))
                                   (rectangle-to-redraw scene)))
               (sdl-rect (sdl:rectangle :x (first scene-rect)
                                        :y (second scene-rect)
                                        :w (1+ (- (third scene-rect) (first scene-rect)))
                                        :h (1+ (- (fourth scene-rect) (second scene-rect)))
                                        :fp nil)))
          (sdl:set-clip-rect sdl-rect :surface lispbuilder-sdl:*default-display*)
          (sdl:blit-surface *scenic-sdl-surface* )
          (sdl:free sdl-rect)
          (sdl:clear-clip-rect lispbuilder-sdl:*default-display*)
          (setf (rectangle-to-redraw scene) nil))
        (sdl:blit-surface *scenic-sdl-surface* lispbuilder-sdl:*default-display*))
    (sdl:update-display)))

(defun run-scene (scene)
  (sdl:with-init ()
    (sdl:window (width scene) (height scene))
    (setf (sdl:frame-rate) 100)
    (render-scene scene)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle () (render-scene scene))
      (:key-down-event (:key key)
                       (when (sdl:key= key :sdl-key-escape)
                         (sdl:push-quit-event)))
      (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
                           (declare (ignore state))
                           (scene-on-mouse-move scene
                                                (make-instance 'mouse-move-event
                                                               :mouse-x x
                                                               :mouse-y y
                                                               :mouse-rel-x x-rel
                                                               :mouse-rel-y y-rel))
                           (render-scene scene))
      (:mouse-button-down-event (:button button :state state :x x :y y)
                                (declare (ignore state))
                                (scene-on-mouse-button scene
                                                       :mouse-button-down
                                                       (make-instance 'mouse-button-event
                                                                      :mouse-x x
                                                                      :mouse-y y
                                                                      :mouse-button button))
                                (render-scene scene))
      (:mouse-button-up-event (:button button :state state :x x :y y)
                              (declare (ignore state))
                              (scene-on-mouse-button scene
                                                     :mouse-button-up
                                                     (make-instance 'mouse-button-event
                                                                    :mouse-x x
                                                                    :mouse-y y
                                                                    :mouse-button button))
                              (render-scene scene))
      (:video-expose-event () (sdl:update-display)))))

