
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

(defun render-scene (scene)
  (unless  (sdl-surface scene)
    (setf (sdl-surface scene)
          (sdl:create-surface (width scene) (height scene))))
  (when (dirty scene)
    (setf (dirty scene) nil)
    (draw-with-cairo (sdl-surface scene)
      (measure-layout scene)
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

(defun run-scene (scene)
  (sdl:with-init ()
    (sdl:window (width scene) (height scene))
    (sdl-cffi:sdl-enable-key-repeat 80 80)
    (setf (sdl:frame-rate) 100)
    (calculate-focusables scene)
    (render-scene scene)
    (lispbuilder-sdl:enable-unicode)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle () (render-scene scene))
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
      (:key-down-event (:state state :scancode scancode :key key
                               :mod mod :mod-key mod-key :unicode unicode)
                       (declare (ignore state scancode mod))
                       (when (sdl:key= key :sdl-key-escape)
                         (sdl:push-quit-event))
                       (scene-on-key
                        scene
                        :key-down
                        (make-instance 'key-event
                                       :key (sdl-translate-key key)
                                       :modifiers (mapcar #'sdl-translate-key mod-key)
                                       :unicode (if (= 0 unicode)
                                                    nil
                                                    (code-char unicode)))))
      (:key-up-event (:state state :scancode scancode :key key
                             :mod mod :mod-key mod-key :unicode unicode)
                     (declare (ignore state scancode mod))
                     (scene-on-key
                      scene
                      :key-up
                      (make-instance 'key-event
                                     :key (sdl-translate-key key)
                                     :modifiers (mapcar #'sdl-translate-key mod-key)
                                     :unicode (if (= 0 unicode)
                                                    nil
                                                    (code-char unicode)))))
      (:video-expose-event () (sdl:update-display)))))

(defun sdl-translate-key (key)
  (intern (subseq (symbol-name key) 8) "KEYWORD"))