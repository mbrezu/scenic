
;; patch lispbuilder-sdl
(setf (symbol-function 'lispbuilder-sdl-base::create-surface)
      (lambda (width height
               &key (bpp 32) surface (type :sw)
               enable-color-key pixel-alpha enable-alpha rle-accel pixels pitch)
        (declare (ignore bpp pixel-alpha pixels pitch))
        "create a surface compatible with the supplied :surface, if provided."
        (let ((surf nil) (flags nil))
          (when enable-color-key
            (push sdl-cffi::SDL-SRC-COLOR-KEY flags))
          (when enable-alpha
            (push sdl-cffi::SDL-SRC-ALPHA flags))
          (when rle-accel
            (push sdl-cffi::SDL-RLE-ACCEL flags))
          (case type
            (:sw (push sdl-cffi::SDL-SW-SURFACE flags))
            (:hw (push sdl-cffi::SDL-HW-SURFACE flags)))
          (if (lispbuilder-sdl-base::is-valid-ptr surface)
              (error "shouldn't happen")
              (setf surf (sdl-cffi::SDL-Create-RGB-Surface (lispbuilder-sdl-base::set-flags flags)
                                                               width height
                                                               ;; bpp Rmask Gmask Bmask Amask
                                                               32
                                                               #x00ff0000
                                                               #x0000ff00
                                                               #x000000ff
                                                               0)))
          surf)))

