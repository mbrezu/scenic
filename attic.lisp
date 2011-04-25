;; This file isn't meant to be included in the Lisp image; it's just
;; old code that I still need close by.

(defun cairo-test ()
  (draw-with-cairo (sdl:create-surface 800 600)
    (cl-cairo2:set-source-rgb 0.2 0.2 1)
    (cl-cairo2:paint)
    (cl-cairo2:move-to 10 10.5)
    (cl-cairo2:line-to 100 10.5)
    (cl-cairo2:set-source-rgb 1 0.3 0.3)
    (cl-cairo2:set-line-width 1)
    (cl-cairo2:stroke)

    (cl-cairo2:set-font-size 40)
    (cl-cairo2:set-source-rgb 0 0 0)
    (cl-cairo2:select-font-face "Arial" :normal :normal)
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
    ))