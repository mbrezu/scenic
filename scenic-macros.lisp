
(in-package :scenic-macros)

(defmacro bg (color child)
  `(make-instance 'background
                  :fill-color ,color
                  :child ,child))

(defmacro border (color width child)
  `(make-instance 'border
                  :stroke-color ,color
                  :stroke-width ,width
                  :child ,child))

(defmacro scene (width height widget)
  `(make-instance 'scene
                  :width ,width
                  :height ,height
                  :widget ,widget))

(defmacro spc (width height)
  `(make-instance 'placeholder
                  :width ,width
                  :height ,height))

(defmacro pad (left top right bottom &body child)
  `(make-instance 'padding
                  :left-padding ,left
                  :top-padding ,top
                  :right-padding ,right
                  :bottom-padding ,bottom
                  :child ,(first child)))

(defmacro upad (padding &body child)
  (let ((g-padding (gensym "padding")))
    `(let ((,g-padding ,padding))
       (make-instance 'padding
                      :left-padding ,g-padding
                      :top-padding ,g-padding
                      :right-padding ,g-padding
                      :bottom-padding ,g-padding
                      :child ,(first child)))))

(defmacro vbox (space &body children)
  `(make-instance 'vertical-box
                  :space-between-cells ,space
                  :children (list ,@children)))

(defmacro stk (&body children)
  `(make-instance 'stack
                  :children (list ,@children)))

(defmacro flr ()
  `(make-instance 'filler))

(defmacro lbl (text &key
               (face "Arial") (size 12) (color '(list 0 0 0)) (slant :normal) (weight :normal))
  `(make-instance 'label
                  :text ,text
                  :font-face ,face
                  :font-size ,size
                  :font-color ,color
                  :font-slant ,slant
                  :font-weight ,weight))

(defmacro hbox (space &body children)
  `(make-instance 'horizontal-box
                  :space-between-cells ,space
                  :children (list ,@children)))

(defmacro btn (child)
  `(make-instance 'button
                  :child ,child))

(defmacro btntxt (text)
  `(btn (border (list 0.2 0.2 0.2)
                1
                (bg (list 0.7 0.7 0.7)
                    (upad 2 (lbl ,text :size 12))))))