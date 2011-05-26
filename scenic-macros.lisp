
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

(defun vbox (space layout-options children)
  (make-instance 'box
                 :space-between-cells space
                 :orientation :vertical
                 :layout-options layout-options
                 :children children))

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

(defun hbox (space layout-options children)
  (make-instance 'box
                 :space-between-cells space
                 :orientation :horizontal
                 :layout-options layout-options
                 :children children))

(defmacro btn (child)
  `(make-instance 'button
                  :child ,child))

(defmacro btntxt (text)
  `(btn (upad 2 (lbl ,text :size 18))))

(defmacro toggle (text)
  `(make-instance 'toggle-button
                  :state nil
                  :child (upad 2 (lbl ,text :size 18))))

(defmacro hslider (min max page)
  (let ((g-min (gensym "min")))
    `(let ((,g-min ,min))
       (make-instance 'slider
                      :orientation :horizontal
                      :min-value ,g-min
                      :max-value ,max
                      :page-size ,page
                      :current-min-position ,g-min))))

(defmacro szr (child &key (min-width nil) (min-height nil) (max-width nil) (max-height nil))
  `(make-instance 'sizer
                  :child ,child
                  :min-width ,min-width
                  :min-height ,min-height
                  :max-width ,max-width
                  :max-height ,max-height))

(defmacro arr (direction)
  `(make-instance 'arrow :direction ,direction))

(defmacro hsbar (min max page)
  (let ((g-min (gensym "min")))
    `(let ((,g-min ,min))
       (make-instance 'scrollbar
                      :orientation :horizontal
                      :min-value ,g-min
                      :max-value ,max
                      :page-size ,page
                      :current-min-position ,g-min))))

(defmacro vsbar (min max page)
  (let ((g-min (gensym "min")))
    `(let ((,g-min ,min))
       (make-instance 'scrollbar
                      :orientation :vertical
                      :min-value ,g-min
                      :max-value ,max
                      :page-size ,page
                      :current-min-position ,g-min))))

(defmacro img (image-path)
  `(make-instance 'image
                  :image (get-image ,image-path)))
