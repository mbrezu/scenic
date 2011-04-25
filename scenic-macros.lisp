
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

(defmacro layer (widget &body children)
  `(make-instance 'layer
                  :widget ,widget
                  :children ,children))

(defmacro scene (width height &body layers)
  `(make-instance 'scene
                  :width ,width
                  :height ,height
                  :layers (list ,@layers)))

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
  `(make-instance 'padding
                  :left-padding ,padding
                  :top-padding ,padding
                  :right-padding ,padding
                  :bottom-padding ,padding
                  :child ,(first child)))

(defmacro vbox (space &body children)
  `(make-instance 'vertical-box
                  :space-between-cells ,space
                  :children (list ,@children)))

(defmacro stk (&body children)
  `(make-instance 'stack
                  :children (list ,@children)))

(defmacro flr ()
  `(make-instance 'filler))