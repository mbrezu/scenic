
(defpackage :scenic
  (:use :cl)
  (:export run-scene
           widget min-width
           min-height max-width max-height measured-width measured-height
           layout-left layout-top layout-width layout-height
           parent
           event-mouse-down event-mouse-up event-mouse-enter event-mouse-move event-mouse-leave
           event-key-down event-key-up event-got-focus event-lost-focus
           measure layout paint-scene
           container vertical-box stack children space-between-cells
           padding left-padding top-padding right-padding bottom-padding child
           border stroke-color stroke-width
           background fill-color
           placeholder width height
           event handled
           mouse-event mouse-x mouse-y mouse-button modifiers mouse-move-event
           mouse-rel-x mouse-rel-y mouse-button-event
           widget-list
           key-event key
           scene
           filler
           label text font-face font-size font-color font-slant font-weight
           horizontal-box
           button
           add-mouse-move add-mouse-enter add-mouse-leave))

(defpackage :scenic-macros
  (:use :cl :scenic)
  (:export bg border scene spc pad upad vbox flr stk lbl hbox btn btntxt))

(defpackage :scenic-test
  (:use :cl :scenic-macros))

