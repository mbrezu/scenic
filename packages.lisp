
(defpackage :scenic
  (:use :cl)
  (:export run-scene
           eventful add-event-handler on-event
           widget min-width
           min-height max-width max-height measured-width measured-height
           layout-left layout-top layout-width layout-height
           parent
           measure layout paint-scene
           container box stack children space-between-cells
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
           button
           clickable
           stateful state
           toggle-button
           horizontal-slider vertical-slider min-value max-value page-size current-min-position
           sizer min-width min-height max-width max-height
           capture-mouse release-mouse
           invalidate
           print-all
           arrow direction
           scrollbar))

(defpackage :scenic-macros
  (:use :cl :scenic)
  (:export bg border scene spc pad upad vbox flr stk lbl hbox
           btn btntxt toggle hslider szr arr hsbar vsbar))

(defpackage :scenic-test
  (:use :cl :scenic-macros))

