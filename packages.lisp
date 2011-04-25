
(defpackage :scenic
  (:use :cl)
  (:export run-scene
           widget min-width
           min-height max-width max-height measured-width measured-height
           layout-left layout-top layout-width layout-height
           parent
           event-mouse-down event-mouse-up event-mouse-enter event-mouse-move event-mouse-leave
           event-key-down event-key-up event-got-focus event-lost-focus
           measure layout paint
           container vertical-box stack children space-between-cells
           padding left-padding top-padding right-padding bottom-padding child
           border stroke-color stroke-width
           background fill-color
           placeholder width height
           event handled
           mouse-event mouse-x mouse-y mouse-button modifiers
           key-event key
           layer layers
           scene layers
           bg border layer scene spc pad upad vbox))

(defpackage :scenic-test
  (:use :cl))