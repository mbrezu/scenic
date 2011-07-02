
(defpackage :scenic-utils
  (:use :cl)
  (:export max-box print-all draw-button-raw pass-to-child
           ifhorizontal aif awhen let-from-options
           fill-list it groups bif bwhen
           gen-print-object gen-serializer
           yes-no-query -> set2val1
           validate-layout-spec is-auto sorted-auto-indices
           intersperse))

(defpackage :scenic
  (:use :cl :scenic-utils)
  (:export run-scene
           eventful add-event-handler on-event
           widget measured-width measured-height
           name auto-name
           set-measured
           layout-left layout-top layout-width layout-height
           set-layout
           parent
           measure layout paint-scene
           container stack children space-between-cells
           padding left-padding top-padding right-padding bottom-padding child
           border stroke-color stroke-width
           background fill-color
           placeholder width height
           event handled
           mouse-event mouse-x mouse-y mouse-button modifiers mouse-move-event
           mouse-rel-x mouse-rel-y mouse-button-event
           widget-list
           key-event key
           scene invalidate-scene
           filler
           textattr
           label text font-face font-size font-color font-slant font-weight
           button
           clickable
           stateful state
           toggle-button
           slider min-value max-value page-size current-min-position
           sizer min-width min-height max-width max-height
           capture-mouse release-mouse
           invalidate
           arrow direction
           scrollbar
           orientable orientation
           image image-path get-image
           aligner
           clipper
           glass opacity
           henchman
           scroll-view horizontal-offset vertical-offset
           inside-width inside-height
           scroll-view-measured-event inner-width inner-height outer-width outer-height
           textbox cursor-position selection-start caret-color selection-color
           calculate-focusables
           *test-channel-enabled* *event-recording-enabled* *session-record*
           reset-event-log test-channel-write
           serialize
           replay-scene-session
           resource read-stream-to-string
           read-resource
           write-gzipped-resource
           read-gzipped-resource
           checkbox
           radio-button))

(defpackage :scenic-grid
  (:use :cl :scenic :scenic-utils)
  (:export grid))

(defpackage :scenic-helpers
  (:use :cl :scenic :scenic-grid :scenic-utils)
  (:export scene background border
           placeholder
           padding uniform-padding
           vertical-box horizontal-box
           filler
           stack
           label button button-text toggle
           horizontal-slider vertical-slider
           sizer arrow
           horizontal-scrollbar vertical-scrollbar
           image grid
           aligner
           clipper
           glass
           henchman
           scroll-view scroll-view-auto
           textbox
           checkbox
           radio-button
           group-stateful-buttons))

(defpackage :scenic-test
  (:use :cl :scenic-helpers :scenic-utils)
  (:export run-all-tests run-auto-tests))

