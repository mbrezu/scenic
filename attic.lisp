; drawing a line with alpha
(cl-cairo2:move-to (layout-left object) (layout-top object))
(cl-cairo2:line-to (+ (layout-left object) (layout-width object))
                   (+ (layout-top object) (layout-height object)))
(cl-cairo2:set-source-rgba 0 0 0 0.5)
(cl-cairo2:set-line-width 1)
(cl-cairo2:stroke)