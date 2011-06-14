
(asdf:defsystem #:scenic
  :serial t
  :depends-on (#:lispbuilder-sdl
               #:cl-cairo2)
  :components ((:file "sdl-patch")
               (:file "packages")
               (:file "scenic-utils")
               (:file "scenic-images")
               (:file "scenic-events")
               (:file "scenic-classes")
               (:file "scenic-textbox")
               (:file "scenic-containers")
               (:file "scenic-grid")
               (:file "scenic-scene")
               (:file "scenic-decorators")
               (:file "scenic-buttons")
               (:file "scenic-scroll")
               (:file "scenic-helpers")
               (:file "scenic")
               (:file "scenic-test")))
