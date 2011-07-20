
(asdf:defsystem #:scenic
  :serial t
  :depends-on (#:lispbuilder-sdl
               #:cl-cairo2
               #:gzip-stream
               #:bordeaux-threads)
  :components ((:file "packages")
               (:file "scenic-utils")
               (:file "scenic-resources")
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
               (:file "scenic-test")
               (:file "scenic-auto-test")))
